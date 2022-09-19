rm(list = ls())
library(data.table)
library(tidyverse)
library(data.table)
library(mlr3)
library(mlr3learners)
library(mlr3extralearners) 
library(mlr3verse)
library(mlr3tuning)
library(fastDummies)
library(mlr3pipelines)
library(lubridate)
data = read_csv("R/ttnk/train.csv", show_col_types = FALSE)
test_data = read_csv("R/ttnk/test.csv", show_col_types = FALSE)

tidy_data = function(dt){
  colnames(dt) = make.names(colnames(dt), unique = T)
  
  dt = as.data.table(dt)
  dt %>%
    select_if(is.numeric) %>%
    setnafill("locf") -> data_with_numeric
  
  dt %>%
    select_if(
      \(x) !is.numeric(x)
    ) -> data_with_fac
  
  data_with_fac[is.na(data_with_fac)]="unknow"
  dt = cbind(data_with_numeric, data_with_fac)
  
  dt= dt %>%
    mutate_if(
      \(x){
        (class(x) == "character" || class(x)  == "logical") && n_distinct(x) <= 5
      },
      as.factor
    ) %>%
    select_if(
      \(x) !is_character(x)
    ) %>%
    dummy_columns(
      remove_most_frequent_dummy=TRUE, 
      remove_selected_columns=TRUE
    ) %>%
    mutate_if(
      is.numeric,
      as.double
    )
  colnames(dt) = make.names(colnames(dt), unique = T)
  return(dt)
}
data = tidy_data(data)
test_data = tidy_data(test_data)

tidy_ans = function(predict_result){
  predict_result %>%
    as.data.table() %>%
    mutate(
      Transported = if_else(
        response == 1,
        "False",
        "True"
      ),
      PassengerId = read_csv("R/ttnk/test.csv", show_col_types = FALSE) %>% 
        select(PassengerId) %>% 
        pull()
    ) %>%
    select(PassengerId, Transported)
}

task = as_task_classif(data, target = "Transported_FALSE", id="ttnk_TASK")
learner = lrn("classif.lightgbm")
set_threads(learner, n=8)

measure = msr("classif.acc")
cv = rsmp("cv", folds=5)
hout = rsmp("holdout")
terminator = trm("evals", n_evals = 20)
fselector = fs("random_search")

instance_ft = FSelectInstanceSingleCrit$new(
  task=task,
  learner = learner,
  resampling = hout,
  measure = measure,
  terminator = terminator,
)
fselector$optimize(instance_ft)
task$select(instance_ft$result_feature_set)


search_space = ps(
  num_iterations = p_int(lower=1, upper = 1000),
  learning_rate = p_dbl(lower=0.01, upper = 0.2)
)

tuner = tnr("grid_search", resolution = 10)
final_model = AutoTuner$new(
  learner = learner,
  resampling = cv,
  measure = measure,
  search_space = search_space,
  terminator = trm("evals", n_evals = 100),
  tuner = tuner
)  
final_model$train(task)
final_model$predict_newdata(test_data) -> predict_result

tidy_ans(predict_result) %>% fwrite("ans_with_fs.csv")


##===========================================================
### 建立管道
#### 所有pipe的参数
as.data.table(mlr_pipeops)

#### 直接构建pipe
pca = po("pca")
learner = po("learner", lrn("classif.rpart"))

filter = po(
  "filter",
  filter = mlr3filters::flt("variance"),
  # 指定filter的参数
  param_vals = list(filter.frac = 0.5)
)

flt("variance")$param_set %>% as.data.table()

##====================================================
## %>>%
library("magrittr")

gr = po("scale") %>>% po("pca")
gr$plot(html = FALSE)

mutate = po("mutate")
graph = mutate %>>% filter
graph$plot()


### 建立分支
graph = Graph$new()$
  add_pipeop(mutate)$
  add_pipeop(filter)$
  add_edge("mutate", "variance")
graph$add_pipeop(po("pca"))
graph$add_pipeop(po("pca", id = "pca2"))
graph$plot()

##==========================================================
##正式建模
mutate = po("mutate")
filter = po("filter",
            filter = mlr3filters::flt("variance"),
            param_vals = list(filter.frac = 0.5))

graph = mutate %>>%
  filter %>>%
  po("learner",
     learner = lrn("classif.rpart"))


task = tsk("iris")
# graph$train(task)
# graph$predict(task)

### 推荐写法
glrn = as_learner(graph)
cv3 = rsmp("cv", folds = 3)
resample(task, glrn, cv3)


### 设置超参数
glrn$param_set$values$variance.filter.frac = 0.25
cv3 = rsmp("cv", folds = 3)
resample(task, glrn, cv3)

### 搜索超参数
ps = ps(
  classif.rpart.cp = p_dbl(lower = 0, upper = 0.05),
  variance.filter.frac = p_dbl(lower = 0.25, upper = 1)
)
instance = TuningInstanceSingleCrit$new(
  task = task,
  learner = glrn,
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  search_space = ps,
  terminator = trm("evals", n_evals = 20)
)
tuner = tnr("random_search")
tuner$optimize(instance)

glrn$param_set$values = instance$result_learner_param_vals
glrn$train(task)
glrn$predict(task)$score(msr("classif.acc"))

##=================================================================
### 集成模型
### bagging

task = tsk("iris")
train.idx = sample(seq_len(task$nrow), 120)
test.idx = setdiff(seq_len(task$nrow), train.idx)

single_pred = po("subsample", frac = 0.7) %>>%
  po(
    "filter",
    filter = mlr3filters::flt("variance"),
    param_vals = list(filter.frac = 0.5)
  ) %>>%
  po("learner", lrn("classif.rpart"))

## 复制10份
pred_set = ppl("greplicate", single_pred, 10L)
bagging = pred_set %>>%
  po("classifavg", innum = 10)
bagging$plot()

baglrn = as_learner(bagging)

str_flatten(
  c(
    str_c("classif.rpart_", 1:10, ".cp = p_dbl(lower = 0, upper = 0.05)"),
    str_c("variance_", 1:10, ".filter.frac = p_dbl(lower = 0.25, upper = 1)")
  ),
  collapse = ", "
) -> temp_str

str_c(
  "ps(", temp_str, ")"
) %>% parse(text = .) %>% eval() -> ps

  
# ps = ps(
#   classif.rpart_1.cp = p_dbl(lower = 0, upper = 0.05),
#   variance_1.filter.frac = p_dbl(lower = 0.25, upper = 1)
# )
instance = TuningInstanceSingleCrit$new(
  task = task,
  learner = baglrn,
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  search_space = ps,
  terminator = trm("evals", n_evals = 100)
)
tuner = tnr("random_search")
tuner$optimize(instance)

baglrn$param_set$values = instance$result_learner_param_vals
baglrn$train(task)
baglrn$predict(task)$score(msr("classif.acc"))


#####################################################################
## stacking
task = as_task_classif(data, target = "Transported_FALSE", id="ttnk_TASK")

filter_rpart = po("filter", filter = mlr3filters::flt("variance"), id = "rpart_filter", filter.frac = 0.5)
filter_lgbm = po("filter", filter = mlr3filters::flt("variance"), id = "lgbm_filter", filter.frac = 0.5)
filter_abm = po("filter", filter = mlr3filters::flt("variance"), id = "abm_filter", filter.frac = 0.5)
filter_fls = po("filter", filter = mlr3filters::flt("variance"), id = "fls_filter", filter.frac = 0.5)
filter_PART = po("filter", filter = mlr3filters::flt("variance"), id = "PART_filter", filter.frac = 0.5)
filter_fnn = po("filter", filter = mlr3filters::flt("variance"), id = "fnn_filter", filter.frac = 0.5)

lrn_0 = filter_rpart %>>% 
  po("learner_cv", lrn("classif.rpart", predict_type = "prob"), id = "rpart_cv", resampling.folds=5)
lrn_1 = filter_lgbm %>>% 
  po("learner_cv", lrn("classif.lightgbm", predict_type = "prob"), id = "lgbm_cv", resampling.folds=5)
lrn_2 = filter_abm %>>% 
  po("learner_cv", lrn("classif.AdaBoostM1", predict_type = "prob"), id = "abm_cv", resampling.folds=5)
lrn_4 = filter_fls %>>% 
  po("learner_cv", lrn("classif.featureless", predict_type = "prob"), id = "fls_cv", resampling.folds=5)
lrn_5 = filter_PART %>>% 
  po("learner_cv", lrn("classif.PART", predict_type = "prob"), id = "PART_cv", resampling.folds=5)
lrn_6 = filter_fnn %>>% 
  po("learner_cv", lrn("classif.fnn", predict_type = "prob"), id = "fnn_cv", resampling.folds=5)

level_0 = gunion(list(
  lrn_0, 
  lrn_1, 
  lrn_2, 
  lrn_4, 
  lrn_5,
  lrn_6
))

combined = level_0 %>>% po("featureunion", 6) %>>% 
  po("filter", filter = mlr3filters::flt("find_correlation"), id = "cor_filter", filter.frac=0.5)
stack = combined %>>% 
  po("learner", lrn("classif.log_reg"))
stack$plot(html = FALSE)

stack = as_learner(stack)
stack$param_set %>% as.data.table() %>% view()

param_search_space = ps(
  rpart_filter.filter.frac = p_dbl(0, 1),
  lgbm_filter.filter.frac = p_dbl(0, 1),
  abm_filter.filter.frac = p_dbl(0, 1),
  fls_filter.filter.frac = p_dbl(0, 1),
  cor_filter.filter.frac = p_dbl(0, 1),
  fnn_filter.filter.frac = p_dbl(0, 1),
  rpart_cv.cp = p_dbl(0, 1),
  rpart_cv.maxdepth = p_int(1, 30),
  lgbm_cv.num_iterations = p_int(1, 1000)
)

instance = TuningInstanceSingleCrit$new(
  task = task,
  learner = stack,
  resampling = rsmp("holdout"),
  measure = msr("classif.acc"),
  search_space = param_search_space,
  # terminator = trm("clock_time", stop_time = ymd_hms("20220916 20:45:00", tz="Asia/Shanghai"))
  terminator = trm("run_time", secs = 1800)
)
tuner = tnr("random_search")
tuner$optimize(instance)

stack$param_set$values = instance$result_learner_param_vals
stack$train(task)
stack$predict_newdata(test_data) %>% tidy_ans() %>% fwrite("ans_with_stacking-new.csv")





















