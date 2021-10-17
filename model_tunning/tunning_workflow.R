## Tunning workflow 
library(mlr3)
library(mlr3verse)

## Reading in the data 
data <- read.csv("full_table.csv") %>% select(-c(SYMBOL,Date,X))

data_no_na <- data[ , colSums(is.na(data)) == 0]


## Linear model with GDP model
model <- lm(ADJCLOSE~gdp,data=data_no_na)
summary(model)
#resid
data_no_na$residuals <- model$residuals

## Environment PCA
scaled_data <- data_no_na %>% select(-c(ADJCLOSE)) %>% scale()

model_x <- as.matrix(dplyr::select(data_no_na, -c(ADJCLOSE)))
pca <- prcomp(model_x, center = TRUE, scale = TRUE)
summary(pca)

components_1_4 <- as.data.frame(pca$x) %>% select(PC1,PC2,PC3,PC4) 
components_1_4$ADJCLOSE <- data_no_na$ADJCLOSE
components_1_4$residuals <- data_no_na$residuals

components_1_3 <- as.data.frame(pca$x) %>% select(PC1,PC2,PC3) 
components_1_3$ADJCLOSE <- data_no_na$ADJCLOSE
components_1_3$residuals <- data_no_na$residuals


# Setting up the Environment model xgboost PCA 1_3 -----------------------------------


## Setting up the data and task 
task <- TaskRegr$new("Enviornment", backend = components_1_4, target = "ADJCLOSE")

## splitting the data into a train test set
set.seed(100)
train_set = sample(task$nrow, 0.8 * task$nrow)
test_set = setdiff(seq_len(task$nrow), train_set)


# Tune Xgboost ------------------------------------------------------------

## setting tune budget 
stop_time = Sys.time()
stop_time <- stop_time + (60 *5) ## 15 min training budget 

termination <- trm("clock_time", stop_time = stop_time)

## tune framework
AT <- AutoTuner$new(
  learner = 
    lrn("regr.xgboost", eta = to_tune(.01,1),
        gamma = to_tune(0,10000),
        alpha = to_tune(p_int(0,10000)),
        max_depth = to_tune(p_int(2,10000)), 
        min_child_weight = to_tune(0,10000),
        max_delta_step = to_tune(0,10000),
        subsample = to_tune(.01,1),
        num_parallel_tree = to_tune(p_int(1,100) )
    ),
  resampling = rsmp("cv", folds = 5),
  measure = msr("regr.rmse"),
  terminator =  termination, 
  tuner = tnr("random_search")
  #  search_space = NULL,
  #  store_tuning_instance = TRUE,
  #  store_benchmark_result = TRUE,
  #  store_models = FALSE,
  #  check_values = FALSE
)

## Runs the Autotuner framework over the ranges in the def
AT$train(task, row_ids = train_set)
xgboost <- AT$model$learner

xgboost$predict(task, row_ids = test_set)


# Tune K-nearest-neighbors ---------------------------------------------------

## setting tune budget 
stop_time = Sys.time()
stop_time <- stop_time + (60 *5) ## 5 min training budget 

termination <- trm("clock_time", stop_time = stop_time)

## tune framework
AT2 <- AutoTuner$new(
  learner = 
    lrn("regr.kknn",
        k = to_tune(3,8), ## cannot be more than my obs
        distance = to_tune(1,20),
        kernel = to_tune(c("rectangular", "triangular", "epanechnikov", 
                           "biweight", "triweight", "cos", "inv", "gaussian", 
                           "rank", "optimal")),
        scale = to_tune(c(TRUE,FALSE))
    ),
  resampling = rsmp("cv", folds = 5),
  measure = msr("regr.rmse"),
  terminator =  termination, 
  tuner = tnr("random_search")
  #  search_space = NULL,
  #  store_tuning_instance = TRUE,
  #  store_benchmark_result = TRUE,
  #  store_models = FALSE,
  #  check_values = FALSE
)

## Runs the Autotuner framework over the ranges in the def
AT2$train(task, row_ids = train_set)
KNN <- AT2$model$learner


# benchmarking 2 models -------------------------------------------------

## The list of learners for benchmark
learners <- list(
  xgboost,                
  KNN)

## resampling pattern
resampling <- rsmp("cv",folds = 5)

## making the benchmark grid of algorithms, 
design = benchmark_grid(
  tasks = task,
  learners = learners,
  resamplings = resampling)

## Running the benchmark 
set.seed(100)
benchmark = benchmark(design)

measures = list(
  msr("regr.rmse", predict_sets = "test", id = "rmse_test")
)

## storing and printing the results 
results_1_4 = benchmark$aggregate(measures)


# 3 components ------------------------------------------------------------

# Setting up the Environment model xgboost PCA 1_3 ------------------------


## Setting up the data and task 
task <- TaskRegr$new("Enviornment", backend = components_1_3, target = "ADJCLOSE")

## splitting the data into a train test set
set.seed(100)
train_set = sample(task$nrow, 0.8 * task$nrow)
test_set = setdiff(seq_len(task$nrow), train_set)


# Tune Xgboost ------------------------------------------------------------

## setting tune budget 
stop_time = Sys.time()
stop_time <- stop_time + (60 *5) ## 15 min training budget 

termination <- trm("clock_time", stop_time = stop_time)

## tune framework
AT <- AutoTuner$new(
  learner = 
    lrn("regr.xgboost", eta = to_tune(.01,1),
        gamma = to_tune(0,10000),
        alpha = to_tune(p_int(0,10000)),
        max_depth = to_tune(p_int(2,10000)), 
        min_child_weight = to_tune(0,10000),
        max_delta_step = to_tune(0,10000),
        subsample = to_tune(.01,1),
        num_parallel_tree = to_tune(p_int(1,100) )
    ),
  resampling = rsmp("cv", folds = 5),
  measure = msr("regr.rmse"),
  terminator =  termination, 
  tuner = tnr("random_search")
  #  search_space = NULL,
  #  store_tuning_instance = TRUE,
  #  store_benchmark_result = TRUE,
  #  store_models = FALSE,
  #  check_values = FALSE
)

## Runs the Autotuner framework over the ranges in the def
AT$train(task, row_ids = train_set)
xgboost <- AT$model$learner

# Tune K-nearest-neighbors ---------------------------------------------------

## setting tune budget 
stop_time = Sys.time()
stop_time <- stop_time + (60 *5) ## 5 min training budget 

termination <- trm("clock_time", stop_time = stop_time)

## tune framework
AT2 <- AutoTuner$new(
  learner = 
    lrn("regr.kknn",
        k = to_tune(3,20), ## cannot be more than my obs
        distance = to_tune(1,8),
        kernel = to_tune(c("rectangular", "triangular", "epanechnikov", 
                           "biweight", "triweight", "cos", "inv", "gaussian", 
                           "rank", "optimal")),
        scale = to_tune(c(TRUE,FALSE))
    ),
  resampling = rsmp("cv", folds = 5),
  measure = msr("regr.rmse"),
  terminator =  termination, 
  tuner = tnr("random_search")
  #  search_space = NULL,
  #  store_tuning_instance = TRUE,
  #  store_benchmark_result = TRUE,
  #  store_models = FALSE,
  #  check_values = FALSE
)

## Runs the Autotuner framework over the ranges in the def
AT2$train(task, row_ids = train_set)
KNN <- AT2$model$learner


# benchmarking 2 models -------------------------------------------------

## The list of learners for benchmark
learners <- list(
  xgboost,                
  KNN)

## resampling pattern
resampling <- rsmp("cv",folds = 10)

## making the benchmark grid of algorithms, 
design = benchmark_grid(
  tasks = task,
  learners = learners,
  resamplings = resampling)

## Running the benchmark 
set.seed(100)
benchmark = benchmark(design)

measures = list(
  msr("regr.rmse", predict_sets = "test", id = "rmse_test")
)

## storing and printing the results 
performance_1_3 = benchmark$aggregate(measures)
print(performance_1_3)


# Predictive Model Tuning -------------------------------------------------
predictive_data <- components_1_3

predictive_data$gdp <- data_no_na$gdp


task_final <- TaskRegr$new("Enviornment", 
                           backend = predictive_data, 
                           target = "ADJCLOSE")

## splitting the data into a train test set
set.seed(100)
train_set = sample(task_final$nrow, 0.8 * task_final$nrow)
test_set = setdiff(seq_len(task_final$nrow), train_set)

## setting tune budget 
stop_time = Sys.time()
stop_time <- stop_time + (60 *15) ## 5 min training budget 

termination <- trm("clock_time", stop_time = stop_time)

## tune framework
AT2 <- AutoTuner$new(
  learner = 
    lrn("regr.kknn",
        k = to_tune(3,8), ## cannot be more than my obs
        distance = to_tune(1,50),
        kernel = to_tune(c("rectangular", "triangular", "epanechnikov", 
                           "biweight", "triweight", "cos", "inv", "gaussian", 
                           "rank", "optimal")),
        scale = to_tune(c(TRUE,FALSE))
    ),
  resampling = rsmp("cv", folds = 5),
  measure = msr("regr.rmse"),
  terminator =  termination, 
  tuner = tnr("random_search")
  #  search_space = NULL,
  #  store_tuning_instance = TRUE,
  #  store_benchmark_result = TRUE,
  #  store_models = FALSE,
  #  check_values = FALSE
)

## Runs the Autotuner framework over the ranges in the def
AT2$train(task, row_ids = train_set)
KNN_predictive <- AT2$model$learner

## setting tune budget 
stop_time = Sys.time()
stop_time <- stop_time + (60 *10) ## 15 min training budget 

termination <- trm("clock_time", stop_time = stop_time)

## tune framework
AT <- AutoTuner$new(
  learner = 
    lrn("regr.xgboost", eta = to_tune(.01,1),
        gamma = to_tune(0,10000),
        alpha = to_tune(p_int(0,10000)),
        max_depth = to_tune(p_int(2,10000)), 
        min_child_weight = to_tune(0,10000),
        max_delta_step = to_tune(0,10000),
        subsample = to_tune(.01,1),
        num_parallel_tree = to_tune(p_int(1,100) )
    ),
  resampling = rsmp("cv", folds = 5),
  measure = msr("regr.rmse"),
  terminator =  termination, 
  tuner = tnr("random_search")
  #  search_space = NULL,
  #  store_tuning_instance = TRUE,
  #  store_benchmark_result = TRUE,
  #  store_models = FALSE,
  #  check_values = FALSE
)

## Runs the Autotuner framework over the ranges in the def
AT$train(task, row_ids = train_set)
xgboost <- AT$model$learner

# benchmarking predictive models -------------------------------------------------

## The list of learners for benchmark
learners <- list(
  xgboost,                
  KNN)

## resampling pattern
resampling <- rsmp("cv",folds = 10)

## making the benchmark grid of algorithms, 
design = benchmark_grid(
  tasks = task,
  learners = learners,
  resamplings = resampling)

## Running the benchmark 
set.seed(100)
benchmark = benchmark(design)

measures = list(
  msr("regr.rmse", predict_sets = "test", id = "rmse_test")
)

## storing and printing the results 
performance_1_3 = benchmark$aggregate(measures)
print(performance_1_3)


