library(h2o)
library(recipes)
library(tidyverse)
library(tidymodels)
library(parsnip)
library(rsample)
library(workflows)
library(caret)
library(xgboost)

data_train <- read_rds("00_data/train_data.rds")

data_test <- read_rds("00_data/test_data.rds")

recipe <- recipe (BAD ~ ., 
                  data = data_train ) %>%
    step_dummy(REASON, JOB,one_hot = TRUE)

recipe %>% prep() %>% bake( new_data = data_train ) %>% glimpse()

data_train <- recipe %>% prep() %>% bake( new_data = data_train )
data_test <- recipe %>% prep() %>% bake( new_data = data_test )

h2o.init()

y <- "BAD"
x <-  setdiff(y = y,names(data_train))


automl_models_h2o <- h2o.automl(x,y,
                                training_frame = data_train %>% as.h2o(),
                                max_runtime_secs = 20,
                                nfolds = 5
                                )

automl_models_h2o @ leaderboard


h2o.get_best_model(automl_models_h2o) %>% h2o.saveModel("01_model/StackedEnsemble_AllModels_2_AutoML_1_20240109_90937")

# h2o_model <- h2o.loadModel("01_model/StackedEnsemble_AllModels_2_AutoML_1_20240109_90937/StackedEnsemble_AllModels_2_AutoML_1_20240109_90937")

h2o_model <- h2o.loadModel("01_model/StackedEnsemble_AllModels_2_AutoML_1_20240109_90937/StackedEnsemble_AllModels_1_AutoML_1_20240416_82227")


# H20 MODEL EVALUATION----
h2o_predict <- h2o_model %>% h2o.predict(data_test %>% as.h2o()) %>% as_tibble()

h2o_eval <- tibble(predicted=h2o_predict$predict, actual=data_test$BAD)

confusionMatrix(h2o_eval$actual,h2o_eval$predicted)

# XGBOOST Model----

# Level 1 tune:----
boost_tree <- boost_tree(
    learn_rate = tune(),
    mtry       = tune(),
    trees      = 300,
    mode       = "classification"
) %>%
    set_engine("xgboost") 

wflw_spec_boost_tune <- workflow() %>% 
    add_model(boost_tree) %>% 
    add_recipe(recipe) 

set.seed(123)

resamples_kfold <- data_train %>% 
    vfold_cv(v=10)

tune <- tune_grid(
    object = wflw_spec_boost_tune,
    resamples = resamples_kfold,
    param_info = parameters( wflw_spec_boost_tune) %>% 
        update(learn_rate=learn_rate(range=c(0.1,0.5)), mtry = mtry(range=c(1,20))),
    grid = 10,
    control = control_grid(verbose=T, allow_par = T)
)

best_results <- tune %>% show_best(metric="accuracy", n=5)
best_results

# Level 2 tune----

boost_tree_1 <- boost_tree(
    mode       = "classification",
    learn_rate = best_results$learn_rate[1],
    mtry       = best_results$mtry[1],
    trees      = tune(),
    min_n      = tune(),
    tree_depth = tune(),
    loss_reduction = tune()
    ) %>% 
    set_engine("xgboost")

wflw_spec_boost_tune_1 <- workflow() %>% 
    add_model(boost_tree) %>% 
    add_recipe(recipe)

tune_1 <- tune_grid( 
    object = wflw_spec_boost_tune_1,
    resamples = resamples_kfold,
    param_info = parameters(wflw_spec_boost_tune_1),
    grid = 10,
    control = control_grid(verbose = T, allow_par = T)
)

best_results <- tune_1 %>% show_best(metric="accuracy",n=5)
best_results %>% dplyr::slice(1)

xg_boost_final_wflw <- wflw_spec_boost_tune_1 %>% 
    finalize_workflow(parameters= best_results %>% dplyr::slice(1)) %>% 
    fit(data_train)


saveRDS(xg_boost_final_wflw ,"01_model/xg_boost_model.rds")

# XGBOOST EVALUATION----
prediction_xg_boost <- xg_boost_final_wflw  %>% predict(data_test) %>% as_tibble()

eval_data <- tibble(estimate = prediction_xg_boost$.pred_class,truth = data_test$BAD)

eval_data %>% yardstick::metrics(truth, estimate)

confusionMatrix(eval_data$truth, eval_data$estimate)

