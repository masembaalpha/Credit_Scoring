#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 # CREDICT RISK ANALYSIS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data used in this post can be download from: 
# http://www.creditriskanalytics.net/uploads/1/9/5/1/19511601/hmeq.csv.
# Objective if this analysis is to create lending scores
 
 # LIBRARIES---- 
 library(tidyverse)
 library(magrittr)
 library(DataExplorer)
 library(mice)
 library(correlationfunnel)
 library(lime)
 
 # Parallel Processing
 library(parallel)
 library(doFuture)
 
 # Scoring
 library(scorecard)
 
 # Clear workspace: 
 rm(list = ls())

#  1.0: DATA PROCESSING----

# Import data: 
# hmeq <- read.csv("http://www.creditriskanalytics.net/uploads/1/9/5/1/19511601/hmeq.csv")
# hmeq <- read_rds("00_data/hmeq.rds") %>% 
#    mutate_at(c("BAD"),as_factor) %>% 
#    mutate(across(where(is.character),.fn=~if_else(.=="",NA,.)))
   


 ## 1.0.0 DATA EXPLORATION----
# Check missing values
# plot_missing(hmeq)
# # Check distribution of the categorical data
# plot_bar(hmeq)
# plot_histogram(hmeq,ncol = 5)

# 1.1 DATA MANAGEMENENT---
#
# ## 1.1.0 IMPUTATION----
# hmeq_tbl <- hmeq %>%
#   # Logical imputation
#   mutate(REASON=ifelse(!is.na(MORTDUE) & REASON=="","HomeImp",REASON)) %>%
#   # Drop all cases where data is insufficient: 50% of the rows have missing values
#   mutate(cols_missing_pct=rowSums(is.na(.))/ncol(.)) %>%
#   filter(cols_missing_pct<0.5) %>%
#   drop_columns("cols_missing_pct") %>%
#   mutate(across(where(is.character),as_factor))
#
# # Use MICE - Classification and Regression Trees (CART)to impute Missing values
#
# set.seed(123)
#
# imput_matrix <- mice(hmeq_tbl, method="cart")
#
# hmeq_complete_tbl <-imput_matrix %>%
#   complete()
# 
# plot_missing(hmeq_complete_tbl)
# 
# df <- hmeq_complete_tbl
# plot_histogram(df, ncol = 5)
# 
# 
# # 1.1.2 Export clean data
# 
# write_rds(df,"00_data/hmeq_clean_tbl.rds")
# 
# 
# 
# ##  1.1.2 Scenario 1: SELECT BEST PREDICTORS TO LOAN DEFAULT------

# Perform Step Wise - backward and forward process----
df <- read_rds("00_data/hmeq_clean_tbl.rds")


# Split our data: 
threshold <- 0.8
df_train <- df %>% 
  group_by(BAD) %>% 
  sample_frac(threshold) %>% 
  ungroup() # Use 80% data set for training model. 

df_test <- dplyr::setdiff(df, df_train) # Use 80% data set for validation. 

# df_train %>% write_rds("00_data/train_data.rds")
# df_test %>% write_rds("00_data/test_data.rds")

# set.seed(123)
# 
# 
# logistic_1 <-  glm(BAD ~ ., data = df %>% droplevels(), family = "binomial")
# #summary(logistic_1)
# 
# # Odd ratios
# logistic_1$coefficients %>% round(3)

# Stepwise logistic model
# stepwise_logit <- step(logistic_1,direction = "both")
# 
# predictors_selected <- stepwise_logit$model %>% colnames()
# 
# predictors_selected


# Data frame for training Logistic Regression: 
# df_train_tbl<- df_train %>% select(all_of(predictors_selected))
# 
# table(df_train_tbl$BAD)

# 2.0 SCORE CARD CALCULATION----

## 2.1 WEIGHT OF EVIDENCE (WOE) AND INFORMATION VALUE (IV) CALCULATION----

n_cores <- parallel::detectCores() # Determine the number of cores in a computer

# Generates optimal binning for numerical, factor and categorical variables: 
bins_var <- woebin(df_train, y = "BAD", no_cores = n_cores, positive = "BAD|1")

# Creates a data frame of binned variables for Logistic Regression: 
df_train_woe <- woebin_ply(df_train, bins_var, no_cores = n_cores)

# Logistic Regression:
logistic_2 <- glm(BAD ~ ., family = "binomial", data = df_train_woe)

# Show results: 
# logistic_2 %>% summary()

# Odd ratios
# logistic_2$coefficients %>% exp() %>% round(3)

# 
# write_rds(df_train_woe,"00_data/training_data_woe.rds")

#* The Value of current property has a lower contribution towards loan delinquency compared to the rest of the factors

# 2.2 SCORECARD CALCULATION 
score_card <- scorecard(bins_var, logistic_2, points0 = 600, odds0 = 20, pdo = 50)

# names(score_card)

# 3.0 Visualize Results----
## 3.1 Score card Table----

iv_for_predictors_point <- score_card %>% 
  bind_rows() %>% 
  dplyr::slice(-1) %>% 
  #select(-breaks, -is_special_values, -count, -count_distr, -neg, -pos, -posprob) %>% 
  select(variable, bin, woe, points, bin_iv, total_iv) %>% 
  mutate_if(is.numeric, function(x) {round(x, 3)}) %>% 
  mutate(bin = bin %>% 
           str_replace_all("\\[", "From ") %>% 
           str_replace_all("\\,", " to ") %>% 
           str_replace_all("\\)", ""))



iv_for_predictors_point %>% 
  knitr::kable(col.names = c("Predictor", "Group", "WOE", "Scorecard", "Bin IV", "Total IV"))


## 3.2  Visualize Information Values for predictors: -----

iv_values <- iv_for_predictors_point %>% 
      group_by(variable) %>% 
      summarise(iv_var = mean(total_iv)) %>% 
      ungroup() %>% 
      arrange(iv_var) %>% 
      mutate(variable = factor(variable, levels = variable)) 


theme_set(theme_minimal())

palette <- c("#377eb8","grey40")
iv_values %>%
  mutate(
    color_code = ifelse(iv_var<0.1,1,0) %>% as_factor()
  ) %>% 
  ggplot(aes(variable, iv_var, fill=color_code)) + 
  geom_col()+
  scale_fill_manual(values = palette)+
  coord_flip()+
  
  #geom_col(data = iv_values %>% filter(iv_var < 0.1), aes(variable, iv_var), fill = "grey60") + 
  geom_text( data=. %>% filter(color_code==0),
             aes(label = round(iv_var, 3)), hjust = -0.1, size = 5, color = "grey40") + 
  geom_text(data=. %>% filter(color_code==0), 
            aes(label = round(iv_var, 3)), hjust = -.1, size = 5, color = "#377eb8") + 
  labs(title = "Figure 1: Information Value (IV) for Variables", 
       x = NULL, y = "Information Value (IV)") + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.8)) + 
  theme(
    panel.grid.major.y = element_blank(),
    legend.position    = "none",
    plot.margin        = unit(c(1, 1, 1, 1), "cm")
    ) 


# Scorecard point for all observations from train data set: 
score_card_fun <- function(data,scorecard){
  data <- scorecard_ply(data, scorecard, only_total_score = FALSE, print_step = 0) %>% as.data.frame()
  return(data)
}

my_points_train <- df_train %>% score_card_fun(scorecard = score_card)

# Some statistics scorecard by group: 
df_scored_train <- df_train %>% 
  mutate(SCORE = my_points_train$score) %>% 
  mutate(BAD = case_when(BAD == 1 ~ "Default", TRUE ~ "NonDefault")) 


#Samplemcase
Case_test <-  df_test %>% slice(11) %>% 
  mutate(SCORE = 
           score_card_fun(., score_card)$score
         ) %>% 
  mutate(BAD = case_when(BAD == 1 ~ "Default", TRUE ~ "NonDefault"))

# df_scored_train %>% 
#   group_by(BAD) %>% 
#   summarise(across(SCORE,
#                      list(min=min, max=max, median=sd, mean=mean,count=NROW))) %>% 
#   #summarise_each(funs(min, max, median, mean, n()), SCORE) %>% 
#   mutate_if(is.numeric, function(x) {round(x, 0)}) %>% 
#   knitr::kable(caption = "Table 1: Scorecad Points by Group for Train Data")


mean_score_train <- df_scored_train %>% 
      group_by(BAD) %>% 
      summarise(tb = mean(SCORE)) %>% 
      ungroup()
Case_test_mean_score <- Case_test %>%
  group_by(BAD) %>% 
  summarise(tb = mean(SCORE)) %>% 
  ungroup()


lower_bound <- mean_score_train$tb[1] %>% round(0)
upper_bound <- mean_score_train$tb[2] %>% round(0)

case_bound <- Case_test_mean_score$tb[1] %>% round(0)



df_scored_train %>% 
  ggplot(aes(SCORE, color = BAD, fill = BAD)) + 
  geom_density(alpha = 0.3) + 
  geom_vline(aes(xintercept = lower_bound), linetype = "dashed", color = "red") + 
  geom_vline(aes(xintercept = upper_bound), linetype = "dashed", color = "blue") + 
  geom_vline(aes(xintercept = case_bound), 
             linetype = "solid",
             color = case_when(case_bound<lower_bound~"maroon",
                              case_bound>upper_bound~"darkgreen",
                              TRUE~"yellow")) + 
  geom_text(aes(x = lower_bound, y = 0.0042, label = lower_bound), color = "red", size = 4) + 
  geom_text(aes(x = upper_bound, y = 0.0042, label = upper_bound), color = "blue", size = 4) + 
  geom_text(aes(x = case_bound, y = 0.0032, 
                label = case_bound), size = 4,
            color = case_when(case_bound<lower_bound~"maroon",
                             case_bound>upper_bound~"darkgreen",
                             TRUE~"yellow")) + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = c(0.2, 0.8)) + 
  labs(x = NULL, y = NULL, 
       title   = "Figure 2: Score card Distribution by two Credit Groups for Train Data", 
       caption = "The scorecard point is a numeric expression measuring creditworthiness. 
       Commercial Banks usually utilize it as a method to support the decision-making about credit applications.")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Scorecard Points for test data set: 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   
# df_test <- df_test 
# 
# df_test_woe <- woebin_ply(df_test, bins_var, no_cores = n_cores)
# 
# my_points_test <- scorecard_ply(df_test, score_card, print_step = 0, 
#                                 only_total_score = FALSE) %>% as.data.frame()
# 
# # df_test_woe %>% write_rds("00_data/test_data_woe.rds")
# 
# df_scored_test <- df_test %>% 
#   mutate(SCORE = my_points_test$score) %>% 
#   mutate(BAD = case_when(BAD == 1 ~ "Default", TRUE ~ "NonDefault")) 
# 
# df_scored_test %>% 
#   group_by(BAD) %>% 
#   summarise(across(SCORE,
#                      list(min=min, max=max, median=sd, mean=mean,count=NROW))) %>% 
#   #summarise_each(funs(min, max, median, mean, n()), SCORE) %>% 
#   mutate_if(is.numeric, function(x) {round(x, 0)}) %>% 
#   knitr::kable(caption = "Table 2: Scorecad Points by Group for Test Data")


# 3.3 Explainer using LIME ----
library(parsnip)
library(h2o)
library(glue)
library(tidymodels)


h2o.init()
h2o_model <- h2o.loadModel("01_model/StackedEnsemble_AllModels_2_AutoML_1_20240109_90937/StackedEnsemble_AllModels_1_AutoML_1_20240416_82227")
xg_boost <- readRDS("01_model/xg_boost_model.rds")
# logistic_fit <-  logistic_model %>% fit(BAD~.,data=df_train_woe)

explanation <- function(train_data,test_data, model){
  explainer <- train_data  %>% 
    dplyr::select(-BAD) %>%
    lime::lime(model = model,
         n_bins = 4,
         bin_continuous = TRUE,
         quantile_bins = FALSE)
  
  
  explanation<- test_data %>% 
    dplyr::select(-BAD) %>% 
    dplyr::slice(11) %>% 
    lime::explain(explainer =  explainer,
                  n_labels = 1,
                  n_features = 10,
                  n_permutations = 500,
                  kernel_width = 1)
  return(explanation)
}

recipe <- recipe (BAD ~ ., 
                  data = df_train ) %>%
  step_dummy(REASON, JOB,one_hot = TRUE)

train_data <- recipe %>% prep() %>% bake( new_data = df_train ) 
test_data <- recipe %>% prep() %>% bake( new_data = df_test )

explanation <- explanation(
  train_data = train_data ,
  test_data = test_data,
  model = h2o_model
)



#plot_features(explanation = explanation, ncol = 4)


# select specific cases:

#Transformation

plot_features_contribution <- function(explanation,ncol){
  
  data_transformed <- explanation %>% 
    as_tibble() %>% 
    mutate(
      feature_desc=as_factor(feature_desc) %>% 
        fct_reorder(abs(feature_weight),.desc = FALSE),
      key=ifelse(feature_weight>0,"Supports","Contradicts") %>% 
        fct_relevel("Supports"),
      case_text=glue("Default probability explanation for Customer: {case}"),
      label_text=glue("Classifed as: { ifelse(label=='p1','Default', 'Non - default')} "),
      prob_text=glue("Probability of classification: {round(label_prob,1) %>% scales::percent()}"),
      r2_text =glue("Model Fit r2: {model_r2 %>% round(2)}")
    ) %>% 
    dplyr::select(feature_desc,feature_weight,key,case_text:r2_text)
  
  data_transformed %>% 
    ggplot(aes(feature_desc,feature_weight,fill=key))+
    geom_col() + 
    coord_flip()+
    scale_fill_manual(values = palette)+
    labs(y="Weight",x="Feature")+
    facet_wrap(~case_text+label_text+prob_text+r2_text,
               ncol=ncol,scales="free")
}

explanation %>% #filter( case %in% 10) %>% 
  plot_features_contribution(ncol=2)


# End of explainer-----
# 
# 
# df_scored_test %>% 
#   group_by(BAD) %>% 
#   summarise(tb = mean(SCORE)) %>% 
#   ungroup() -> mean_score_test
# 
# 
# df_scored_test %>% 
#   ggplot(aes(SCORE, color = BAD, fill = BAD)) + 
#   geom_density(alpha = 0.3) + 
#   geom_vline(aes(xintercept = lower_bound), linetype = "dashed", color = "red") + 
#   geom_vline(aes(xintercept = upper_bound), linetype = "dashed", color = "blue") + 
#   geom_text(aes(x = lower_bound, y = 0.0042, label = mean_score_test$tb[1] %>% round(0)), color = "red", size = 4) + 
#   geom_text(aes(x = upper_bound, y = 0.0042, label = mean_score_test$tb[2] %>% round(0)), color = "blue", size = 4) + 
#   theme(legend.title = element_blank()) + 
#   theme(legend.position = c(0.2, 0.8)) + 
#   labs(x = NULL, y = NULL, 
#        title    = "Figure 3: Scorecard Distribution by two Credit Groups for Test Data", 
#        caption  = "The scorecard point is a numeric expression measuring creditworthiness.
#        Commercial Banks usually utilize it as a method to support the decision-making about credit applications.")
# 
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #   ### Some Criteria for Model Evaluation in Context of Credit Scoring
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # - Gain or **lift** is a measure of the effectiveness of a classification model calculated 
# # as the ratio between the results obtained with and without the model. Gain and lift charts 
# # are visual aids for evaluating performance of classification models. However, in contrast 
# # to the confusion matrix that evaluates models on the whole population gain or lift chart 
# # evaluates model performance in a portion of the population.
# # 
# # - K-S or **Kolmogorov-Smirnov chart** measures performance of classification models. More 
# # accurately, K-S is a measure of the degree of separation between the positive and negative 
# # distributions. The K-S is 100 if the scores partition the population into two separate groups 
# # in which one group contains all the positives and the other all the negatives. On the other hand, 
# # If the model cannot differentiate between positives and negatives, then it is as if the model 
# # selects cases randomly from the population. The K-S would be 0. In most classification models the 
# # K-S will fall between 0 and 100, and that the higher the value the better the model is at separating 
# # the positive from negative cases.
# # 
# # - **Area under ROC curve** is often used as a measure of quality of the classification models. 
# # A random classifier has an area under the curve of 0.5, while AUC for a perfect classifier is 
# # equal to 1. In practice, most of the classification models have an AUC between 0.5 and 1.
# # 
# # - **Precision** and **Recall** (for more detail: https://en.wikipedia.org/wiki/Precision_and_recall). 
# # 
# # Model performance criteria will be presented in the following code chunk. 
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Convert to binned data frame for test data: 
df_test_woe <- woebin_ply(df_test, bins_var)
# 
# Calculate probability of default (PD) for observations belonging test data:
test_pred <- predict(logistic_2, df_test_woe, type = "response") %>% as_tibble() %>% 
 mutate(value=case_when(value > 0.6 ~ 1, TRUE~0) %>% as_factor()) 
# Model Performance for test data:

h2o_pred <- h2o_model%>% h2o.predict(test_data %>% as.h2o()) %>% as_tibble()

library(caret)

test_data 

confusionMatrix(h2o_pred$predict, test_data$BAD)

perf_eva(h2o_pred$predict, test_data$BAD,
         show_plot = c("ks", "lift"),
         title = "Test Data")
# 
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #   ### Probabilities of Default
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create a data frame of PD:
prob_default_df <- df_test %>%
  mutate(prob_default = test_pred$value,
         BAD = case_when(BAD == 1 ~ "Default", TRUE ~ "NonDefault"))
# Show some statistics:

prob_default_df %>%
  group_by(BAD) %>%
  summarise_each(funs(min, max, median, mean, n()), prob_default) %>%
  mutate_if(is.numeric, function(x) {round(x, 4)}) %>%
  knitr::kable(caption = "Table 3: Probabilities of Default by Group for Test Data")
# 
# 
# # PD Distribution by group: 
# 
# prob_default_df %>% 
#   group_by(BAD) %>% 
#   summarise(tb = mean(prob_default)) %>% 
#   ungroup() -> mean_prob_test
# 
# prob_default_df %>% 
#   ggplot(aes(prob_default, color = BAD, fill = BAD)) + 
#   geom_density(alpha = 0.3) + 
#   geom_vline(aes(xintercept = mean_prob_test$tb[1]), linetype = "dashed", color = "red") + 
#   geom_vline(aes(xintercept = mean_prob_test$tb[2]), linetype = "dashed", color = "blue") + 
#   geom_text(aes(x = 0.44, y = 9.2, label = mean_prob_test$tb[1] %>% round(4)), color = "red", size = 4) + 
#   geom_text(aes(x = 0.1 + 0.07, y = 9.2, 
#                 label = mean_prob_test$tb[2] %>% round(4)), color = "blue", size = 4) + 
#   theme(legend.title = element_blank()) + 
#   theme(legend.position = c(0.88, 0.8)) + 
#   theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) + 
#   labs(x = NULL, y = NULL, title = "Figure 4: PD Distribution by two Credit Groups for Test Data") 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  ### Expected Loss
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Notes:
#   1. Here we compute the loss given default and equate it to 1 - a 0% recovery rate (RR)
#       (we assume that given a default, entire loan is a loss with no recoverable amounts)
#   2. In this case, the loan amount becomes the exposure at default (EAD)
#   3. We then calculate the expected loss: EL=PD*LGD*EAD 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

## add a new column LGD using add_column()

prob_default_df <- prob_default_df %>%
  add_column(LGD = "1")

head(prob_default_df)

# we calculate the expected loss by
prob_default_df$EL = prob_default_df$prob_default*as.numeric(prob_default_df$LGD)*prob_default_df$LOAN

# Print the total portfolio size
Portfolio = sum(prob_default_df$LOAN)

# Print the sum of the expected loss
Expected_Loss = sum(prob_default_df$EL)

# Print the loss ratio
LR = Expected_Loss/Portfolio
