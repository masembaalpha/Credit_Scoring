# https://www.r-bloggers.com/2020/05/using-xgboost-with-tidymodels/#:~:text=We%20use%20the%20parsnip%20package,tuning%20in%20a%20subsequent%20step.


# data
library(AmesHousing)

# data cleaning
library(janitor)

# data prep
library(dplyr)

# tidymodels
library(rsample)
library(recipes)
library(parsnip)
library(tune)
library(dials)
library(workflows)
library(yardstick)

# speed up computation with parrallel processing (optional)
library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)


# set the random seed so we can reproduce any simulated results.
set.seed(1234)

# load the housing data and clean names
ames_data <- make_ames() %>%
    janitor::clean_names()