#Daily Assignment 15
install.packages('palmerpenguins')
install.packages("tidyverse")

library(palmerpenguins)
library(tidyverse)

?penguins

tidyverse_packages()

set.seed(123)


data("penguins")

penguins_split <- initial_split(penguins, prop = 0.7)

train_data <- training(split)
test_data <- testing(split)

penguin_folds <- vfold_cv(train_data, v = 10)

penguin_folds



#Daily Assignment 16
train_data_clean <- na.omit(train_data)
test_data_clean <- na.omit(test_data)

penguin_folds <- vfold_cv(train_data_clean, v = 10)

log_reg_model <- multinom_reg(mode = "classification") %>%
  set_engine("nnet")

rand_forest_model <- rand_forest(mode = "classification") %>%
  set_engine("randomForest")

wf_obj <- workflow_set(
  preproc = list(recipe(species ~ ., data = train_data_clean)),
  models = list(log_reg = log_reg_model, rf = rand_forest_model),
  cross = TRUE
)

wf_obj <- wf_obj %>%
  workflow_map("fit_resamples", resamples = penguin_folds)

metrics_results <- wf_obj %>%
  collect_metrics() %>%
  filter(.metric == "accuracy")

metrics_results


#Based on the tibble showing accuracy metrics, the random forest model is better than the logistic regression model and would be the best choice for this problem.
#the random forest model appears to have the highest accuracy (0.9957) compared to the logistic regression model (0.9915).
