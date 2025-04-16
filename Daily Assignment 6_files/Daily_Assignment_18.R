library(tidyverse)
library(tidymodels)
library(xgboost)
library(ranger)

#URLs
covid_url <-  'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv'
pop_url   <- 'https://raw.githubusercontent.com/mikejohnson51/csu-ess-330/refs/heads/main/resources/co-est2023-alldata.csv'

#Ingest
data   <- readr::read_csv(covid_url)
census <- readr::read_csv(pop_url) 


library(dplyr)

covid_latest <- data %>%
  group_by(state) %>%
  filter(date == max(date)) %>%
  select(state, deaths, cases)

census_clean <- census %>%
  select(STNAME, CTYNAME, POPESTIMATE2023) %>%
  rename(state = STNAME)

merged_data <- covid_latest %>%
  left_join(census_clean, by = "state") %>%
  drop_na()


#splitting data
set.seed(123)
data_split <- initial_split(merged_data, prop = 0.8)
train_data <- training(data_split)
test_data  <- testing(data_split)

#recipe
rec <- recipe(deaths ~ cases + POPESTIMATE2023, data = train_data) %>%
  step_log(cases, base = 10) %>%
  step_log(POPESTIMATE2023, base = 10) %>%
  step_normalize(all_predictors())

#models
rf_model <- rand_forest(trees = 500) %>%
  set_mode("regression") %>%
  set_engine("ranger")

xgb_model <- boost_tree(trees = 500, learn_rate = 0.1) %>%
  set_mode("regression") %>%
  set_engine("xgboost")

lm_model <- linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")


#workflows
rf_wf <- workflow() %>% add_recipe(rec) %>% add_model(rf_model)
xgb_wf <- workflow() %>% add_recipe(rec) %>% add_model(xgb_model)
lm_wf <- workflow() %>% add_recipe(rec) %>% add_model(lm_model)

#fit models
rf_fit <- rf_wf %>% fit(data = train_data)
xgb_fit <- xgb_wf %>% fit(data = train_data)
lm_fit <- lm_wf %>% fit(data = train_data) 


#evaluation
test_data <- test_data %>% ungroup()

test_results <- test_data %>%
  mutate(
    rf_pred  = predict(rf_fit, new_data = test_data)$.pred,
    xgb_pred = predict(xgb_fit, new_data = test_data)$.pred,
    lm_pred  = predict(lm_fit, new_data = test_data)$.pred
  ) %>%
  select(deaths, rf_pred, xgb_pred, lm_pred)

metrics <- metric_set(rmse, rsq)
eval_results <- bind_rows(
  metrics(test_results, truth = deaths, estimate = rf_pred) %>% mutate(model = "Random Forest"),
  metrics(test_results, truth = deaths, estimate = xgb_pred) %>% mutate(model = "XGBoost"),
  metrics(test_results, truth = deaths, estimate = lm_pred) %>% mutate(model = "Linear Regression")
)

print(eval_results)


ggplot(test_results, aes(x = deaths)) +
  geom_point(aes(y = rf_pred, color = "Random Forest")) +
  geom_point(aes(y = xgb_pred, color = "XGBoost")) +
  geom_point(aes(y = lm_pred, color = "Linear Regression")) +
  labs(y = "Predicted Deaths", x = "True Deaths", color = "Model") +
  theme_minimal()
