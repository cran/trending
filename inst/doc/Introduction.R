## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  fig.width = 7,
  fig.height = 5
)

## ----message=FALSE------------------------------------------------------------
library(outbreaks)  # for data
library(trending)   # for trend fitting
library(dplyr)      # for data manipulation

# load data
data(covid19_england_nhscalls_2020)

# select 6 weeks of data (from a period when the prevalence was decreasing)
last_date <- as.Date("2020-05-28")
first_date <- last_date - 8*7
pathways_recent <-
  covid19_england_nhscalls_2020 %>%
  filter(date >= first_date, date <= last_date) %>%
  group_by(date, day, weekday) %>%
  summarise(count = sum(count), .groups = "drop")

# split data for fitting and prediction
dat <-
  pathways_recent %>%
  group_by(date <= first_date + 6*7) %>%
  group_split()

fitting_data <- dat[[2]]
pred_data <- select(dat[[1]], date, day, weekday)

## -----------------------------------------------------------------------------
(model  <- glm_nb_model(count ~ day + weekday))
(fitted_model <- fit(model, fitting_data))
fitted_model %>% get_result()

# default
fitted_model %>% 
  predict(pred_data) %>%
  get_result()

# without uncertainty
fitted_model %>% 
  predict(pred_data, uncertain = FALSE) %>% 
  get_result()

# without prediction intervals
fitted_model %>% 
  predict(pred_data, add_pi = FALSE) %>% 
  get_result()

# bootstraped prediction intervals
fitted_model %>% 
  predict(pred_data, simulate_pi = TRUE) %>% 
  get_result()

## -----------------------------------------------------------------------------
(model2  <- glm_nb_model(count ~ day + nonexistent))
(fitted_model2 <- fit(model2, fitting_data))
get_result(fitted_model2)
get_errors(fitted_model2)

## -----------------------------------------------------------------------------
models  <- list(
  simple = lm_model(count ~ day),
  glm_poisson = glm_model(count ~ day, family = "poisson"),
  glm_negbin = glm_nb_model(count ~ day + weekday),
  will_error = glm_nb_model(count ~ day + nonexistant)
)
(fitted_tbl <- fit(models, fitting_data))
get_result(fitted_tbl)

## -----------------------------------------------------------------------------
(pred <- predict(fitted_tbl, pred_data))
get_result(pred)

