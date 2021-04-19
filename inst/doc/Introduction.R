## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(outbreaks)  # for data
library(trending)   # for trend fitting
library(dplyr, warn.conflicts = FALSE)  # for data manipulation

# load data
data(covid19_england_nhscalls_2020)

# define a model
model  <- glm_nb_model(count ~ day + weekday)

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

fitted_model <- fit(model, fitting_data)

# default
fitted_model %>% 
  predict(pred_data) %>%
  glimpse()

# without prediction intervals
fitted_model %>% 
  predict(pred_data, add_pi = FALSE) %>% 
  glimpse()

# without uncertainty
fitted_model %>% 
  predict(pred_data, uncertainty = FALSE) %>% 
  glimpse()

# non-bootstraped (parametric) prediction intervals
fitted_model %>% 
  predict(pred_data, simulate_pi = FALSE) %>% 
  glimpse()

## -----------------------------------------------------------------------------
models  <- list(
  simple = lm_model(count ~ day),
  glm_poisson = glm_model(count ~ day, family = "poisson"),
  glm_negbin = glm_nb_model(count ~ day + weekday),
  will_error = glm_nb_model(count ~ day + nonexistant)
)

res <- models %>%
  fit(fitting_data)

res

res %>% glimpse()


## -----------------------------------------------------------------------------
res <- models %>%
  fit(fitting_data) %>% 
  predict(pred_data)

res

res %>% glimpse()

