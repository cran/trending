## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  fig.width = 7,
  fig.height = 5
)

## ----poisson------------------------------------------------------------------
library(ciTools)
library(trending)
library(ggplot2)
library(patchwork)
library(MASS)


# generate data
x <- rnorm(100, mean = 0)
y <- rpois(n = 100, lambda = exp(1.5 + 0.5*x))
dat <- data.frame(x = x, y = y)
fit <- glm(y ~ x , family = poisson(link = "log"))

# use ciTools to add prediction interval
dat1 <- add_pi(dat, fit, names = c("lpb", "upb"), alpha = 0.1, nsims = 20000)
head(dat1)

# add intervals with trending (no uncertainty in parameters)
poisson_model <- glm_model(y ~ x, family = "poisson")
fitted_model <- fit(poisson_model, dat)
dat2 <- predict(fitted_model, uncertain = FALSE, alpha = 0.1)
head(dat2)

# add intervals with trending (uncertainty in parameters)
dat3 <- predict(fitted_model, alpha = 0.1)
head(dat3)

# plots
p1 <- ggplot(dat1, aes(x, y)) +
  geom_point(size = 1) +
  geom_line(aes(y = pred), size = 1.2) +
  geom_ribbon(aes(ymin = lpb, ymax = upb), alpha = 0.2) +
  geom_ribbon(aes(ymin = `lower_pi`, ymax = `upper_pi`), data = dat2, alpha = 0.4) +
  ggtitle("Poisson regression with prediction intervals and no uncertainty in parameters", 
          subtitle = "Model fit (black line), with bootstrap intervals (gray), parametric intervals (dark gray)") +
  coord_cartesian(ylim=c(0, 30))

p2 <- ggplot(dat1, aes(x, y)) +
  geom_point(size = 1) +
  geom_line(aes(y = pred), size = 1.2) +
  geom_ribbon(aes(ymin = lpb, ymax = upb), alpha = 0.4) +
  geom_ribbon(aes(ymin = `lower_pi`, ymax = `upper_pi`), data = dat3, alpha = 0.2) +
  ggtitle("Poisson regression with prediction intervals and uncertainty in parameters", 
          subtitle = "Model fit (black line), with parametric intervals (gray), bootstrap intervals (dark gray)") +
  coord_cartesian(ylim=c(0, 30))

p1 / p2


## ----quasipoisson-------------------------------------------------------------
# generate data
x <- runif(n = 100, min = 0, max = 2)
mu <- exp(1 + x)
y <- rnegbin(n = 100, mu = mu, theta = mu/(5 - 1))
dat <- data.frame(x = x, y = y)
fit <- glm(y ~ x, family = quasipoisson(link = "log"))

# use ciTools to add prediction interval
dat1 <- add_pi(dat, fit, names = c("lpb", "upb"), alpha = 0.1, nsims = 20000)
head(dat1)

# add intervals with trending (no uncertainty in parameters)
quasipoisson_model <- glm_model(y ~ x, family = quasipoisson(link = "log"))
fitted_model <- fit(quasipoisson_model, dat)
dat2 <- predict(fitted_model, uncertain = FALSE, alpha = 0.1)
head(dat2)

# add intervals with trending (uncertainty in parameters)
dat3 <- predict(fitted_model, alpha = 0.1)
head(dat3)

# plots
p3 <- ggplot(dat1, aes(x, y)) +
  geom_point(size = 1) +
  geom_line(aes(y = pred), size = 1.2) +
  geom_ribbon(aes(ymin = lpb, ymax = upb), alpha = 0.2) +
  geom_ribbon(aes(ymin = `lower_pi`, ymax = `upper_pi`), data = dat2, alpha = 0.4) +
  ggtitle("Quasipoisson regression with prediction intervals and no uncertainty in parameters", 
          subtitle = "Model fit (black line), with bootstrap intervals (gray), parametric intervals (dark gray)") +
  coord_cartesian(ylim=c(0, 30))

p4 <- ggplot(dat1, aes(x, y)) +
  geom_point(size = 1) +
  geom_line(aes(y = pred), size = 1.2) +
  geom_ribbon(aes(ymin = lpb, ymax = upb), alpha = 0.4) +
  geom_ribbon(aes(ymin = `lower_pi`, ymax = `upper_pi`), data = dat3, alpha = 0.2) +
  ggtitle("Quasipoisson regression with prediction intervals and uncertainty in parameters", 
          subtitle = "Model fit (black line), with parametric intervals (gray), bootstrap intervals (dark gray)") +
  coord_cartesian(ylim=c(0, 30))

p3 / p4


