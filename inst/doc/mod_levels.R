## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(manymome)
dat <- data_med_mod_ab
print(head(dat), digits = 3)

## -----------------------------------------------------------------------------
library(lavaan)
dat$w1x <- dat$w1 * dat$x
dat$w2m <- dat$w2 * dat$m
mod <-
"
m ~ x + w1 + w1x
y ~ m + w2 + w2m
m ~~ w2 + w2m
w2  ~~ w2m + x + w1 + w1x
w2m ~~ x + w1 + w1x
x   ~~ w1 + w1x
w1  ~~ w1x
"
fit <- sem(model = mod, data = dat)

## -----------------------------------------------------------------------------
w1levels <- mod_levels(w = "w1", fit = fit)
w1levels

## -----------------------------------------------------------------------------
w1levels <- mod_levels(w = "w1", fit = fit,
                       sd_from_mean = c(-1, 1))
w1levels

## -----------------------------------------------------------------------------
w1levels <- mod_levels(w = "w1", fit = fit,
                       w_method = "percentile")
w1levels

## -----------------------------------------------------------------------------
w1levels <- mod_levels(w = "w1", fit = fit,
                       w_method = "percentile",
                       percentiles = c(.25, .75))
w1levels

## -----------------------------------------------------------------------------
w1levels <- mod_levels(w = "w1", fit = fit,
                       values = c(2, 4, 8))
w1levels

## -----------------------------------------------------------------------------
out <- cond_indirect_effects(wlevels = w1levels,
                             x = "x", y = "m",
                             fit = fit)
out

## -----------------------------------------------------------------------------
wlevels_list <- mod_levels_list("w1", "w2", fit = fit)
wlevels_list

## -----------------------------------------------------------------------------
wlevels_list <- mod_levels_list("w1", "w2", fit = fit,
                                merge = TRUE)
wlevels_list

## -----------------------------------------------------------------------------
wlevels_list <- mod_levels_list("w1", "w2", fit = fit,
                                w_method = "percentile",
                                percentiles = c(.25, .75),
                                merge = TRUE)
wlevels_list

## -----------------------------------------------------------------------------
w1levels <- mod_levels(w = "w1", fit = fit)
w1levels
w2levels <- mod_levels(w = "w2", fit = fit, values = c(2, 5))
w2levels
wlevels_all <- merge_mod_levels(w1levels, w2levels)
wlevels_all

## -----------------------------------------------------------------------------
dat <- data_med_mod_serial_cat
print(head(dat), digits = 3)

## -----------------------------------------------------------------------------
w1dummies <- factor2var(dat$w1, prefix = "w1")
head(w1dummies)
# Add them to the dataset
dat[, c("w1group2", "w1group3")] <- w1dummies
print(head(dat), digits = 3)

## -----------------------------------------------------------------------------
dat$w1group2x <- dat$w1group2 * dat$x
dat$w1group3x <- dat$w1group3 * dat$x
mod <-
"
m1 ~ x + w1group2 + w1group3 + w1group2x + w1group3x
y ~ m1 + x
"
fit <- sem(model = mod, data = dat)

## -----------------------------------------------------------------------------
w1levels <- mod_levels(w = c("w1group2", "w1group3"), fit = fit)
w1levels

## -----------------------------------------------------------------------------
w1levels <- mod_levels(w = c("w1group2", "w1group3"), fit = fit,
                       prefix = "w1")
w1levels

## -----------------------------------------------------------------------------
w1levels <- mod_levels(w = c("w1group2", "w1group3"), fit = fit,
                       prefix = "w1",
                       reference_group_label = "group1")
w1levels

## -----------------------------------------------------------------------------
out <- cond_indirect_effects(wlevels = w1levels,
                             x = "x", y = "y", m = "m1",
                             fit = fit)
out

## -----------------------------------------------------------------------------
w1levels <- mod_levels(w = c("w1group2", "w1group3"), fit = fit,
                       values = list(group1 = c(0, 0),
                                     group2 = c(1, 0),
                                     group3 = c(0, 1)))
w1levels

## -----------------------------------------------------------------------------
xlevels <- mod_levels(w = "x", fit = fit,
                      sd_from_mean = c(-1, 1))
xlevels
w1levels <- mod_levels(w = c("w1group2", "w1group3"), fit = fit,
                       prefix = "w1",
                       reference_group_label = "group1")
w1levels
wlevels_all <- merge_mod_levels(xlevels, w1levels)
wlevels_all

