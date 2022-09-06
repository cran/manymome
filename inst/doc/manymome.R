## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----dataset------------------------------------------------------------------
library(manymome)
dat <- data_med_mod_ab
print(head(dat), digits = 3)

## ----draw_model, echo = FALSE-------------------------------------------------
library(semPlot)
suppressMessages(library(lavaan))
dat$xw1 <- dat$x * dat$w1
dat$mw2 <- dat$m * dat$w2
mod <-
"
xw1 ~ x + w1
m ~ xw1
mw2 ~ m + w2
y ~ mw2
y ~ x
"
fit0 <- sem(mod, dat, do.fit = FALSE, fixed.x = FALSE)
layout_m <- matrix(c("w1", NA, NA, NA, "w2",
                   NA, NA, "m", NA, NA,
                   NA, "xw1", NA, "mw2", NA,
                   "x", NA, NA, NA, "y"), 4, 5, byrow = TRUE)
p <- semPaths(fit0,
              layout = layout_m,
              nCharNodes = 0,
              exoCov = FALSE,
              residuals = FALSE,
              sizeMan = 10,
              asize = 10,
              DoNotPlot = TRUE)
p$graphAttributes$Nodes$width[c(1, 3)] <- .01
p$graphAttributes$Nodes$height[c(1, 3)] <- .01
p$graphAttributes$Edges$asize[c(1, 4)] <- 0
plot(p)

## ----fit_model----------------------------------------------------------------
library(lavaan)
# Form the product terms
dat$w1x <- dat$w1 * dat$x
dat$w2m <- dat$w2 * dat$m
mod <-
"
m ~ x + w1 + w1x + c1 + c2
y ~ m + w2 + w2m + x + c1 + c2
# Covariances of the error term of m with w2m and w2
m ~~ w2m + w2
# Covariance between other variables
# They need to be added due to the covariances added above
# See Kwan and Chan (2018) and Miles et al. (2015)
w2m ~~ w2 + x + w1 + w1x + c1 + c2
w2  ~~ x + w1 + w1x + c1 + c2
x   ~~ w1 + w1x + c1 + c2
w1  ~~ w1x + c1 + c2
w1x ~~ c1 + c2
c1  ~~ c2
"
fit <- sem(model = mod,
           data = dat,
           fixed.x = FALSE,
           estimator = "MLR")

## ----estimates----------------------------------------------------------------
parameterEstimates(fit)[parameterEstimates(fit)$op == "~", ]

## ----do_boot------------------------------------------------------------------
fit_boot <- do_boot(fit = fit,
                    R = 100,
                    seed = 53253,
                    ncores = 1)

## ----cond_indirect------------------------------------------------------------
out_cond <- cond_indirect_effects(wlevels =c("w1", "w2"),
                                  x = "x",
                                  y = "y",
                                  m = "m",
                                  fit = fit,
                                  boot_ci = TRUE,
                                  boot_out = fit_boot)

## ----out_cond-----------------------------------------------------------------
out_cond

## ----get_one------------------------------------------------------------------
get_one_cond_indirect_effect(out_cond, 1)

## ----cond_stdxy---------------------------------------------------------------
out_cond_stdxy <- cond_indirect_effects(wlevels =c("w1", "w2"),
                                        x = "x",
                                        y = "y",
                                        m = "m",
                                        fit = fit,
                                        boot_ci = TRUE,
                                        boot_out = fit_boot,
                                        standardized_x = TRUE,
                                        standardized_y = TRUE)

## ----out_cond_stdxy-----------------------------------------------------------
out_cond_stdxy

## ----momome-------------------------------------------------------------------
out_momome <- index_of_momome(x = "x",
                              y = "y",
                              m = "m",
                              w = "w1",
                              z = "w2",
                              fit = fit,
                              boot_ci = TRUE,
                              boot_out = fit_boot)

## ----out_momome---------------------------------------------------------------
out_momome

## ----plot_mome1, echo = FALSE-------------------------------------------------
library(semPlot)
suppressMessages(library(lavaan))
dat$xw1 <- dat$x * dat$w1
dat$mw2 <- dat$m * dat$w2
mod <-
"
xw1 ~ x + w1
m ~ xw1
# mw2 ~ m + w2
y ~ m
y ~ x
"
fit0 <- sem(mod, dat, do.fit = FALSE, fixed.x = FALSE)
layout_m <- matrix(c("w1", NA, "m", NA, NA,
                   NA, "xw1", NA, "mw2", NA,
                   "x", NA, NA, NA, "y"), 3, 5, byrow = TRUE)
p <- semPaths(fit0,
              layout = layout_m,
              nCharNodes = 0,
              exoCov = FALSE,
              residuals = FALSE,
              sizeMan = 10,
              asize = 10,
              DoNotPlot = TRUE)
p$graphAttributes$Nodes$width[c(1)] <- .01
p$graphAttributes$Nodes$height[c(1)] <- .01
p$graphAttributes$Edges$asize[c(1)] <- 0
plot(p)

## ----fit_mome1----------------------------------------------------------------
library(lavaan)
dat$w1x <- dat$w1 * dat$x
mod2 <-
"
m ~ x + w1 + w1x + c1 + c2
y ~ m + x + c1 + c2
"
fit2 <- sem(model = mod2,
           data = dat,
           fixed.x = FALSE,
           estimator = "MLR")

## ----est_mome1----------------------------------------------------------------
parameterEstimates(fit2)[parameterEstimates(fit2)$op == "~", ]

## ----do_boot2-----------------------------------------------------------------
fit2_boot <- do_boot(fit = fit2,
                    R = 100,
                    seed = 53253,
                    ncores = 1)

## ----mome---------------------------------------------------------------------
out_mome <- index_of_mome(x = "x",
                          y = "y",
                          m = "m",
                          w = "w1",
                          fit = fit2,
                          boot_ci = TRUE,
                          boot_out = fit2_boot)

## ----out_mome-----------------------------------------------------------------
out_mome

## ----dataset_me---------------------------------------------------------------
library(manymome)
dat <- data_serial
print(head(dat), digits = 3)

## ----draw_med, echo = FALSE---------------------------------------------------
library(semPlot)
suppressMessages(library(lavaan))
mod <-
"m1 ~ x + c1 + c2
m2 ~ m1 + x + c1 + c2
y ~ m2 + m1 + x + c1 + c2
"
fit0 <- sem(mod, dat, do.fit = FALSE, fixed.x = FALSE)
layout_m <- matrix(c(NA, "m1", "m2", NA,
                     "x", NA, NA, "y",
                     "c1", NA, NA, NA,
                     "c2", NA, NA, NA), 4, 4, byrow = TRUE)
p <- semPaths(fit0,
              layout = layout_m,
              nCharNodes = 0,
              exoCov = FALSE,
              residuals = FALSE,
              sizeMan = 10,
              asize = 5,
              DoNotPlot = TRUE)
plot(p)

## -----------------------------------------------------------------------------
mod_med <- "
m1 ~ x + c1 + c2
m2 ~ m1 + x + c1 + c2
y ~ m2 + m1 + x + c1 + c2
"
fit_med <- sem(model = mod_med,
               data = dat,
               fixed.x = TRUE)

## ----est_med------------------------------------------------------------------
parameterEstimates(fit_med)[parameterEstimates(fit_med)$op == "~", ]

## ----do_indirect--------------------------------------------------------------
out_med <- indirect_effect(x = "x",
                           y = "y",
                           m = c("m1", "m2"),
                           fit = fit_med,
                           boot_ci = TRUE,
                           R = 100,
                           seed = 43143,
                           ncores = 1)

## ----out_med------------------------------------------------------------------
out_med

## ----do_indirect_stdxy--------------------------------------------------------
out_med_stdxy <- indirect_effect(x = "x",
                                 y = "y",
                                 m = c("m1", "m2"),
                                 fit = fit_med,
                                 boot_ci = TRUE,
                                 boot_out = out_med,
                                 standardized_x = TRUE,
                                 standardized_y = TRUE)
out_med_stdxy

## -----------------------------------------------------------------------------
out_x_m2_y <- indirect_effect(x = "x",
                              y = "y",
                              m = "m2",
                              fit = fit_med,
                              boot_ci = TRUE,
                              boot_out = out_med)
out_x_m2_y

## ----xm1y---------------------------------------------------------------------
out_x_m1_y <- indirect_effect(x = "x",
                              y = "y",
                              m = "m1",
                              fit = fit_med,
                              boot_ci = TRUE,
                              boot_out = out_med)
out_x_m1_y

## ----total_ind----------------------------------------------------------------
total_ind <- out_med + out_x_m1_y + out_x_m2_y
total_ind

## ----each_ind-----------------------------------------------------------------
coef(out_med)
coef(out_x_m1_y)
coef(out_x_m2_y)

## ----xdirect------------------------------------------------------------------
out_x_direct <- indirect_effect(x = "x",
                                y = "y",
                                fit = fit_med,
                                boot_ci = TRUE,
                                boot_out = out_med)
out_x_direct

## ----total_effect-------------------------------------------------------------
total_effect <- out_med + out_x_m1_y + out_x_m2_y + out_x_direct
total_effect

