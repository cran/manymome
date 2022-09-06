## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----data---------------------------------------------------------------------
library(manymome)
dat <- data_med
head(dat)

## ----draw_model, echo = FALSE-------------------------------------------------
library(semPlot)
suppressMessages(library(lavaan))
mod <-
"
m ~ x + c1 + c2
y ~ m + x + c1 + c2
"
fit0 <- sem(mod, dat, do.fit = FALSE, fixed.x = FALSE)
layout_m <- matrix(c(NA, "m", NA,
                     "x", NA, "y",
                     "c1", NA, NA,
                     "c2", NA, NA), 4, 3, byrow = TRUE)
p <- semPaths(fit0,
              layout = layout_m,
              nCharNodes = 0,
              exoCov = FALSE,
              residuals = FALSE,
              sizeMan = 10,
              asize = 10,
              DoNotPlot = TRUE)
plot(p)
text(label = "(Covariances excluded for readability)",
     x = .25, y = -1,
     adj = c(.5, .5))

## ----fit_by_lavaan------------------------------------------------------------
mod <-
"
m ~ x + c1 + c2
y ~ m + x + c1 + c2
"
fit_lavaan <- sem(model = mod, data = dat,
           fixed.x = FALSE,
           estimator = "MLR")
summary(fit_lavaan)

## ----do_boot_lavaan-----------------------------------------------------------
boot_out_lavaan <- do_boot(fit = fit_lavaan,
                           R = 100,
                           ncores = 1)

## ----do_boot_lav_save, eval = FALSE-------------------------------------------
#  ### Use saveRDS() ###
#  # Save the output
#  saveRDS(boot_out_lavaan, file = "boot_out_lavaan.rds")
#  # Load the output
#  boot_out_lavaan <- readRDS("boot_out_lavaan.rds")
#  
#  ### Use save() ###
#  # Save the output
#  save(boot_out_lavaan, file = "boot_out_lavaan.RData")
#  # Load the output
#  load("boot_out_lavaan.RData")

## ----boot_out_est-------------------------------------------------------------
boot_out_lavaan[[1]]$est

## ----boot_out_implied_stats---------------------------------------------------
boot_out_lavaan[[1]]$implied_stats

## ----indirect_lav-------------------------------------------------------------
out_lavaan <- indirect_effect(x = "x",
                              y = "y",
                              m = "m",
                              fit = fit_lavaan,
                              boot_ci = TRUE,
                              boot_out = boot_out_lavaan)
out_lavaan

## ----fit_by_lm----------------------------------------------------------------
# Fit Models
lm_m <- lm(m ~ x + c1 + c2, dat)
lm_y <- lm(y ~ m + x + c1 + c2, dat)
#
# ###### Regression: Predict m ######
summary(lm_m)
#
# ###### Regression: Predict y ######
#
summary(lm_y)

## ----lm2list------------------------------------------------------------------
fit_lm <- lm2list(lm_m, lm_y)
fit_lm

## ----do_boot_lm---------------------------------------------------------------
boot_out_lm <- do_boot(fit = fit_lm,
                       R = 100,
                       seed = 98715)

## ----boot_out_est_lm----------------------------------------------------------
boot_out_lm[[1]]$est

## ----boot_out_implied_stats_lm------------------------------------------------
boot_out_lm[[1]]$implied_stats

## ----indirect_lm--------------------------------------------------------------
out_lm <- indirect_effect(x = "x",
                          y = "y",
                          m = "m",
                          fit = fit_lm,
                          boot_ci = TRUE,
                          boot_out = boot_out_lm)
out_lm

