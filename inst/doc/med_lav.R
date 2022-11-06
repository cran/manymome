## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----data---------------------------------------------------------------------
library(manymome)
dat <- data_sem
print(round(head(dat), 1))

## ----draw_model, echo = FALSE-------------------------------------------------
# Adapted from vignette("set_sem_layout")
# of the semptools package
library(semPlot)
suppressMessages(library(lavaan))
mod <-
"
f1 =~ x01 + x02 + x03
f2 =~ x04 + x05 + x06 + x07
f3 =~ x08 + x09 + x10
f4 =~ x11 + x12 + x13 + x14
f3 ~  f1 + f2
f4 ~  f1 + f3
"
fit0 <- sem(mod, dat, do.fit = FALSE, fixed.x = FALSE)
p <- semPaths(fit0,
              residuals = FALSE,
              sizeMan = 5,
              node.width = 1,
              edge.label.cex = .75,
              style = "ram",
              mar = c(5, 5, 5, 5),
              DoNotPlot = TRUE)
if (requireNamespace("semptools", quietly = TRUE)) {
    library(semptools)
    indicator_order  <- c("x07", "x06", "x05", "x04",
                          "x03", "x02", "x01",
                          "x14", "x13", "x12", "x11",
                          "x08", "x09", "x10")
    indicator_factor <- c( "f2",  "f2",  "f2",  "f2",
                          "f1",  "f1",  "f1",
                          "f4",  "f4",  "f4",  "f4",
                          "f3",  "f3",  "f3")
    factor_layout <- matrix(c("f1",   NA,   NA,
                              NA, "f3", "f4",
                            "f2",   NA,   NA), byrow = TRUE, 3, 3)
    factor_point_to <- matrix(c("left",     NA,      NA,
                                    NA, "down", "right",
                                "left",     NA,      NA), byrow = TRUE, 3, 3)
    indicator_push <- c(f3 = 1.5,
                        f4 = 1.5,
                        f1 = 1.5,
                        f2 = 1.5)
    indicator_spread <- c(f1 = 1.5,
                          f2 = 1.75,
                          f3 = 1,
                          f4 = 1.75)
    p2 <- set_sem_layout(p,
                        indicator_order = indicator_order,
                        indicator_factor = indicator_factor,
                        factor_layout = factor_layout,
                        factor_point_to = factor_point_to,
                        indicator_push = indicator_push,
                        indicator_spread = indicator_spread)
    p2 <- set_curve(p2, c("f2 ~~ f1" = -2.5))
    plot(p2)
  } else {
    plot(p)
  }

## ----fit_mode-----------------------------------------------------------------
mod <-
"
f1 =~ x01 + x02 + x03
f2 =~ x04 + x05 + x06 + x07
f3 =~ x08 + x09 + x10
f4 =~ x11 + x12 + x13 + x14
f3 ~  f1 + f2
f4 ~  f1 + f3
"
fit_med <- sem(model = mod,
               data = dat)

## ----est----------------------------------------------------------------------
est <- parameterEstimates(fit_med)
est[est$op == "~", ]

## ----do_boo-------------------------------------------------------------------
boot_out_med <- do_boot(fit_med,
                        R = 100,
                        seed = 98171,
                        ncores = 1)

## ----med_f1_f3_f4-------------------------------------------------------------
out_f1f3f4 <- indirect_effect(x = "f1",
                              y = "f4",
                              m = "f3",
                              fit = fit_med,
                              boot_ci = TRUE,
                              boot_out = boot_out_med)
out_f1f3f4

## ----med_f2_f3_f4-------------------------------------------------------------
out_f2f3f4 <- indirect_effect(x = "f2",
                              y = "f4",
                              m = "f3",
                              fit = fit_med,
                              boot_ci = TRUE,
                              boot_out = boot_out_med)
out_f2f3f4

## ----med_f1_f3_f4_std---------------------------------------------------------
std_f1f3f4 <- indirect_effect(x = "f1",
                              y = "f4",
                              m = "f3",
                              fit = fit_med,
                              boot_ci = TRUE,
                              boot_out = boot_out_med,
                              standardized_x = TRUE,
                              standardized_y = TRUE)
std_f1f3f4

## ----std_f2_f3_f4-------------------------------------------------------------
std_f2f3f4 <- indirect_effect(x = "f2",
                              y = "f4",
                              m = "f3",
                              fit = fit_med,
                              boot_ci = TRUE,
                              boot_out = boot_out_med,
                              standardized_x = TRUE,
                              standardized_y = TRUE)
std_f2f3f4

## ----direct_f1f4--------------------------------------------------------------
out_f1f4 <- indirect_effect(x = "f1",
                            y = "f4",
                            fit = fit_med,
                            boot_ci = TRUE,
                            boot_out = boot_out_med)
out_f1f4

## ----total_f1f4---------------------------------------------------------------
out_f1_total <- out_f1f3f4 + out_f1f4
out_f1_total

## ----ind_diff-----------------------------------------------------------------
out_f1_diff <- out_f1f4 - out_f1f3f4
out_f1_diff

## ----all_paths----------------------------------------------------------------
all_paths <- all_indirect_paths(fit = fit_med)
all_paths

## ----all_indirect_est---------------------------------------------------------
out_all <- many_indirect_effects(paths = all_paths,
                                 fit = fit_med,
                                 boot_ci = TRUE,
                                 boot_out = boot_out_med)

## -----------------------------------------------------------------------------
out_all

## -----------------------------------------------------------------------------
out1 <- out_all[[1]]
out1

## -----------------------------------------------------------------------------
out2 <- out_all[["f2 -> f3 -> f4"]]
out2

