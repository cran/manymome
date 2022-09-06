## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----data---------------------------------------------------------------------
library(manymome)
dat <- data_med_mod_a
print(head(dat), digits = 3)

## ----draw_model, echo = FALSE-------------------------------------------------
library(semPlot)
suppressMessages(library(lavaan))
dat$wx <- dat$x * dat$w
#dat$mw2 <- dat$m * dat$w2
mod <-
"
wx ~ x + w
m ~ wx
y ~ m
y ~ x
"
fit0 <- sem(mod, dat, do.fit = FALSE, fixed.x = FALSE)
layout_m <- matrix(c("w", NA, "m", NA, NA,
                   NA, "wx", NA, NA, NA,
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

## ----fit_lm-------------------------------------------------------------------
lm_m <- lm(m ~ x*w + c1 + c2, dat)
lm_y <- lm(y ~ m + x + c1 + c2, dat)

## ----est----------------------------------------------------------------------
# ###### Predict m ######
#
summary(lm_m)
#
# ###### Predict y ######
#
summary(lm_y)

## ----lm2list------------------------------------------------------------------
fit_lm <- lm2list(lm_m, lm_y)
fit_lm

## ----do_boo-------------------------------------------------------------------
boot_out_lm <- do_boot(fit_lm,
                       R = 100,
                       seed = 54532,
                       ncores = 1)

## ----ind_xmy_on_w-------------------------------------------------------------
out_xmy_on_w <- cond_indirect_effects(wlevels = "w",
                                      x = "x",
                                      y = "y",
                                      m = "m",
                                      fit = fit_lm,
                                      boot_ci = TRUE,
                                      boot_out = boot_out_lm)
out_xmy_on_w

## ----mome---------------------------------------------------------------------
out_mome <- index_of_mome(x = "x",
                          y = "y",
                          m = "m",
                          w = "w",
                          fit = fit_lm,
                          boot_ci = TRUE,
                          boot_out = boot_out_lm)
out_mome

## ----std_xmy_on_w-------------------------------------------------------------
std_xmy_on_w <- cond_indirect_effects(wlevels = "w",
                                      x = "x",
                                      y = "y",
                                      m = "m",
                                      fit = fit_lm,
                                      boot_ci = TRUE,
                                      boot_out = boot_out_lm,
                                      standardized_x = TRUE,
                                      standardized_y = TRUE)
std_xmy_on_w

