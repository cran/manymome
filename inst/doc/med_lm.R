## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----data---------------------------------------------------------------------
library(manymome)
dat <- data_med_complicated
print(round(head(dat), 2))

## ----draw_model, echo = FALSE-------------------------------------------------
library(semPlot)
suppressMessages(library(lavaan))
mod <-
"
m11 ~ x1 + x2
m12 ~ m11 + x1 + x2
m2 ~ x1 + x2
y1 ~ m2 + m12 + m11 + x1 + x2
y2 ~ m2 + m12 + m11 + x1 + x2
"
fit0 <- sem(mod, dat, do.fit = FALSE, fixed.x = FALSE)
layout_m <- matrix(c(  NA,  "m11",   NA, "m12",   NA,
                     "x1",     NA,   NA,    NA, "y1",
                     "x2",     NA,   NA,    NA, "y2",
                       NA,     NA, "m2",    NA,   NA), byrow = TRUE, 4, 5)
p <- semPaths(fit0,
              residuals = FALSE,
              sizeMan = 8,
              exoCov = FALSE,
              node.width = 1,
              edge.label.cex = .50,
              label.cex = .75,
              style = "ram",
              mar = c(5, 5, 5, 5),
              layout = layout_m,
              DoNotPlot = TRUE)
p$graphAttributes$Edges$color[18:19] <- "white"
plot(p)
text(-1.2, -1, paste("(Covariances and\ncontrol variables",
                    "omitted for readability)", sep = "\n"),
     adj = c(0, .5))

## ----fit_lm-------------------------------------------------------------------
lm_m11 <- lm(m11 ~ x1 + x2 + c1 + c2, dat)
lm_m12 <- lm(m12 ~ m11 + x1 + x2 + c1 + c2, dat)
lm_m2 <- lm(m2 ~ x1 + x2 + c1 + c2, dat)
lm_y1 <- lm(y1 ~ m12 + m2 + m11 + x1 + x2 + c1 + c2, dat)
lm_y2 <- lm(y2 ~ m12 + m2 + m11 + x1 + x2 + c1 + c2, dat)

## ----est, echo = FALSE--------------------------------------------------------
est <- matrix("", 5, 5)
colnames(est) <- c("m11", "m12", "m2", "y1", "y2")
rownames(est) <- c("x1", "x2", "m11", "m12", "m2")
tmp <- coef(lm_m11)[-c(1, 4, 5)]
est[names(tmp), "m11"] <- formatC(tmp, digits = 3, format = "f")
tmp <- coef(lm_m12)[-c(1, 5, 6)]
est[names(tmp), "m12"] <- formatC(tmp, digits = 3, format = "f")
tmp <- coef(lm_m2)[-c(1, 4, 5)]
est[names(tmp), "m2"] <- formatC(tmp, digits = 3, format = "f")
tmp <- coef(lm_y1)[-c(1, 7, 8)]
est[names(tmp), "y1"] <- formatC(tmp, digits = 3, format = "f")
tmp <- coef(lm_y2)[-c(1, 7, 8)]
est[names(tmp), "y2"] <- formatC(tmp, digits = 3, format = "f")
print(as.data.frame(est), quote = FALSE)

## ----lm2list------------------------------------------------------------------
fit_lm <- lm2list(lm_m11, lm_m12, lm_m2, lm_y1, lm_y2)
fit_lm

## ----do_boo-------------------------------------------------------------------
boot_out_lm <- do_boot(fit_lm,
                       R = 100,
                       seed = 54532,
                       ncores = 1)

## ----ind_x1m11m12y1-----------------------------------------------------------
out_x1m11m12y1 <- indirect_effect(x = "x1",
                                  y = "y1",
                                  m = c("m11", "m12"),
                                  fit = fit_lm,
                                  boot_ci = TRUE,
                                  boot_out = boot_out_lm)
out_x1m11m12y1

## ----ind_x2y2m2---------------------------------------------------------------
out_x2m2y2 <- indirect_effect(x = "x2",
                              y = "y2",
                              m = "m2",
                              fit = fit_lm,
                              boot_ci = TRUE,
                              boot_out = boot_out_lm)
out_x2m2y2

## ----ind_x2m11m12-------------------------------------------------------------
out_x2m11m12 <- indirect_effect(x = "x2",
                                y = "m12",
                                m = "m11",
                                fit = fit_lm,
                                boot_ci = TRUE,
                                boot_out = boot_out_lm)
out_x2m11m12

## ----std_x1m11m12y1-----------------------------------------------------------
std_x1m11m12y1 <- indirect_effect(x = "x1",
                                  y = "y1",
                                  m = c("m11", "m12"),
                                  fit = fit_lm,
                                  boot_ci = TRUE,
                                  boot_out = boot_out_lm,
                                  standardized_x = TRUE,
                                  standardized_y = TRUE)
std_x1m11m12y1

## ----std_x1m2y1---------------------------------------------------------------
std_x1m2y1 <- indirect_effect(x = "x1",
                              y = "y1",
                              m = "m2",
                              fit = fit_lm,
                              boot_ci = TRUE,
                              boot_out = boot_out_lm,
                              standardized_x = TRUE,
                              standardized_y = TRUE)
std_x1m2y1

## ----ind_x1m11y1--------------------------------------------------------------
out_x1m11y1 <- indirect_effect(x = "x1",
                               y = "y1",
                               m = "m11",
                               fit = fit_lm,
                               boot_ci = TRUE,
                               boot_out = boot_out_lm)
out_x1m11y1

## ----ind_x1m12y1--------------------------------------------------------------
out_x1m12y1 <- indirect_effect(x = "x1",
                               y = "y1",
                               m = "m12",
                               fit = fit_lm,
                               boot_ci = TRUE,
                               boot_out = boot_out_lm)
out_x1m12y1

## ----ind_x1m2y1---------------------------------------------------------------
out_x1m2y1 <- indirect_effect(x = "x1",
                               y = "y1",
                               m = "m2",
                               fit = fit_lm,
                               boot_ci = TRUE,
                               boot_out = boot_out_lm)
out_x1m2y1

## ----total_ind_x1y1-----------------------------------------------------------
out_x1y1_total <- out_x1m11m12y1 + out_x1m11y1 + out_x1m12y1 + out_x1m2y1
out_x1y1_total

## ----ind_diff-----------------------------------------------------------------
out_x1_diff <- out_x1m11m12y1 - out_x1m2y1
out_x1_diff

