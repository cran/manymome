library(manymome)
suppressMessages(library(lavaan))

dat <- modmed_x1m3w4y1
n <- nrow(dat)
set.seed(860314)
dat$gp <- sample(c("gp1", "gp3"), n, replace = TRUE)
dat$city <- sample(c("alpha", "sigma"), n, replace = TRUE)
lm_m1 <- lm(m1 ~ x * w1, dat)
lm_m2 <- lm(m2 ~ m1 + gp + city, dat)
lm_m3 <- lm(m3 ~ m1 + x * gp, dat)
lm_y <- lm(y ~ m2 + m3 + x * w4, dat)
lm_m1_mm <- model.matrix(lm_m1)[, 4]
lm_m2_mm <- model.matrix(lm_m2)[, -c(1:2)]
lm_m3_mm <- model.matrix(lm_m3)[, 5, drop = FALSE]
lm_y_mm <- model.matrix(lm_y)[, 6]
dat2 <- cbind(dat, lm_m1_mm, lm_m2_mm, lm_m3_mm, lm_y_mm)
fit <- list(lm_m1, lm_m2, lm_m3, lm_y)

# Moderated mediation

out_mm_1 <- mod_levels_list("w4", "gp", fit = fit, merge = TRUE)

out_1 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit)
out_2 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit,
                               standardized_x = TRUE)
out_3 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit,
                               standardized_y = TRUE)
out_4 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit,
                               standardized_x = TRUE, standardized_y = TRUE)

out_5 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit,
                               boot_ci = TRUE, seed = 87415,
                               parallel = FALSE, progress = FALSE,
                               R = 40)
fit_boot_out <- lm2boot_out(fit, R = 40, seed = 87415, progress = FALSE)
out_6 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit,
                               standardized_x = TRUE,
                               boot_ci = TRUE, boot_out = fit_boot_out)
out_7 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit,
                               standardized_y = TRUE,
                               boot_ci = TRUE, boot_out = fit_boot_out)
out_8 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit,
                               standardized_x = TRUE, standardized_y = TRUE,
                               boot_ci = TRUE, boot_out = fit_boot_out)

tmp <- capture.output(print(out_1))
tmp <- capture.output(print(out_2))
tmp <- capture.output(print(out_3))
tmp <- capture.output(print(out_4))
tmp <- capture.output(print(out_5))
tmp <- capture.output(print(out_6))
tmp <- capture.output(print(out_7))
tmp <- capture.output(print(out_8))

# Moderation only

outmo_mm_1 <- mod_levels("gp", fit = fit)

outmo_1 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit)
outmo_2 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                               standardized_x = TRUE)
outmo_3 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                               standardized_y = TRUE)
outmo_4 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                               standardized_x = TRUE, standardized_y = TRUE)

outmo_5 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                               boot_ci = TRUE, seed = 87415,
                               parallel = FALSE, progress = FALSE,
                               R = 40)
# fit_boot_out <- lm2boot_out(fit, R = 100, seed = 87415)
outmo_6 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                               standardized_x = TRUE,
                               boot_ci = TRUE, boot_out = fit_boot_out)
outmo_7 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                               standardized_y = TRUE,
                               boot_ci = TRUE, boot_out = fit_boot_out)
outmo_8 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                               standardized_x = TRUE, standardized_y = TRUE,
                               boot_ci = TRUE, boot_out = fit_boot_out)

tmp <- capture.output(print(outmo_1))
tmp <- capture.output(print(outmo_2))
tmp <- capture.output(print(outmo_3))
tmp <- capture.output(print(outmo_4))
tmp <- capture.output(print(outmo_5))
tmp <- capture.output(print(outmo_6))
tmp <- capture.output(print(outmo_7))
tmp <- capture.output(print(outmo_8))

# Manual Check

lm_m1 <- lm(m1 ~ x * city, dat)
lm_m2 <- lm(m2 ~ x * gp, dat)
fit <- list(lm_m1, lm_m2)

outmo_chk1 <- cond_indirect_effects(wlevels = "city", x = "x", y = "m1", fit = fit)
outmo_chk2 <- cond_indirect_effects(wlevels = "gp", x = "x", y = "m2", fit = fit)

test_that("manual check of categorical moderator", {
  out1 <- unname(coef(outmo_chk1))
  tmp <- coef(lm_m1)
  chk1 <- unname(c(tmp["x"], tmp["x"] + tmp["x:citysigma"]))
  expect_equal(out1, chk1)
  out2 <- unname(coef(outmo_chk2))
  tmp <- coef(lm_m2)
  chk2 <- unname(c(tmp["x"], tmp["x"] + tmp["x:gpgp3"]))
  expect_equal(out2, chk2)
  })


