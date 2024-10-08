skip_on_cran()

library(testthat)
library(manymome)

dat <- data_med_mod_b_mod
lm_m <- lm(m ~ x*w1 + c1 + c2, dat)
lm_y <- lm(y ~ w1*m*w2 + x + c1 + c2, dat)
lm1_list <- lm2list(lm_m, lm_y)
lm2_list <- lm2list(lm_m)
lm3_list <- lm2list(lm_y)
lm1fit <- lm2ptable(lm1_list)
lm2fit <- lm2ptable(lm2_list)
lm3fit <- lm2ptable(lm3_list)


suppressMessages(suppressMessages(library(lavaan)))
dat$w1x <- dat$w1 * dat$x
dat$w1m <- dat$w1 * dat$m
dat$w2m <- dat$w2 * dat$m
dat$w1w2 <- dat$w1 * dat$w2
dat$w1mw2 <- dat$w1 * dat$m * dat$w2
mod1 <-
"
m ~ a * x + af1 * w1 + ad1 * w1x + c1 + c2
y ~ b * m + cp * x + bf1 * w1 + bf2 * w2 +
    bd1 * w1m + bd2 * w2m + bd12 * w1w2 + be12 * w1mw2 + c1 + c2
# Covariances not added. Not necessary for this test.
"
mod2 <-
"
m ~ a * x + af1 * w1 + ad1 * w1x + c1 + c2
# Covariances not added. Not necessary for this test.
"
mod3 <-
"
y ~ b * m + cp * x + bf1 * w1 + bf2 * w2 +
    bd1 * w1m + bd2 * w2m + bd12 * w1w2 + be12 * w1mw2 + c1 + c2
# Covariances not added. Not necessary for this test.
"
fit1 <- sem(mod1, dat, fixed.x = FALSE, warn = FALSE)
fit2 <- sem(mod2, dat, fixed.x = FALSE, warn = FALSE)
fit3 <- sem(mod3, dat, fixed.x = FALSE, warn = FALSE)

wvalues <- c(w1 = 5, w2 = -4)

dat2 <- data_med_mod_b_mod
dat2$w1 <- dat2$w1 - wvalues["w1"]
dat2$w2 <- dat2$w2 - wvalues["w2"]
lm_m2 <- lm(m ~ x*w1 + c1 + c2, dat2)
lm_y2 <- lm(y ~ w1*m*w2 + x + c1 + c2, dat2)
x_cond <- coef(lm_m2)["x"]
x_cond_se <- sqrt(vcov(lm_m2)["x", "x"])
m_cond <- coef(lm_y2)["m"]
m_cond_se <- sqrt(vcov(lm_y2)["m", "m"])

dat2$w1x <- dat2$w1 * dat2$x
dat2$w1m <- dat2$w1 * dat2$m
dat2$w2m <- dat2$w2 * dat2$m
dat2$w1w2 <- dat2$w1 * dat2$w2
dat2$w1mw2 <- dat2$w1 * dat2$m * dat2$w2
fit1_chk <- sem(mod1, dat2, fixed.x = FALSE, warn = FALSE)
fit2_chk <- sem(mod2, dat2, fixed.x = FALSE, warn = FALSE)
fit3_chk <- sem(mod3, dat2, fixed.x = FALSE, warn = FALSE)
x_scond <- coef(fit1_chk)["a"]
x_scond_se <- parameterEstimates(fit1_chk)[1, "se"]
m_scond <- coef(fit1_chk)["b"]
m_scond_se <- parameterEstimates(fit1_chk)[6, "se"]
m3_scond <- coef(fit3_chk)["b"]
m3_scond_se <- parameterEstimates(fit3_chk)[1, "se"]

# Moderation
ce_1a <- indirect_i(x = "x", y = "m",
                    fit = NULL,
                    est = lm1fit$est,
                    data = lm1fit$data,
                    implied_stats = lm1fit$implied_stats,
                    wvalues = wvalues,
                    est_vcov = lm1fit$vcov,
                    df_residual = lm1fit$df_residual)
ce_1b <- indirect_i(x = "m", y = "y",
                    fit = NULL,
                    est = lm1fit$est,
                    data = lm1fit$data,
                    implied_stats = lm1fit$implied_stats,
                    wvalues = wvalues,
                    est_vcov = lm1fit$vcov,
                    df_residual = lm1fit$df_residual)
ce_2 <- indirect_i(x = "x", y = "m",
                   fit = NULL,
                   est = lm2fit$est,
                   data = lm2fit$data,
                   implied_stats = lm2fit$implied_stats,
                   wvalues = wvalues,
                   est_vcov = lm2fit$vcov,
                   df_residual = lm2fit$df_residual)
ce_3 <- indirect_i(x = "m", y = "y",
                   fit = NULL,
                   est = lm3fit$est,
                   data = lm3fit$data,
                   implied_stats = lm3fit$implied_stats,
                   wvalues = wvalues,
                   est_vcov = lm3fit$vcov,
                   df_residual = lm3fit$df_residual)

co_1a <- cond_indirect(x = "x", y = "m",
                       fit = lm1_list,
                       wvalues = wvalues)
co_1b <- cond_indirect(x = "m", y = "y",
                       fit = lm1_list,
                       wvalues = wvalues)
co_2 <- cond_indirect(x = "x", y = "m",
                      fit = lm2_list,
                      wvalues = wvalues)
co_3 <- cond_indirect(x = "m", y = "y",
                      fit = lm3_list,
                      wvalues = wvalues)

test_that("Check SE and df", {
    expect_equal(
        coef(ce_1a),
        x_cond,
        ignore_attr = TRUE
      )
    expect_equal(
        coef(ce_1b),
        m_cond,
        ignore_attr = TRUE
      )
    expect_equal(
        ce_1a$original_se,
        x_cond_se,
        ignore_attr = TRUE
      )
    expect_equal(
        ce_1b$original_se,
        m_cond_se,
        ignore_attr = TRUE
      )

    expect_equal(
        coef(co_1a),
        x_cond,
        ignore_attr = TRUE
      )
    expect_equal(
        coef(co_1b),
        m_cond,
        ignore_attr = TRUE
      )
    expect_equal(
        co_1a$original_se,
        x_cond_se,
        ignore_attr = TRUE
      )
    expect_equal(
        co_1b$original_se,
        m_cond_se,
        ignore_attr = TRUE
      )

    expect_equal(
        ce_1a$original_se,
        ce_2$original_se,
        ignore_attr = TRUE
      )
    expect_equal(
        ce_1b$original_se,
        ce_3$original_se,
        ignore_attr = TRUE
      )
    expect_equal(
        ce_1a$df_residual,
        ce_2$df_residual,
        ignore_attr = TRUE
      )
    expect_equal(
        ce_1b$df_residual,
        ce_3$df_residual,
        ignore_attr = TRUE
      )

    expect_equal(
        co_1a$original_se,
        ce_2$original_se,
        ignore_attr = TRUE
      )
    expect_equal(
        co_1b$original_se,
        ce_3$original_se,
        ignore_attr = TRUE
      )
    expect_equal(
        co_1a$df_residual,
        ce_2$df_residual,
        ignore_attr = TRUE
      )
    expect_equal(
        co_1b$df_residual,
        ce_3$df_residual,
        ignore_attr = TRUE
      )

  })

# SEM

se_1a <- indirect_i(x = "x", y = "m",
                    fit = fit1,
                    est = lav_est(fit1),
                    implied_stats = lav_implied_all(fit1),
                    wvalues = wvalues,
                    est_vcov = get_vcov(fit1),
                    df_residual = lav_df_residual(fit1))

se_1b <- indirect_i(x = "m", y = "y",
                    fit = fit1,
                    est = lav_est(fit1),
                    implied_stats = lav_implied_all(fit1),
                    wvalues = wvalues,
                    est_vcov = get_vcov(fit1),
                    df_residual = lav_df_residual(fit1))

se_2 <- indirect_i(x = "x", y = "m",
                   fit = fit2,
                   est = lav_est(fit2),
                   implied_stats = lav_implied_all(fit2),
                   wvalues = wvalues,
                   est_vcov = get_vcov(fit2),
                   df_residual = lav_df_residual(fit2))

se_3 <- indirect_i(x = "m", y = "y",
                   fit = fit3,
                   est = lav_est(fit3),
                   implied_stats = lav_implied_all(fit3),
                   wvalues = wvalues,
                   est_vcov = get_vcov(fit3),
                   df_residual = lav_df_residual(fit3))

so_1a <- cond_indirect(x = "x", y = "m",
                       fit = fit1,
                       wvalues = wvalues)
so_1b <- cond_indirect(x = "m", y = "y",
                       fit = fit1,
                       wvalues = wvalues)
so_2 <- cond_indirect(x = "x", y = "m",
                      fit = fit2,
                      wvalues = wvalues)
so_3 <- cond_indirect(x = "m", y = "y",
                      fit = fit3,
                      wvalues = wvalues)

test_that("Check SE and df", {
    expect_equal(
        coef(se_1a),
        x_scond,
        ignore_attr = TRUE
      )
    expect_equal(
        coef(se_1b),
        m_scond,
        ignore_attr = TRUE
      )
    expect_equal(
        se_1a$original_se,
        x_scond_se,
        ignore_attr = TRUE
      )
    # # Difference due to the covariances omitted
    # expect_equal(
    #     se_1b$original_se,
    #     m_scond_se,
    #     ignore_attr = TRUE
    #   )

    expect_equal(
        coef(se_3),
        m_scond,
        ignore_attr = TRUE
      )
    expect_equal(
        se_3$original_se,
        m3_scond_se,
        ignore_attr = TRUE
      )

    expect_equal(
        so_1a$original_se,
        se_1a$original_se,
        ignore_attr = TRUE
      )
    expect_equal(
        so_1b$original_se,
        se_1b$original_se,
        ignore_attr = TRUE
      )
    expect_equal(
        so_2$original_se,
        se_2$original_se,
        ignore_attr = TRUE
      )
    expect_equal(
        so_3$original_se,
        se_3$original_se,
        ignore_attr = TRUE
      )

  })

# cond_indirect_effects

cos_1a <- cond_indirect_effects(wlevels = "w1",
                                x = "x", y = "m",
                                fit = lm1_list)
cos_1b <- cond_indirect_effects(wlevels = c("w1", "w2"),
                                x = "m", y = "y",
                                fit = lm1_list)
cos_2 <- cond_indirect_effects(wlevels = "w1",
                               x = "x", y = "m",
                               fit = lm2_list)
cos_3 <- cond_indirect_effects(wlevels = c("w1", "w2"),
                               x = "m", y = "y",
                               fit = lm3_list)

sos_1a <- cond_indirect_effects(wlevels = "w1",
                                x = "x", y = "m",
                                fit = fit1)
sos_1b <- cond_indirect_effects(wlevels = c("w1", "w2"),
                                x = "m", y = "y",
                                fit = fit1)
sos_2 <- cond_indirect_effects(wlevels = "w1",
                               x = "x", y = "m",
                               fit = fit2)
sos_3 <- cond_indirect_effects(wlevels = c("w1", "w2"),
                               x = "m", y = "y",
                               fit = fit3)

test_that("Print", {
    expect_output(print(cos_1b),
                  "Stat",
                  fixed = TRUE)
    expect_output(print(cos_1b),
                  "regression",
                  fixed = TRUE)
    expect_false(any(grepl("pvalue",
                           capture.output(print(cos_1b, pvalue = FALSE)),
                           fixed = TRUE)))
    expect_false(any(grepl("pvalue",
                           capture.output(print(cos_1b, pvalue = FALSE)),
                           fixed = TRUE)))
    expect_false(any(grepl("CI.lo",
                           capture.output(print(cos_1b, se_ci = FALSE)),
                           fixed = TRUE)))
    expect_output(print(sos_1b),
                  "Stat",
                  fixed = TRUE)
    expect_output(print(sos_1b),
                  "lavaan",
                  fixed = TRUE)
    expect_false(any(grepl("pvalue",
                           capture.output(print(sos_1b, pvalue = FALSE)),
                           fixed = TRUE)))
    expect_false(any(grepl("pvalue",
                           capture.output(print(sos_1b, pvalue = FALSE)),
                           fixed = TRUE)))
    expect_false(any(grepl("CI.lo",
                           capture.output(print(sos_1b, se_ci = FALSE)),
                           fixed = TRUE)))
    expect_output(print(sos_1b, level = .60),
                  "60.0%",
                  fixed = TRUE)
  })

# CI

test_that("confint", {
    expect_equal(confint(cos_1a),
                 confint(cos_2),
                 ignore_attr = TRUE)
    expect_equal(confint(cos_1b),
                 confint(cos_3),
                 ignore_attr = TRUE)
    expect_equal(confint(sos_1a),
                 confint(sos_2),
                 ignore_attr = TRUE)
    # Should not be equal due to covariances fixed to zero
    # expect_equal(confint(sos_1b),
    #              confint(sos_3),
    #              ignore_attr = TRUE)
    expect_equal(confint(sos_1a, level = .75),
                 confint(sos_2, level = .75),
                 ignore_attr = TRUE)
    expect_true(all(confint(sos_1a, level = .75)[, 1] >
                    confint(sos_2, level = .85)[, 1]))
    expect_true(all(confint(cos_1b, level = .75)[, 2] <
                    confint(cos_3, level = .85)[, 2]))

    sos_1a_se <- cond_effects_original_se(sos_1a)
    sos_1a_full <- attr(sos_1a, "full_output")
    expect_equal(as.vector(confint(sos_1a_full[[2]])),
                 c(sos_1a_se$cilo[2], sos_1a_se$cihi[2]),
                 ignore_attr = TRUE)
  })
