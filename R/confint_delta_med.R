#' @title Confidence Interval for
#' Delta_Med in a 'delta_med'-Class Object
#'
#' @description Return the confidence
#' interval of the Delta_Med in the
#' output of [delta_med()].
#'
#' @details It returns the nonparametric
#' bootstrap
#' percentile confidence interval of
#' Delta_Med, proposed byLiu, Yuan, and
#' Li (2023). The object must be the
#' output of [delta_med()], with
#' bootstrap confidence interval
#' requested when calling [delta_med()].
#' However, the level of confidence
#' can be different from that used when
#' call [delta_med()].
#'
#' @return
#' A one-row matrix of the confidence
#' interval. All values are `NA` if
#' bootstrap confidence interval was
#' not requested when calling
#' [delta_med()].
#'
#' @param object The output of
#' [delta_med()].
#'
#' @param parm Not used because only
#' one parameter, the Delta_Med, is
#' allowed.
#'
#' @param level The level of confidence,
#' default is `NULL` and the level used
#' when the object was created will be
#' used.
#'
#' @param boot_type If bootstrap
#' confidence interval is to be formed,
#' the type of bootstrap confidence
#' interval. The supported types
#' are `"perc"` (percentile bootstrap
#' confidence interval, the recommended
#' method) and `"bc"`
#' (bias-corrected, or BC, bootstrap
#' confidence interval). If not supplied,
#' the stored `boot_type` will be used.
#'
#' @param ...  Optional arguments.
#' Ignored.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [delta_med()]
#'
#' @examples
#'
#' library(lavaan)
#' dat <- data_med
#' mod <-
#' "
#' m ~ x
#' y ~ m + x
#' "
#' fit <- sem(mod, dat)
#'
#' # Call do_boot() to generate
#' # bootstrap estimates
#' # Use 2000 or even 5000 for R in real studies
#' # Set parallel to TRUE in real studies for faster bootstrapping
#' boot_out <- do_boot(fit,
#'                     R = 45,
#'                     seed = 879,
#'                     parallel = FALSE,
#'                     progress = FALSE)
#' # Remove 'progress = FALSE' in practice
#' dm_boot <- delta_med(x = "x",
#'                      y = "y",
#'                      m = "m",
#'                      fit = fit,
#'                      boot_out = boot_out,
#'                      progress = FALSE)
#' dm_boot
#' confint(dm_boot)
#'
#' @export

confint.delta_med <- function(object,
                              parm,
                              level = NULL,
                              boot_type,
                              ...) {
    if (missing(boot_type)) {
        ci_boot_type <- object$boot_type
      } else {
        ci_boot_type <- boot_type
      }
    if (is.null(level)) {
        ci_level <- object$level
      } else {
        ci_level <- level
      }
    if ((ci_boot_type == object$boot_type) &&
        (ci_level == object$level)) {
          new_ci <- FALSE
        } else {
          new_ci <- TRUE
        }

    # Borrowed from stats::confint()
    probs <- c((1 - ci_level) / 2, 1 - (1 - ci_level) / 2)
    cnames <- paste(format(100 * probs,
                            trim = TRUE,
                            scientific = FALSE,
                            digits = 2), "%")

    if (!is.null(object$boot_ci)) {
        has_boot_ci <- TRUE
      } else {
        has_boot_ci <- FALSE
      }
    out <- matrix(NA, 1, 2)
    rownames(out) <- "Delta_Med"
    colnames(out) <- cnames
    if (!has_boot_ci) {
        return(out)
      } else {
        dm_boot <- object$boot_est
        R <- length(stats::na.omit(dm_boot))
        tmp <- switch(ci_boot_type,
                      perc = "Percentile",
                      bc = "Bias-Corrected")
        if (new_ci) {
            dm_boot_out <- form_boot_ci(est = object$delta_med,
                                        boot_est = dm_boot,
                                        level = ci_level,
                                        boot_type = ci_boot_type)

            out[] <- dm_boot_out$boot_ci
            colnames(out) <- paste(tmp, colnames(out))
          } else {
            out[] <- object$boot_ci
            colnames(out) <- paste(tmp, colnames(out))
          }
        return(out)
      }
  }