#' @title Monte Carlo Estimates for a
#' `lavaan` Output
#'
#' @description Generate Monte Carlo
#' estimates from the output of
#' [lavaan::sem()].
#'
#' @details This function is for
#' advanced users. [do_mc()] is a
#' function users should try first
#' because [do_mc()] has a general
#' interface for input-specific
#' functions like this one.
#'
#' [fit2mc_out()] can be used
#' to extract the stored Monte Carlo
#' estimates so that they can be reused
#' by [indirect_effect()],
#' [cond_indirect_effects()] and related
#' functions to form Monte Carlo
#' confidence intervals for effects such
#' as indirect effects and conditional
#' indirect effects.
#'
#' This approach removes the need to
#' repeat Monte Carlo simulation in each
#' call to
#' [indirect_effect()],
#' [cond_indirect_effects()], and
#' related functions. It also ensures
#' that the same set of Monte Carlo
#' estimates is used in all subsequent
#' analyses.
#'
#' @return A `mc_out`-class object
#' that can be used for the `mc_out`
#' argument of [indirect_effect()],
#' [cond_indirect_effects()], and
#' related functions for forming
#' Monte Carlo confidence intervals.
#'
#' The object is a list with the number
#' of elements equal to the number of
#' Monte Carlo replications. Each element
#' is a
#' list of the parameter estimates and
#' sample variances and covariances of
#' the variables in each Monte Carlo
#' replication.
#'
#' @param fit The fit object. This
#' function only supports a
#' [lavaan::lavaan-class] object.
#' It can also be
#' a `lavaan.mi` object
#' returned by
#' [lavaan.mi::lavaan.mi()] or
#' its wrapper, such as [lavaan.mi::sem.mi()].
#'
#' @param compute_implied_stats If
#' `TRUE`, default, implied statistics
#' will be computed for each replication.
#' Letting users to disable this
#' is an experimental features to let
#' the process run faster.
#'
#' @param progress Logical. Display
#' progress or not. Default is `TRUE`.
#'
#' @seealso [do_mc()], the general
#' purpose function that users should
#' try first before using this function.
#'
#' @examples
#'
#' library(lavaan)
#' data(data_med_mod_ab1)
#' dat <- data_med_mod_ab1
#' dat$"x:w" <- dat$x * dat$w
#' dat$"m:w" <- dat$m * dat$w
#' mod <-
#' "
#' m ~ x + w + x:w + c1 + c2
#' y ~ m + w + m:w + x + c1 + c2
#' "
#'
#' fit <- sem(model = mod, data = dat, fixed.x = FALSE,
#'            baseline = FALSE)
#' # In real research, R should be 5000 or even 10000.
#' fit <- gen_mc_est(fit, R = 100, seed = 453253)
#' fit_mc_out <- fit2mc_out(fit)
#' out <- cond_indirect_effects(wlevels = "w",
#'                              x = "x",
#'                              y = "y",
#'                              m = "m",
#'                              fit = fit,
#'                              mc_ci = TRUE,
#'                              mc_out = fit_mc_out)
#' out
#'
#' @export

fit2mc_out <- function(fit,
                       progress = TRUE,
                       compute_implied_stats = TRUE) {
    if (progress) {
        cat("Stage 1: Simulate estimates\n")
      }
    mc_est <- mc2est(fit,
                     progress = progress)
    if (compute_implied_stats) {
      if (progress) {
        cat("Stage 2: Compute implied statistics\n")
      }
      mc_implied <- mc2implied(fit,
                              progress = progress)
    } else {
      mc_implied <- rep(list(NULL),
                        length(mc_est))
    }
    out <- mapply(function(x, y) list(est = x,
                                      implied_stats = y),
                  x = mc_est,
                  y = mc_implied,
                  SIMPLIFY = FALSE)
    names(out) <- names(mc_est)
    class(out) <- "mc_out"
    out
  }

# Convert stored estimates to a list of
# parameter estimates tables. This is
# preferred because it is what users
# usually see.
#' @noRd

mc2est <- function(fit,
                   progress = TRUE) {
    if (is.null(fit@external$manymome$mc)) {
        stop("Monte Carlo estimates not found. Please run do_mc() first.")
      }
    mc_est0 <- fit@external$manymome$mc
    ptable <- lav_ptable(fit)
    p_free <- ptable$free > 0
    mc_est <- split(mc_est0, row(mc_est0))
    # set_est_i() supports both mc and boot
    if (inherits(fit, "lavaan")) {
        est_df0 <- lav_est(fit,
                           se = FALSE,
                           ci = FALSE,
                           rsquare = FALSE)
      }
    if (inherits(fit, "lavaan.mi")) {
        est_df0 <- lav_est(fit,
                           se = FALSE,
                           ci = FALSE)
      }
    # out_all <- lapply(mc_est, set_est_i,
    #                     fit = fit,
    #                     p_free = p_free,
    #                     est_df = est_df0)

    type <- cond_indirect_check_fit(fit)

    if (type == "lavaan") {
      # Precompute the matching index to avoid using merge()
      est_df0$row_id <- seq_len(nrow(est_df0))
      ptable_template <- as.data.frame(fit@ParTable)
      if ("group" %in% colnames(est_df0)) {
        ptable_tmp <- ptable_template[, c("lhs", "op", "rhs", "block", "group")]
      } else {
        ptable_tmp <- ptable_template[, c("lhs", "op", "rhs")]
      }
      tmp <- merge(ptable_tmp,
                   est_df0,
                   all.x = TRUE,
                   sort = FALSE)
      select_id <- which(!is.na(tmp$row_id))
      match_id <- tmp$row_id[select_id]
      est_df0$row_id <- NULL

    } else {
      ptable_template <- NULL
      match_id <- NULL
      select_id <- NULL
    }

    if (progress) {
        out_all <- suppressWarnings(pbapply::pblapply(mc_est, set_est_i,
                                                     fit = fit,
                                                     p_free = p_free,
                                                     est_df = est_df0,
                                                     ptable = ptable_template,
                                                     match_id = match_id,
                                                     select_id = select_id))
      } else {
        out_all <- suppressWarnings(lapply(mc_est, set_est_i,
                                           fit = fit,
                                           p_free = p_free,
                                           est_df = est_df0,
                                           ptable = ptable_template,
                                           match_id = match_id,
                                           select_id = select_id))
      }
    out_all
  }

# Get the implied statistics from stored parameter estimates
#' @noRd

mc2implied <- function(fit,
                       progress = TRUE) {
    if (is.null(fit@external$manymome$mc)) {
        stop("Monte Carlo estimates not found. Please run do_mc() first.")
      }
    # NOTE: Support fixed.x = TRUE
    mc_est0 <- fit@external$manymome$mc
    mc_est <- split(mc_est0, row(mc_est0))
    if (inherits(fit, "lavaan.mi")) {
        fit_tmp <- methods::new("lavaan",
                      version = as.character(utils::packageVersion("lavaan")))
        fit_tmp@Model <- fit@Model
        fit_tmp@Data <- fit@Data
        fit_tmp@ParTable <- fit@ParTableList[[1]]
        fit_tmp@pta <- fit@pta
        fit_tmp@Options <- fit@Options
      } else {
        fit_tmp <- NULL
      }
    # get_implied_i() supports both mc and boot
    # out_all <- lapply(mc_est, get_implied_i,
    #                     fit = fit,
    #                     fit_tmp = fit_tmp)
    ovnames <- lavaan::lavNames(fit, "ov")
    lvnames <- lavaan::lavNames(fit, "lv")
    if (!is.null(fit_tmp)) {
      ovnames_tmp <- lavaan::lavNames(fit_tmp, "ov")
      lvnames_tmp <- lavaan::lavNames(fit_tmp, "lv")
    } else {
      ovnames_tmp <- NULL
      lvnames_tmp <- NULL
    }
    if (progress) {
        out_all <- suppressWarnings(pbapply::pblapply(mc_est,
                                                      get_implied_i,
                                                      fit = fit,
                                                      fit_tmp = fit_tmp,
                                                      ovnames = ovnames,
                                                      lvnames = lvnames,
                                                      ovnames_tmp = ovnames_tmp,
                                                      lvnames_tmp = lvnames_tmp))
      } else {
        out_all <- suppressWarnings(lapply(mc_est,
                                           get_implied_i,
                                           fit = fit,
                                           fit_tmp = fit_tmp,
                                           ovnames = ovnames,
                                           lvnames = lvnames,
                                           ovnames_tmp = ovnames_tmp,
                                           lvnames_tmp = lvnames_tmp))
      }
    out_all
  }
