% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_effect_vs_w.R
\name{plot_effect_vs_w}
\alias{plot_effect_vs_w}
\alias{fill_wlevels}
\title{Plot an Effect Against a Moderator}
\usage{
plot_effect_vs_w(
  object,
  w = NULL,
  w_label = NULL,
  effect_label = NULL,
  add_zero_line = TRUE,
  always_draw_zero_line = FALSE,
  line_linewidth = 1,
  line_color = "blue",
  shade_the_band = TRUE,
  draw_the_intervals = TRUE,
  band_fill_color = "lightgrey",
  band_alpha = 0.5,
  intervals_color = "black",
  intervals_linetype = "longdash",
  intervals_linewidth = 1,
  zero_line_color = "grey",
  zero_line_linewidth = 1,
  zero_line_linetype = "solid",
  line_args = list(),
  band_args = list(),
  intervals_args = list(),
  zero_line_args = list(),
  level = 0.95
)

fill_wlevels(to_fill, cond_out = NULL, k = 21)
}
\arguments{
\item{object}{The output of
\code{\link[=cond_indirect_effects]{cond_indirect_effects()}}.}

\item{w}{The name of the moderator.
Must be present in \code{object}. If \code{NULL},
the default,
and \code{object} has only one moderator,
then it will be set to that moderator.
Because this function currently only
supports a path with only one moderator,
this argument can be left as \code{NULL} for now.}

\item{w_label}{The label of the
horizontal axis. If \code{NULL},
the default, it will be
\code{paste0("Moderator: ", w)}.}

\item{effect_label}{The label of the
vertical axis. If \code{NULL}, the default,
it will be generated from the path.}

\item{add_zero_line}{Whether a
horizontal line at zero will be drawn.
Default is \code{TRUE}.}

\item{always_draw_zero_line}{If \code{FALSE},
the default,
then the line at zero, if requested
will be drawn
only if zero is within the range of
the plot. If \code{TRUE}, then the line
at zero will always be drawn.}

\item{line_linewidth}{The width of
the line of the effect for each
level of the moderator, to be used
by \code{\link[ggplot2:geom_path]{ggplot2::geom_line()}}. Default is 1.
Always overrides the value of
\code{line_args}.}

\item{line_color}{The color of the line
of the effect for each level of the
moderator, to be used
by \code{\link[ggplot2:geom_path]{ggplot2::geom_line()}}.
Default is "blue".
Always overrides the value of
\code{line_args}.}

\item{shade_the_band}{If \code{TRUE},
the default, a confidence band will
be drawn as a region along the line
if confidence intervals can be retrieved
from \code{object}.}

\item{draw_the_intervals}{If \code{TRUE},
the default, two lines will be drawn
for the confidence intervals along
the line if they can be retrieved
from \code{object}.}

\item{band_fill_color}{The color of
of the confidence band, to be used
by \code{\link[ggplot2:geom_ribbon]{ggplot2::geom_ribbon()}}.
Default is "lightgrey".
Always overrides the value of
\code{band_args}.}

\item{band_alpha}{A number from
0 to 1 for the level
of transparency
of the confidence band, to be used
by \code{\link[ggplot2:geom_ribbon]{ggplot2::geom_ribbon()}}.
Default is \code{.50}.
Always overrides the value of
\code{band_args}.}

\item{intervals_color}{The color of
the lines of the confidence intervals,
to be used
by \code{\link[ggplot2:geom_path]{ggplot2::geom_line()}}. Default is
\code{"black"}.
Always overrides the value of
\code{intervals_args}.}

\item{intervals_linetype}{The line
type of
the lines of the confidence intervals,
to be used
by \code{\link[ggplot2:geom_path]{ggplot2::geom_line()}}. Default is
\code{"longdash"}.
Always overrides the value of
\code{intervals_args}.}

\item{intervals_linewidth}{The line
width of
the lines of the confidence intervals,
to be used
by \code{\link[ggplot2:geom_path]{ggplot2::geom_line()}}. Default is
1.
Always overrides the value of
\code{intervals_args}.}

\item{zero_line_color}{The color of
the line at zero,
to be used
by \code{\link[ggplot2:geom_path]{ggplot2::geom_line()}}. Default is
\code{"grey"}.
Always overrides the value of
\code{zero_line_args}.}

\item{zero_line_linewidth}{The line
width of
the line at zero,
to be used
by \code{\link[ggplot2:geom_path]{ggplot2::geom_line()}}. Default is
1.
Always overrides the value of
\code{zero_line_args}.}

\item{zero_line_linetype}{The line
type of
the line at zero,
to be used
by \code{\link[ggplot2:geom_path]{ggplot2::geom_line()}}. Default is
\code{"solid"}.
Always overrides the value of
\code{zero_line_args}.}

\item{line_args}{A named list of
additional arguments to be passed
to \code{\link[ggplot2:geom_path]{ggplot2::geom_line()}} for the line
of the effect against moderator.
Default is \code{list()}.}

\item{band_args}{A named list of
additional arguments to be passed
to \code{\link[ggplot2:geom_ribbon]{ggplot2::geom_ribbon()}} for the
confidence band.
Default is \code{list()}.}

\item{intervals_args}{A named list of
additional arguments to be passed
to \code{\link[ggplot2:geom_path]{ggplot2::geom_line()}} for the lines
of confidence intervals.
Default is \code{list()}.}

\item{zero_line_args}{A named list of
additional arguments to be passed
to \code{\link[ggplot2:geom_path]{ggplot2::geom_line()}} for the line
at zero.
Default is \code{list()}.}

\item{level}{The level of confidence
for the confidence intervals computed
from the original standard errors. Used only for
paths without mediators and both
\code{x}- and \code{y}-variables are not
standardized.}

\item{to_fill}{The output of
\code{\link[=cond_indirect_effects]{cond_indirect_effects()}} or
\code{\link[=pseudo_johnson_neyman]{pseudo_johnson_neyman()}}, for which
additional levels of the moderator
will be added.}

\item{cond_out}{If \code{to_fill} is
the output of \code{\link[=pseudo_johnson_neyman]{pseudo_johnson_neyman()}},
the original output of \code{\link[=cond_indirect_effects]{cond_indirect_effects()}}
used in the call to \code{\link[=pseudo_johnson_neyman]{pseudo_johnson_neyman()}}
need to be supplied through this argument.}

\item{k}{The desired number of levels
of the moderator.}
}
\value{
\code{\link[=plot_effect_vs_w]{plot_effect_vs_w()}} returns a
\code{ggplot2} graph. Plotted if
not assigned to a name. It can be
further modified like a usual
\code{ggplot2} graph.

\code{\link[=fill_wlevels]{fill_wlevels()}} returns an updated
output of \code{\link[=cond_indirect_effects]{cond_indirect_effects()}}
with the desired number of levels of
the moderator.
}
\description{
It plots an effect, direct
or indirect,
against a moderator, with confidence
band if available.
}
\details{
It receives an output
of \code{\link[=cond_indirect_effects]{cond_indirect_effects()}} and
plot the effect against the
moderator. The effect can be an
indirect effect or a direct effect.

It uses the levels of the moderator
stored in the output of
\code{\link[=cond_indirect_effects]{cond_indirect_effects()}}. Therefore,
the desired levels of the moderator
to be plotted needs to be specified
when calling \code{\link[=cond_indirect_effects]{cond_indirect_effects()}},
as illustrated in the example.

Currently, this function only supports
a path with exactly one moderator,
and the moderator is a numeric variable.
\subsection{Using Original Standard Errors}{

If the following conditions are met, the
stored standard errors, if available,
will be used to
form the confidence intervals:
\itemize{
\item Confidence intervals have not been
formed (e.g., by bootstrapping or
Monte Carlo).
\item The path has no mediators.
\item The model has only one group.
\item The path is moderated by one or
more moderator.
\item Both the \code{x}-variable and the
\code{y}-variable are not standardized.
}

If the model is fitted by OLS
regression (e.g., using \code{\link[stats:lm]{stats::lm()}}),
then the variance-covariance matrix
of the coefficient estimates will be
used, and confidence
intervals are computed from the \emph{t}
statistic.

If the model is fitted by structural
equation modeling using \code{lavaan}, then
the variance-covariance computed by
\code{lavaan} will be used,
and confidence intervals are computed
from the \emph{z} statistic.
}

\subsection{Caution}{

If the model is fitted by structural
equation modeling and has moderators,
the standard errors, \emph{p}-values,
and confidence interval computed
from the variance-covariance matrices
for conditional effects
can only be trusted if all covariances
involving the product terms are free.
If any of them are fixed, for example,
fixed to zero, it is possible
that the model is not invariant to
linear transformation of the variables.
}

The function \code{\link[=fill_wlevels]{fill_wlevels()}} is
a helper to automatically
fill in additional levels
of the moderators, to plot a graph
with smooth confidence band. It
accepts the output of \code{\link[=cond_indirect_effects]{cond_indirect_effects()}}
or \code{\link[=pseudo_johnson_neyman]{pseudo_johnson_neyman()}}, finds
the range of the values of the
moderator, and returns an output
of \code{\link[=cond_indirect_effects]{cond_indirect_effects()}} with
the desired number of levels within
this range. It is intended to be a
helper. If it does not work, users
can still get the desired number of
levels by setting the values manually
when calling \code{\link[=cond_indirect_effects]{cond_indirect_effects()}}.
}
\examples{

dat <- data_med_mod_a
lm_m <- lm(m ~ x*w + c1 + c2, dat)
lm_y <- lm(y ~ m + x + c1 + c2, dat)
fit_lm <- lm2list(lm_m, lm_y)
# Set R to a large value in real research.
boot_out_lm <- do_boot(fit_lm,
                       R = 50,
                       seed = 54532,
                       parallel = FALSE,
                       progress = FALSE)

# Compute the conditional indirect effects
# from 2 SD below mean to 2 SD above mean of the moderator,
# by setting sd_from_mean of cond_indirect_effects().
# Set length.out to a larger number for a smooth graph.
out_lm <- cond_indirect_effects(wlevels = "w",
                                x = "x",
                                y = "y",
                                m = "m",
                                fit = fit_lm,
                                sd_from_mean = seq(-2, 2, length.out = 10),
                                boot_ci = TRUE,
                                boot_out = boot_out_lm)
p <- plot_effect_vs_w(out_lm)
p
# The output is a ggplot2 graph and so can be further customized
library(ggplot2)
# Add the line for the mean of w, the moderator
p2 <- p + geom_vline(xintercept = mean(dat$w),
                     color = "red")
p2


# Use fill_wlevels to add moderator levels:

dat <- data_med_mod_a
lm_m <- lm(m ~ x*w + c1 + c2, dat)
lm_y <- lm(y ~ m + x + c1 + c2, dat)
fit_lm <- lm2list(lm_m, lm_y)
wlevels <- mod_levels(w = "w",
                      sd_from_mean = c(-3, 0, 3),
                      fit = fit_lm)
wlevels
cond_out <- cond_indirect_effects(wlevels = wlevels,
                                  x = "x",
                                  y = "m",
                                  fit = fit_lm)
cond_out
# Only 3 points
p1 <- plot_effect_vs_w(cond_out)
p1
# Increase the number of levels to 15
cond_out_filled <- fill_wlevels(cond_out,
                                k = 15)
cond_out_filled
p2 <- plot_effect_vs_w(cond_out_filled)
p2


}
\seealso{
\code{\link[=cond_indirect_effects]{cond_indirect_effects()}}
}
