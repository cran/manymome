% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cond_indirect_diff.R
\name{confint.cond_indirect_diff}
\alias{confint.cond_indirect_diff}
\title{Confidence Interval of the
Output of 'cond_indirect_diff()'}
\usage{
\method{confint}{cond_indirect_diff}(object, parm, level = 0.95, ...)
}
\arguments{
\item{object}{The output of
\code{\link[=cond_indirect_diff]{cond_indirect_diff()}}.}

\item{parm}{Ignored.}

\item{level}{The level of confidence
for the confidence
interval. Default is .95. Must match
the level of the stored confidence
interval.}

\item{...}{Optional arguments.
Ignored.}
}
\value{
A one-row-two-column data
frame of the confidence limits. If
confidence interval is not available,
the limits are \code{NA}s.
}
\description{
Extract the confidence
interval the output of
\code{\link[=cond_indirect_diff]{cond_indirect_diff()}}.
}
\details{
The \code{confint} method of the
\code{cond_indirect_diff}-class object.

The type of confidence intervals
depends on the call used to
create the object. This function
merely extracts the stored
confidence intervals.
}
