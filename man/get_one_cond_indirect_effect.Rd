% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_indirect_effect.R
\name{get_one_cond_indirect_effect}
\alias{get_one_cond_indirect_effect}
\alias{get_one_cond_effect}
\alias{print_all_cond_indirect_effects}
\alias{print_all_cond_effects}
\title{Get The Conditional Indirect
Effect for One Row of
'cond_indirect_effects' Output}
\usage{
get_one_cond_indirect_effect(object, row)

get_one_cond_effect(object, row)

print_all_cond_indirect_effects(object, ...)

print_all_cond_effects(object, ...)
}
\arguments{
\item{object}{The output of
\code{\link[=cond_indirect_effects]{cond_indirect_effects()}}.}

\item{row}{The row number of the row
to be retrieved.}

\item{...}{Optional arguments to be
passed to teh \code{print} method of
the output of
\code{\link[=indirect_effect]{indirect_effect()}} and
\code{\link[=cond_indirect]{cond_indirect()}}}
}
\value{
\code{\link[=get_one_cond_indirect_effect]{get_one_cond_indirect_effect()}}
returns an \code{indirect}-class object,
similar to the output of
\code{\link[=indirect_effect]{indirect_effect()}} and
\code{\link[=cond_indirect]{cond_indirect()}}. See
\code{\link[=indirect_effect]{indirect_effect()}} and
\code{\link[=cond_indirect]{cond_indirect()}} for details on
these classes.

\code{\link[=print_all_cond_indirect_effects]{print_all_cond_indirect_effects()}}
returns the object invisibly. Called
for its side effect.
}
\description{
Return the conditional
indirect effect of one row of the
output of \code{\link[=cond_indirect_effects]{cond_indirect_effects()}}.
}
\details{
\code{\link[=get_one_cond_indirect_effect]{get_one_cond_indirect_effect()}}
extracts the
corresponding output of
\code{\link[=cond_indirect]{cond_indirect()}} from the requested
row.

\code{\link[=get_one_cond_effect]{get_one_cond_effect()}} is an
alias of \code{\link[=get_one_cond_indirect_effect]{get_one_cond_indirect_effect()}}.

\code{\link[=print_all_cond_indirect_effects]{print_all_cond_indirect_effects()}} loops over
the conditional effects and print all
of them.

\code{\link[=print_all_cond_effects]{print_all_cond_effects()}} is an
alias of \code{\link[=print_all_cond_indirect_effects]{print_all_cond_indirect_effects()}}.
}
\examples{

library(lavaan)
dat <- modmed_x1m3w4y1
mod <-
"
m1 ~ x  + w1 + x:w1
m2 ~ m1
y  ~ m2 + x + w4 + m2:w4
"
fit <- sem(mod, dat,
           meanstructure = TRUE, fixed.x = FALSE,
           se = "none", baseline = FALSE)
est <- parameterEstimates(fit)

# Examples for cond_indirect():

# Conditional effects from x to m1
# when w1 is equal to each of the default levels
out1 <- cond_indirect_effects(x = "x", y = "m1",
                              wlevels = c("w1", "w4"), fit = fit)
get_one_cond_indirect_effect(out1, 3)

# Conditional Indirect effect from x1 through m1 to y,
# when w1 is equal to each of the levels
out2 <- cond_indirect_effects(x = "x", y = "y", m = c("m1", "m2"),
                              wlevels = c("w1", "w4"), fit = fit)
get_one_cond_indirect_effect(out2, 4)


print_all_cond_indirect_effects(out2, digits = 2)

}
\seealso{
\link{cond_indirect_effects}
}
