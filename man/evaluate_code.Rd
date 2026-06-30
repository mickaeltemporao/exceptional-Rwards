\name{evaluate_code}
\alias{evaluate_code}
\title{Evaluate Code and Reward Errors}
\usage{
evaluate_code(expr)
}
\arguments{
\item{expr}{An unevaluated R expression (for example one produced by
\code{\link{quote}}) to evaluate.}
}
\value{
The value of \code{expr}. If evaluating \code{expr} raises an error, the error
is turned into experience points (and a level / progress-bar update) and the
captured error condition is returned invisibly instead of being signalled.
}
\description{
Evaluate an R expression, turning any error it raises into experience points.
This is a legacy helper: once the package is attached with
\code{library(Rwards)} the installed error handler rewards errors
automatically, so \code{evaluate_code} is rarely needed.
}
\examples{
# A successful expression is returned as usual
evaluate_code(quote(1 + 1))
}
\keyword{utilities}
