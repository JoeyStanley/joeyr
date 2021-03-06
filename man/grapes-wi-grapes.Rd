% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grapes.R
\name{\%wi\%}
\alias{\%wi\%}
\title{Within}
\usage{
x \%wi\% range
}
\arguments{
\item{x}{A numeric vector.}

\item{range}{A numeric vector. The highest and lowest values will be used as the range.}
}
\value{
A logical vector.
}
\description{
Here's a very handy "within" range. Checks whether a number is within a range. Capable of handling 
lists so that it can apply to a whole column. Basically I got tired of running a two-part conditional
when I wanted something that was within a range: \code{if (x > 2 & x < 5)}.
}
\examples{
4 \%wi\% c(1,5)
c(2, 3) \%wi\% c(1,5)
c(1:5) \%wi\% c(4,6)
}
