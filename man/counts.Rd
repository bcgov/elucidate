% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/counts.R
\name{counts}
\alias{counts}
\title{Obtain the counts for unique values of a vector.}
\usage{
counts(y, n = "all", order = c("d", "a", "i"), sep = "_", na.rm = TRUE)
}
\arguments{
\item{y}{A vector/variable (required).}

\item{n}{The number of unique values you want frequency counts for. Default is "all".}

\item{order}{"d" for descending/decreasing order. "a" or "i" for
ascending/increasing order.}

\item{sep}{A character string to use to separate unique values from their counts
("_" by default).}

\item{na.rm}{Should missing values be omitted (TRUE/FALSE)?}
}
\value{
A character vector of the unique value frequency counts from the
input vector sorted in the chosen order. Return values are structured as
"value_count", where the "_" portion takes on the value of the sep
argument. Returning a character vector makes subsequent manipulation with
stringr and other tidyverse tools fairly easily.
}
\description{
\code{counts} returns the most or least common unique value(s)
of a vector depending on whether ascending or descending sorting is used.
Also useful for identifying data entry errors or rare cases. For complex
use cases see \code{\link{describe}}.
}
\examples{
#using a numeric vector
y <- c(1, 1, 2, 3, 4, 5, 6, 2, 3, 4, 2, 9, 5, 6, 7, 23, 5, 6, 7, 8, 3, 4, 5, 6)

counts(y) #all unique values of y in order of descending frequency \code{y}
counts(y, n = 1) #the most common value of \code{y}
counts(y, n = 5) #the top 5 most common unique values of \code{y}

}
\seealso{
\code{\link{table}}, \code{\link{sort}}
}
\author{
Craig P. Hutton, \email{Craig.Hutton@gov.bc.ca}
}
