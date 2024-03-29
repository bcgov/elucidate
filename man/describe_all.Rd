% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/describe.R
\name{describe_all}
\alias{describe_all}
\title{Obtain descriptive summaries for all variables in a data frame.}
\usage{
describe_all(
  data,
  ...,
  class = "all",
  digits = 3,
  type = 2,
  na.rm = TRUE,
  sep = "_",
  output = c("dt", "tibble")
)
}
\arguments{
\item{data}{A data frame or tibble.}

\item{...}{This special argument accepts any number of unquoted grouping
variable names (also present in the data source) to use for subsetting,
separated by commas, e.g. \verb{group_var1, group_var2}. Also accepts a
character vector of column names or index numbers, e.g. c("group_var1",
"group_var2") or c(1, 2), but not a mixture of formats in the same call. If
no column names are specified, all columns will be used.}

\item{class}{The variable classes in data that you would like summaries for.
Either "all" for all classes, or a character vector indicating which
combinations of output classes you want. Specifying a subset will save time
since summaries are only processed as needed. Options include "d" for
dates, "f" for factors, "c" for character, "l" for logical, and "n" for
numeric. If only a single class is requested or present in the data after
excluding specified grouping variables, a data frame will be returned,
otherwise you'll get a list of data frames (1 per summary class). If the
only chosen class of variables is not detected in the input data an error
will be returned that the class argument needs to be respecified.}

\item{digits}{This determines the number of digits used for rounding of
numeric outputs.}

\item{type}{For numeric and integer vectors this determines the type of
skewness and kurtosis calculations to perform. See
\code{\link[e1071]{skewness}} or \code{\link[psych]{skew}} and
\code{\link[e1071]{kurtosis}} or \code{\link[psych]{kurtosi}} for details.}

\item{na.rm}{This determines whether missing values (NAs) should be removed
before attempting to calculate summary statistics.}

\item{sep}{A character string to use to separate unique values from their
counts ("_" by default). Only applicable to factors and character vectors.}

\item{output}{Output type for each class of variables. dt" for data.table or
"tibble" for tibble.}
}
\value{
The output varies as a function of the class of input data/y,
referred to as "y" below. Each output type is grouped together in a data
frame and returned as a named item of a list, unless there is only one
output type, in which case the data frame is returned directly.

\strong{For all input variables, the following are returned (part 1):}

\describe{
\item{cases}{the total number of cases}
\item{n}{number of complete cases}
\item{na}{the number of missing values}
\item{p_na}{the proportion of total cases with missing values}
}

In addition to part 1, these measures are provided for \strong{dates}:

\describe{
\item{n_unique}{the total number of unique values or levels of y. For dates this tells you how many time points there are}
\item{start}{the earliest or minimum date in y}
\item{end}{the latest or maximum date in y}
}

In addition to part 1, these measures are provided for \strong{factors}:

\describe{
\item{n_unique}{the total number of unique values or levels of y}
\item{ordered}{a logical indicating whether or not y is ordinal}
\item{counts_tb}{the counts of the top and bottom unique values of y in order of decreasing frequency formatted as "value_count". If there are more than 4 unique values of y, only the top 2 and bottom 2 unique values are shown separated by "...". To get counts for all unique values use \code{\link{counts}} or \code{\link{counts_tb}} instead.}
}

In addition to part 1, these measures are provided for \strong{character/string} vectors:

\describe{
\item{n_unique}{the total number of unique values or levels of y}
\item{min_chars}{the minimum number of characters in the values of y}
\item{max_chars}{the maximum number of characters in the values of y}
\item{counts_tb}{the counts of the top and bottom unique values of y in order of decreasing frequency formatted as "value_count". If there are more than 4 unique values of y, only the top 2 and bottom 2 unique values are shown separated by "...". To get counts for all unique values use \code{\link{counts}} or \code{\link{counts_tb}} instead.}
}

In addition to part 1, these measures are provided for \strong{logical} vectors:

\describe{
\item{n_TRUE}{the total number of y values that are TRUE}
\item{n_FALSE}{the total number of y values that are FALSE}
\item{p_TRUE}{the proportion of y values that are TRUE}
}

In addition to part 1, these measures are provided for \strong{numeric} variables:

\describe{
\item{mean}{the mean of y}
\item{sd}{the standard deviation of y}
\item{se}{the standard error of the mean of y}
\item{p0}{the 0th percentile (the minimum) of y}
\item{p25}{the 25th percentile of y}
\item{p50}{the 50th percentile (the median) of y}
\item{p75}{the 25th percentile of y}
\item{p100}{the 100th percentile (the maximum) of y}
\item{skew}{the skewness of the distribution of y}
\item{kurt}{the kurtosis of the distribution of y}
}
}
\description{
This function extends \code{{describe}} by applying to it all
columns of the specified class(es) in a data frame using functional
programming tools from the purrr package (e.g. \code{\link[purrr]{map}}).
To obtain a summary of a single variable in a data frame use
\code{\link{describe}} instead.
}
\examples{

describe_all(mtcars)

\dontrun{
describe_all(pdata) #all summary types in a list

#numeric summary only
describe_all(pdata, high_low, output = "dt", class = "n")

#numeric and logical summaries only
describe_all(pdata, high_low, output = "dt", class = c("n", "l"))
}

}
\seealso{
\code{\link{describe}}
}
\author{
Craig P. Hutton, \email{craig.hutton@gov.bc.ca}
}
