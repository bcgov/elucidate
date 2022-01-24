# Copyright 2021 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

# tbcvs (internal)-----------------------------------------------------------------
#' @title
#' elucidate package internal function
#'
#' @description \code{tbcvs} is an internal function that supports
#'   \code{\link{describe}}.
#'
#' @param y A vector/variable (required).
#'
#' @param sep A character string to use to separate unique values from their
#'   counts ("_" by default).
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gov.bc.ca}
#' @noRd
tbcvs <- function(y, sep = "_") {
  if(!is.character(sep) || length(sep) > 1) {
    stop('Argument "sep" must be a single character string.')
  }
  tab <- sort(table(y, useNA = "no"), decreasing = TRUE)
  len <- length(tab)

  if(len > 4) {
    tab <- tab[c(1, 2, len-1, len)]
    values <- names(tab)
    counts <- as.character(tab)
    out <- paste(values, counts, sep = sep)
    out <- c(out[1:2], "...", out[3:4])
    out <- paste0(out, collapse = ", ")
  } else if (len <= 4) {
    values <- names(tab)
    counts <- as.character(tab)
    out <- paste(values, counts, sep = sep)
    out <- paste0(out, collapse = ", ")
  }

  return(out)
}

# describe -------------------------------------------------------------
#' @title
#' Obtain a descriptive summary of a variable.
#'
#' @description Obtain a useful array of common summary statistics for a
#'   vector/variable with customized output depending on the class of variable.
#'   Uses a combination of tidyverse packages and data.table to provide a
#'   user-friendly interface that is pipe-friendly while leveraging the
#'   excellent performance of data.table. The use of the ... argument also makes
#'   it incredibly easy to obtain summaries split by grouping variables. While
#'   other similar functions exist in other packages (e.g.
#'   \code{\link[psych]{describeBy}} or \code{\link[skimr]{skim}}), this version
#'   provides the some of the useful added outputs of the psych package (e.g.
#'   se, skew, and kurtosis for numeric variables) while at the same time
#'   offering slightly more concise syntax than skim (e.g. no preceding group_by
#'   operation is needed for group-wise calculations) while still achieving
#'   comparable processing times to the alternatives. To obtain summaries for
#'   all variables in a data frame use \code{\link{describe_all}} instead.
#'
#' @importFrom data.table as.data.table
#' @importFrom data.table uniqueN
#' @importFrom data.table dcast
#' @importFrom lubridate is.instant
#' @importFrom stats sd
#' @importFrom stats quantile
#' @importFrom stats na.omit
#' @importFrom tibble as_tibble
#' @importFrom tidyr separate
#'
#' @param data Either a vector or a data frame or tibble containing the
#'   vector ("y") to be summarized and any grouping variables.
#'
#' @param y If the data object is a data.frame, this is the variable for which
#'   you wish to obtain a descriptive summary. You can use either the quoted or
#'   unquoted name of the variable, e.g. "y_var" or y_var.
#'
#' @param ... If the data object is a data.frame, this special argument accepts
#'   any number of unquoted grouping variable names (also present in the data
#'   source) to use for subsetting, separated by commas, e.g. `group_var1,
#'   group_var2`. Also accepts a character vector of column names or index
#'   numbers, e.g. c("group_var1", "group_var2") or c(1, 2), but not a mixture
#'   of formats in the same call. If no column names are specified, all columns
#'   will be used.
#'
#' @param digits This determines the number of digits used for rounding of
#'   numeric outputs.
#'
#' @param type For numeric and integer vectors this determines the type of
#'   skewness and kurtosis calculations to perform. See
#'   \code{\link[e1071]{skewness}} or \code{\link[psych]{skew}} and
#'   \code{\link[e1071]{kurtosis}} or \code{\link[psych]{kurtosi}} for details.
#'
#' @param na.rm This determines whether missing values (NAs) should be removed
#'   before attempting to calculate summary statistics.
#'
#' @param sep A character string to use to separate unique values from their
#'   counts ("_" by default). Only applicable to factors and character vectors.
#'
#' @param output "tibble" for tibble or "dt" for data.table. Tibble is used as
#'   the default output to facilitate subsequent use/modification of the output
#'   with the tidyverse collection of packages.
#'
#' @return The output varies as a function of the class of input data/y, referred to as "y" below
#'
#'   \strong{For all input variables, the following are returned (part 1):}
#'
#'   \describe{
#'     \item{cases}{the total number of cases}
#'     \item{n}{number of complete cases}
#'     \item{na}{the number of missing values}
#'     \item{p_na}{the proportion of total cases with missing values}
#'   }
#'
#'   In addition to part 1, these measures are provided for \strong{dates}:
#'
#'   \describe{
#'     \item{n_unique}{the total number of unique values or levels of y. For dates this tells you how many time points there are}
#'     \item{start}{the earliest or minimum date in y}
#'     \item{end}{the latest or maximum date in y}
#'   }
#'
#'   In addition to part 1, these measures are provided for \strong{factors}:
#'
#'   \describe{
#'     \item{n_unique}{the total number of unique values or levels of y}
#'     \item{ordered}{a logical indicating whether or not y is ordinal}
#'     \item{counts_tb}{the counts of the top and bottom unique values of y in order of decreasing frequency formatted as "value_count". If there are more than 4 unique values of y, only the top 2 and bottom 2 unique values are shown separated by "...". To get counts for all unique values use \code{\link{counts}} instead.}
#'   }
#'
#'   In addition to part 1, these measures are provided for \strong{character/string} vectors:
#'
#'   \describe{
#'     \item{n_unique}{the total number of unique values or levels of y}
#'     \item{min_chars}{the minimum number of characters in the values of y}
#'     \item{max_chars}{the maximum number of characters in the values of y}
#'     \item{counts_tb}{the counts of the top and bottom unique values of y in order of decreasing frequency formatted as "value_count". If there are more than 4 unique values of y, only the top 2 and bottom 2 unique values are shown separated by "...". To get counts for all unique values use \code{\link{counts}} instead.}
#'   }
#'
#'   In addition to part 1, these measures are provided for \strong{logical} vectors:
#'
#'   \describe{
#'     \item{n_TRUE}{the total number of y values that are TRUE}
#'     \item{n_FALSE}{the total number of y values that are FALSE}
#'     \item{p_TRUE}{the proportion of y values that are TRUE}
#'   }
#'
#'   In addition to part 1, these measures are provided for \strong{numeric} variables:
#'
#'   \describe{
#'     \item{mean}{the mean of y}
#'     \item{sd}{the standard deviation of y}
#'     \item{se}{the standard error of the mean of y}
#'     \item{p0}{the 0th percentile (the minimum) of y}
#'     \item{p25}{the 25th percentile of y}
#'     \item{p50}{the 50th percentile (the median) of y}
#'     \item{p75}{the 25th percentile of y}
#'     \item{p100}{the 100th percentile (the maximum) of y}
#'     \item{skew}{the skewness of the distribution of y}
#'     \item{kurt}{the kurtosis of the distribution of y}
#'   }
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gov.bc.ca}
#'
#' @examples
#'
#' describe(data = pdata, y = y1) #no grouping variables, numeric input class
#' describe(pdata, y1, high_low) #one grouping variable, numeric input class
#' describe(pdata, g) #factor input class
#' describe(pdata, even) #logical input class
#'
#' @references
#' Altman, D. G., & Bland, J. M. (2005). Standard deviations and standard
#' errors. Bmj, 331(7521), 903.
#'
#' Bulmer, M. G. (1979). Principles of statistics. Courier Corporation.
#'
#' D. N. Joanes and C. A. Gill (1998), Comparing measures of sample skewness and
#' kurtosis. The Statistician, 47, 183-189.
#'
#' @seealso \code{\link[base]{mean}}, \code{\link[stats]{sd}}, \code{\link{se}},
#'   \code{\link[stats]{quantile}}, \code{\link{skewness}}, \code{\link{kurtosis}},
#'   \code{\link{counts}}, \code{\link{counts_tb}}
#'
#' @export
describe <- function(data, y = NULL, ..., digits = 3, type = 2, na.rm = TRUE, sep = "_",  output = c("tibble", "dt")){

  output <- match.arg(output)
  if((is.vector(data) || is.factor(data)) || lubridate::is.instant(data)) {
    if(is.numeric(data)){
      dt <- data.table::as.data.table(data)
      description <- dt[, .(cases = .N,
                            n = sum(!is.na(data)),
                            na = sum(is.na(data)),
                            p_na = round(sum(is.na(data))/length(data), digits),
                            mean = round(sum(data, na.rm = na.rm)/length(na.omit(data)), digits),
                            sd = round(stats::sd(data, na.rm = na.rm), digits),
                            se = round(se(data, na.rm = na.rm), digits),
                            p0 = round(as.double(min(data, na.rm = na.rm)), digits),
                            p25 = round(stats::quantile(data, probs = 0.25, na.rm = na.rm), digits),
                            p50 = round(as.double(stats::median(data, na.rm = na.rm)), digits),
                            p75 = round(stats::quantile(data, probs = 0.75, na.rm = na.rm), digits),
                            p100 = round(as.double(max(data, na.rm = na.rm)), digits),
                            skew = round(skewness(data, type = type, na.rm = na.rm), digits),
                            kurt = round(kurtosis(data, type = type, na.rm = na.rm), digits))]

    } else if (is.logical(data)){
      dt <- data.table::as.data.table(data)
      description <- dt[, .(cases = .N,
                            n = sum(!is.na(data)),
                            na = sum(is.na(data)),
                            p_na = round(sum(is.na(data))/length(data), digits),
                            n_TRUE = round(sum(data, na.rm = na.rm), digits),
                            n_FALSE = round(sum(data == 0, na.rm = na.rm), digits),
                            p_TRUE = round(sum(data, na.rm = na.rm)/length(na.omit(data)), digits))]

    } else if (lubridate::is.instant(data)){
      dt <- data.table::as.data.table(data)
      description <- dt[, .(cases = .N,
                            n = sum(!is.na(data)),
                            na = sum(is.na(data)),
                            p_na = round(sum(is.na(data))/length(data), digits),
                            n_unique = data.table::uniqueN(data),
                            start = min(data, na.rm = na.rm),
                            end = max(data, na.rm = na.rm))]

    } else if (is.factor(data)) {
      dt <- data.table::as.data.table(data)
      suppressMessages(
        description <- dt[, .(cases = .N,
                              n = sum(!is.na(data)),
                              na = sum(is.na(data)),
                              p_na = round(sum(is.na(data))/length(data), digits),
                              n_unique = data.table::uniqueN(data),
                              ordered = is.ordered(data),
                              counts_tb = tbcvs(data, sep = sep))]
      )
    } else if (is.character(data)) {
      dt <- data.table::as.data.table(data)
      suppressMessages(
        description <- dt[, .(cases = .N,
                              n = sum(!is.na(data)),
                              na = sum(is.na(data)),
                              p_na = round(sum(is.na(data))/length(data), digits),
                              n_unique = data.table::uniqueN(data),
                              min_chars = as.integer(min(nchar(data), na.rm = na.rm)),
                              max_chars = as.integer(max(nchar(data), na.rm = na.rm)),
                              counts_tb = tbcvs(data, sep = sep))]
      )
    } else {
      stop(paste0("Input data class not currently supported.",
           "\nCurrently supported vector classes include: numeric/integer, factor, date/POSIXct/POSIXlt, logical, & character"))
    }
  } else {
    if(missing(y)){
      stop(paste0("If a non-vector (e.g. data frame) is supplied to the data argument, y must also be specified.",
           "\nIf you want summaries for all variables in data, use describe_all() instead"))
    } else {
      if(is.error(class(data[[y]]))) {
        y_str <- deparse(substitute(y))
      } else if(!is.character(y) || length(y) > 1){
        stop(paste0('`y` must be a single symbol or character string representing a column',
             '\nin the input data frame supplied to the `data` argument.'))
      }
    }
    if(!missing(...)) {
      g <- group_parser(data, ...)
    }

    dt <- data.table::as.data.table(data)

    if(is.numeric(dt[[y_str]])){
      if(!missing(...)){
        description <- dt[, .(cases = .N,
                              n = sum(!is.na(get(y_str))),
                              na = sum(is.na(get(y_str))),
                              p_na = round(sum(is.na(get(y_str)))/length(get(y_str)), digits),
                              mean = round(sum(get(y_str), na.rm = na.rm)/length(na.omit(get(y_str))), digits),
                              sd = round(stats::sd(get(y_str), na.rm = na.rm), digits),
                              se = round(se(get(y_str), na.rm = na.rm), digits),
                              p0 = round(as.double(min(get(y_str), na.rm = na.rm)), digits),
                              p25 = round(stats::quantile(get(y_str), probs = 0.25, na.rm = na.rm), digits),
                              p50 = round(as.double(stats::median(get(y_str), na.rm = na.rm)), digits),
                              p75 = round(stats::quantile(get(y_str), probs = 0.75, na.rm = na.rm), digits),
                              p100 = round(as.double(max(get(y_str), na.rm = na.rm)), digits),
                              skew = round(skewness(get(y_str), type = type, na.rm = na.rm), digits),
                              kurt = round(kurtosis(get(y_str), type = type, na.rm = na.rm), digits)),
                          by = eval(g)]
      } else {
        description <- dt[, .(cases = .N,
                              n = sum(!is.na(get(y_str))),
                              na = sum(is.na(get(y_str))),
                              p_na = round(sum(is.na(get(y_str)))/length(get(y_str)), digits),
                              mean = round(sum(get(y_str), na.rm = na.rm)/length(na.omit(get(y_str))), digits),
                              sd = round(stats::sd(get(y_str), na.rm = na.rm), digits),
                              se = round(se(get(y_str), na.rm = na.rm), digits),
                              p0 = round(as.double(min(get(y_str), na.rm = na.rm)), digits),
                              p25 = round(stats::quantile(get(y_str), probs = 0.25, na.rm = na.rm), digits),
                              p50 = round(as.double(stats::median(get(y_str), na.rm = na.rm)), digits),
                              p75 = round(stats::quantile(get(y_str), probs = 0.75, na.rm = na.rm), digits),
                              p100 = round(as.double(max(get(y_str), na.rm = na.rm)), digits),
                              skew = round(skewness(get(y_str), type = type, na.rm = na.rm), digits),
                              kurt = round(kurtosis(get(y_str), type = type, na.rm = na.rm), digits))]
      }
    } else if (is.logical(dt[[y_str]])) {
      if(!missing(...)){
        description <- dt[, .(cases = .N,
                              n = sum(!is.na(get(y_str))),
                              na = sum(is.na(get(y_str))),
                              p_na = round(sum(is.na(get(y_str)))/length(get(y_str)), digits),
                              n_TRUE = round(sum(get(y_str), na.rm = na.rm), digits),
                              n_FALSE = round(sum(get(y_str) == 0, na.rm = na.rm), digits),
                              p_TRUE = round(sum(get(y_str), na.rm = na.rm)/length(na.omit(get(y_str))), digits)),
                          by = eval(g)]
      } else {
        description <- dt[, .(cases = .N,
                              n = sum(!is.na(get(y_str))),
                              na = sum(is.na(get(y_str))),
                              p_na = round(sum(is.na(get(y_str)))/length(get(y_str)), digits),
                              n_TRUE = round(sum(get(y_str), na.rm = na.rm), digits),
                              n_FALSE = round(sum(get(y_str) == 0, na.rm = na.rm), digits),
                              p_TRUE = round(sum(get(y_str), na.rm = na.rm)/length(na.omit(get(y_str))), digits))]
      }
    } else if (lubridate::is.instant(dt[[y_str]])) {
      if(!missing(...)){
        description <- dt[, .(cases = .N,
                              n = sum(!is.na(get(y_str))),
                              na = sum(is.na(get(y_str))),
                              p_na = round(sum(is.na(get(y_str)))/length(get(y_str)), digits),
                              n_unique = data.table::uniqueN(get(y_str)),
                              start = min(get(y_str), na.rm = na.rm),
                              end = max(get(y_str), na.rm = na.rm)),
                          by = eval(g)]
      } else {
        description <- dt[, .(cases = .N,
                              n = sum(!is.na(get(y_str))),
                              na = sum(is.na(get(y_str))),
                              p_na = round(sum(is.na(get(y_str)))/length(get(y_str)), digits),
                              n_unique = data.table::uniqueN(get(y_str)),
                              start = min(get(y_str), na.rm = na.rm),
                              end = max(get(y_str), na.rm = na.rm))]
      }
    } else if (is.factor(dt[[y_str]])) {
      if(!missing(...)){
        suppressMessages(
          description <- dt[, .(cases = .N,
                                n = sum(!is.na(get(y_str))),
                                na = sum(is.na(get(y_str))),
                                p_na = round(sum(is.na(get(y_str)))/length(get(y_str)), digits),
                                n_unique = data.table::uniqueN(get(y_str)),
                                ordered = is.ordered(get(y_str)),
                                counts_tb = tbcvs(get(y_str), sep = sep)),
                            by = eval(g)]
        )
      } else {
        suppressMessages(
          description <- dt[, .(cases = .N,
                                n = sum(!is.na(get(y_str))),
                                na = sum(is.na(get(y_str))),
                                p_na = round(sum(is.na(get(y_str)))/length(get(y_str)), digits),
                                n_unique = data.table::uniqueN(get(y_str)),
                                ordered = is.ordered(get(y_str)),
                                counts_tb = tbcvs(get(y_str), sep = sep))]
        )
      }
    } else if (is.character(dt[[y_str]])) {
      if(!missing(...)){
        suppressMessages(
          description <- dt[, .(cases = .N,
                                n = sum(!is.na(get(y_str))),
                                na = sum(is.na(get(y_str))),
                                p_na = round(sum(is.na(get(y_str)))/length(get(y_str)), digits),
                                n_unique = data.table::uniqueN(get(y_str)),
                                min_chars = as.integer(min(nchar(get(y_str)), na.rm = na.rm)),
                                max_chars = as.integer(max(nchar(get(y_str)), na.rm = na.rm)),
                                counts_tb = tbcvs(get(y_str), sep = sep)),
                            by = eval(g)]
        )
      } else {
        suppressMessages(
          description <- dt[, .(cases = .N,
                                n = sum(!is.na(get(y_str))),
                                na = sum(is.na(get(y_str))),
                                p_na = round(sum(is.na(get(y_str)))/length(get(y_str)), digits),
                                n_unique = data.table::uniqueN(get(y_str)),
                                min_chars = as.integer(min(nchar(get(y_str)), na.rm = na.rm)),
                                max_chars = as.integer(max(nchar(get(y_str)), na.rm = na.rm)),
                                counts_tb = tbcvs(get(y_str), sep = sep))]
        )
      }
    } else {
      stop("Input data class supplied to y argument not currently supported.\nCurrently supported vector classes include: numeric/integer, factor, date/POSIXct/POSIXlt, logical, & character")
    }
  }
  if(output == "tibble") {
    description <- tibble::as_tibble(description)
    return(description)
  } else {
    if(output == "dt")
      return(description)
  }
}

# describe_all ------------------------------------------------------------
#' @title
#' Obtain descriptive summaries for all variables in a data frame.
#'
#' @description This function extends \code{{describe}} by applying to it all
#'   columns of the specified class(es) in a data frame using functional
#'   programming tools from the purrr package (e.g. \code{\link[purrr]{map}}).
#'   To obtain a summary of a single variable in a data frame use
#'   \code{\link{describe}} instead.
#'
#' @importFrom data.table as.data.table
#' @importFrom data.table %chin%
#' @importFrom dplyr select
#' @importFrom dplyr select_if
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr group_cols
#' @importFrom lubridate is.instant
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @importFrom purrr map
#' @importFrom tidyselect everything
#'
#' @param data A data frame or tibble.
#'
#' @param ... This special argument accepts any number of unquoted grouping
#'   variable names (also present in the data source) to use for subsetting,
#'   separated by commas, e.g. `group_var1, group_var2`. Also accepts a
#'   character vector of column names or index numbers, e.g. c("group_var1",
#'   "group_var2") or c(1, 2), but not a mixture of formats in the same call. If
#'   no column names are specified, all columns will be used.
#'
#' @param class The variable classes in data that you would like summaries for.
#'   Either "all" for all classes, or a character vector indicating which
#'   combinations of output classes you want. Specifying a subset will save time
#'   since summaries are only processed as needed. Options include "d" for
#'   dates, "f" for factors, "c" for character, "l" for logical, and "n" for
#'   numeric. If only a single class is requested or present in the data after
#'   excluding specified grouping variables, a data frame will be returned,
#'   otherwise you'll get a list of data frames (1 per summary class). If the
#'   only chosen class of variables is not detected in the input data an error
#'   will be returned that the class argument needs to be respecified.
#'
#' @param digits This determines the number of digits used for rounding of
#'   numeric outputs.
#'
#' @param type For numeric and integer vectors this determines the type of
#'   skewness and kurtosis calculations to perform. See
#'   \code{\link[e1071]{skewness}} or \code{\link[psych]{skew}} and
#'   \code{\link[e1071]{kurtosis}} or \code{\link[psych]{kurtosi}} for details.
#'
#' @param na.rm This determines whether missing values (NAs) should be removed
#'   before attempting to calculate summary statistics.
#'
#' @param sep A character string to use to separate unique values from their
#'   counts ("_" by default). Only applicable to factors and character vectors.
#'
#' @param output Output type for each class of variables. "tibble" for tibble or
#'   "dt" for data.table. Tibble is used as the default output to facilitate
#'   subsequent use/modification of the output with the tidyverse collection of
#'   packages.
#'
#' @return The output varies as a function of the class of input data/y,
#'   referred to as "y" below. Each output type is grouped together in a data
#'   frame and returned as a named item of a list, unless there is only one
#'   output type, in which case the data frame is returned directly.
#'
#'   \strong{For all input variables, the following are returned (part 1):}
#'
#'   \describe{
#'     \item{cases}{the total number of cases}
#'     \item{n}{number of complete cases}
#'     \item{na}{the number of missing values}
#'     \item{p_na}{the proportion of total cases with missing values}
#'   }
#'
#'   In addition to part 1, these measures are provided for \strong{dates}:
#'
#'   \describe{
#'     \item{n_unique}{the total number of unique values or levels of y. For dates this tells you how many time points there are}
#'     \item{start}{the earliest or minimum date in y}
#'     \item{end}{the latest or maximum date in y}
#'   }
#'
#'   In addition to part 1, these measures are provided for \strong{factors}:
#'
#'   \describe{
#'     \item{n_unique}{the total number of unique values or levels of y}
#'     \item{ordered}{a logical indicating whether or not y is ordinal}
#'     \item{counts_tb}{the counts of the top and bottom unique values of y in order of decreasing frequency formatted as "value_count". If there are more than 4 unique values of y, only the top 2 and bottom 2 unique values are shown separated by "...". To get counts for all unique values use \code{\link{counts}} or \code{\link{counts_tb}} instead.}
#'   }
#'
#'   In addition to part 1, these measures are provided for \strong{character/string} vectors:
#'
#'   \describe{
#'     \item{n_unique}{the total number of unique values or levels of y}
#'     \item{min_chars}{the minimum number of characters in the values of y}
#'     \item{max_chars}{the maximum number of characters in the values of y}
#'     \item{counts_tb}{the counts of the top and bottom unique values of y in order of decreasing frequency formatted as "value_count". If there are more than 4 unique values of y, only the top 2 and bottom 2 unique values are shown separated by "...". To get counts for all unique values use \code{\link{counts}} or \code{\link{counts_tb}} instead.}
#'   }
#'
#'   In addition to part 1, these measures are provided for \strong{logical} vectors:
#'
#'   \describe{
#'     \item{n_TRUE}{the total number of y values that are TRUE}
#'     \item{n_FALSE}{the total number of y values that are FALSE}
#'     \item{p_TRUE}{the proportion of y values that are TRUE}
#'   }
#'
#'   In addition to part 1, these measures are provided for \strong{numeric} variables:
#'
#'   \describe{
#'     \item{mean}{the mean of y}
#'     \item{sd}{the standard deviation of y}
#'     \item{se}{the standard error of the mean of y}
#'     \item{p0}{the 0th percentile (the minimum) of y}
#'     \item{p25}{the 25th percentile of y}
#'     \item{p50}{the 50th percentile (the median) of y}
#'     \item{p75}{the 25th percentile of y}
#'     \item{p100}{the 100th percentile (the maximum) of y}
#'     \item{skew}{the skewness of the distribution of y}
#'     \item{kurt}{the kurtosis of the distribution of y}
#'   }
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gov.bc.ca}
#'
#' @examples
#'
#' describe_all(mtcars)
#'
#' \dontrun{
#' describe_all(pdata) #all summary types in a list
#'
#' #numeric summary only
#' describe_all(pdata, high_low, output = "dt", class = "n")
#'
#' #numeric and logical summaries only
#' describe_all(pdata, high_low, output = "dt", class = c("n", "l"))
#' }
#'
#' @seealso \code{\link{describe}}
#'
#' @export
describe_all <- function(data, ..., class = "all", digits = 3, type = 2, na.rm = TRUE, sep = "_", output = c("tibble", "dt")) {
  output <- match.arg(output)

  if(any(class %ni%  c("all", "d", "f", "c", "l", "n"))) {
    stop('class argument should be either "all" or any combination of "d", "f", "c", "l", and/or "n" as a character vector')
  }

  if("data.frame" %ni% class(data)) {
    stop("input data must be a data frame")
  }

  if(!missing(...)) {
    g <- group_parser(data, ...)
  }

  if(class == "all" || "d" %chin% class) {
    date_cols <- vapply(data, lubridate::is.instant, FUN.VALUE = logical(1))
    if(!missing(...)) {
      date_cols <- length(setdiff(names(data)[date_cols], g))
    } else {
      date_cols <- sum(date_cols)
    }
  }
  if(class == "all" || "f" %chin% class) {
    fct_cols <- vapply(data, is.factor, FUN.VALUE = logical(1))
    if(!missing(...)) {
      fct_cols <- length(setdiff(names(data)[fct_cols], g))
    } else {
      fct_cols <- sum(fct_cols)
    }
  }
  if(class == "all" || "c" %chin% class){
    chr_cols <- vapply(data, is.character, FUN.VALUE = logical(1))
    if(!missing(...)) {
      chr_cols <- length(setdiff(names(data)[chr_cols], g))
    } else {
      chr_cols <- sum(chr_cols)
    }
  }
  if(class == "all" || "l" %chin% class) {
    lgl_cols <- vapply(data, is.logical, FUN.VALUE = logical(1))
    if(!missing(...)) {
      lgl_cols <- length(setdiff(names(data)[lgl_cols], g))
    } else {
      lgl_cols <- sum(lgl_cols)
    }
  }
  if(class == "all" || "n" %chin% class) {
    num_cols <- vapply(data, is.numeric, FUN.VALUE = logical(1))
    if(!missing(...)) {
      num_cols <- length(setdiff(names(data)[num_cols], g))
    } else {
      num_cols <- sum(num_cols)
    }
  }

  data <- data.table::as.data.table(data)
  ls <- list()

  if(!missing(...)) {
    if((class == "all" || "d" %chin% class) && date_cols >= 1){
      ls[["date"]] <- data[,  purrr::map_dfr(.SD,
                                             ~describe(.x, digits = digits, type = type, na.rm = na.rm, output = output),
                                             .id = "variable"), by = eval(g), .SDcols = lubridate::is.instant]
    }
    if((class == "all" || "f" %chin% class) && fct_cols >= 1){
      ls[["factor"]] <- data[,  purrr::map_dfr(.SD,
                                               ~describe(.x, digits = digits, order = order, na.rm = na.rm, sep = sep, output = output),
                                               .id = "variable"), by = eval(g), .SDcols = is.factor]
    }
    if((class == "all" || "c" %chin% class) && chr_cols >= 1){
      ls[["character"]] <- data[,  purrr::map_dfr(.SD,
                                                  ~describe(.x, digits = digits, order = order, na.rm = na.rm, sep = sep, output = output),
                                                  .id = "variable"), by = eval(g), .SDcols = is.character]
    }
    if((class == "all" || "l" %chin% class) && lgl_cols >= 1){
      ls[["logical"]] <- data[,  purrr::map_dfr(.SD,
                                                ~describe(.x, na.rm = na.rm, output = output),
                                                .id = "variable"), by = eval(g), .SDcols = is.logical]
    }
    if((class == "all" || "n" %chin% class) && num_cols >= 1){
      ls[["numeric"]] <- data[,  purrr::map_dfr(.SD,
                                                ~describe(.x, digits = digits, type = type, na.rm = na.rm, output = output),
                                                .id = "variable"), by = eval(g), .SDcols = is.numeric]
    }
  } else {
    if((class == "all" || "d" %chin% class) && date_cols >= 1){
      ls[["date"]] <- data[,  purrr::map_dfr(.SD,
                                             ~describe(.x, output = output),
                                             .id = "variable"), .SDcols = lubridate::is.instant]

    }
    if((class == "all" || "f" %chin% class) && fct_cols >= 1){
      ls[["factor"]] <- data[,  purrr::map_dfr(.SD,
                                               ~describe(.x, digits = digits, order = order, na.rm = na.rm, sep = sep, output = output),
                                               .id = "variable"), .SDcols = is.factor]
    }
    if((class == "all" || "c" %chin% class) && chr_cols >= 1){
      ls[["character"]] <- data[,  purrr::map_dfr(.SD,
                                                  ~describe(.x, digits = digits, order = order, na.rm = na.rm, sep = sep, output = output),
                                                  .id = "variable"), .SDcols = is.character]
    }
    if((class == "all" || "l" %chin% class) && lgl_cols >= 1){
      ls[["logical"]] <- data[,  purrr::map_dfr(.SD,
                                                ~describe(.x, na.rm = na.rm, output = output),
                                                .id = "variable"),.SDcols = is.logical]
    }
    if((class == "all" || "n" %chin% class) && num_cols >= 1){
      ls[["numeric"]] <- data[,  purrr::map_dfr(.SD,
                                                ~describe(.x, digits = digits, type = type, na.rm = na.rm, output = output),
                                                .id = "variable"), .SDcols = is.numeric]
    }
  }

  if(output == "tibble") {
    ls <- lapply(ls, tibble::as_tibble)
  }

  if(length(ls) == 0){
    stop("Data object contains no variables of the chosen class(es), respecify class argument!")
  }
  if(length(ls) == 1){
    ls <- ls[[1]]
  }
  return(ls)
}
