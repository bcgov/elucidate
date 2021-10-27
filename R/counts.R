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

# counts ------------------------------------------------------------------
#' @title
#' Obtain the counts for unique values of a vector.
#'
#' @description \code{counts} returns the most or least common unique value(s)
#'   of a vector depending on whether ascending or descending sorting is used.
#'   Also useful for identifying data entry errors or rare cases. For complex
#'   use cases see \code{\link{describe}}.
#'
#' @param y A vector/variable (required).
#'
#' @param n The number of unique values you want frequency counts for. Default is "all".
#'
#' @param order "d" for descending/decreasing order. "a" or "i" for
#'   ascending/increasing order.
#'
#' @param sep A character string to use to separate unique values from their counts
#'   ("_" by default).
#'
#' @param na.rm Should missing values be omitted (TRUE/FALSE)?
#'
#' @return A character vector of the unique value frequency counts from the
#'   input vector sorted in the chosen order. Return values are structured as
#'   "value_count", where the "_" portion takes on the value of the sep
#'   argument. Returning a character vector makes subsequent manipulation with
#'   stringr and other tidyverse tools fairly easily.
#'
#'
#' @author Craig P. Hutton, \email{Craig.Hutton@@gov.bc.ca}
#'
#' @examples
#' #using a numeric vector
#' y <- c(1, 1, 2, 3, 4, 5, 6, 2, 3, 4, 2, 9, 5, 6, 7, 23, 5, 6, 7, 8, 3, 4, 5, 6)
#'
#' counts(y) #all unique values of y in order of descending frequency \code{y}
#' counts(y, n = 1) #the most common value of \code{y}
#' counts(y, n = 5) #the top 5 most common unique values of \code{y}
#'
#' @seealso \code{\link{table}}, \code{\link{sort}}
#'
#' @export
counts <- function(y, n = "all", order = c("d", "a", "i"), sep = "_", na.rm = TRUE) {
  order <-  match.arg(order, several.ok = FALSE)
  if(!is.character(y) || !is.numeric(y)) {
    y <- as.character(y)
  }
  if(order == "d") {
    if(na.rm == TRUE) {
      tab <- sort(table(y, useNA = "no"), decreasing = TRUE)
    } else {
      tab <- sort(table(y, useNA = "ifany"), decreasing = TRUE)
    }
  } else if (order == "a" || order == "i") {
    if(na.rm == TRUE) {
      tab <- sort(table(y, useNA = "no"))
    } else {
      tab <- sort(table(y, useNA = "ifany"))
    }
  }
  values <- names(tab)
  counts <- as.character(tab)

  out <- paste0(values, sep, counts)
  if(n != "all") {
    out <- out[1:n]
  }
  return(out)
}



# counts_all --------------------------------------------------------------
#' @title
#' Obtain the counts for unique values of all variables in a data frame.
#'
#' @description \code{counts_all} is an extension of \code{\link{counts}} that
#'   returns the most or least common unique value(s) for each column vector of
#'   a data frame. Also useful for identifying data entry errors or rare cases.
#'   For complex use cases see \code{\link{describe_all}}.
#'
#' @importFrom purrr map
#'
#' @param data A data frame (required).
#'
#' @param n The number of unique values you want frequency counts for. Default is "all".
#'
#' @param order "d" for descending/decreasing order. "a" or "i" for
#'   ascending/increasing order.
#'
#' @param sep A character string to use to separate unique values from their counts
#'   ("_" by default).
#'
#' @param na.rm Should missing values be omitted (TRUE/FALSE)?
#'
#' @return A list of character vectors of the unique value frequency counts for
#'   each variable of the input data frame sorted in the chosen order. Return
#'   values are structured as "value_count", where the "_" portion takes on the
#'   value of the sep argument. Returning a character vector makes subsequent
#'   manipulation with stringr and other tidyverse tools fairly easily.
#'
#'
#' @author Craig P. Hutton, \email{Craig.Hutton@@gov.bc.ca}
#'
#' @examples
#' #using a numeric vector
#' data <- data(mtcars)
#'
#' #all unique values of all variables in \code{data} in order of descending frequency
#' counts_all(data)
#'
#' #the most common values of all variables in \code{data}
#' counts_all(data, n = 1)
#'
#' #the top 5 most common unique values for all variables in  \code{data}
#' counts_all(data, n = 5)
#'
#' @seealso \code{\link{table}}, \code{\link{sort}}, \code{\link[purrr]{map}}
#'
#' @export
counts_all <- function(data, n = "all", order = c("d", "a", "i"), sep = "_", na.rm = TRUE) {
  order <-  match.arg(order, several.ok = FALSE)
  if(na.rm == TRUE) {
    out <- purrr::map(data, ~as.character(na.omit(counts(.x, n = n, order = order, sep = sep, na.rm = na.rm))))
  } else {
    out <- purrr::map(data, ~counts(.x, n = n, order = order, sep = sep, na.rm = na.rm))
  }

  return(out)
}


# counts_tb: top and bottom n counts --------------------------------------
#' @title
#' Obtain the top and bottom "n" counts for unique values of a vector.
#'
#' @description \code{counts_tb} is a convenience extension of
#'   \code{\link{counts}} that returns the most AND least common "n" unique
#'   value(s) a vector. This is useful for identifying data entry errors or rare
#'   cases. For complex use cases see \code{\link{describe}}.
#'
#' @importFrom tidyr separate
#' @importFrom stats na.omit
#'
#' @param y A vector.
#'
#' @param n The number of top & bottom unique values you want frequency counts
#'   for. Default is 10 (fewer than "n" will be shown if there aren't "n" unique
#'   values).
#'
#' @param sep This only needs to be modified if some values to be counted
#'   contain an underscore, in which case you should change it to a character
#'   string that is not present in any of the values of y.
#'
#' @param na.rm Should missing values be omitted (TRUE/FALSE)?
#'
#' @return A list of data frames of the top and bottom counts for each variable
#'   of the input data frame. Return value columns are "top_v" = top value,
#'   "top_n" = count of the top value in the same row of the adjacent top_v
#'   column, "bot_v" = bottom value, & "bot_n" = count of the bottom value in
#'   the same row of the adjacent bot_v column.
#'
#' @author Craig P. Hutton, \email{Craig.Hutton@@gov.bc.ca}
#'
#' @examples
#' #using the mtcars data
#' data(mtcars)
#'
#' counts_tb(mtcars$cyl) #top & bottom values
#'
#' @seealso \code{\link{counts_tb_all}}, \code{\link{counts_all}}, \code{\link{counts}}
#'
#' @export
counts_tb <- function(y, n = 10, sep = "_", na.rm = TRUE) {
  top <- counts(y, n = n, order = "d", sep = sep, na.rm = na.rm)
  bot <- counts(y, n = n, order = "a", sep = sep, na.rm = na.rm)
  out <- na.omit(data.frame(top, bot))
  out <- tidyr::separate(out, col = "top", into = c("top_v", "top_n"), sep = sep)
  out <- tidyr::separate(out, col = "bot", into = c("bot_v", "bot_n"), sep = sep)
  return(out)
}

# counts_tb_all -----------------------------------------------------------
#' @title
#' Obtain the top and bottom "n" counts for unique values of all variables in a
#' data frame.
#'
#' @description \code{counts_tb_all} is an extension of \code{\link{counts_tb}}
#'   that returns the most or least common unique value(s) for each column
#'   vector of a data frame. Also useful for identifying data entry errors or
#'   rare cases. For complex use cases see \code{\link{describe_all}}.
#'
#' @importFrom purrr map
#' @importFrom stats na.omit
#'
#' @param data A data frame (required).
#'
#' @param n The number of top & bottom unique values you want frequency counts
#'   for. Default is 10 (fewer than "n" will be shown if there aren't "n" unique
#'   values).
#'
#' @param sep This only needs to be modified if some values to be counted
#'   contain an underscore, in which case you should change it to a character
#'   string that is not present in any of the values in the data source.
#'
#' @param na.rm Should missing values be omitted (TRUE/FALSE)?
#'
#' @return A list of data frames of the top and bottom counts for each variable
#'   of the input data frame. Return value columns are "top_v" = top value,
#'   "top_n" = count of the top value in the same row of the adjacent top_v
#'   column, "bot_v" = bottom value, & "bot_n" = count of the bottom value in
#'   the same row of the adjacent bot_v column.
#'
#'
#' @author Craig P. Hutton, \email{Craig.Hutton@@gov.bc.ca}
#'
#' @examples
#' #using the mtcars data
#' df <- data(mtcars)
#'
#' counts_tb_all(df) #(up to) the top & bottom 10 values
#'
#' #the most & least common values of all variables in \code{data}
#' counts_tb_all(df, n = 1)
#'
#' #the top 5 most & least common unique values for all variables in  \code{data}
#' counts_tb_all(df, n = 5)
#'
#' @seealso \code{\link{counts_all}}, \code{\link{counts_tb}}, \code{\link{counts}}
#'
#' @export
counts_tb_all <- function(data, n = 10, sep = "_", na.rm = TRUE) {
  out <- purrr::map(data,
                    ~na.omit(counts_tb(.x, n = n, sep = sep, na.rm = na.rm)))
  return(out)
}
