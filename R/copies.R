# Copyright 2020 Province of British Columbia
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


# copies ------------------------------------------------------------------
#' @title
#' Check the number of copies/duplicated rows in a data frame.
#'
#' @description Checks a data frame for copied/duplicated rows based on
#'   specified variables to use for checking (via `...`) or all columns (if
#'   unspecified). Also allows filtering of the output to retain all records
#'   with copy # info, a subset of distinct records, or a subset of duplicated
#'   records. This flexibility makes `copies` similar to both
#'   \code{\link[janitor]{get_dupes}}) & \code{\link[dplyr]{distinct}}), while
#'   at the same time providing greater flexibility through a larger array of
#'   output options and competitive performance by using `data.table` as a
#'   backend.
#'
#' @importFrom data.table as.data.table
#' @importFrom data.table setkeyv
#' @importFrom data.table setkey
#' @importFrom data.table key
#' @importFrom data.table setorderv
#' @importFrom data.table setorder
#' @importFrom tibble as_tibble
#'
#' @param data a data frame, tibble, or data.table.
#'
#' @param ... This special argument accepts any number of unquoted column names
#'   (also present in the data source) to use when searching for duplicates. If
#'   no column names are specified, all columns will be used.
#'
#' @param filter Shortcuts for filtering (retaining a subset of) the rows of the
#'   output based on the number of copies detected. Options include: `"all"` =
#'   all rows that were present in the input (default), `"dupes"` = only rows
#'   that were found to be duplciated (mimics the behaviour of
#'   \code{\link[janitor]{get_dupes}}), `"unique"` = only rows that appear as a
#'   single copy (not duplicated at all), `"first"` = keeps the 1st copy in
#'   cases where duplicates are detected (mimics the behaviour of
#'   \code{\link[dplyr]{distinct}} & \code{\link{unique}}), and `"last"` = keeps
#'   the last copy in cases where duplicates are detected. Note: if `"dupes"` is
#'   selected a message will be printed to the console indicating whether or
#'   not duplicates were detected.
#'
#' @param keep_all_cols If column names are specified using `...`, this allows
#'   you to drop unspecified columns, similarly to the `.keep_all` argument for
#'   `dplyr::distinct()``
#'
#' @param sort_by_copies Only applicable to the "all" & "dupes" filtering
#'   options. If TRUE, sorts the results by the number of copies, in order
#'   specified by the `order` argument. Default is FALSE to maximize
#'   performance.
#'
#' @param order Only applicable to the "all" & "dupes" filtering options. If
#'   sort_by_copies is set to TRUE, this controls whether the results should be
#'   sorted in order of descending/decreasing = "d" (the default) or
#'   ascending/increasing = "a" or "i" copy numbers.
#'
#' @param na_last should rows of the specified columns with missing values be
#'   listed below non-missing values (TRUE/FALSE)? Default is FALSE.
#'
#' @param output "tibble" for tibble, "dt" for data.table, or "data.frame" for a
#'   data frame. "same", the default option, returns the same format as the
#'   input data.
#'
#' @return If `filter` argument is set to "all", returns a modified version of the
#'   input data frame with two additional columns added to the end/right side:
#'
#'     - `copy_number` = the row copy number which is included to allow
#'     subsequent filtering based on the 1st or last copy detected.
#'
#'     - `n_copies` = the total number of copies detected
#'
#'   If `filter` is set to `dupes`, then only the `n_copies` column is appended
#'   and only duplicated rows are returned. If any other of the other `filter`
#'   argument options are chosen, only the chosen subset of the rows & columns
#'   will be returned.
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gov.bc.ca}
#'
#' @examples
#'
#' # check based on one variable & return all rows with copy indicators
#' copies(pdata, g, filter = "all") #the default
#'
#' # check based on one variable & return duplicated rows only
#' copies(pdata, g, filter = "dupes")
#'
#' # check based on one variable & return distinct/unique rows only
#' copies(pdata, g, filter = "unique")
#'
#' # check based on one variable & return the 1st detected copy for cases where
#' # more than one copy is detected (like `dplyr::distinct()` or `unique()`)
#' copies(pdata, g, filter = "first")
#'
#' # check based on one variable & return the last detected copy for cases where
#' # more than one copy is detected (like `unique()` with fromLast = TRUE`)
#' copies(pdata, g, filter = "last")
#'
#' \dontrun{
#' copies(pdata, high_low, g) #check based on 2 variables
#'
#' copies(pdata) #check based on all columns
#' }
#'
#' @seealso \code{\link[data.table]{duplicated}},
#'   \code{\link[janitor]{get_dupes}}, \code{\link[dplyr]{distinct}},
#'   \code{\link{unique}}
#'
#' @export
copies <- function(data, ...,
                   filter = c("all", "dupes", "first", "last", "unique"),
                   keep_all_cols = TRUE,
                   sort_by_copies = FALSE,
                   order = c("d", "a", "i"),
                   na_last = FALSE,
                   output = c("same", "tibble", "dt", "data.frame")) {

  filter <- match.arg(filter, several.ok = FALSE)
  order <-  match.arg(order, several.ok = FALSE)
  output <-  match.arg(output, several.ok = FALSE)


  if(!missing(...)) {
    g <- gsub(" ", "", unlist(strsplit(deparse(substitute(list(...))), "[(,)]")))[-1]
    if(keep_all_cols == FALSE) {
      data <- data[, g]
    }
  } else {
    message("No column names specified - using all columns.")
    c_names <- names(data)
    g <- c_names
  }

  .classes <- class(data)

  if("data.frame" %ni% .classes) {
    stop("Input data must be a data.table, tibble, or data.frame.")
  }


  if("data.table" %ni% .classes) {
    data <- data.table::as.data.table(data)
  }

  if(filter == "dupes") {
    data[, n_copies := .N,
         by = eval(g)]
    data <- data[n_copies > 1]
    dupe_count <- nrow(data)
    if(dupe_count != 0) {
      message(paste0("Duplicated rows detected! ",
                     dupe_count, " of ", nrow(data), " rows in the input data have multiple copies."))
    } else {
      message("No duplicates detected.")
    }

  } else if(filter == "all") {
    data[, `:=` (copy_number = 1:.N,
                 n_copies = .N),
         by = eval(g)]
  } else if(filter == "first") {
    if(!missing(...)) {
      data <- unique(data, by = eval(g), fromLast = FALSE)
    } else {
      data <- unique(data, fromLast = FALSE)
    }
  } else if(filter == "last"){
    if(!missing(...)) {
      data <- unique(data, by = eval(g), fromLast = TRUE)
    } else {
      data <- unique(data, fromLast = FALSE)
    }
  } else if(filter == "unique") {
    data[, n_copies := .N,
         by = eval(g)]
    data <- data[n_copies == 1]
    data[, n_copies := NULL]
  }

  if(sort_by_copies == FALSE) {
    if (output == "dt") {
      return(data)

    } else if(output == "tibble") {
      data <- tibble::as_tibble(data)
      return(data)

    } else if(output == "data.frame") {
      data <- as.data.frame(data)
      return(data)

    } else if ("data.table" %in% .classes && output == "same") {
      return(data)

    } else if ("tbl" %in% .classes && output == "same") {
      data <- tibble::as_tibble(data)
      return(data)

    } else if ("data.frame" %in% .classes) {
      data <- as.data.frame(data)
      return(data)
    }
  } else if(sort_by_copies == TRUE && filter %in% c("all", "dupes")) {
    if(order == "d") {
      if(!missing(...)) {
        data.table::setorderv(data, c("n_copies", eval(g)),
                              na.last = na_last,
                              order = -1)
      } else {
        data.table::setorderv(data, "n_copies", na.last = na_last,
                             order = -1)
      }
    } else if (order %in% c("a", "i")) {
      if(!missing(...)) {
        data.table::setorderv(data, c("n_copies", eval(g)),
                              na.last = na_last,
                              order = 1)
      } else {
        data.table::setorderv(data, "n_copies", na.last = na_last,
                             order = 1)
      }
    }
  }

  if (output == "dt") {
    return(data)

  } else if(output == "tibble") {
    data <- tibble::as_tibble(data)
    return(data)

  } else if(output == "data.frame") {
    data <- as.data.frame(data)
    return(data)

  } else if ("data.table" %in% .classes && output == "same") {
    return(data)

  } else if ("tbl" %in% .classes && output == "same") {
    data <- tibble::as_tibble(data)
    return(data)

  } else if ("data.frame" %in% .classes) {
    data <- as.data.frame(data)
    return(data)
  }
}
