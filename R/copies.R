# Copyright 2021 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

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
#'   backend. \code{\link{dupes}} is also available as a convenience shortcut
#'   for `copies(filter = "dupes", sort_by_copies = TRUE)`.
#'
#' @importFrom data.table as.data.table
#' @importFrom data.table setorderv
#' @importFrom data.table setorder
#' @importFrom tibble as_tibble
#'
#' @param data a data frame, tibble, or data.table.
#'
#' @param ... This special argument accepts any number of unquoted column names
#'   (also present in the data source) to use when searching for duplicates,
#'   e.g. `x, y, z`. Also accepts a character vector of column names or index
#'   numbers, e.g. c("x", "y", "z") or c(1, 2, 3), but not a mixture of formats
#'   in the same call. If no column names are specified, all columns will be
#'   used.
#'
#' @param filter Shortcuts for filtering (retaining a subset of) the rows of the
#'   output based on the number of copies detected. Options include: `"all"` =
#'   all rows that were present in the input (default), `"dupes"` = only rows
#'   that were found to be duplicated (mimics the behaviour of
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
#'   \code{\link{unique}}, \code{\link{dupes}}
#'
#' @export
copies <- function(data, ...,
                   filter = c("all", "dupes", "first", "last", "unique"),
                   keep_all_cols = TRUE,
                   sort_by_copies = FALSE,
                   order = c("d", "a", "i"),
                   na_last = FALSE,
                   output = c("same", "tibble", "dt", "data.frame")) {

  filter <- match.arg(filter)
  order <- match.arg(order)
  output <- match.arg(output)

  if(!missing(...)) {
    g <- group_parser(as.data.frame(data), ...)
  } else {
    message("No column names specified - using all columns.")
    .cols <- names(data)
    g <- .cols
  }

  .classes <- class(data)

  if("data.frame" %ni% .classes) {
    stop("Input data must be a data.table, tibble, or data.frame.")
  }

  if("data.table" %ni% .classes) {
    .dt <- data.table::as.data.table(data)
  } else {
    .dt <- data.table::as.data.table(as.data.frame(data))
    #this is conversion and reversal is necessary to prevent subsequent
    #modification of the original data source in the global environment when the
    #input is already a data.table due to the use of the := operator below.
  }

  if(filter == "dupes") {
    .dt[, n_copies := .N,
        by = eval(g)]
    orig_rows <- nrow(data)
    .dt <- .dt[n_copies > 1]
    dupe_count <- nrow(.dt)
    if(dupe_count != 0) {
      message(paste0("Duplicated rows detected! ",
                     dupe_count, " of ", orig_rows,
                     " rows in the input data have multiple copies."))
    } else {
      message("No duplicates detected.")
    }

  } else if(filter == "all") {
    .dt[, `:=` (copy_number = 1:.N,
                n_copies = .N),
        by = eval(g)]
  } else if(filter == "first") {
    if(!missing(...)) {
      .dt <- unique(.dt, by = eval(g), fromLast = FALSE)
    } else {
      .dt <- unique(.dt, fromLast = FALSE)
    }
  } else if(filter == "last"){
    if(!missing(...)) {
      .dt <- unique(.dt, by = eval(g), fromLast = TRUE)
    } else {
      .dt <- unique(.dt, fromLast = FALSE)
    }
  } else if(filter == "unique") {
    .dt[, n_copies := .N,
        by = eval(g)]
    .dt <- .dt[n_copies == 1]
    .dt[, n_copies := NULL]
  }

  if(sort_by_copies == FALSE) {
    if (output == "dt") {
      return(.dt[])

    } else if(output == "tibble") {
      .dt <- tibble::as_tibble(.dt)
      return(.dt)

    } else if(output == "data.frame") {
      .dt <- as.data.frame(.dt)
      return(.dt)

    } else if("data.table" %in% .classes && output == "same") {
      return(.dt[])

    } else if("tbl" %in% .classes && output == "same") {
      .dt <- tibble::as_tibble(.dt)
      return(.dt)

    } else if("data.frame" %in% .classes) {
      .dt <- as.data.frame(.dt)
      return(.dt)
    }
  } else if(sort_by_copies == TRUE && filter %in% c("all", "dupes")) {
    if(order == "d") {
      if(!missing(...)) {
        data.table::setorderv(.dt, c("n_copies", eval(g)),
                              na.last = na_last,
                              order = -1)
      } else {
        data.table::setorderv(.dt, "n_copies", na.last = na_last,
                              order = -1)
      }
    } else if(order %in% c("a", "i")) {
      if(!missing(...)) {
        data.table::setorderv(.dt, c("n_copies", eval(g)),
                              na.last = na_last,
                              order = 1)
      } else {
        data.table::setorderv(.dt, "n_copies", na.last = na_last,
                              order = 1)
      }
    }
  }

  if(output == "dt") {
    return(.dt[])

  } else if(output == "tibble") {
    .dt <- tibble::as_tibble(.dt)
    return(.dt)

  } else if(output == "data.frame") {
    .dt <- as.data.frame(.dt)
    return(.dt)

  } else if("data.table" %in% .classes && output == "same") {
    return(.dt)

  } else if("tbl" %in% .classes && output == "same") {
    .dt <- tibble::as_tibble(.dt)
    return(.dt)

  } else if("data.frame" %in% .classes) {
    .dt <- as.data.frame(.dt)
    return(.dt)
  }
}

# dupes ------------------------------------------------------------------
#' @title
#' Check the number of duplicated rows in a data frame.
#'
#' @description Checks a data frame for duplicated rows based on specified
#'   variables to use for checking (via `...`) or all columns (if
#'   unspecified).`dupes` is a convenience shortcut for \code{\link{copies}}
#'   with the "filter" argument set to "dupes" and the "sort_by_copies" argument
#'   set to TRUE by default. For greater flexibility in checking row copy
#'   numbers or filtering for distinct rows, use \code{\link{copies}} instead.
#'   `dupes` behaves similarly to \code{\link[janitor]{get_dupes}}) but is
#'   substantially faster due to the use of `data.table` as a backend.
#'
#' @importFrom data.table as.data.table
#' @importFrom data.table setorderv
#' @importFrom data.table setorder
#' @importFrom tibble as_tibble
#'
#' @param data a data frame, tibble, or data.table.
#'
#' @param ... This special argument accepts any number of unquoted column names
#'   (also present in the data source) to use when searching for duplicates,
#'   e.g. `x, y, z`. Also accepts a character vector of column names or index
#'   numbers, e.g. c("x", "y", "z") or c(1, 2, 3), but not a mixture of formats
#'   in the same call. If no column names are specified, all columns will be
#'   used.
#'
#' @param keep_all_cols If column names are specified using `...`, this allows
#'   you to drop unspecified columns, similarly to the `.keep_all` argument for
#'   `dplyr::distinct()``
#'
#' @param sort_by_copies If TRUE (the default), sorts the results by the number
#'   of copies, in order specified by the `order` argument.
#'
#' @param order If sort_by_copies is set to TRUE, this controls whether the
#'   results should be sorted in order of descending/decreasing = "d" (the
#'   default) or ascending/increasing = "a" or "i" copy numbers.
#'
#' @param na_last should rows of the specified columns with missing values be
#'   listed below non-missing values (TRUE/FALSE)? Default is FALSE.
#'
#' @param output "tibble" for tibble, "dt" for data.table, or "data.frame" for a
#'   data frame. "same", the default option, returns the same format as the
#'   input data.
#'
#' @return A subset of the input data frame consisting of duplicated rows that were
#'   detected based on specified variables used to condition the search. A
#'   message will also be printed to the console indicating whether or not
#'   duplicates were detected. An `n_copies` column is appended specifying the
#'   total number of copies of each row that were detected.
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gov.bc.ca}
#'
#' @examples
#'
#' # check for duplicates based on one variable, "g" in this case
#' dupes(pdata, g)
#'
#' \dontrun{
#' dupes(pdata, high_low, g) #check based on 2 variables
#'
#' # check based on all variables, i.e. fully duplicated rows
#' dupes(pdata)
#' }
#'
#' @seealso \code{\link{copies}}, \code{\link[janitor]{get_dupes}}
#'
#' @export
dupes <- function(data, ...,
                  keep_all_cols = TRUE,
                  sort_by_copies = TRUE,
                  order = c("d", "a", "i"),
                  na_last = FALSE,
                  output = c("same", "tibble", "dt", "data.frame")) {

  order <- match.arg(order)
  output <- match.arg(output)

  if(!missing(...)) {
    g <- group_parser(as.data.frame(data), ...)
  } else {
    message("No column names specified - using all columns.")
    .cols <- names(data)
    g <- .cols
  }

  .classes <- class(data)

  if("data.frame" %ni% .classes) {
    stop("Input data must be a data.table, tibble, or data.frame.")
  }


  if("data.table" %ni% .classes) {
    .dt <- data.table::as.data.table(data)
  } else {
    .dt <- data.table::as.data.table(as.data.frame(data))
    #this is conversion and reversal is necessary to prevent subsequent
    #modification of the original data source in the global environment when the
    #input is already a data.table due to the use of the := operator below.
  }

  .dt[, n_copies := .N, by = eval(g)]
  orig_rows <- nrow(data)
  .dt <- .dt[n_copies > 1]
  dupe_count <- nrow(.dt)

  if(dupe_count != 0) {
    message(paste0("Duplicated rows detected! ",
                   dupe_count, " of ", orig_rows,
                   " rows in the input data have multiple copies."))
  } else {
    message("No duplicates detected.")
  }


  if(sort_by_copies == FALSE) {
    if (output == "dt") {
      return(.dt[])

    } else if(output == "tibble") {
      .dt <- tibble::as_tibble(.dt)
      return(.dt)

    } else if(output == "data.frame") {
      .dt <- as.data.frame(.dt)
      return(.dt)

    } else if ("data.table" %in% .classes && output == "same") {
      return(.dt[])

    } else if ("tbl" %in% .classes && output == "same") {
      .dt <- tibble::as_tibble(.dt)
      return(.dt)

    } else if ("data.frame" %in% .classes) {
      .dt <- as.data.frame(.dt)
      return(.dt)
    }
  } else if(sort_by_copies == TRUE) {
    if(order == "d") {
      if(!missing(...)) {
        data.table::setorderv(.dt, c("n_copies", eval(g)),
                              na.last = na_last,
                              order = -1)
      } else {
        data.table::setorderv(.dt, "n_copies", na.last = na_last,
                              order = -1)
      }
    } else if (order %in% c("a", "i")) {
      if(!missing(...)) {
        data.table::setorderv(.dt, c("n_copies", eval(g)),
                              na.last = na_last,
                              order = 1)
      } else {
        data.table::setorderv(.dt, "n_copies", na.last = na_last,
                              order = 1)
      }
    }
  }

  if (output == "dt") {
    return(.dt[])

  } else if(output == "tibble") {
    .dt <- tibble::as_tibble(.dt)
    return(.dt)

  } else if(output == "data.frame") {
    .dt <- as.data.frame(.dt)
    return(.dt)

  } else if ("data.table" %in% .classes && output == "same") {
    return(.dt)

  } else if ("tbl" %in% .classes && output == "same") {
    .dt <- tibble::as_tibble(.dt)
    return(.dt)

  } else if ("data.frame" %in% .classes) {
    .dt <- as.data.frame(.dt)
    return(.dt)
  }
}
