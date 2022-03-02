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

#standard error of the mean####
#' @title
#' Calculate the standard error of the mean.
#'
#' @description
#' \code{se} returns the standard error of the mean.
#'
#' @importFrom stats na.omit
#' @importFrom stats sd
#'
#' @param y a numeric vector/variable.
#'
#' @param na.rm Should missing values be removed before attempting to calculate
#'   the standard error (TRUE/FALSE)?
#'
#' @author Craig P. Hutton, \email{Craig.Hutton@@gov.bc.ca}
#'
#' @examples
#' y <- c(1:100)
#' se(y)
#'
#' @references
#' Altman, D. G., & Bland, J. M. (2005). Standard deviations and standard
#' errors. Bmj, 331(7521), 903.
#'
#' @seealso \code{\link[stats]{sd}}, \code{\link{sqrt}}, \code{\link{length}}
#'
#' @export
se <- function(y, na.rm = TRUE) {
  if(na.rm == TRUE){
    y <- na.omit(y)
  }
  se <- stats::sd(y)/sqrt(length(y))
  return(se)
}

# inverse quantile:get the quantiles corresponding to a vector of  --------
#' @title
#' Get the quantile(s) corresponding to a vector of values.
#'
#' @description
#' \code{inv_quantile} returns the quantile(s) for a specific value or vector of values.
#'
#' @importFrom stats na.omit
#' @importFrom stats sd
#'
#' @param y a numeric vector/variable.
#'
#' @param values the values or vector of specific values in y for which you would like the to know the quantiles
#'
#' @param digits determines the how many digits the result is rounded to
#'
#' @author Craig P. Hutton, \email{Craig.Hutton@@gov.bc.ca}
#'
#' @examples
#' y <- c(1:100)
#' inv_quantile(y = y, values = c(2, 25, 50, 75, 95), digits = 2)
#'
#' @seealso \code{\link[stats]{ecdf}}, \code{\link{round}}
#'
#' @export
inv_quantile <- function(y, values, digits = NULL){
  f <- stats::ecdf(y)
  out <- f(values)
  if(!missing(digits)){
    out <- round(out, digits = digits)
  }
  return(out)
}

# mode ------------------------------------------------------------------
#' @title
#' Obtain the mode of a vector.
#'
#' @description
#' \code{mode} returns the mode AKA most common value rounded to 3 decimal
#' places if y is a numeric vector. If y is non-numeric the most common value is
#' returned as a character string. For additional unique values of y sorted by
#' decreasing frequency, use counts() instead. N.B. this function overwrites a
#' base R function mode() with the same name that is a convenience shortcut used
#' to specify the storage mode of an object. However, this conflict isn't much
#' of an issue because that alternative function can still be accessed using the
#' full function name = "storage.mode()".
#'
#' @param y a numeric vector/variable.
#'
#' @param digits This determines the number of digits used for rounding of
#'   numeric outputs. Default = 3.
#'
#' @param inv This allows you to get the inverse or opposite of the mode, i.e.
#'   the least common value or "anti-mode". Default is FALSE.
#'
#' @param na.rm This determines whether missing values (NAs) should be removed
#'   before attempting to count values and extract the mode (or anti-mode).
#'
#' @author Craig P. Hutton, \email{Craig.Hutton@@gov.bc.ca}
#'
#' @examples
#' y <- c(1:100, 2, 2, 4, 5, 6, 25, 50)
#'
#' mode(y) #returns the mode
#'
#' mode(y, inv = TRUE) #returns the anti-mode
#
#' @seealso \code{\link{counts}}
#'
#' @export
mode <- function(y, digits = 3, inv = FALSE, na.rm = TRUE){
  if(na.rm == TRUE) {
    y <- na.omit(y)
  }
  if(inv == FALSE) {
    if(na.rm == TRUE) {
      out <- names(sort(table(y, useNA = "no"), decreasing = TRUE))[1]
    } else {
      out <- names(sort(table(y, useNA = "ifany"), decreasing = TRUE))[1]
    }
  } else if (inv == TRUE) {
    if(na.rm == TRUE) {
      out <- names(sort(table(y, useNA = "no"), decreasing = FALSE))[1]
    } else {
      out <- names(sort(table(y, useNA = "ifany"), decreasing = FALSE))[1]
    }
  }
  if(is.numeric(y)) {
    out <- round(as.numeric(out), digits)
  }
  return(out)
}

# fmean (internal) ------------------------------------------------------------
#' @title
#' elucidate package internal wrapper function for the faster calculation of the
#' mean of a numeric vector using the sum and length functions.
#'
#' @description \code{fmean} is an internal function that supports
#'   \code{\link{skewness}} and \code{\link{kurtosis}}.
#'
#' @param y A vector/variable (required).
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gov.bc.ca}
#' @noRd
fmean <- function(y) {
  sum(y)/length(y)
}


# skewness --------------------------------------------------------------------
#' @title
#' Calculate the skewness of a numeric variable.
#'
#' @description
#' \code{skewness} calculates the degree of skewness in a numerical
#' distribution. Implements the same functionality as the skewness functions in
#' the psych and e1071 packages for convenience so that those packages are not
#' required as dependencies for elucidate. Unlike the e1071 and psych packages,
#' the elucidate version uses the type 2 calculation as a default since this
#' incorporates a correction for sample size that also matches what is used in
#' SPSS and SAS.
#'
#' @param y a numeric vector/variable.
#'
#' @param na.rm should missing values be removed before attempting to calculate
#' skewness? Default is TRUE.
#'
#' @param type the calculation method you wish to use. See
#'   \code{\link[e1071]{skewness}} and Joanes & Gill (1998) for details.
#'
#' @author Craig P. Hutton, \email{Craig.Hutton@@gov.bc.ca}
#'
#' @examples
#' y <- rnorm(1:1000, 100, 15)
#' skewness(y)
#'
#' @references
#' D. N. Joanes and C. A. Gill (1998), Comparing measures of sample skewness and
#' kurtosis. The Statistician, 47, 183–189.
#'
#' @seealso \code{\link[e1071]{skewness}}, \code{\link[psych]{skew}}
#'
#' @export
skewness <- function(y, na.rm = TRUE, type = 2) {
  #types are the same as e1071 & psych
  if(na.rm == TRUE) {
    y <- na.omit(y)
  }
  n <- length(y)

  if(type == 1){ #computed by moments::skewness
    out <- (sum((y - fmean(y))^3)/n)/
      ((sum((y -  fmean(y))^2)/n)^(3/2))
  } else if(type == 2){ #SPSS & SAS default
    out <- (sum((y - fmean(y))^3)/n)/
      ((sum((y -  fmean(y))^2)/n)^(3/2))
    out <- out*((sqrt(n*(n-1)))/(n-2))
  } else if(type == 3){ #e1071::skewness & psych::skew default
    out <- (sum((y - fmean(y))^3)/n)/
      (sd(y)^3)
  }
  return(out)
}


# kurtosis ----------------------------------------------------------------
#' @title
#' Calculate the kurtosis of a numeric variable.
#'
#' @description
#' \code{kurtosis} calculates the degree of kurtosis in a numerical
#' distribution. Implements the same functionality as the kurtosis functions in
#' the psych and e1071 packages for convenience so that those packages are not
#' required as dependencies for elucidate. Unlike the e1071 and psych packages,
#' the elucidate version uses the type 2 calculation as a default since this
#' incorporates a correction for sample size that also matches what is used in
#' SPSS and SAS.
#'
#' @param y a numeric vector/variable.
#'
#' @param na.rm should missing values be removed before attempting to calculate
#' kurtosis? Default is TRUE.
#'
#' @param type the calculation method you wish to use. See
#'   \code{\link[e1071]{kurtosis}} and Joanes & Gill (1998) for details.
#'
#' @author Craig P. Hutton, \email{Craig.Hutton@@gov.bc.ca}
#'
#' @examples
#' y <- rnorm(1:1000, 100, 15)
#' kurtosis(y)
#'
#' @references
#' D. N. Joanes and C. A. Gill (1998), Comparing measures of sample skewness and
#' kurtosis. The Statistician, 47, 183–189.
#'
#' @seealso \code{\link[e1071]{kurtosis}}, \code{\link[psych]{kurtosi}}
#'
#' @export
kurtosis <- function(y, na.rm = TRUE, type = 2) {
  #types are the same as e1071 & psych
  if(na.rm == TRUE) {
    y <- na.omit(y)
  }
  n <- length(y)
  g2 <- ((sum((y - fmean(y))^4)/n)/
           ((sum((y - fmean(y))^2)/n)^2))-3
  if(type == 1) { #computed by moments::kurtosis
    out <- g2
  } else if(type == 2){ #computed by SPSS & SAS
    out <- (n-1)*((n+1)*g2 +6)/((n-2)*(n-3))
  } else if(type == 3){ #e1071::kurtosis & psych::kurtosi default
    out <- (g2 + 3)*(1- 1/n)^2 - 3
  }
  return(out)
}

# static_to_dynamic -------------------------------------------------------
#' @title Convert a static data frame or ggplot object to a dynamic form to
#'   facilitate interactive data exploration.
#'
#' @description Uses \code{\link[plotly]{ggplotly}} or
#'   \code{\link[DT]{datatable}} to convert a ggplot2 object or data
#'   frame/tibble to a dynamic & interactive form. The input object is a
#'   dataframe with over 10,000 rows (by default, this threshold is adjustable),
#'   \code{\link[reactable]{reactable}} will be used instead of
#'   \code{\link[DT]{datatable}}, because the client-side version of
#'   \code{\link[DT]{datatable}} (implemented here) doesn't perform well or may
#'   crash your R studio session for larger datasets than this. Note that
#'   `ggplot2` graphs rendered by `plot_raincloud()` and `plot_pie()` currently
#'   cannot be properly converted to plotly format because of incompatibility
#'   with \code{\link[plotly]{ggplotly}}.
#'
#' @importFrom DT datatable
#' @importFrom plotly ggplotly
#' @importFrom tibble is_tibble
#' @importFrom ggplot2 is.ggplot
#' @importFrom grDevices rgb
#' @importFrom grDevices col2rgb
#'
#' @param static_object A data frame, tibble, or ggplot2 object.
#'
#' @param caption Add a caption/title to the dynamic table/figure.
#'
#' @param reactable Affects data frame inputs only. overrides the row-limit to
#'   convert a dataframe to reactable() format instead of datatable() format
#'   even when there are fewer rows than reactable_threshold. You might want to
#'   do this to take advantage of custom highlighting/stripe colours or grouping
#'   via the "group_by" argument. Alternatively, you could set
#'   reactable_threshold to 0 and achieve the same effect.
#'
#' @param reactable_threshold Affects data frame inputs only. Determines the
#'   threshold for the number of rows in a input data frame beyond which a
#'   reactable() is generated as output instead of a datatable().
#'
#' @param group_by If the input is a data frame and reactable is TRUE and/or the
#'   number of rows exceeds the reactable_threshold, this allows you to group the
#'   reactable output by input columns, which can be specified using a character
#'   vector.
#'
#' @param reactable_stripe_colour If the input is a data frame and reactable is
#'   TRUE and/or the number of rows exceeds the reactable_threshold, this allows
#'   you to change the row striping colour (specified using a hexcode or base R
#'   colour name). Use `elucidate::colour_options()` to see which colour options
#'   are available.
#'
#' @param reactable_highlight_colour If the input is a data frame and reactable is
#'   TRUE and/or the number of rows exceeds the reactable_threshold, this allows
#'   you to change the row highlight colour (specified using a hexcode or base R
#'   colour name). Use `elucidate::colour_options()` to see which colour options
#'   are available.
#'
#' @param reactable_selected_colour If the input is a data frame and reactable
#'   is TRUE and/or the number of rows exceeds the reactable_threshold, this
#'   allows you to change the background colour of selected rows (specified
#'   using a hexcode or base R colour name). Use `elucidate::colour_options()`
#'   to see which colour options are available.
#'
#' @return If a data frame or tibble was provided, the output will be a
#'   DataTables or reactable html widget (according to criteria specified
#'   above). If a ggplot object was provided, the output will be a plotly html
#'   widget.
#'
#'  Unique features of the DataTable-derived output for data frames include
#'  Excel-like cell editing and filling, the ability to rearrange and/or
#'  selectively hide columns, and convenience buttons for printing or
#'  downloading the table to Excel, csv, or PDF format. Unfortunately the
#'  client-side version of DataTables doesn't perform well for larger datasets.
#'  In such cases (>10,000 rows) a reactable() is returned instead, which also
#'  has some nice unique functions including stripes that are more clearly
#'  visible, row highlighting when hovering, the ability to selectively
#'  highlight rows to facilitate visual comparisons, and options to modify the
#'  highlight, stripe, and background colours. The reactable, but not datatable
#'  version, also lets you group the output by variables of interest so they can
#'  be selectively collapsed or expanded. Both output versions allow you to do
#'  search-based filtering of rows for some or all of the columns.
#'
#' @author Craig P. Hutton, \email{Craig.Hutton@@gov.bc.ca}
#'
#' @examples
#'
#' \dontrun{
#' data(mtcars)
#' library(ggplot2)
#'
#' p1 <- ggplot(mtcars, aes(x = mpg)) + geom_histogram(binwidth = 1)
#' static_to_dynamic(p1)
#'
#' static_to_dynamic(mtcars)
#'
#' static_to_dynamic(mtcars, reactable = TRUE)
#'
#' static_to_dynamic(mtcars, reactable = TRUE, group_by = "cyl")
#'
#' static_to_dynamic(mtcars, reactable = TRUE,
#'                        reactable_hightlight_colour = "lightgreen")
#' }
#'
#' @seealso \code{\link[DT]{datatable}}, \code{\link[plotly]{ggplotly}}
#'
#' @export
static_to_dynamic <- function(static_object, caption = NULL,
                              reactable = FALSE,
                              reactable_threshold = 10000,
                              group_by = NULL,
                              reactable_stripe_colour = "#e4e2e9",
                              reactable_highlight_colour = "#a28adb",
                              reactable_selected_colour = "#aaaadb") {

  if (is.data.frame(static_object) && nrow(static_object) > reactable_threshold || reactable == TRUE) {
    if (!grepl("^#", reactable_stripe_colour)) {
      #conversion of r colour name to hex code in 2 steps, equivalent to
      #gplots::col2hex() function without requiring the extra dependency.
      rgb_col <- grDevices::col2rgb(reactable_stripe_colour)
      hex_col <- grDevices::rgb(red = rgb_col[1,]/255,
                                green = rgb_col[2,]/255,
                                blue = rgb_col[3,]/255)
      reactable_stripe_colour <- hex_col
    }
    if (!grepl("^#", reactable_highlight_colour)) {
      rgb_col <- grDevices::col2rgb(reactable_highlight_colour)
      hex_col <- grDevices::rgb(red = rgb_col[1,]/255,
                                green = rgb_col[2,]/255,
                                blue = rgb_col[3,]/255)
      reactable_highlight_colour <- hex_col

    }
    if (!grepl("^#", reactable_selected_colour)) {
      rgb_col <- grDevices::col2rgb(reactable_selected_colour)
      hex_col <- grDevices::rgb(red = rgb_col[1,]/255,
                                green = rgb_col[2,]/255,
                                blue = rgb_col[3,]/255)
      reactable_selected_colour <- hex_col
    }
    if(missing(group_by)) {
      dynamic_object <- reactable::reactable(static_object,
                                             filterable = TRUE,
                                             compact = TRUE,
                                             sortable = TRUE,
                                             outlined = TRUE,
                                             resizable = TRUE,
                                             striped = TRUE,
                                             searchable = TRUE,
                                             highlight = TRUE,
                                             paginationType = "jump",
                                             selection = "multiple",
                                             onClick = "select",
                                             theme = reactable::reactableTheme(
                                               rowSelectedStyle = list(backgroundColor = reactable_selected_colour,
                                                                       boxShadow = "inset 2px 0 0 0 #000000"),
                                               stripedColor = reactable_stripe_colour,
                                               highlightColor = reactable_highlight_colour,
                                               cellPadding = "8px 12px",
                                               style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
                                               searchInputStyle = list(width = "100%")
                                             ))
    } else {
      dynamic_object <- reactable::reactable(static_object,
                                             filterable = TRUE,
                                             compact = TRUE,
                                             sortable = TRUE,
                                             outlined = TRUE,
                                             resizable = TRUE,
                                             striped = TRUE,
                                             searchable = TRUE,
                                             highlight = TRUE,
                                             paginationType = "jump",
                                             groupBy = group_by,
                                             selection = "multiple",
                                             onClick = "select",
                                             theme = reactable::reactableTheme(
                                               rowSelectedStyle = list(backgroundColor = reactable_selected_colour, boxShadow = "inset 2px 0 0 0 #000000"),
                                               stripedColor = reactable_stripe_colour,
                                               highlightColor = reactable_highlight_colour,
                                               cellPadding = "8px 12px",
                                               style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
                                               searchInputStyle = list(width = "100%")
                                             ))
    }
    if(!missing(caption)) {
      dynamic_object <- htmlwidgets::prependContent(dynamic_object,
                                                    shiny::h2(class = "title", caption))
    }
    return(dynamic_object)
  } else if (is.data.frame(static_object) && nrow(static_object) <= reactable_threshold) {
    if(!missing(caption)){
      dynamic_object <-  DT::datatable(static_object,
                                       filter = "top",
                                       caption = htmltools::tags$caption(
                                         style = 'caption-side: top; text-align: left;',
                                         caption),
                                       editable = T,
                                       class = "display compact",
                                       extensions = c("ColReorder", "Buttons", "AutoFill",
                                                      "KeyTable", "Select", "Scroller"),
                                       selection = "none",
                                       options = list(autoWidth = TRUE,
                                                      searchHighlight = TRUE,
                                                      autoFill = TRUE, colReorder = TRUE, keys = TRUE,
                                                      rowReorder = TRUE,
                                                      select = list(style = "os", items = "cell"),
                                                      dom = "Bfrtip", buttons = list(I("colvis"),
                                                                                     "copy", "print",
                                                                                     list(extend = "collection",
                                                                                          buttons = c("csv", "excel", "pdf"),
                                                                                          text = "Download"))))
    } else {
      dynamic_object <-  DT::datatable(static_object,
                                       filter = "top",
                                       editable = T,
                                       class = "display compact",
                                       extensions = c("ColReorder", "Buttons", "AutoFill",
                                                      "KeyTable", "Select", "Scroller"),
                                       selection = "none",
                                       options = list(autoWidth = TRUE,
                                                      searchHighlight = TRUE,
                                                      autoFill = TRUE, colReorder = TRUE, keys = TRUE,
                                                      rowReorder = TRUE,
                                                      select = list(style = "os", items = "cell"),
                                                      dom = "Bfrtip", buttons = list(I("colvis"),
                                                                                     "copy", "print",
                                                                                     list(extend = "collection",
                                                                                          buttons = c("csv", "excel", "pdf"),
                                                                                          text = "Download"))))
    }
    return(dynamic_object)
  } else if(ggplot2::is.ggplot(static_object)) {
    if(!missing(caption)){
      static_object <- static_object + ggplot2::ggtitle(caption)
      dynamic_object <- plotly::ggplotly(static_object)
      return(dynamic_object)
    } else if(missing(caption)) {
      dynamic_object <- plotly::ggplotly(static_object)
      return(dynamic_object)
    }
  }
}

# wash_df --------------------------------------------------------------------
#' @title clean up a data frame by parsing/updating column classes.
#'
#' @description Clean up a data frame by parsing/updating column classes,
#'   converting column names to a common case for easier use, and remove empty
#'   rows & columns. A convenience wrapper for some helpful routine cleaning
#'   functions from the janitor, readr, & tibble packages.
#'
#' @importFrom janitor remove_empty
#' @importFrom janitor clean_names
#' @importFrom tibble rownames_to_column
#' @importFrom tibble column_to_rownames
#' @importFrom purrr map_dfc
#' @importFrom readr parse_guess
#'
#' @param data A messy data frame that contains inappropriate column
#'   classifications, inconsistently structured column names, empty rows/columns
#'
#' @param clean_names If TRUE (default), applies
#'   \code{\link[janitor]{clean_names}} to reformat column names according to
#'   the specified case.
#'
#' @param case The case/format you want column names to be converted to if
#'   clean_names = TRUE. Default is snake_case.
#'
#' @param remove_empty If TRUE (the default), applies
#'   \code{\link[janitor]{remove_empty}} to remove empty rows &/or columns as
#'   per remove_which
#'
#' @param remove_which Either "rows" to remove empty rows, "cols" to remove
#'   empty columns, or c("rows", "cols") to remove both (the default).
#'
#' @param parse If TRUE (the default), applies \code{\link[readr]{parse_guess}}
#'   to each column in data to guess the appropriate column classes and update
#'   them accordingly.
#'
#' @param guess_integer If TRUE, will classify variables containing whole
#'   numbers as integer, otherwise they are classified as the more general
#'   double/numeric class.
#'
#' @param na A character vector of values that should be read as missing/NA when
#'   parse = TRUE. Default is c("", "NA").
#'
#' @param rownames_to_column If TRUE, applies
#'   \code{\link[tibble]{rownames_to_column}} to add the row names of data as a
#'   column. This is often helpful when cleaning up a data frame or tibble that
#'   used to be a matrix with row names.
#'
#' @param col_name If rownames_to_column = TRUE, this specifies the name of the
#'   new column to store the row names in.
#'
#' @param column_to_rownames If TRUE, applies
#'   \code{\link[tibble]{column_to_rownames}} to use the values of a column as
#'   the row names for the data object.
#'
#' @param names_col If column_to_rownames = TRUE, this specifies the column
#'   containing the names you want to assign to the rows of the data object.
#'
#' @return An updated version of the input data, modified according to the
#'   chosen options.
#'
#' @author Craig P. Hutton, \email{Craig.Hutton@@gov.bc.ca}
#'
#' @examples
#' data(mtcars)
#'
#' mtcars$`Extra Column` <- rep(NA, length.out = nrow(mtcars)) #add an empty column
#'
#' mtcars[33:50,] <- NA #add some missing rows
#'
#' mtcars #now mtcars is messy & more like a real raw data set
#'
#' #clean it up and convert the row names to a column
#' mtcars <- wash_df(mtcars, rownames_to_column = TRUE, col_name = "car")
#'
#' mtcars #the empty rows and column are gone, huzzah! So is that awkard column name!
#'
#' #or turn a column with rownames into row names
#' mtcars <- wash_df(mtcars, column_to_rownames = TRUE, names_col = "car")
#' mtcars
#'
#' @seealso \code{\link[janitor]{remove_empty}}
#'
#' @export
wash_df <- function(data, clean_names = TRUE, case = "snake",
                    remove_empty = TRUE, remove_which = c("rows", "cols"),
                    parse = TRUE, guess_integer = FALSE, na = c("", "NA"),
                    rownames_to_column = FALSE, col_name = "rowname",
                    column_to_rownames = FALSE, names_col = "rowname"){

  if (remove_empty == TRUE) {
    data <- janitor::remove_empty(data, which = remove_which)
  }

  if (clean_names == TRUE) {
    data <- janitor::clean_names(data, case = case)
  }

  if (rownames_to_column == TRUE) {
    data <- tibble::rownames_to_column(data, var = col_name)
  }

  if (parse == TRUE) {
    data <- purrr::map_dfc(data, ~as.character(.x))
    data <- purrr::map_dfc(data, ~readr::parse_guess(.x, na = na,
                                         guess_integer = guess_integer))
  }

  if (column_to_rownames == TRUE) {
    data <- tibble::column_to_rownames(data, var = names_col)
  }
  return(data)
}

# translate --------------------------------------------------------------
#' @title Recode a variable using a matched pair of vectors.
#'
#' @description Recode a variable using a pair of vectors matched by row (i.e. a
#'   dictionary of the sort commonly made using a spreadsheet). This allows you
#'   to translate a variable from one coding scheme to another using arbitrary
#'   conditional matching in situations where it would be overly tedious to use
#'   recode, if_else, or case_when to recode a variable because there are many
#'   conditional replacements to be specified.
#'
#' @param y A vector that you wish to translate/recode.
#'
#' @param old A vector containing values of the old coding scheme with
#'   corresponding values/rows in "new". Should be the same length as the "new"
#'   vector.
#'
#' @param new A vector containing values of the new coding scheme with
#'   corresponding values/rows in "old". Should be the same length as the "old"
#'   vector.
#'
#' @return An updated version of the input vector that has been translated from
#'   the old coding scheme to the new one.
#'
#' @author Craig P. Hutton, \email{Craig.Hutton@@gov.bc.ca}
#'
#' @examples
#'
#' data(mtcars)
#'
#' old_values <- c(1:10)
#'
#' new_values <- c("one", "two", "three", "four", "five",
#'                 "six", "seven", "eight", "nine", "ten")
#'
#' #use it on its own
#' translate(y = mtcars$cyl, old = old_values, new = new_values)
#'
#' #or within a dplyr::mutate call
#' dplyr::mutate(mtcars,
#'               translated_cyl = translate(cyl,
#'                                          old = old_values,
#'                                          new = new_values))
#'
#' @seealso \code{\link[base]{match}}
#'
#' @export
translate <- function(y, old, new) {
  out <- new[match(y, old)]
  return(out)
}

# is.error (internal) -----------------------------------------------------
#' @title
#' Does an R expression evaluate to an error? This is presently an internal
#' function used by multiple other elucidate functions.
#'
#' @description Checks whether its argument evaluates to an error. This function
#'   mostly just implements a
#'   \href{http://adv-r.had.co.nz/Exceptions-Debugging.html}{code example} from
#'   Hadley Wickham's Advanced R book.
#'
#' @param e An R expression to checked to see if it fails or not.
#'
#' @param silent If FALSE, an error message will be printed if appropriate.
#'
#' @return TRUE if `e` evaluates to an error (i.e. fails), FALSE otherwise.
#'
#' @author Craig P. Hutton, \email{Craig.Hutton@@gov.bc.ca}
#'
#' @examples
#'
#' #The following example was the original use case that motivated adding this
#' #function to `elucidate`. When writing a custom function that involves
#' #subsetting data frames by named columns, you might want to check if a user
#' #supplied the variable name as a character string (for standard evaluation) or
#' #as an unquoted name (i.e. a symbol; for non-standard evaluation)...
#'
#' \dontrun{
#' is.error(mtcars[[mpg]]) #TRUE
#' #This expression evaluates to TRUE as it returns an error because the correct
#' #syntax for subsetting a data frame with a variable name in base R is to
#' wrap the variable name in quotation marks
#'
#' is.error(mtcars[["mpg"]]) #FALSE
#' #This expression returns FALSE because the subsetting expression does not fail
#' }
#'
#' @seealso \code{\link{try}}
#'
#' @noRd
is.error <- function(e, silent = TRUE) {
  inherits(try(e, silent = silent), "try-error")
}

# recode_errors_vec (internal) --------------------------------------------
#' @title elucidate package internal function used by
#'   \code{\link{recode_errors}}.
#'
#' @description elucidate package internal function used by
#' \code{\link{recode_errors}} to re-code erroneous values of column vectors
#' when the input data is a data.frame.
#'
#' @importFrom lubridate is.Date
#'
#' @param data A vector/variable (required).
#'
#' @param errors vector of erroneous values to be recoded.
#'
#' @param replacement The value you wish to replace all errors with. Default =
#'   NA.
#'
#' @param ind An optional vector specifying the indices the data object for
#'   which to replace erroneous values. Default is all indices in data.
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gov.bc.ca}
#' @noRd
recode_errors_vec <- function(data, errors, replacement = NA, ind = NULL) {
  if(lubridate::is.Date(data)) {
    data <- as.character(data)
    if(!missing(ind)) {
      data[ind][data[ind] %in% errors] <- replacement
    } else {
      data[data %in% errors] <- replacement
    }
    data <- as.Date(data)
  } else if (is.factor(data) && !is.na(replacement) && missing(ind)) {
    levels(data)[levels(data) %in% errors] <- replacement
    data <- droplevels(data)
  } else {
    if(is.factor(data) && !is.na(replacement) && !missing(ind)) {
      lvls <- as.character(levels(data))
      levels(data)[levels(data) %in% errors] <- replacement
      data <- as.character(data)
      if(!missing(ind)) {
        data[ind][data[ind] %in% errors] <- replacement
      } else {
        data[data %in% errors] <- replacement
      }
      data <- factor(data, levels = c(lvls, replacement))
    } else {
      if(!missing(ind)) {
        data[ind][data[ind] %in% errors] <- replacement
      } else {
        data[data %in% errors] <- replacement
      }
      if(is.factor(data)) {
        data <- droplevels(data)
      }
    }
  }
  return(data)
}

# recode_errors --------------------------------------------------------
#' @title
#' Recode a vector of erroneous values in a vector or data frame column(s).
#'
#' @description
#' Recode a vector of erroneous values for a vector or column(s) in a dataframe
#' by replacing those values with NA or another common indicator value. Similar
#' to \code{\link[dplyr]{na_if}} but is more flexible, e.g. it can operate on
#' multiple columns/rows (for data frames), or value indices (for vectors).
#'
#' @importFrom data.table as.data.table
#' @importFrom data.table is.data.table
#' @importFrom tibble is_tibble
#'
#' @param data A vector, data frame, tibble, or matrix.
#'
#' @param errors A vector of erroneous values to be recoded.
#'
#' @param replacement The value you wish to replace all errors with. Default =
#'   NA.
#'
#' @param rows A vector specifying the rows of the data object for which to
#'   replace erroneous values. Default is all rows in data. Use "ind"
#'   instead if data is a vector.
#'
#' @param cols A vector specifying the columns of the data object for which to
#'   replace erroneous values. Default is all columns in data. Use "ind"
#'   instead if data is a vector.
#'
#' @param ind If data is a vector, this accepts a vector specifying the indices
#'   the data object for which to replace erroneous values. Default is all
#'   indices in data. Use "rows" &/or "cols" to specify indices to operate upon
#'   if data is not a vector.
#'
#' @author Craig P. Hutton, \email{Craig.Hutton@@gov.bc.ca}
#'
#' @examples
#'
#' #if (hypothetically) values of 0 & 8 were actually data entry errors in the
#' #mtcars dataset these can easily be recoded as NAs
#'
#' data(mtcars)
#'
#' recode_errors(mtcars,
#'   cols = c(8:11), #specify a column number range
#'   errors = c(0, 8))
#'
#' @export
recode_errors <- function(data, errors, replacement = NA, rows = NULL, cols = NULL, ind = NULL) {
  .classes <- class(data)
  if("data.table" %in% .classes) {
    if(!missing(cols)) {
      data <- as.data.frame(data)
    }
  }
  if(is.data.frame(data) || is.matrix(data)){
    if(!data.table::is.data.table(data)){
      data <- data.table::as.data.table(data)
    }
    if(missing(cols) && missing(rows)) {
      data <- data[, lapply(.SD, recode_errors_vec, errors = errors, replacement = replacement)]
    } else if (missing(cols) && !missing(rows)) {
      data <- data[, lapply(.SD, recode_errors_vec, errors = errors, replacement = replacement, ind = rows)]
    } else if (!missing(cols) && !missing(rows)) {
      data <- data[, (cols) := lapply(.SD, recode_errors_vec, errors = errors, replacement = replacement, ind = rows), .SDcols = cols]
    } else if (!missing(cols) && missing(rows)) {
      data <- data[, (cols) := lapply(.SD, recode_errors_vec, errors = errors, replacement = replacement), .SDcols = cols]
    }
    if("data.table" %in% .classes){
      return(data[])
    } else if ("tbl_df" %in% .classes) {
      return(tibble::as_tibble(data))
    } else if ("data.frame" %in% .classes) {
      return(as.data.frame(data))
    } else if ("matrix "%in% .classes) {
      return(as.matrix(data))
    }
  } else {
    if(lubridate::is.Date(data)) {
      data <- as.character(data)
      if(!missing(ind)) {
        data[ind][data[ind] %in% errors] <- replacement
      } else {
        data[data %in% errors] <- replacement
      }
      data <- as.Date(data)
    } else if (is.factor(data) && !is.na(replacement) && missing(ind)) {
      levels(data)[levels(data) %in% errors] <- replacement
      data <- droplevels(data)
    } else {
      if(is.factor(data) && !is.na(replacement) && !missing(ind)) {
        lvls <- as.character(levels(data))
        levels(data)[levels(data) %in% errors] <- replacement
        data <- as.character(data)
        if(!missing(ind)) {
          data[ind][data[ind] %in% errors] <- replacement
        } else {
          data[data %in% errors] <- replacement
        }
        data <- factor(data, levels = c(lvls, replacement))
      } else {
        if(!missing(ind)) {
          data[ind][data[ind] %in% errors] <- replacement
        } else {
          data[data %in% errors] <- replacement
        }
        if(is.factor(data)) {
          data <- droplevels(data)
        }
      }
    }
    return(data)
  }
}

# "%ni%"-------------------------------------------------------------
#' @title
#' negative value matching
#'
#' @description "%ni%" is a convenience shortcut for the
#'   negation of the match function (i.e. the %in%" operator).
#'
#' @param x the values to be negatively matched.
#'
#' @param y the values to be negatively matched against.
#'
#' @return A logical vector of the same length as x which is FALSE for matching
#'   values of y and TRUE for non-matching values of y.
#'
#' @author Craig P. Hutton, \email{Craig.Hutton@@gov.bc.ca}
#'
#' @examples
#' c(1:100) %ni% c(2, 3, 5, 10, 78:91)
#'
#' #subset data to extract rows with matching values using "%in%"
#' subset(pdata, g %in% c("a", "e"))
#'
#' #subset data to extract rows with non-matching values using "%ni%"
#' subset(pdata, g %ni% c("a", "e"))
#'
#' #equivalent to subset function for tidyverse users
#' dplyr::filter(pdata, g %ni% c("a", "e"))
#'
#' @seealso \code{\link{match}}, \code{\link{Negate}}
#'
#' @export
"%ni%" <- function(x, y) {
  nm <- !(x %in% y)
  return(nm)
}

# consum: consecutive summation -------------------------------------------
#' @title
#' consecutive summation
#'
#' @description Calculate a rolling sum of consecutive TRUE values for a logical
#'   vector or "1" values for a binary vector. The total will continue
#'   increasing as it moves along the vector and encounters only 1/TRUE, then
#'   will reset after encountering a 0/FALSE value. When working with monthly
#'   service utilization data with rows organized by ID and month, this is
#'   useful for estimating things like how many consecutive months someone used
#'   a service.
#'
#' @importFrom data.table rowid
#' @importFrom data.table rleid
#'
#' @param x the logical or binary vector to summed.
#'
#' @param skip_na Should consecutive counting resume or reset after encountering
#'   a missing value (NA)? Default is FALSE.
#'
#' @return A numeric vector of the same length as x with the consecutive total.
#'
#' @author Craig P. Hutton, \email{Craig.Hutton@@gov.bc.ca}
#'
#' @examples
#'
#' x <- c(rep(1, 8), rep(0, 5), rep(1, 4), rep(NA_real_, 2), rep(1, 6))
#'
#' consum(x)
#'
#' consum(x, skip_na = TRUE)
#'
#'
#' @seealso \code{\link{match}}, \code{\link{Negate}}
#'
#' @export
consum <- function(x, skip_na = FALSE) {
  if(!is.logical(x) && all(x %in% c(0, 1, NA, NA_integer_, NA_real_)) == FALSE) {
    stop("The x argument input must be a logical or binary vector.")
  }
  if(skip_na == TRUE) {
    x[!is.na(x)] <- data.table::rowid(data.table::rleid(x[!is.na(x)]))*x[!is.na(x)]
  } else {
    x <- data.table::rowid(data.table::rleid(x))*x
  }
  return(x)
}



# group_parser (internal) -------------------------------------------------
#' @title
#' elucidate package internal function to parse a set of symbols, character
#' strings, or numbers into a vector of column names
#'
#' @description `group_parser` is an internal function that supports multiple
#'   other elucidate functions which attempt to parse ... as a vector of column
#'   names to use for group-wise operations.
#'
#' @importFrom data.table is.data.table
#'
#' @param data A data frame.
#'
#' @param ... any number of unquoted or quoted column names and/or a set of
#'   numeric column indices
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gov.bc.ca}
#'
#' @noRd
group_parser <- function(data, ...) {
  if(data.table::is.data.table(data)) {
    if(is.error(names(data[, list(...)]))) {
      g <- gsub(" ", "", unlist(strsplit(deparse(substitute(list(...))), "[(,)]")))[-1]
    } else {
      g <- unlist(list(...))
      if(!is.character(g)) {
        g <- names(data)[g]
      }
    }
  } else {
    if(is.error(names(data[, c(...)]))) {
      g <- gsub(" ", "", unlist(strsplit(deparse(substitute(list(...))), "[(,)]")))[-1]
    } else {
      g <- unlist(list(...))
      if(!is.character(g)) {
        g <- names(data)[g]
      }
    }
  }
  if(data.table::is.data.table(data)) {
    if(is.error(names(data[, ..g]))) {
      stop("One or more of the variable names or indices supplied using",
           "\nthe special ellipsis argument (`...`) cannot be found in the input data.",
           "\nPlease ensure all variable names or indices are valid and try again.")
    }
  } else {
    if(is.error(names(data[, g]))) {
      stop("One or more of the variable names or indices supplied using",
           "\nthe special ellipsis argument (`...`) cannot be found in the input data.",
           "\nPlease ensure all variable names or indices are valid and try again.")
    }
  }
  return(g)
}
