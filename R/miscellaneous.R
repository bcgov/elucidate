# misc functions ----------------------------------------------------------

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
#' @param na.rm Should missing valued be removed before attempting to calculate
#'   the standard error (TRUE/FALSE)?
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gmail.com}
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

#inverse quantile:get the quantiles corresponding to a vector of values####
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
#' @author Craig P. Hutton, \email{craig.hutton@@gmail.com}
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

# mode of y ---------------------------------------------------------------
#' @title
#' Obtain the mode of a numeric variable.
#'
#' @description
#' \code{mode_of_y} returns the mode AKA most common value rounded to 3 decimal
#' places. For additional unique values of y sorted by decreasing frequency, use
#' mcvals instead
#'
#' @param y a numeric vector/variable.
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gmail.com}
#'
#' @examples
#' y <- c(1:100)
#' mode_of_y(y)
#'
#' @export
mode_of_y <- function(y){
  out <- names(sort(table(y), decreasing = T))[1]
  out <- out %>% as.numeric(out) %>% round(3)
  return(out)
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
#' @author Craig P. Hutton, \email{craig.hutton@@gmail.com}
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
skewness <- function (y, na.rm = TRUE, type = 2) {
  #types are the same as e1071 & psych
  if(na.rm == TRUE) {
    y <- na.omit(y)
  }
  n <- length(y)

  if(type == 1){ #computed by moments::skewness
    out <- (sum((y - mean(y))^3)/n)/
      ((sum((y -  mean(y))^2)/n)^(3/2))
  } else if(type == 2){ #SPSS & SAS default
    out <- (sum((y - mean(y))^3)/n)/
      ((sum((y -  mean(y))^2)/n)^(3/2))
    out <- out*((sqrt(n*(n-1)))/(n-2))
  } else if(type == 3){ #e1071::skewness & psych::skew default
    out <- (sum((y - mean(y))^3)/n)/
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
#' @author Craig P. Hutton, \email{craig.hutton@@gmail.com}
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
  g2 <- ((sum((y - mean(y))^4)/n)/
           ((sum((y - mean(y))^2)/n)^2))-3
  if(type == 1) { #computed by moments::kurtosis
    out <- g2
  } else if(type == 2){ #computed by SPSS & SAS
    out <- (n-1)*((n+1)*g2 +6)/((n-2)*(n-3))
  } else if(type == 3){ #e1071::kurtosis & psych::kurtosi default
    out <- (g2 + 3)*(1- 1/n)^2 - 3
  }
  return(out)
}

#most common values in a vector####
#' @title
#' Obtain the most common values or value (i.e. the mode) of a vector.
#'
#' @description \code{mcvals} returns the most common unique value(s) of a
#' vector. Also useful for identifying data entry errors or rare cases.
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_replace_all
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr group_keys
#' @importFrom purrr map_dbl
#' @importFrom purrr set_names
#' @importFrom tidyr unite
#' @importFrom tidyr separate
#' @importFrom tibble rownames_to_column
#'
#' @param data Data frame or tibble containing the column you want the most
#'   common values for (optional).
#'
#' @param y A vector/variable (required).
#'
#' @param ... Any number of grouping variables (also present in the data source)
#'   to use for subsetting, separated by commas (e.g. \code{group_var1,
#'   group_var2}). If grouping variables are supplied, then data must also be.
#'
#' @param n The number of unique values you want frequency counts for (in order
#'   of descending count). Default is "all".
#'
#' @return A data frame of the most common values from the input vector sorted in
#'   descending order. The names of the output, printed as the 1st row when the
#'   output is called, are the unique input values of y. The 2nd row of the
#'   output contains the counts of the identified unique values. If grouping
#'   variables are specified, then the values and counts for each level of the
#'   grouping variable are returned. See the \code{\link{table}} function
#'   documentation for more information.
#'
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gmail.com}
#'
#' @examples
#' #using a numeric vector
#' y <- c(1, 1, 2, 3, 4, 5, 6, 2, 3, 4, 2, 9, 5, 6, 7, 23, 5, 6, 7, 8, 3, 4, 5, 6)
#'
#' mcvals(y = y) #all unique values of y in order of descending frequency \code{y}
#' mcvals(y = y, n = 1) #the most common value of \code{y}
#' mcvals(y = y, n = 5) #the top 5 most common unique values of \code{y}
#'
#' #using a character vector
#' ch_vect <- c("a", "b", "c", "a", "b", "a")
#' mcvals(y = ch_vect)
#' mcvals(y = ch_vect, n = 1)
#' mcvals(y = ch_vect, n = 5)
#'
#' #top 5 values of y split by a grouping variable
#' data(mtcars)
#' mcvals(mtcars, y = mpg, cyl, n = 5)
#'
#' @seealso \code{\link{table}}, \code{\link{sort}},
#'   \code{\link{length}}
#'
#' @export
mcvals <- function(data = NULL, y, ..., n = "all"){
  mcvs <- function(y, n) {
    if(n == "all"){
      value <- y %>% table() %>% sort(., decreasing = T) %>% names() %>% as.character()
      count <- y %>% table() %>% sort(., decreasing = T) %>% as.character()

      mcv_df <- rbind(value, count) %>% as.data.frame
      names(mcv_df) <- stringr::str_replace_all(names(mcv_df), pattern = "V", replacement = "#")

      mcv_df
      return(mcv_df)
    } else {
      value <- y %>% table() %>% sort(., decreasing = T) %>% .[1:n] %>% names() %>% as.character()
      count <- y %>% table() %>% sort(., decreasing = T) %>% .[1:n] %>% as.character()

      mcv_df <- rbind(value, count) %>% as.data.frame
      names(mcv_df) <- stringr::str_replace_all(names(mcv_df), pattern = "V", replacement = "#")

      mcv_df
      return(mcv_df)
    }
  }

  if(missing(data) & missing(...)){
    out <- mcvs(y, n = n)
    return(out)
  } else if(!missing(data) & missing(...)){
    if(n == "all"){
      out <- data %>% dplyr::select({{y}}) %>% mcvs(n = "all")
      return(out)
    } else {
      out <- data %>% dplyr::select({{y}}) %>% mcvs(n = n)
      return(out)
    }

  } else if(!missing(data) & !missing(...)){
    if(n == "all"){
      keys <- data %>%
        dplyr::group_by(...) %>%
        dplyr::group_keys()

      max_len <- data %>% select({{y}}, ...) %>%
        dplyr::group_by(...) %>%
        dplyr::group_map(~mcvs(.x, n = n)) %>%
        purrr::map_dbl(~length(.x)) %>%
        max(na.rm = T)

      mcv_out <- data %>% select({{y}}, ...) %>%
        dplyr::group_by(...) %>%
        dplyr::group_map(~mcvs(.x, n = max_len)) %>%
        purrr::set_names(x = ., nm = tidyr::unite(keys, col = "group")$group) %>%
        do.call(rbind, .) %>%
        tibble::rownames_to_column(var = "group") %>%
        dplyr::mutate(group = stringr::str_replace_all(group, "\\..{1,10}", "")) %>%
        tidyr::separate(group, into = names(keys))

      mcv_out <- mcv_out %>%
        dplyr::mutate(measure = rep(c("value", "count"), nrow(mcv_out)/2)) %>%
        dplyr::select(ncol(.), 1:(ncol(.)-1))
      return(mcv_out)
    } else{
      keys <- data %>%
        dplyr::group_by(...) %>%
        dplyr::group_keys()

      mcv_out <- data %>% select({{y}}, ...) %>%
        dplyr::group_by(...) %>%
        dplyr::group_map(~mcvs(.x, n = n)) %>%
        purrr::set_names(x = ., nm = tidyr::unite(keys, col = "group")$group) %>%
        do.call(rbind, .) %>%
        tibble::rownames_to_column(var = "group") %>%
        dplyr::mutate(group = stringr::str_replace_all(group, "\\..{1,10}", "")) %>%
        tidyr::separate(group, into = names(keys))

      mcv_out <- mcv_out %>%
        dplyr::mutate(measure = rep(c("value", "count"), nrow(mcv_out)/2)) %>%
        dplyr::select(ncol(.), 1:(ncol(.)-1))
      return(mcv_out)
    }
  }
}

#most common values for all vectors in a data frame####
#' @title Obtain the most common value or values of each column in a data frame.
#'
#' @description \code{mcvals_all} extends \code{\link{mcvals}} by
#'   returning the most common unique value(s) for all columns of a data frame or
#'   tibble, with or without first splitting the data into groups. Useful for
#'   identifying data entry errors or rare cases, among other things.
#'
#' @importFrom magrittr %>%
#' @importFrom purrr map_df
#' @importFrom purrr map
#' @importFrom purrr set_names
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr group_keys
#' @importFrom dplyr group_map
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_cols
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr separate
#' @importFrom tidyr unite
#' @importFrom rlang enquos
#'
#'
#' @param data A data frame or tibble.
#'
#' @param ... Any number of grouping variables (also present in the data source)
#'   to use for subsetting, separated by commas (e.g. \code{group_var1,
#'   group_var1}).
#'
#' @param n The number of unique values you want frequency counts for from all
#'   variables in data when data frame output is requested (the default). If you
#'   want all unique values, set output = "list" instead.
#'
#' @param output Request either a data frame (\code{output} = "df") with the
#'   number of unique values for each variable in data specified by \code{n}, or
#'   a list (\code{output} = "list") for all value counts.
#'
#' @return A data frame or list of the top \code{n} most common value(s) from
#'   all columns in \code{data}, shown in descending order (from left to right
#'   across the numbered columns in the output, e.g. #1 = most common value AKA
#'   the mode, #2 = 2nd most common value, etc.). When in "df" output mode, if
#'   the range of indices passed to \code{n} exceeds the number of unique values
#'   for a measure, the output will contain missing values for those indices.
#'   See the \code{\link[base]{table}} and \code{\link[purrr]{map}}
#'   documentation for more information.
#'
#'   Note that when using grouping variables and requesting
#'   list output (for all of the unique values of each variable), the results will be structured as follows:
#'
#'   $group_1
#'   $group_1$measure_1
#'   y #just ignore this
#'   A, B, C, D, E, F (example unique values of measure 1 for group 1)
#'   5, 4, 3, 2, 1, 1 (example count for each of the above unique values)
#'
#'   $group_1$measure_2
#'   y
#'   2010, 2011, 2012, 2013, 2014, 2015 (unique values of measure 1 for group 1)
#'      5,    4,    3,    2,    1,    1 (count for each of the above unique values)
#'
#'   $group_1$measure_3
#'   y
#'   male, female (unique values of measure 1 for group 1)
#'     60,     60 (count for each of the above unique values)
#'
#'   $group_2
#'   $group_2$measure_1
#'   y #just ignore this
#'    A, B, C, D, E, F (unique values of measure 1 for group 1)
#'   10, 9, 8, 7, 2, 1 (count for each of the above unique values)
#'
#'   $group_2$measure_2
#'   y
#'   2010, 2011, 2012, 2013, 2014, 2015 (unique values of measure 1 for group 1)
#'     15,    8,    8,    6,    2,    2 (count for each of the above unique values)
#'
#'   $group_2$measure_3
#'   y
#'   male, female (unique values of measure 1 for group 1)
#'    109,   109 (count for each of the above unique values)
#'
#'   If multiple grouping variables are provided to ... then the output will look more like this,
#'   e.g. for 2 factors called A and B:
#'
#'   $Alevel1name_Blevel1name$measure_1
#'   y #just ignore this
#'   A, B, C, D, E, F (example unique values of measure 1 for group 1)
#'   5, 4, 3, 2, 1, 1 (example count for each of the above unique values)
#'
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gmail.com}
#'
#' @examples
#' data(mtcars)
#'
#' #without piping
#' mcvals_all(mtcars)
#'
#' #with piping
#' mtcars %>% mcvals_all
#'
#' #get the top 10 for each variable instead
#' mtcars %>% mcvals_all(n = 10)
#'
#' #get unique values for each variable split by levels of a grouping variable
#' mtcars %>% mcvals_all(cyl)
#'
#' mtcars %>% mcvals_all(cyl, am) #split by two grouping variables
#'
#' #to obtain all unqiue values, set output = "list":
#' mtcars %>% mcvals_all(output = "list")
#' mtcars %>% mcvals_all(cyl, output = "list")
#'
#' @seealso \code{\link{table}}, \code{\link[purrr]{map}}, \code{\link{mcvals}}
#'
#' @export
mcvals_all <- function(data, ..., n = 10, output = "df"){
  mcvs <- function(y) {
    mcvs <- table(y) %>% sort(., decreasing = T)
    return(mcvs)

  }
  map_mcvs <- function(data, n = 5, output = "df") {
    if (output == "df") {
      values <- data %>%
        purrr::map_df(~names(mcvs(.x)[1:n])) %>%
        purrr::map_df(as.character)

      names(values) <- paste0(names(values), "_value")

      counts <- data %>%
        purrr::map_df(~mcvs(.x)[1:n]) %>%
        purrr::map_df(as.character)

      names(counts) <- paste0(names(counts), "_count")

      mcv_df <- dplyr::bind_cols(values, counts)

      mcv_df <- mcv_df %>% t %>%
        as.data.frame %>%
        tibble::rownames_to_column("measure") %>%
        tidyr::separate(col = measure, into = c("variable", "metric"), sep = "_") %>%
        dplyr::arrange(variable)

      names(mcv_df) <- stringr::str_replace_all(names(mcv_df), pattern = "V", replacement = "#")
      return(mcv_df)

    } else if (output == "list") {
      mcv_list <- data %>% purrr::map(~mcvals(y = .x))
      return(mcv_list)
    }
  }
  if(missing(...)){
    mcv_out <- data %>% map_mcvs(n = n, output = output)
    return(mcv_out)
  } else if (!missing(...) & output == "list") {
    group_vars <- enquos(...)
    keys <- data %>%
      dplyr::group_by(!!!group_vars) %>%
      dplyr::group_keys()

    mcv_out <- data %>%
      dplyr::group_by(!!!group_vars) %>%
      dplyr::group_map(~map_mcvs(.x, output = "list")) %>%
      purrr::set_names(x = ., nm = tidyr::unite(keys, col = "group")$group)
    return(mcv_out)
  } else {
    group_vars <- enquos(...)
    keys <- data %>%
      dplyr::group_by(!!!group_vars) %>%
      dplyr::group_keys()

    mcv_out <- data %>%
      dplyr::group_by(!!!group_vars) %>%
      dplyr::group_map(~map_mcvs(.x, n = n)) %>%
      purrr::set_names(x = ., nm = tidyr::unite(keys, col = "group")$group) %>%
      do.call(rbind, .) %>%
      tibble::rownames_to_column(var = "group") %>%
      dplyr::mutate(group = stringr::str_replace_all(group, "\\..{1,10}", "")) %>%
      tidyr::separate(group, into = names(keys)) %>%
      dplyr::arrange(variable)
    return(mcv_out)
  }
}


# static_to_dynamic -------------------------------------------------------
#' @title Convert a static data frame or ggplot object to a dynamic form.
#'
#' @description Uses \code{\link[plotly]{ggplotly}} or
#'   \code{\link[DT]{datatable}} to convert a ggplot2 object or data
#'   frame/tibble to a dynamic & interactive form. To get inline boxplots and
#'   histograms for the table, use the interactive option (interactive = T) of
#'   \code{\link{describe}} or \code{\link{describe_ci}} instead.
#'
#' @importFrom magrittr %>%
#' @importFrom DT datatable
#' @importFrom plotly ggplotly
#' @importFrom tibble is_tibble
#' @importFrom ggplot2 is.ggplot
#'
#' @param static_object A data frame, tibble, or ggplot2 object.
#'
#' @param caption Add a caption/title to the dynamic table/figure.
#'
#' @return If a data frame or tibble was provided, the output will be a
#'   datatables html widget. If a ggplot object was provided, the output will be
#'   a plotly html widget.
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gmail.com}
#'
#' @examples
#'
#' \dontrun{
#' data(mtcars)
#' library(ggplot2)
#'
#' p1 <- ggplot(mtcars, aes(x = mpg)) + geom_histogram(binwidth = 1)
#' p1 %>% static_to_dynamic(caption = "Figure 1")
#'
#' mtcars %>% static_to_dynamic(caption = "Table 1")
#' }
#'
#' @seealso \code{\link[DT]{datatable}}, \code{\link[plotly]{ggplotly}},
#'   \code{\link{describe}}, \code{\link{describe_ci}}
#'
#' @export
static_to_dynamic <- function(static_object, caption = NULL){
  if(tibble::is_tibble(static_object) | is.data.frame(static_object)){
    dynamic_object <- static_object %>% DT::datatable(filter = "top",
                                                      caption = htmltools::tags$caption(
                                                        style = 'caption-side: top; text-align: left;',
                                                        caption),
                                                      editable = T,
                                                      class = "display compact",
                                                      extensions = c("AutoFill", "ColReorder", "Buttons",
                                                                     "KeyTable", "RowReorder", "Select", "Scroller"),
                                                      selection = "none",
                                                      options = list(autoWidth = TRUE,
                                                                     autoFill = TRUE,
                                                                     colReorder = TRUE,
                                                                     keys = TRUE,
                                                                     rowReorder = TRUE,
                                                                     select = list(style = 'os',
                                                                                   items = 'cell'),
                                                                     dom = 'Bfrtip',
                                                                     buttons = list(I('colvis'), 'copy', 'print',
                                                                                    list(extend = 'collection',
                                                                                         buttons = c('csv', 'excel', 'pdf'),
                                                                                         text = 'Download'))
                                                      ))

    return(dynamic_object)
  } else if(ggplot2::is.ggplot(static_object)) {
    if(!missing(caption)){
      static_object <- static_object + ggplot2::ggtitle(caption)
      dynamic_object <- static_object %>% plotly::ggplotly()
      return(dynamic_object)
    } else if(missing(caption)) {
      dynamic_object <- static_object %>% plotly::ggplotly()
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
#' @importFrom magrittr %>%
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
#' @return An updated version of the input vector that has been translated from
#'   the old coding scheme to the new one.
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gmail.com}
#'
#' @examples
#' library(magrittr)
#' data(mtcars)
#'
#' mtcars$`Extra Column` <- rep(NA, length.out = nrow(mtcars)) #add an empty column
#'
#' mtcars[33:50,] <- NA #add some missing rows
#'
#' mtcars #now mtcars is messy & more like a real raw data set
#'
#' #clean it up and convert the row names to a column
#' mtcars <- mtcars %>% wash_df(rownames_to_column = TRUE, col_name = "car")
#'
#' mtcars #the empty rows and column are gone, huzzah! So is that awkard column name!
#'
#' #or turn a column with rownames into row names
#' mtcars <- mtcars %>% wash_df(column_to_rownames = TRUE, names_col = "car")
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
    data <- data %>%
      janitor::remove_empty(which = remove_which)
  }

  if (clean_names == TRUE) {
    data <- data %>%
      janitor::clean_names(case = case)
  }

  if (rownames_to_column == TRUE) {
    data <- data %>%
      tibble::rownames_to_column(., var = col_name)
  }

  if (parse == TRUE) {
    data <- data %>%
      purrr::map_dfc(~as.character(.x)) %>%
      purrr::map_dfc(~readr::parse_guess(.x, na = na, guess_integer = guess_integer))
  }

  if (column_to_rownames == TRUE) {
    data <- data %>%
      tibble::column_to_rownames(., var = names_col)
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
#' @importFrom magrittr %>%
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
#' @author Craig P. Hutton, \email{craig.hutton@@gmail.com}
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
#' mtcars %>%
#'   dplyr::mutate(translated_cyl = translate(cyl,
#'                                     old = old_values,
#'                                     new = new_values))
#'
#' @seealso \code{\link[base]{match}}
#'
#' @export
translate <- function(y, old, new) {
  out <- new[match(y, old)]
  return(out)
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
#' @author Craig P. Hutton, \email{craig.hutton@@gmail.com}
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
recode_errors <- function(data, errors, replacement = NA,
                          rows = c(1:nrow(data)),
                          cols = c(1:ncol(data)),
                          ind = c(1:length(data))) {
  if(is.data.frame(data) == TRUE | is_tibble(data) == TRUE | is.matrix(data) == TRUE){
    for (i in 1:length(errors)) {
      data[rows, cols][data[rows, cols] == errors[i]] <- replacement
    }
    return(data)
  } else {
    for (i in 1:length(errors)) {
      data[ind][data[ind] %in% errors[i]] <- replacement
    }
    return(data)
  }
}
