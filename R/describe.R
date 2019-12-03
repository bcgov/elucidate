# tcv (internal)---------------------------------------------------------------------
#' @title
#' elucidate package internal function
#'
#' @description \code{tcv} is an internal function that supports
#'   \code{\link{describe}}.
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_c
#' @importFrom stringr str_length
#' @importFrom stringr str_pad
#'
#'
#' @param y A vector/variable (required).
#'
#' @param n The number of unique values you want frequency counts for. Default is "all".
#'
#' @param order "d" for descending/decreasing order. "a" or "i" for
#'   ascending/increasing order.
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gmail.com}
tcv <- function(y, n = "all", order = "d") {
  if(order == "d") {
    tab <- sort(table(y), decreasing = TRUE)
  } else if (order == "a" || order == "i") {
    tab <- sort(table(y))
  }
  values <- names(tab)
  counts <- as.character(tab)

  out <- stringr::str_c(values, counts, sep = "_")

  if(n != "all") {
    out <- out[1:n]
  }
  x <- c(1:length(out)) %>% as.character()
  x_max_len <- max(stringr::str_length(x))
  if(n != "all" && n > 9){
    if(x_max_len <= 99) {
      x <- stringr::str_pad(x, width = 2, side = "left", pad = "0")
    } else if (x_max_len <= 999) {
      x <- stringr::str_pad(x, width = 3, side = "left", pad = "0")
    } else if (x_max_len <= 9999){
      x <- stringr::str_pad(x, width = 4, side = "left", pad = "0")
    } else if (x_max_len <= 99999){
      x <- stringr::str_pad(x, width = 5, side = "left", pad = "0")
    } else {
      x <- stringr::str_pad(x, width = x_max_len, side = "left", pad = "0")
    }
  }
  out <- stringr::str_c("v", x,"_n", "-", out)
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
#'   skew and kurtosis for numeric variables) while at the same time offering
#'   slightly more concise syntax than skim (e.g. no preceding group_by
#'   operation is needed for group-wise calculations) and enhanced customization
#'   options via clearly defined arguments than the alternatives (e.g. ascending
#'   or descending sorting, modifying the number of unique values to return
#'   counts for character/numeric vectors) while still achieving comparable
#'   processing times to the alternatives. To obtain summaries for all variables
#'   in a data frame use \code{\link{describe_all}} instead.
#'
#' @importFrom magrittr %>%
#' @importFrom data.table as.data.table
#' @importFrom data.table uniqueN
#' @importFrom data.table dcast
#' @importFrom lubridate is.Date
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
#'   you wish to obtain a descriptive summary
#'
#' @param ... If the data object is a data.frame, this special argument accepts
#'   any number of unquoted grouping variable names (also present in the data source)
#'   to use for subsetting, separated by commas (e.g. \code{group_var1,
#'   group_var2})
#'
#' @param digits This determines the number of digits used for rounding of
#'   numeric outputs.
#'
#' @param order For variables/vectors of class "character" or "factor" this
#'   determines if the unique values should be sorted in order of
#'   descending/decreasing = "d" or ascending/increasing = "a" or "i" frequency
#'   counts.
#'
#' @param n For variables/vectors of class "character" or "factor" this
#'   determines how many of the top unique values based on increasing or
#'   decreasing frequency (as specified by the order argument) are
#'   saved/printed. E.g. for just the most common value, set order = "d"
#'   (default) and n = 1.
#'
#' @param type For numeric and integer vectors this determines the type of
#'   skewness and kurtosis calculations to perform. See
#'   \code{\link[e1071]{skewness}} or \code{\link[psych]{skew}} and
#'   \code{\link[e1071]{kurtosis}} or \code{\link[psych]{kurtosi}} for details.
#'
#' @param na.rm This determines whether missing values (NAs) should be removed
#'   before attempting to calculate summary statistics.
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
#'     \item{cases}{the proportion of total cases with missing values}
#'   }
#'
#'   In addition to part 1, these measures are provided for \strong{dates}:
#'
#'   \describe{
#'    \item{n_unique}{the total number of unique values or levels of y. For dates this tells you how many time points there are}
#'     \item{start}{the earliest or minimum date in y}
#'     \item{end}{the latest or maximum date in y}
#'     \item{p_FALSE}{the proportion of y values that are FALSE}
#'   }
#'
#'   In addition to part 1, these measures are provided for \strong{factors}:
#'
#'   \describe{
#'     \item{n_unique}{the total number of unique values or levels of y}
#'     \item{ordered}{a logical indicating whether or not y is ordinal}
#'     \item{value_n}{a series of outputs for the top "n" or "all" unique values of y with their frequency counts following the format "value_count"}
#'   }
#'
#'   In addition to part 1, these measures are provided for \strong{character/string} vectors:
#'
#'   \describe{
#'     \item{n_unique}{the total number of unique values or levels of y}
#'     \item{value_n}{a series of outputs for the top "n" or "all" unique values of y with their frequency counts following the format "value_count"}
#'   }
#'
#'   In addition to part 1, these measures are provided for \strong{logcial} vectors:
#'
#'   \describe{
#'     \item{n_TRUE}{the total number of y values that are TRUE}
#'     \item{n_FALSE}{the total number of y values that are FALSE}
#'     \item{p_TRUE}{the proportion of y values that are TRUE}
#'     \item{p_FALSE}{the proportion of y values that are FALSE}
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
#'     \item{skew}{the skewness of the distibution of y}
#'     \item{kurt}{the kurtosis of the distibution of y}
#'   }
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gmail.com}
#'
#' @examples
#'
#' describe(data = pdata, y = y1) #no grouping variables, numeric input class
#' describe(pdata, y1, high_low) #one grouping variable, numeric input class
#' describe(pdata, g2) #factor input class
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
#'   \code{\link[stats]{quantile}}, \code{\link{skewness}}, \code{\link{kurtosis}}
#'
#' @export
describe <- function(data, y = NULL, ..., digits = 3, order = "d", n = "all", type = 2, na.rm = TRUE, output = "tibble"){
  if((is.vector(data) | is.factor(data)) | lubridate::is.Date(data)) {
    if(is.numeric(data)){
      dt <- data.table::as.data.table(data)
      description <- dt[, .(cases = .N,
                            n = sum(!is.na(data)),
                            na = sum(is.na(data)),
                            p_na = round(sum(is.na(data))/length(data), digits),
                            mean = round(sum(data, na.rm = na.rm)/length(na.omit(data)), digits),
                            sd = round(stats::sd(data, na.rm = na.rm), digits),
                            se = round(se(data, na.rm = na.rm), digits),
                            p0 = round(stats::quantile(data, probs = 0, na.rm = na.rm), digits),
                            p25 = round(stats::quantile(data, probs = 0.25, na.rm = na.rm), digits),
                            p50 = round(stats::quantile(data, probs = 0.50, na.rm = na.rm), digits),
                            p75 = round(stats::quantile(data, probs = 0.75, na.rm = na.rm), digits),
                            p100 = round(stats::quantile(data, probs = 1, na.rm = na.rm), digits),
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
                            p_TRUE = round(sum(data, na.rm = na.rm)/length(na.omit(data)), digits),
                            p_FALSE = round(sum(data == 0, na.rm = na.rm)/length(na.omit(data)), digits))]
    } else if (lubridate::is.Date(data)){
      dt <- data.table::as.data.table(data)
      description <- dt[, .(cases = .N,
                            n = sum(!is.na(data)),
                            na = sum(is.na(data)),
                            p_na = round(sum(is.na(data))/length(data), digits),
                            n_unique = data.table::uniqueN(data),
                            start = min(data, na.rm = na.rm),
                            end = max(data))]
    } else if (is.factor(data)) {
      dt <- data.table::as.data.table(data)
      suppressMessages(
        description <- dt[, .(cases = .N,
                              n = sum(!is.na(data)),
                              na = sum(is.na(data)),
                              p_na = round(sum(is.na(data))/length(data), digits),
                              n_unique = data.table::uniqueN(data),
                              ordered = is.ordered(data),
                              tcvs = tcv(data, n = n, order = order))] %>%
          tidyr::separate(tcvs, into = c("value", "count"), sep = "-")  %>% stats::na.omit() %>%
          data.table::dcast(formula = ... ~ value, value.var = "count")
      )
    } else {
      dt <- data.table::as.data.table(data)
      suppressMessages(
        description <- dt[, .(cases = .N,
                              n = sum(!is.na(data)),
                              na = sum(is.na(data)),
                              p_na = round(sum(is.na(data))/length(data), digits),
                              n_unique = data.table::uniqueN(data),
                              tcvs = tcv(data, n = n, order = order))] %>%
          tidyr::separate(tcvs, into = c("value", "count"), sep = "-")  %>% stats::na.omit() %>%
          data.table::dcast(formula = ... ~ value, value.var = "count")
      )
    }
  } else {
    if(missing(y)){
      stop("If a non-vector (e.g. data frame) is supplied to the data argument, y must also be specified.\nIf you want summaries for all variables in data, use elucidate::describe_all() instead")
    }
    dt <- data.table::as.data.table(data)
    y <- deparse(substitute(y))
    g <- gsub(" ", "", unlist(strsplit(deparse(substitute(list(...))), "[(,)]")))[-1]

    if(is.numeric(dt[[y]])){
      if(!missing(...)){
        description <- dt[, .(cases = .N,
                              n = sum(!is.na(get(y))),
                              na = sum(is.na(get(y))),
                              p_na = round(sum(is.na(get(y)))/length(get(y)), digits),
                              mean = round(sum(get(y), na.rm = na.rm)/length(na.omit(get(y))), digits),
                              sd = round(stats::sd(get(y), na.rm = na.rm), digits),
                              se = round(se(get(y), na.rm = na.rm), digits),
                              p0 = round(stats::quantile(get(y), probs = 0, na.rm = na.rm), digits),
                              p25 = round(stats::quantile(get(y), probs = 0.25, na.rm = na.rm), digits),
                              p50 = round(stats::quantile(get(y), probs = 0.50, na.rm = na.rm), digits),
                              p75 = round(stats::quantile(get(y), probs = 0.75, na.rm = na.rm), digits),
                              p100 = round(stats::quantile(get(y), probs = 1, na.rm = na.rm), digits),
                              skew = round(skewness(get(y), type = type, na.rm = na.rm), digits),
                              kurt = round(kurtosis(get(y), type = type, na.rm = na.rm), digits)),
                          by = eval(g)]
      } else {
        description <- dt[, .(cases = .N,
                              n = sum(!is.na(get(y))),
                              na = sum(is.na(get(y))),
                              p_na = round(sum(is.na(get(y)))/length(get(y)), digits),
                              mean = round(sum(get(y), na.rm = na.rm)/length(na.omit(get(y))), digits),
                              sd = round(stats::sd(get(y), na.rm = na.rm), digits),
                              se = round(se(get(y), na.rm = na.rm), digits),
                              p0 = round(stats::quantile(get(y), probs = 0, na.rm = na.rm), digits),
                              p25 = round(stats::quantile(get(y), probs = 0.25, na.rm = na.rm), digits),
                              p50 = round(stats::quantile(get(y), probs = 0.50, na.rm = na.rm), digits),
                              p75 = round(stats::quantile(get(y), probs = 0.75, na.rm = na.rm), digits),
                              p100 = round(stats::quantile(get(y), probs = 1, na.rm = na.rm), digits),
                              skew = round(skewness(get(y), type = type, na.rm = na.rm), digits),
                              kurt = round(kurtosis(get(y), type = type, na.rm = na.rm), digits))]
      }
    } else if (is.logical(dt[[y]])) {
      if(!missing(...)){
        description <- dt[, .(cases = .N,
                              n = sum(!is.na(get(y))),
                              na = sum(is.na(get(y))),
                              p_na = round(sum(is.na(get(y)))/length(get(y)), digits),
                              n_TRUE = round(sum(get(y), na.rm = na.rm), digits),
                              n_FALSE = round(sum(get(y) == 0, na.rm = na.rm), digits),
                              p_TRUE = round(sum(get(y), na.rm = na.rm)/length(na.omit(get(y))), digits),
                              p_FALSE = round(sum(get(y) == 0, na.rm = na.rm)/length(na.omit(get(y))), digits)),
                          by = eval(g)]
      } else {
        description <- dt[, .(cases = .N,
                              n = sum(!is.na(get(y))),
                              na = sum(is.na(get(y))),
                              p_na = round(sum(is.na(get(y)))/length(get(y)), digits),
                              n_TRUE = round(sum(get(y), na.rm = na.rm), digits),
                              n_FALSE = round(sum(get(y) == 0, na.rm = na.rm), digits),
                              p_TRUE = round(sum(get(y), na.rm = na.rm)/length(na.omit(get(y))), digits),
                              p_FALSE = round(sum(get(y) == 0, na.rm = na.rm)/length(na.omit(get(y))), digits))]
      }
    } else if (lubridate::is.Date(dt[[y]])) {
      if(!missing(...)){
        description <- dt[, .(cases = .N,
                              n = sum(!is.na(get(y))),
                              na = sum(is.na(get(y))),
                              p_na = round(sum(is.na(get(y)))/length(get(y)), digits),
                              n_unique = data.table::uniqueN(get(y)),
                              start = min(get(y), na.rm = na.rm),
                              end = max(get(y))),
                          by = eval(g)]
      } else {
        description <- dt[, .(cases = .N,
                              n = sum(!is.na(get(y))),
                              na = sum(is.na(get(y))),
                              p_na = round(sum(is.na(get(y)))/length(get(y)), digits),
                              n_unique = data.table::uniqueN(get(y)),
                              start = min(get(y), na.rm = na.rm),
                              end = max(get(y)))]
      }
    } else if (is.factor(dt[[y]])) {
      if(!missing(...)){
        suppressMessages(
          description <- dt[, .(cases = .N,
                                n = sum(!is.na(get(y))),
                                na = sum(is.na(get(y))),
                                p_na = round(sum(is.na(get(y)))/length(get(y)), digits),
                                n_unique = data.table::uniqueN(get(y)),
                                ordered = is.ordered(data),
                                tcvs = tcv(get(y), n = n, order = order)),
                            by = eval(g)] %>%
            tidyr::separate(tcvs, into = c("value", "count"), sep = "-")  %>% stats::na.omit() %>%
            data.table::dcast(formula = ... ~ value, value.var = "count")
        )
      } else {
        suppressMessages(
          description <- dt[, .(cases = .N,
                                n = sum(!is.na(get(y))),
                                na = sum(is.na(get(y))),
                                p_na = round(sum(is.na(get(y)))/length(get(y)), digits),
                                n_unique = data.table::uniqueN(get(y)),
                                ordered = is.ordered(data),
                                tcvs = tcv(get(y), n = n, order = order))] %>%
            tidyr::separate(tcvs, into = c("value", "count"), sep = "-")  %>% stats::na.omit() %>%
            data.table::dcast(formula = ... ~ value, value.var = "count")
        )
      }
    } else {
      if(!missing(...)){
        suppressMessages(
          description <- dt[, .(cases = .N,
                                n = sum(!is.na(get(y))),
                                na = sum(is.na(get(y))),
                                p_na = round(sum(is.na(get(y)))/length(get(y)), digits),
                                n_unique = data.table::uniqueN(get(y)),
                                tcvs = tcv(get(y), n = n, order = order)),
                            by = eval(g)] %>%
            tidyr::separate(tcvs, into = c("value", "count"), sep = "-")  %>% stats::na.omit() %>%
            data.table::dcast(formula = ... ~ value, value.var = "count")
        )
      } else {
        suppressMessages(
          description <- dt[, .(cases = .N,
                                n = sum(!is.na(get(y))),
                                na = sum(is.na(get(y))),
                                p_na = round(sum(is.na(get(y)))/length(get(y)), digits),
                                n_unique = data.table::uniqueN(get(y)),
                                tcvs = tcv(get(y), n = n, order = order))] %>%
            tidyr::separate(tcvs, into = c("value", "count"), sep = "-")  %>% stats::na.omit() %>%
            data.table::dcast(formula = ... ~ value, value.var = "count")
        )
      }
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
# end of describe ---------------------------------------------------------



# dscr_all (internal) -----------------------------------------------------
#' @title
#' elucidate package internal function
#'
#' @description \code{dscr_all} is an internal function that supports
#'   \code{\link{describe_all}}.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select_if
#' @importFrom purrr map
#' @importFrom data.table %chin%
#' @importFrom data.table rbindlist
#' @importFrom tibble as_tibble
#'
#' @param data input vector (required).
#' @param class passed to \code{\link{describe}}
#' @param n passed to \code{\link{describe}}
#' @param digits passed to \code{\link{describe}}
#' @param type passed to \code{\link{describe}}
#' @param output passed to \code{\link{describe}}
#' @param order passed to \code{\link{describe}}
#' @param na.rm passed to \code{\link{describe}}
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gmail.com}
dscr_all <- function(data, class = "all", n = 5, digits = 3, type = 2, output = "dt", order = "d", na.rm = TRUE) {
  ls <- list()

  if(class == "all" || "d" %chin% class) {
    date_data <- dplyr::select_if(data, lubridate::is.Date)
  }
  if(class == "all" || "f" %chin% class) {
    fct_data <- dplyr::select_if(data, is.factor)
  }
  if(class == "all" || "c" %chin% class){
    chr_data <- dplyr::select_if(data, is.character)
  }
  if(class == "all" || "l" %chin% class) {
    lgl_data <- dplyr::select_if(data, is.logical)
  }
  if(class == "all" || "n" %chin% class) {
    num_data <- dplyr::select_if(data, is.numeric)
  }
  if((class == "all" || "d" %chin% class) && ncol(date_data) != 0){
    if(output == "tibble") {
      ls[["date"]] <- date_data %>%
        purrr::map(~describe(.x, output = "dt")) %>%
        data.table::rbindlist(use.names = TRUE, idcol = "variable", fill = TRUE) %>%
        tibble::as_tibble()
    } else {
      ls[["date"]] <- date_data %>%
        purrr::map(~describe(.x, output = "dt")) %>%
        data.table::rbindlist(use.names = TRUE, idcol = "variable", fill = TRUE)
    }
  }

  if((class == "all" || "f" %chin% class) && ncol(fct_data) != 0){
    if(output == "tibble") {
      ls[["factor"]] <- fct_data %>%
        purrr::map(~describe(.x, n = n, digits = digits, order = order, output = "dt")) %>%
        data.table::rbindlist(use.names = TRUE, idcol = "variable", fill = TRUE) %>%
        tibble::as_tibble()
    } else {
      ls[["factor"]] <- fct_data %>%
        purrr::map(~describe(.x, n = n, digits = digits, order = order, output = "dt")) %>%
        data.table::rbindlist(use.names = TRUE, idcol = "variable", fill = TRUE)
    }
  }
  if((class == "all" || "c" %chin% class) && ncol(chr_data) != 0){
    if(output == "tibble") {
      ls[["character"]] <- chr_data %>%
        purrr::map(~describe(.x, n = n, digits = digits, order = order, output = "dt")) %>%
        data.table::rbindlist(use.names = TRUE, idcol = "variable", fill = TRUE) %>%
        tibble::as_tibble()
    } else {
      ls[["character"]] <- chr_data %>%
        purrr::map(~describe(.x, n = n, digits = digits, order = order, output = "dt")) %>%
        data.table::rbindlist(use.names = TRUE, idcol = "variable", fill = TRUE)
    }
  }
  if((class == "all" || "l" %chin% class) && ncol(lgl_data) != 0){
    if(output == "tibble") {
      ls[["logical"]] <- lgl_data %>%
        purrr::map(~describe(.x, output = "dt")) %>%
        data.table::rbindlist(use.names = TRUE, idcol = "variable", fill = TRUE) %>%
        tibble::as_tibble()
    } else {
      ls[["logical"]] <- lgl_data %>%
        purrr::map(~describe(.x, output = "dt")) %>%
        data.table::rbindlist(use.names = TRUE, idcol = "variable", fill = TRUE)
    }
  }

  if((class == "all" || "n" %chin% class) && ncol(num_data) != 0){
    if(output == "tibble") {
      ls[["numeric"]] <- num_data %>%
        purrr::map(~describe(.x, digits = digits, type = type, output = "dt")) %>%
        data.table::rbindlist(use.names = TRUE, idcol = "variable", fill = TRUE) %>%
        tibble::as_tibble()
    } else {
      ls[["numeric"]] <- num_data %>%
        purrr::map(~describe(.x, digits = digits, type = type, output = "dt")) %>%
        data.table::rbindlist(use.names = TRUE, idcol = "variable", fill = TRUE)
    }
  }

  if(length(ls) == 1){
    ls <- ls[[1]]
  }
  return(ls)
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
#' @importFrom magrittr %>%
#' @importFrom data.table as.data.table
#' @importFrom data.table %chin%
#' @importFrom dplyr select
#' @importFrom dplyr select_if
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr group_cols
#' @importFrom lubridate is.Date
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @importFrom purrr map
#' @importFrom tidyselect everything
#'
#' @param data A data frame or tibble.
#'
#' @param ... This special argument accepts any number of unquoted grouping
#'   variable names (also present in the data source) to use for subsetting,
#'   separated by commas (e.g. \code{group_var1, group_var2})
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
#' @param order For variables/vectors of class "character" or "factor" this
#'   determines if the unique values should be sorted in order of
#'   descending/decreasing = "d" or ascending/increasing = "a" or "i" frequency
#'   counts.
#'
#' @param n For variables/vectors of class "character" or "factor" this
#'   determines how many of the top unique values based on increasing or
#'   decreasing frequency (as specified by the order argument) are
#'   saved/printed. E.g. for just the most common value, set order = "d"
#'   (default) and n = 1.
#'
#' @param type For numeric and integer vectors this determines the type of
#'   skewness and kurtosis calculations to perform. See
#'   \code{\link[e1071]{skewness}} or \code{\link[psych]{skew}} and
#'   \code{\link[e1071]{kurtosis}} or \code{\link[psych]{kurtosi}} for details.
#'
#' @param na.rm This determines whether missing values (NAs) should be removed
#'   before attempting to calculate summary statistics.
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
#'     \item{cases}{the proportion of total cases with missing values}
#'   }
#'
#'   In addition to part 1, these measures are provided for \strong{dates}:
#'
#'   \describe{
#'    \item{n_unique}{the total number of unique values or levels of y. For dates this tells you how many time points there are}
#'     \item{start}{the earliest or minimum date in y}
#'     \item{end}{the latest or maximum date in y}
#'     \item{p_FALSE}{the proportion of y values that are FALSE}
#'   }
#'
#'   In addition to part 1, these measures are provided for \strong{factors}:
#'
#'   \describe{
#'     \item{n_unique}{the total number of unique values or levels of y}
#'     \item{ordered}{a logical indicating whether or not y is ordinal}
#'     \item{value_n}{a series of outputs for the top "n" or "all" unique values of y with their frequency counts following the format "value_count"}
#'   }
#'
#'   In addition to part 1, these measures are provided for \strong{character/string} vectors:
#'
#'   \describe{
#'     \item{n_unique}{the total number of unique values or levels of y}
#'     \item{value_n}{a series of outputs for the top "n" or "all" unique values of y with their frequency counts following the format "value_count"}
#'   }
#'
#'   In addition to part 1, these measures are provided for \strong{logcial} vectors:
#'
#'   \describe{
#'     \item{n_TRUE}{the total number of y values that are TRUE}
#'     \item{n_FALSE}{the total number of y values that are FALSE}
#'     \item{p_TRUE}{the proportion of y values that are TRUE}
#'     \item{p_FALSE}{the proportion of y values that are FALSE}
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
#'     \item{skew}{the skewness of the distibution of y}
#'     \item{kurt}{the kurtosis of the distibution of y}
#'   }
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gmail.com}
#'
#' @examples
#'
#' describe_all(mtcars)
#'
#' \dontrun{
#' describe_all(pdata) #all summary types in a list
#'
#' #numeric summary only
#' pdata %>% describe_all(high_low, output = "dt", class = "n")
#'
#' #numeric and logical summaries only
#' pdata %>%
#' describe_all(high_low, output = "dt", class = c("n", "l"))
#' }
#'
#' @seealso \code{\link{describe}}
#'
#' @export
describe_all <- function(data, ..., class = "all", digits = 3, order = "d", n = 5,
                         type = 2, na.rm = TRUE, output = "tibble") {
  if(!missing(...)) {
    gdata <-  data %>% dplyr::group_by(...)
    ndata <- gdata %>% tidyr::nest()
    g_cols <-  gdata %>% dplyr::select(dplyr::group_cols()) %>% ncol()

    if(class == "all" || "d" %chin% class) {
      date_cols <- gdata %>% dplyr::select_if(lubridate::is.Date) %>% ncol()
      date_cols <- date_cols - g_cols
    }
    if(class == "all" || "f" %chin% class) {
      fct_cols <- gdata %>% dplyr::select_if(is.factor) %>% ncol()
      fct_cols <- fct_cols - g_cols
    }
    if(class == "all" || "c" %chin% class){
      chr_cols <- gdata %>% dplyr::select_if(is.character) %>% ncol()
      chr_cols <- chr_cols - g_cols
    }
    if(class == "all" || "l" %chin% class) {
      lgl_cols <- gdata %>% dplyr::select_if(is.logical) %>% ncol()
      lgl_cols <- lgl_cols - g_cols
    }
    if(class == "all" || "n" %chin% class) {
      num_cols <- gdata %>% dplyr::select_if(is.numeric) %>% ncol()
      num_cols <- num_cols - g_cols
    }
    ls <- list()
    if((class == "all" || "d" %chin% class) && date_cols >= 1){
      if(output == "tibble") {
        ls[["date"]] <- ndata %>%
          dplyr::mutate(data = purrr::map(data, ~dscr_all(.x, class = "d"))) %>%
          tidyr::unnest(data) %>% dplyr::select(variable, tidyselect::everything()) %>% dplyr::arrange(variable)
      } else if (output == "dt"){
        ls[["date"]] <- ndata %>%
          dplyr::mutate(data = purrr::map(data, ~dscr_all(.x, class = "d"))) %>%
          tidyr::unnest(data) %>% dplyr::select(variable, tidyselect::everything()) %>% dplyr::arrange(variable) %>%
          data.table::as.data.table()
      }
    }
    if((class == "all" || "f" %chin% class) && fct_cols >= 1){
      if(output == "tibble") {
        ls[["factor"]] <- ndata %>%
          dplyr::mutate(data = purrr::map(data, ~dscr_all(.x, n = n, digits = digits, order = order, class = "f"))) %>%
          tidyr::unnest(data) %>% dplyr::select(variable, tidyselect::everything()) %>%
          dplyr::arrange(variable)
      } else if (output == "dt"){
        ls[["factor"]] <- ndata %>%
          dplyr::mutate(data = purrr::map(data, ~dscr_all(.x, n = n, digits = digits, order = order, class = "f"))) %>%
          tidyr::unnest(data) %>% dplyr::select(variable, tidyselect::everything()) %>%
          dplyr::arrange(variable) %>%
          data.table::as.data.table()
      }
    }
    if((class == "all" || "c" %chin% class) && chr_cols >= 1){
      if(output == "tibble") {
        ls[["character"]] <- ndata %>%
          dplyr::mutate(data = purrr::map(data, ~dscr_all(.x, n = n, digits = digits, order = order, class = "c"))) %>%
          tidyr::unnest(data) %>% dplyr::select(variable, tidyselect::everything()) %>%
          dplyr::arrange(variable)
      } else if (output == "dt"){
        ls[["character"]] <- ndata %>%
          dplyr::mutate(data = purrr::map(data, ~dscr_all(.x, n = n, digits = digits, order = order, class = "c"))) %>%
          tidyr::unnest(data) %>% dplyr::select(variable, tidyselect::everything()) %>%
          dplyr::arrange(variable) %>%
          data.table::as.data.table()
      }
    }
    if((class == "all" || "l" %chin% class) && lgl_cols >= 1){
      if(output == "tibble") {
        ls[["logical"]] <- ndata %>%
          dplyr::mutate(data = purrr::map(data, ~dscr_all(.x, class = "l"))) %>%
          tidyr::unnest(data) %>% dplyr::select(variable, tidyselect::everything()) %>%
          dplyr::arrange(variable)
      } else if (output == "dt"){
        ls[["logical"]] <- ndata %>%
          dplyr::mutate(data = purrr::map(data, ~dscr_all(.x, class = "l"))) %>%
          tidyr::unnest(data) %>% dplyr::select(variable, tidyselect::everything()) %>%
          dplyr::arrange(variable) %>%
          data.table::as.data.table()
      }
    }
    if((class == "all" || "n" %chin% class) && num_cols >= 1){
      if(output == "tibble") {
        ls[["numeric"]] <- ndata %>%
          dplyr::mutate(data = purrr::map(data, ~dscr_all(.x, digits = digits, type = type, class = "n"))) %>%
          tidyr::unnest(data) %>% dplyr::select(variable, tidyselect::everything()) %>%
          dplyr::arrange(variable)
      } else if (output == "dt"){
        ls[["numeric"]] <- ndata %>%
          dplyr::mutate(data = purrr::map(data, ~dscr_all(.x, digits = digits, type = type, class = "n"))) %>%
          tidyr::unnest(data) %>% dplyr::select(variable, tidyselect::everything()) %>%
          dplyr::arrange(variable) %>%
          data.table::as.data.table()
      }
    }
    if(length(ls) == 0){
      stop("Data object contains no variables of the chosen class(es), respecify class argument!")
    }
    if(length(ls) == 1){
      ls <- ls[[1]]
    }
    return(ls)
  } else {
    out <- dscr_all(data, class = class, n = n, digits = digits, type = type,
                    output = output, order = order, na.rm = na.rm)
    return(out)
  }
}

