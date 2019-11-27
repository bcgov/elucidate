#descriptive summaries with arbitrary group subdivisions####
#' @title
#'
#' Obtain a descriptive summary of a variable.
#'
#' @description Obtain a useful array of common summary statistics for a numeric
#'   variable/vector, or the counts of the most frequent unique values of a
#'   non-numeric variable, within a data frame with or without first separating
#'   it into groups based on any number of grouping variables. Also
#'   pipe-friendly (data is the 1st argument). While other similar functions
#'   exist in other packages (e.g. \code{\link[psych]{describeBy}}), the use of
#'   tidy evaluation using \code{\link[dplyr]{dplyr}} pipes makes it easy to subset
#'   using an arbitrary number of grouping variables. For exploratory or
#'   publication/presentation purposes, further customization is made easier
#'   using the interactive mode.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr group_keys
#' @importFrom dplyr group_map
#' @importFrom dplyr bind_cols
#' @importFrom dplyr select
#' @importFrom dplyr summarize
#' @importFrom dplyr mutate
#' @importFrom tidyselect contains
#' @importFrom stats sd
#' @importFrom stats shapiro.test
#' @importFrom stats quantile
#' @importFrom stats na.omit
#' @importFrom stats IQR
#' @importFrom sparkline spk_chr
#' @importFrom sparkline spk_add_deps
#' @importFrom DT datatable
#' @importFrom tibble rownames_to_column
#' @importFrom stringr str_replace_all
#' @importFrom purrr set_names
#' @importFrom purrr map_dbl
#' @importFrom purrr map_dfc
#' @importFrom purrr as_vector
#' @importFrom tidyr unite
#' @importFrom tidyr separate
#' @importFrom tidyr pivot_wider
#' @importFrom htmltools tags
#'
#' @param data A data frame or tibble containing the dependent measure "Y" and
#'   any grouping variables.
#'
#' @param y The numeric variable for which you wish to obtain a descriptive
#'   summary and normality assessment
#'
#' @param ... Any number of grouping variables (also present in the data source)
#'   to use for subsetting, separated by commas (e.g. \code{group_var1,
#'   group_var2})
#'
#' @param n The number of unique values you want frequency counts for (default =
#'   5). Ignored if y is numeric. If you want all unique values or the unique
#'   value counts for a numeric variable, use code{\link{mcvals}} instead.
#'
#' @param interactive Determines whether a data frame or an interactive html
#'   table with options for editing and exporting and in line box plots and
#'   histograms produced by \code{\link[sparkline]{sparkline}} and
#'   \code{\link[DT]{datatable}} are returned.
#'
#' @param paging Set to F to disable paging of an interactive table. Ignored if
#'   interactive = F.
#'
#' @param caption Add a caption/title to the table (interactive mode only).
#'
#' @return A vector of summary statistics if y is numeric, or sample size info
#'   and the most common n values of y if it is non-numeric, split into
#'   group(s).
#'
#'   For all input variables, the following are returned:
#'
#'   cases = the total number of cases
#'
#'   n = number of complete cases
#'
#'   na = number of missing values
#'
#'   p_na = percentage of total cases with missing values
#'
#'   These additional measures are provided for numeric variables:
#'
#'   mean = the mean of y
#'
#'   mode = the mode AKA most common value of y
#'
#'   sd = standard deviation of y
#'
#'   se = standard error of the mean of y
#'
#'   p0 = the minimum AKA the 0th percentile of y
#'
#'   p25 = the 25th percentile of y
#'
#'   p50 = the median AKA 50th percentile of y
#'
#'   p75 = the 75th percentile of y
#'
#'   p100 = the maximum AKA the 100th percentile of y
#'
#'   IQR = the interquartile range of y (75th percentile - 25th percentile)
#'
#'   hist = histogram of y (interactive mode only)
#'
#'   box = boxplot of y (interactive mode only)
#'
#'   For non-numeric variables, the top n (default = 5) most common unique
#'   values and their frequencies are also returned, e.g. Val_1 = most common
#'   value, Ct_1 = # of occurences of the most common value, etc.
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gmail.com}
#'
#' @examples
#' data(gapminder, package = "gapminder") #load the gapminder life expectancy data
#'
#' #without piping:
#' describe(data = gapminder, y = lifeExp) #no grouping variables
#' describe(gapminder, lifeExp, continent) #one grouping variable
#' describe(gapminder, lifeExp, continent, country) #two grouping variables
#'
#' #with piping:
#' library(magrittr)
#'
#' gapminder %>% describe(lifeExp) #no grouping variables
#' gapminder %>% describe(lifeExp, continent) #one grouping variables
#' gapminder %>% describe(lifeExp, continent, country) #two grouping variables
#'
#' #interactive version
#' \dontrun{
#' gapminder %>% describe(lifeExp, continent, interactive = T)
#' }
#'
#'
#' @references
#' Altman, D. G., & Bland, J. M. (2005). Standard deviations and standard
#' errors. Bmj, 331(7521), 903.
#'
#' Bulmer, M. G. (1979). Principles of statistics. Courier Corporation.
#'
#'
#' @seealso \code{\link[base]{mean}}, \code{\link[stats]{sd}}, \code{\link[elucidate]{se}},
#'   \code{\link[stats]{quantile}}, \code{\link[stats]{IQR}},
#'   \code{\link[sparkline]{sparkline}}, \code{\link[DT]{datatable}}
#'
#' @export
describe <- function(data, y, ..., n = "all", interactive = FALSE, paging = TRUE, caption = NULL){
  mode_of_y <- function(y){
    out <- names(sort(table(y), decreasing = T))[1]
    out <- out %>% as.numeric(out) %>% round(3)
    return(out)
  }
  se <- function(y) {
    se <- stats::sd(y, na.rm = T)/sqrt(length(na.omit(y)))
    return(se)
  }
  describe_num <- function(data, y, ..., interactive = FALSE, paging = TRUE, caption = NULL){
    if(interactive == FALSE){
      suppressWarnings(
        description <- data %>%
          dplyr::group_by(...) %>%
          dplyr::summarise(cases = length({{y}}),
                           n = sum(!is.na({{y}})),
                           na = sum(is.na({{y}})),
                           p_na = round(sum(is.na({{y}}))/length({{y}})*100, 2),
                           mode = round(mode_of_y({{y}}), 3),
                           mean = round(mean({{y}}, na.rm = T), 3),
                           sd = round(stats::sd({{y}}, na.rm = T), 3),
                           se = round(se({{y}}), 3),
                           p0 = round(stats::quantile({{y}}, probs = 0, na.rm = T), 3),
                           p25 = round(stats::quantile({{y}}, probs = 0.25, na.rm = T), 3),
                           p50 = round(stats::quantile({{y}}, probs = 0.50, na.rm = T), 3),
                           p75 = round(stats::quantile({{y}}, probs = 0.75, na.rm = T), 3),
                           p100 = round(stats::quantile({{y}}, probs = 1, na.rm = T), 3),
                           IQR = round(stats::IQR({{y}}, na.rm = TRUE), 3)
          )
      )
      return(description)
    } else if(interactive == TRUE){
      suppressWarnings(
        description <- data %>%
          dplyr::group_by(...) %>%
          dplyr::summarise(cases = length({{y}}),
                           n = sum(!is.na({{y}})),
                           na = sum(is.na({{y}})),
                           p_na = round(sum(is.na({{y}}))/length({{y}})*100, 2),
                           mode = round(mode_of_y({{y}}), 3),
                           mean = round(mean({{y}}, na.rm = T), 3),
                           sd = round(stats::sd({{y}}, na.rm = T), 3),
                           se = round(se({{y}}), 3),
                           p0 = round(stats::quantile({{y}}, probs = 0, na.rm = T), 3),
                           p25 = round(stats::quantile({{y}}, probs = 0.25, na.rm = T), 3),
                           p50 = round(stats::quantile({{y}}, probs = 0.50, na.rm = T), 3),
                           p75 = round(stats::quantile({{y}}, probs = 0.75, na.rm = T), 3),
                           p100 = round(stats::quantile({{y}}, probs = 1, na.rm = T), 3),
                           IQR = round(stats::IQR({{y}}, na.rm = TRUE), 3),
                           box = sparkline::spk_chr(
                             {{y}}, type ="box",
                             chartRangeMin = min({{y}}, na.rm = T), chartRangeMax = max({{y}}, na.rm = T)),
                           hist = sparkline::spk_chr(
                             {{y}}, type ="bar",
                             chartRangeMin = min({{y}}, na.rm = T), chartRangeMax = max({{y}}, na.rm = T))
          )
      )
      description <- description %>% DT::datatable(filter = "top", escape = FALSE,
                                                   caption = htmltools::tags$caption(
                                                     style = 'caption-side: top; text-align: left;',
                                                     caption),
                                                   editable = list(target = "cell", disable = list(columns = c(14, 15))),
                                                   class = "display compact",
                                                   extensions = c("AutoFill", "ColReorder", "Buttons",
                                                                  "KeyTable", "RowReorder", "Select", "Scroller"),
                                                   selection = "none",
                                                   options = list(paging = paging,
                                                                  autoWidth = TRUE,
                                                                  columnDefs = list(list(className = 'dt-center', targets = c(14, 15))),
                                                                  autoFill = list(columns = c(1, 2), focus = 'click'),
                                                                  colReorder = TRUE,
                                                                  keys = TRUE,
                                                                  rowReorder = TRUE,
                                                                  select = list(style = 'os',
                                                                                items = 'cell'),
                                                                  dom = 'Bfrtip',
                                                                  buttons = list(I('colvis'), 'copy', 'print',
                                                                                 list(extend = 'collection',
                                                                                      buttons = c('csv', 'excel', 'pdf'),
                                                                                      text = 'Download')),
                                                                  fnDrawCallback = htmlwidgets::JS(
                                                                    'function(){
                                  HTMLWidgets.staticRender();
                                  }'))) %>% sparkline::spk_add_deps()

      return(description)
    }
  }
  c_var_n <- function(data, y, ...){
    out <- data %>%
      dplyr::group_by(...) %>%
      dplyr::summarise(cases = length({{y}}),
                n = sum(!is.na({{y}})),
                na = sum(is.na({{y}})),
                p_na = round(sum(is.na({{y}}))/length({{y}})*100, 2))
    return(out)
  }
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
  y_var <- data %>% dplyr::select({{y}}) %>% purrr::as_vector()
  if(is.numeric(y_var)){
    out <- describe_num(data = data, y = {{y}}, ... = ..., interactive = interactive, caption = caption)
    return(out)
  } else {
    if(missing(...)){
      part1 <- c_var_n(data = data, y = {{y}}, ... = ...) %>% purrr::map_dfc(~as.character(.x))

      part2 <- mcvals(data = data, y = {{y}}, ... = ..., n = n) %>%
        tibble::rownames_to_column(var = "measure") %>%
        tidyr::pivot_wider(names_from = measure, values_from = tidyselect::contains("#")) %>%
        purrr::map_dfc(~as.character(.x))
    } else if(!missing(...)){
      part1 <- c_var_n(data = data, y = {{y}}, ... = ...) %>% purrr::map_dfc(~as.character(.x))

      part2 <- mcvals(data = data, y = {{y}}, ... = ..., n = n) %>%
        tidyr::pivot_wider(names_from = measure, values_from = tidyselect::contains("#")) %>%
        purrr::map_dfc(~as.character(.x))
    }
    p2_col_names <- part2 %>% names

    p2_col_names <- p2_col_names %>% stringr::str_replace_all("^(#)(.)_(val)(ue)$", "Val_\\2")
    p2_col_names <- p2_col_names %>% stringr::str_replace_all("^(#)(.)_(count)$", "Ct_\\2")
    names(part2) <- p2_col_names

    out <- suppressMessages(
      part1 %>% dplyr::bind_cols(part2)
    )
    if(interactive == TRUE){
      out <- out %>% static_to_dynamic(caption = caption)
    }
    return(out)
  }

}
# end of describe ---------------------------------------------------------
