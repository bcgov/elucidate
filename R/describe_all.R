#describe_all####
#' @title
#'
#' Obtain a descriptive summary of all variables in a data frame or tibble.
#'
#' @description Extends \code{\link[elucidate]{describe}} by looping/mapping it (see
#'   \code{\link[purrr]{map}} for more info.) across all variables in a data
#'   frame or tibble, with or without first separating it into groups based on
#'   any number of grouping variables. Also pipe-friendly (data is the 1st
#'   argument). While other similar functions exist in other packages (e.g.
#'   \code{\link[skimr]{skimr}}), describe_all provides an expanded set of summary
#'   statistics and adds dynamic functionality via the interactive output option
#'   that provides inline boxplots in addition to histograms. To obtain
#'   confidence intervals and skewness, kurtosis, and normality metrics for the
#'   sample mean and median of each numeric variable use
#'   \code{\link[elucidate]{describe_ci_all}} instead. To obtain frequency counts for the
#'   unique values/levels of all variables, use \code{\link[elucidate]{mcvals_all}}
#'   instead. For exploratory or publication/presentation purposes, further
#'   customization is made easier using the interactive mode, the output of
#'   which can easily be copied or exported in various formats (e.g. csv, excel,
#'   pdf, image file).
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr group_modify
#' @importFrom dplyr select_if
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr summarize
#' @importFrom dplyr left_join
#' @importFrom dplyr ungroup
#' @importFrom purrr negate
#' @importFrom purrr map_dfc
#' @importFrom purrr map_dfr
#' @importFrom tidyselect contains
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_split
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_c
#' @importFrom stats sd
#' @importFrom stats quantile
#' @importFrom stats IQR
#' @importFrom stats na.omit
#' @importFrom sparkline spk_chr
#' @importFrom sparkline spk_add_deps
#' @importFrom DT datatable
#' @importFrom htmltools tags
#' @importFrom htmlwidgets JS
#'
#' @param data A data frame or tibble containing the at least one numerical
#'   variable and any grouping variables.
#'
#' @param ... Any number of grouping variables (also present in the data source)
#'   to use for subsetting, separated by commas (e.g. \code{group_var1,
#'   group_var2})
#'
#' @param class Obtain a descriptive summary of numeric variables (class =
#'   "numeric"), non-numeric variables (class = "other"), or all variables
#'   (class = "all"; the default). If all is requested, the output will be a
#'   list.
#'
#' @param interactive Determines whether a data frame or an interactive html
#'   table with in line box plots and histograms produced by
#'   \code{\link[sparkline]{sparkline}} and \code{\link[DT]{datatable}} are
#'   returned with options for editing and exporting. Since
#'   \code{\link[DT]{datatable}} requires 2-dimensional input, interactive
#'   tables are only generated if class = "numeric", class = "other", or if
#'   class = "all" but all variables in the dataset are either "numeric" or
#'   "non-numeric", but not a mixture.
#'
#' @param n The number of unique values you want frequency counts of for the
#'   non-numeric variables in the data object (default = 5). Ignored if class =
#'   "numeric". If you want all unique values or the unique value counts for a
#'   numeric variable, use code{\link[elucidate]{mcvals_all}} instead.
#'
#' @param paging Set to F to disable paging of an interactive table (interactive
#'   mode only).
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
#' library(gapminder) #load the gapminder dataset
#'
#' #without piping:
#' describe_all(data = gapminder) #no grouping variables
#'
#' describe_all(gapminder, continent) #one grouping variable
#'
#' \dontrun{
#' #with piping:
#' library(magrittr)
#'
#' gapminder %>% describe_all() #no grouping variables
#'
#' gapminder %>% describe_all(continent) #one grouping variable
#'
#' gapminder %>% describe_all(continent, country, class = "numeric") #two grouping variables
#'
#' #arrange the output by a grouping factor instead of measurement variables using dplyr::arrange
#'
#' gapminder %>% describe_all(continent) %>% dplyr::arrange(continent)
#'
#' #filter the output using dplyr::filter to more easily compare groups on a subset of measures
#'
#' gapminder %>%
#' describe_all(continent) %>%
#' dplyr::filter(variable == "lifeExp" | variable == "pop") #examine just life expectancy and populaton
#'
#' #interactive version
#'
#' gapminder %>% describe_all(continent, class = "numeric",interactive = T)
#' }
#'
#' @seealso \code{\link[elucidate]{describe}}
#'
#' @export
describe_all <- function(data, ..., class = "all", interactive = F, n = 5,  paging = T, caption = NULL) {
  mode_of_y <- function(y){
    out <- names(sort(table(y), decreasing = T))[1]
    out <- out %>% as.numeric(out) %>% round(3)
    return(out)
  }
  se <- function(y) {
    se <- stats::sd(y, na.rm = T)/sqrt(length(na.omit(y)))
    return(se)
  }
  describe_num <- function(data, y){
    suppressWarnings(
      out <- data %>%
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
    return(out)
  }

  describe_spk <- function(data, y) {
    suppressWarnings(
      out <- data %>%
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
    return(out)
  }

  map_describe_num <- function(data) {
    num_data <- data %>%
      dplyr::select_if(is.numeric)
    n_var_names <- num_data %>% names

    res <- num_data %>%
      purrr::map_dfr(~describe_num(data, y = .)) %>%
      dplyr::mutate(variable = n_var_names) %>%
      dplyr::select(variable, 1:ncol(.))
    return(res)
  }

  map_describe_spk <- function(data) {
    num_data <- data %>%
      dplyr::select_if(is.numeric)
    n_var_names <- num_data %>% names
    suppressWarnings(
      res <- num_data %>%
        purrr::map_dfr(~describe_spk(data, y = .)) %>%
        dplyr::mutate(variable = n_var_names) %>%
        dplyr::select(variable, 1:ncol(.))
    )
    return(res)
  }


  map_describe_chr <- function(data, n = 5){
    data <- data %>% dplyr::select_if(purrr::negate(is.numeric))
    cols <- data %>% dplyr::select_if(purrr::negate(is.numeric)) %>% ncol
    cols
    if(cols == 0){
      stop("Data object contains no non-numeric variables, use class = \"numeric\" instead")
    }
    map_describe_chr_a <- function(data) {
      describe_chr_a <- function(data, y, n = 5){
        c_var_n <- function(data, y){
          out <- data %>%
            dplyr::summarise(cases = length({{y}}),
                             n = sum(!is.na({{y}})),
                             na = sum(is.na({{y}})),
                             p_na = round(sum(is.na({{y}}))/length({{y}})*100, 2))
          return(out)
        }
        part1 <- c_var_n(data = data, y = {{y}}) %>% purrr::map_dfr(~as.character(.x))
        return(part1)
      }

      c_names <- data %>% names

      res <- data %>%
        purrr::map_dfr(~describe_chr_a(data, y = .)) %>%
        dplyr::mutate(variable = c_names) %>%
        dplyr::select(variable, 1:ncol(.))
      return(res)
    }


    map_describe_chr_b <- function(data, n = 5){
      mcvs <- function(y, n = 5) {
        value <- y %>% table() %>% sort(., decreasing = T) %>% .[1:n] %>% names() %>% as.character()
        count <- y %>% table() %>% sort(., decreasing = T) %>% .[1:n] %>% as.character()

        suppressWarnings(
          mcv_df <- rbind(value, count) %>% as.data.frame
        )
        names(mcv_df) <- stringr::str_replace_all(names(mcv_df), pattern = "V", replacement = "#")

        mcv_df
        return(mcv_df)
      }

      c_names <- names(data)

      suppressWarnings(
        part2 <- data %>% purrr::map_dfr(~mcvs(.x, n = n)) %>%
          dplyr::mutate(measure = rep(c("value", "count"), nrow(.)/2),
                        variable = unlist(stringr::str_split(stringr::str_c(c_names, c_names, sep = "_"), "_"))
          ) %>%
          dplyr::select(variable,
                        measure, 1:(ncol(.)-1)) %>%
          tidyr::pivot_wider(names_from = measure, values_from = tidyselect::contains("#"))
      )
      part2

      p2_col_names <- part2 %>% names

      p2_col_names <- p2_col_names %>% stringr::str_replace_all("^(#)(.)_(val)(ue)$", "Val_\\2")
      p2_col_names <- p2_col_names %>% stringr::str_replace_all("^(#)(..)_(val)(ue)$", "Val_\\2")
      p2_col_names <- p2_col_names %>% stringr::str_replace_all("^(#)(...)_(val)(ue)$", "Val_\\2")
      p2_col_names <- p2_col_names %>% stringr::str_replace_all("^(#)(.)_(count)$", "Ct_\\2")
      p2_col_names <- p2_col_names %>% stringr::str_replace_all("^(#)(..)_(count)$", "Ct_\\2")
      p2_col_names <- p2_col_names %>% stringr::str_replace_all("^(#)(...)_(count)$", "Ct_\\2")
      names(part2) <- p2_col_names
      part2


      chr_data <- data %>% purrr::map_dfc(as.character) %>%
        dplyr::select_if(purrr::negate(is.numeric))
      c_var_names <- chr_data %>% names

      return(part2)
    }


    part1 <- map_describe_chr_a(data)
    part2 <- map_describe_chr_b(data, n = n)
    suppressMessages(
      out <- part1 %>%
        dplyr::left_join(part2)
    )
    return(out)
  }

  if(class == "numeric"){
    if(interactive == FALSE){
      if(missing(...)){
        description <- data %>% map_describe_num()
        return(description)
      } else {
        suppressWarnings(
          description <- data %>%
            dplyr::group_by(...) %>%
            dplyr::group_modify(~map_describe_num(.x)) %>%
            dplyr::arrange(variable) %>% dplyr::ungroup()
        )
        return(description)
      }

    } else if(interactive == TRUE) {
      if(missing(...)){
        description <- data %>% map_describe_spk()

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
      } else {
        suppressWarnings(
          description <- data %>%
            dplyr::group_by(...) %>%
            dplyr::group_modify(~map_describe_spk(.x)) %>%
            dplyr::arrange(variable) %>% dplyr::ungroup()
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
  } else if(class == "other"){
    if(interactive == FALSE){
      if(missing(...)){
        description <- data %>% map_describe_chr(n = n)
        return(description)
      } else {
        suppressWarnings(
          description <- data %>%
            dplyr::group_by(...) %>%
            dplyr::group_modify(~map_describe_chr(.x, n = n)) %>%
            dplyr::arrange(variable) %>% dplyr::ungroup()
        )
        return(description)
      }

    } else if(interactive == TRUE) {
      if(missing(...)){
        description <- data %>% map_describe_chr(n = n)
        description <- description %>% static_to_dynamic(caption = caption)
        return(description)
      } else {
        suppressWarnings(
          description <- data %>%
            dplyr::group_by(...) %>%
            dplyr::group_modify(~map_describe_chr(.x, n = n)) %>%
            dplyr::arrange(variable) %>% dplyr::ungroup()
        )
        description <- description %>% static_to_dynamic(caption = caption)
        return(description)
      }
    }
  } else if(class == "all"){
    chr_cols <- data %>% dplyr::select_if(purrr::negate(is.numeric)) %>% ncol
    num_cols <- data %>% dplyr::select_if(is.numeric) %>% ncol

    if(interactive == FALSE){
      if(missing(...)){
        if(chr_cols == 0 & num_cols != 0){
          description <- data %>% map_describe_num()
          return(description)
        } else if(chr_cols != 0 & num_cols == 0){
          description <- data %>% map_describe_chr(n = n)
          return(description)
        } else {
          numeric_summary <- data %>% map_describe_num()
          non_numeric_summary <- data %>% map_describe_chr(n = n)
          description <- list("numeric" = numeric_summary,
                              "non_numeric" = non_numeric_summary)
          return(description)
        }
      } else {
        if(chr_cols == 0 & num_cols != 0){
          suppressWarnings(
            description <- data %>%
              dplyr::group_by(...) %>%
              dplyr::group_modify(~map_describe_num(.x)) %>%
              dplyr::arrange(variable) %>% dplyr::ungroup()
          )
          return(description)
        } else if(chr_cols != 0 & num_cols == 0){
          suppressWarnings(
            description <- data %>%
              dplyr::group_by(...) %>%
              dplyr::group_modify(~map_describe_chr(.x, n = n)) %>%
              dplyr::arrange(variable) %>% dplyr::ungroup()
          )
          return(description)
        } else {
          suppressWarnings(
            numeric_summary <- data %>%
              dplyr::group_by(...) %>%
              dplyr::group_modify(~map_describe_num(.x)) %>%
              dplyr::arrange(variable) %>% dplyr::ungroup()
          )
          suppressWarnings(
            non_numeric_summary <- data %>%
              dplyr::group_by(...) %>%
              dplyr::group_modify(~map_describe_chr(.x, n = n)) %>%
              dplyr::arrange(variable) %>% dplyr::ungroup()
          )
          suppressWarnings(
            description <- list("numeric" = numeric_summary,
                                "non_numeric" = non_numeric_summary)
          )
          return(description)
        }
      }
    }
  }
}

# end of describe_all -----------------------------------------------------
