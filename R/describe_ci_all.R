#describe_ci_all####
#' @title
#'
#' Confidence intervals for the mean and/or median of all numeric variables in a
#' data frame
#'
#' @description Extends \code{\link[elucidate]{describe_ci}} by mapping it across all
#'   numeric variables in a data frame or tibble, with or without first
#'   separating it into groups based on any number of grouping variables. Also
#'   pipe-friendly (data is the 1st argument). While other similar functions
#'   exist in other packages (e.g. \code{\link[skimr]{skimr}}), describe_ci_all
#'   provides an easier way to get groupwise confidence intervals and adds
#'   dynamic functionality via the interactive output option that provides
#'   inline boxplots in addition to histograms to facilitate rapid data
#'   exploration. To obtain frequency counts for the unique values or levels of
#'   all non-numeric (& numeric) variables, use \code{\link[elucidate]{mcvals_all}} instead
#'   (depending on whether you want the results sorted by most or least frequent
#'   unique values).
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr case_when
#' @importFrom stats shapiro.test
#' @importFrom stats quantile
#' @importFrom stats IQR
#' @importFrom stats median
#' @importFrom stats na.omit
#' @importFrom stats qnorm
#' @importFrom moments skewness
#' @importFrom moments kurtosis
#' @importFrom boot boot.ci
#' @importFrom simpleboot one.boot
#' @importFrom sparkline spk_chr
#' @importFrom sparkline spk_add_deps
#' @importFrom DT datatable
#' @importFrom htmltools tags
#'
#' @param data A data frame or tibble containing the dependent measure "Y" and
#'   any grouping variables.
#'
#' @param ... Any number of grouping variables (also present in the data source)
#'   to use for subsetting, separated by commas (e.g. \code{group_var1,
#'   group_var2})
#'
#' @param ci_level The confidence level to use for constructing confidence intervals.
#' Default is set to \code{CI_level = 0.95} for 95 percent CIs.
#'
#' @param mode Specify whether you want parametric (normal) CIs, based on the
#'   mean (mode = "p"), non-parametric boostrapped CIs (mode = "np") based on
#'   the median, or both (mode = "both"). For the parametric mode, only the
#'   sample size, mean, normality assessments, skewness, kurtosis, and CIs based
#'   on the normal distribution are provided. For the non-parametric mode, only
#'   sample size info, the median, IQR, and bootstrapped CIs for the median are
#'   provided. This is done to minimize processing time for large datasets so
#'   that you can first check if the data are normally distributed (and whether
#'   or not normal CIs based on the mean are therefore useful) before investing
#'   the extra time it takes to get bootstrapped CIs. For all
#'   results, use mode = "both".
#'
#' @param sw_alpha The alpha-level to use to evaluate the null hypothesis for the
#'   Shaprio-Wilk test. Default is set to \code{alpha = 0.05}.
#'
#' @param replicates The number of bootstrap replicates to use for the BCa CIs
#'   of the median (in non-parametric & combined modes). Default is 2,000, as
#'   recommended by Efron & Tibshirani (1993). For publications, or if you need
#'   more precise estimates, more replications (e.g. >= 5,000) are recommended.
#'   N.B. more replications will of course take longer to run. If you get the
#'   error: "estimated adjustment 'a' is NA" then try again with more
#'   replications.
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
#' @return A vector of summary statistics and distribution test results, split
#'   into group(s):
#'
#'   n = number of complete cases
#'
#'   na = number of missing values
#'
#'   mean = the mean of y
#'
#'   sd = standard deviation of y
#'
#'   mean_LB = lower bound of the confidence interval for the mean (assuming
#'   normality)
#'
#'   mean_UB = upper bound of the confidence interval for the mean (assuming
#'   normality)
#'
#'   p50 = the median AKA 50th percentile of y
#'
#'   IQR = the interquartile range of y = 75th percentile - 25th percentile
#'
#'   p50_LB = lower bound of the boostrapped confidence interval for the median
#'   (no distributional assumptions)
#'
#'   p50_UB = upper bound of the bootsrapped confidence interval for the median
#'   (no distributional assumptions)
#'
#'   skew = skewness of the distribution of y.
#'
#'   kurt = kurtosis of the distribution of y.
#'
#'   SW_W = the Shaprio-Wilk test statistic "W" (see
#'   \code{\link[stats]{shapiro.test}}) comparing the distribution of y to a
#'   normal distribution. The SW test requires an n between 3-3,000 to produce
#'   informative/useful results and will consequently return "NA" if the sample
#'   size falls outside of this range. If an n > 3,000 then normality is best
#'   evaluated using graphical methods, e.g. the box plots and histograms
#'   provided in the interactive mode (or a qqplot).
#'
#'   SW_p = the Shaprio-Wilk test p-value. Returns NA if n < 3 or > 3,000.
#'
#'   normal = whether or not the normality assumption is valid based on the
#'   obtained p-value and chosen alpha level (default is 0.05). If the data are
#'   non-normally distributed use the percentile(median)-based non-parametric
#'   statistics, not the parametric ones. Requires n = 3-3,000.
#'
#'   hist = histogram of y (interactive mode only)
#'
#'   box = boxplot of y (interactive mode only)
#'
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gmail.com}
#'
#' @examples
#' library(gapminder) #load the gapminder dataset
#'
#' #without piping:
#' describe_ci_all(data = gapminder) #no grouping variables
#'
#' describe_ci_all(gapminder, continent) #one grouping variable
#'
#' describe_ci_all(gapminder, continent, country) #two grouping variables
#'
#' \dontrun{
#' #with piping:
#' library(magrittr)
#'
#' gapminder %>% describe_ci_all() #no grouping variables
#'
#' gapminder %>% describe_ci_all(continent) #one grouping variable
#'
#' gapminder %>% describe_ci_all(continent, country) #two grouping variables
#'
#' #arrange the output by a grouping factor instead of measurement variables using dplyr::arrange
#'
#' gapminder %>% describe_ci_all(continent) %>% dplyr::arrange(continent)
#'
#' #adjust the alpha level for the Shaprio-Wilk test
#' gapminder %>% describe_ci_all(SW_alpha = 0.1)
#' gapminder %>% describe_ci_all(SW_alpha = 0.01)
#'
#' #adjust the confidence level for confidence intervals
#' gapminder %>% describe_ci_all(ci_level = 0.90)
#'
#' #adjusting the confidence level affects both mean and median-derived ci_alls
#' gapminder %>% describe_ci_all(ci_level = 0.8, mode = "both")
#'
#' #obtain non-parametric ci_alls based on the median only
#' gapminder %>% describe_ci_all(mode = "np")
#' gapminder %>% describe_ci_all(country, mode = "np")
#'
#' #adjust the number of replicates to use in constructing the BCa ci_alls for the
#' #confidence interval of the median (note that this will affect run time)
#' gapminder %>% describe_ci_all(mode = "np", replicates = 5000)
#'
#' #set interactive = TRUE or interactive = T for the interactive output instead of a tibble
#' gapminder %>% describe_ci_all(continent, interactive = T)
#' }
#'
#' @references
#' Altman, D. G., & Bland, J. M. (2005). Standard deviations and standard
#' errors. Bmj, 331(7521), 903.
#'
#' Efron, B. (1987). Better bootstrap confidence intervals. Journal of the
#' American statistical Association, 82(397), 171-185.
#'
#' Efron, B., & Tibshirani, R. J. (1993). An introduction to the bootstrap. New
#' York: Chapman & Hall.
#'
#' Shapiro, S. S. and Wilk, M. B. (1965). "An analysis of variance test for
#' normality (complete samples)", Biometrika, 52, 3 and 4, pages 591-611.
#'
#' @seealso \code{\link[elucidate]{describe_ci}}
#'
#' @export
describe_ci_all <- function(data, ..., ci_level = 0.95, mode = "p", sw_alpha = 0.05,
                            replicates = 2000, interactive = FALSE, paging = TRUE, caption = NULL){

  describe_ci_spk <- function(data, y, ci_level = 0.95, mode = "p", sw_alpha = 0.05,
                              replicates = 2000){
    if (mode == "p") {
      suppressWarnings(
        description <- data %>%
          dplyr::summarise(n = sum(!is.na({{y}})),
                           na = sum(is.na({{y}})),
                           mean = round(mean({{y}}, na.rm = T), 3),
                           sd = round(stats::sd({{y}}, na.rm = T), 3),
                           mean_LB = round(mean({{y}}, na.rm = T) - (abs(qnorm((1-ci_level)/2))*se({{y}})), 3),
                           mean_UB = round(mean({{y}}, na.rm = T) + (abs(qnorm((1-ci_level)/2))*se({{y}})), 3),
                           skew = round(moments::skewness({{y}}, na.rm = T), 3),
                           kurt = round(moments::kurtosis({{y}}, na.rm = T), 3),
                           SW_W = ifelse(sum(!is.na({{y}})) >= 3 & sum(!is.na({{y}})) <= 3000,
                                         as.character(round(shapiro.test({{y}})$statistic, 3)), "NA"),
                           SW_p = ifelse(sum(!is.na({{y}})) >= 3 & sum(!is.na({{y}})) <= 3000,
                                         as.character(round(shapiro.test({{y}})$p.value, 3)), "NA"),
                           box = sparkline::spk_chr(
                             {{y}}, type ="box",
                             chartRangeMin = min({{y}}, na.rm = T), chartRangeMax = max({{y}}, na.rm = T)),
                           hist = sparkline::spk_chr(
                             {{y}}, type ="bar",
                             chartRangeMin = min({{y}}, na.rm = T), chartRangeMax = max({{y}}, na.rm = T))
          ) %>%
          dplyr::mutate(normal = dplyr::case_when(SW_p != "NA" & SW_p >= sw_alpha ~ "yes",
                                                  SW_p != "NA" & SW_p < sw_alpha ~ "no",
                                                  SW_p == "NA" ~ "NA"),
                        SW_W = as.numeric(SW_W),
                        SW_p = as.numeric(SW_p)
          ) %>% dplyr::select(1:SW_p, normal, box, hist)
      )
      return(description)
    } else if (mode == "np") {
      suppressWarnings(
        description <- data %>%
          dplyr::summarise(n = sum(!is.na({{y}})),
                           na = sum(is.na({{y}})),
                           p50 = round(stats::quantile({{y}}, probs = 0.50, na.rm = TRUE), 3),
                           IQR = round(stats::IQR({{y}}, na.rm = TRUE), 3),
                           p50_LB = boot::boot.ci(boot.out = simpleboot::one.boot({{y}}, median,
                                                                                  R = replicates, na.rm = TRUE),
                                                  conf = ci_level, type = "bca")$bca[4],
                           p50_LB = round(p50_LB, 3),
                           p50_UB = boot::boot.ci(boot.out = simpleboot::one.boot({{y}}, median,
                                                                                  R = replicates, na.rm = TRUE),
                                                  conf = ci_level, type = "bca")$bca[5],
                           p50_UB = round(p50_UB, 3),
                           box = sparkline::spk_chr(
                             {{y}}, type ="box",
                             chartRangeMin = min({{y}}, na.rm = T), chartRangeMax = max({{y}}, na.rm = T)),
                           hist = sparkline::spk_chr(
                             {{y}}, type ="bar",
                             chartRangeMin = min({{y}}, na.rm = T), chartRangeMax = max({{y}}, na.rm = T))
          )
      )
      return(description)
    } else if (mode == "both") {
      suppressWarnings(
        description <- data %>%
          dplyr::summarise(cases = length({{y}}),
                           n = sum(!is.na({{y}})),
                           na = sum(is.na({{y}})),
                           mean = round(mean({{y}}, na.rm = T), 3),
                           sd = round(stats::sd({{y}}, na.rm = T), 3),
                           mean_LB = round(mean({{y}}, na.rm = T) - (abs(qnorm((1-ci_level)/2))*se({{y}})), 3),
                           mean_UB = round(mean({{y}}, na.rm = T) + (abs(qnorm((1-ci_level)/2))*se({{y}})), 3),
                           p50 = round(stats::quantile({{y}}, probs = 0.50, na.rm = TRUE), 3),
                           IQR = round(stats::IQR({{y}}, na.rm = TRUE), 3),
                           p50_LB = boot::boot.ci(boot.out = simpleboot::one.boot({{y}}, median,
                                                                                  R = replicates, na.rm = TRUE),
                                                  conf = ci_level, type = "bca")$bca[4],
                           p50_LB = round(p50_LB, 3),
                           p50_UB = boot::boot.ci(boot.out = simpleboot::one.boot({{y}}, median,
                                                                                  R = replicates, na.rm = TRUE),
                                                  conf = ci_level, type = "bca")$bca[5],
                           p50_UB = round(p50_UB, 3),
                           skew = round(moments::skewness({{y}}, na.rm = T), 3),
                           kurt = round(moments::kurtosis({{y}}, na.rm = T), 3),
                           SW_W = ifelse(sum(!is.na({{y}})) >= 3 & sum(!is.na({{y}})) <= 3000,
                                         as.character(round(shapiro.test({{y}})$statistic, 3)), "NA"),
                           SW_p = ifelse(sum(!is.na({{y}})) >= 3 & sum(!is.na({{y}})) <= 3000,
                                         as.character(round(shapiro.test({{y}})$p.value, 3)), "NA"),
                           box = sparkline::spk_chr(
                             {{y}}, type ="box",
                             chartRangeMin = min({{y}}, na.rm = T), chartRangeMax = max({{y}}, na.rm = T)),
                           hist = sparkline::spk_chr(
                             {{y}}, type ="bar",
                             chartRangeMin = min({{y}}, na.rm = T), chartRangeMax = max({{y}}, na.rm = T))
          ) %>%
          dplyr::mutate(normal = dplyr::case_when(SW_p != "NA" & SW_p >= sw_alpha ~ "yes",
                                                  SW_p != "NA" & SW_p < sw_alpha ~ "no",
                                                  SW_p == "NA" ~ "NA"),
                        SW_W = as.numeric(SW_W),
                        SW_p = as.numeric(SW_p)
          ) %>% dplyr::select(1:SW_p, normal, box, hist)
      )
      return(description)
    }
  }

  map_describe_ci <- function(data, ci_level = 0.95, mode = "p", sw_alpha = 0.05,
                              skew_alternative = "two.sided", kurt_alternative = "two.sided",
                              replicates = 2000) {
    num_data <- data %>%
      dplyr::select_if(is.numeric)
    n_var_names <- num_data %>% names

    res <- num_data %>%
      purrr::map_dfr(~describe_ci(data, y = ., ci_level = ci_level, mode = mode, sw_alpha = sw_alpha,
                                  replicates = replicates)) %>%
      dplyr::mutate(variable = n_var_names) %>%
      dplyr::select(variable, 1:ncol(.))
    return(res)
  }

  map_describe_ci_spk <- function(data, ci_level = 0.95, mode = "p", sw_alpha = 0.05,
                                  skew_alternative = "two.sided", kurt_alternative = "two.sided",
                                  replicates = 2000) {
    num_data <- data %>%
      dplyr::select_if(is.numeric)
    n_var_names <- num_data %>% names
    suppressWarnings(
      res <- num_data %>%
        purrr::map_dfr(~describe_ci_spk(data, y = ., ci_level = ci_level, mode = mode, sw_alpha = sw_alpha,
                                        replicates = replicates)) %>%
        dplyr::mutate(variable = n_var_names) %>%
        dplyr::select(variable, 1:ncol(.))
    )
    return(res)
  }

  if(interactive == FALSE){
    if(missing(...)){
      description <- data %>% map_describe_ci(., ci_level = ci_level, mode = mode, sw_alpha = sw_alpha,
                                              replicates = replicates)
      return(description)
    } else {
      suppressWarnings(
        description <- data %>%
          dplyr::group_by(...) %>%
          dplyr::group_modify(~map_describe_ci(.x, ci_level = ci_level, mode = mode, sw_alpha = sw_alpha,
                                               replicates = replicates)) %>%
          dplyr::arrange(variable) %>% dplyr::ungroup()
      )
      return(description)
    }
    #convert to dynamic
  } else if(interactive == TRUE){
    if(missing(...)){
      description <- data %>% map_describe_ci_spk(., ci_level = ci_level, mode = mode, sw_alpha = sw_alpha,
                                                  replicates = replicates)

      description <- description %>% DT::datatable(filter = "top", escape = FALSE,
                                                   caption = htmltools::tags$caption(
                                                     style = 'caption-side: top; text-align: left;',
                                                     caption),
                                                   editable = list(target = "cell",
                                                                   disable = list(columns = c("box", "hist"))),
                                                   class = "display compact",
                                                   extensions = c("AutoFill", "ColReorder", "Buttons",
                                                                  "KeyTable", "RowReorder", "Select", "Scroller"),
                                                   selection = "none",
                                                   options = list(paging = paging,
                                                                  autoWidth = TRUE,
                                                                  columnDefs = list(list(className = 'dt-center',
                                                                                         targets = c("box", "hist"))),
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
          dplyr::group_modify(~map_describe_ci_spk(.x, ci_level = ci_level, mode = mode, sw_alpha = sw_alpha,
                                                   replicates = replicates)) %>%
          dplyr::arrange(variable) %>% dplyr::ungroup()
      )
      description <- description %>% DT::datatable(filter = "top", escape = FALSE,
                                                   caption = htmltools::tags$caption(
                                                     style = 'caption-side: top; text-align: left;',
                                                     caption),
                                                   editable = list(target = "cell", disable = list(columns = c("box", "hist"))),
                                                   class = "display compact",
                                                   extensions = c("AutoFill", "ColReorder", "Buttons",
                                                                  "KeyTable", "RowReorder", "Select", "Scroller"),
                                                   selection = "none",
                                                   options = list(paging = paging,
                                                                  autoWidth = TRUE,
                                                                  columnDefs = list(list(className = 'dt-center',
                                                                                         targets = c("box", "hist"))),
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
}

# end of describe_ci_all -----------------------------------------------------
