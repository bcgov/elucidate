# Copyright 2019 Province of British Columbia
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


# describe_ci -------------------------------------------------------------
#' @title
#' Obtain confidence intervals for a summary statistic of a numeric vector.
#'
#' @description describe_ci extends the functionality of \code{\link{stat_ci}}
#'   by allowing you to obtain confidence intervals for a summary statistic
#'   split by any number of grouping variables. Like \code{\link{stat_ci}} you
#'   can specify any function that operates on a numeric variable and returns a
#'   single value (e.g. mean, median, sd, se, etc.), but unlike the other
#'   elucidate package *_ci function this one always returns either a data.table
#'   or tibble (instead of a named vector). Calculations of confidence intervals
#'   for the mean are obtained based on refernce to the theoretical
#'   normal/gaussian distribution for speed, otherwise bootstrapping is used,
#'   with options for multicore machines to use parallel processing, which can
#'   speed things up quite a bit for larger samples. \code{\link{stat_ci}} may
#'   be useful instead of describe_ci if you need to pass additional arguments
#'   to the chosen summary statistic function (which is what that function uses
#'   the ... argument for). To get confidence intervals for all numeric
#'   variables in a data frame, use \code{\link{describe_ci_all}} instead.
#'
#' @importFrom data.table data.table
#' @importFrom data.table as.data.table
#' @importFrom data.table dcast
#' @importFrom stats na.omit
#' @importFrom tibble as_tibble
#'
#' @param data Either a numeric vector or a data frame or tibble containing the
#'   numeric vector ("y") to be described and any grouping variables ("...").
#'
#' @param y If the data object is a data.frame, this is the variable for which
#'   you wish to obtain a descriptive summary
#'
#' @param ... If the data object is a data.frame, this special argument accepts
#'   any number of unquoted grouping variable names (also present in the data source)
#'   to use for subsetting, separated by commas (e.g. \code{group_var1,
#'   group_var2})
#'
#' @param stat the unquoted name (e.g. mean, not "mean") of a summary statistic
#'   function to calculate confidence intervals for. Only functions which return
#'   a single value and operate on numeric variables are currently supported.
#'
#' @param replicates The number of bootstrap replicates to use to construct
#'   confidence intervals for statistics other than the sample mean. Default is
#'   2,000, as recommended by Efron & Tibshirani (1993). For publications, or if
#'   you need more precise estimates, more replications (e.g. >= 5,000) are
#'   recommended. N.B. more replications will of course take longer to run. If
#'   you get the error: "estimated adjustment 'a' is NA" when ci_type is set to
#'   "bca" then try again with more replications.
#'
#' @param ci_level The confidence level to use for constructing confidence
#'   intervals. Default is set to \code{ci_level = 0.95} for 95 percent CIs.
#'
#' @param ci_type The type of confidence intervals to calculate from the
#'   bootstrap samples. Most of the options available in the underlying boot.ci
#'   function are implemented (except for studentized intervals): "norm" for an
#'   approximation based on the normal distribution, "perc" for percentile,
#'   "basic" for basic, and "bca" for bias-corrected and accelerated. Percentile
#'   intervals are the default since these are typically sufficient when working
#'   with large data sets (e.g. >= 100,000 rows of data) and are faster to
#'   calculate than BCa intervals. However, BCa intervals (the default for the
#'   more primitive \code{\link{stat_ci}} function) tend to provide the most
#'   accurate/least-biased results (Efron, 1987), particularly for small-medium
#'   sized samples, at the obvious cost of requiring more time to calculate. See
#'   \code{\link[boot]{boot.ci}} for details.
#'
#' @param parallel set to TRUE if you want to use multiple cores or FALSE if you
#'   don't (the default). Note that there is some processing overhead involved
#'   when operating in parallel so speed gains may not be very noticable for
#'   smaller samples (and may even take longer than sequential processing). Due
#'   to the nature of the underlying parallelization architecture, performance
#'   gains will likely be greater on non-Windows machines that can use the
#'   "multicore" implementation instead of "snow". For obvious reasons this
#'   option only works on machines with more than 1 logical processing core.
#'
#' @param cores If parallel is set to TRUE, this determines the number of cores
#'   to use. To see how many cores are available on your machine, use
#'   parallel::detectCores(). If cores is unspecified the number of available
#'   cores - 1 will be used by default.
#'
#' @param na.rm should missing values be removed before attempting to calculate
#'   the chosen statistic and confidence intervals? Default is TRUE.
#'
#' @param output "tibble" for tibble or "dt" for data.table. Tibble is used as
#'   the default output to facilitate subsequent use/modification of the output
#'   with the tidyverse collection of packages.
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gov.bc.ca}
#'
#' @examples
#'
#' #using a single core (sequential processing)
#' describe_ci(pdata[1:1000, ], y1, stat = median) #bootstrapped CIs for the median
#' describe_ci(pdata, y1, stat = mean) #the default
#' \dontrun{
#' describe_ci(pdata, y1, high_low, stat = mean) #split by a grouping variable
#' }
#' \dontrun{
#' #using multiple cores (parallel processing)
#' describe_ci(pdata[1:1000, ], y1, stat = sd, parallel = TRUE, cores = 2)
#' }
#'
#' @references
#' Efron, B. (1987). Better bootstrap confidence intervals. Journal of the
#' American statistical Association, 82(397), 171-185.
#'
#' Efron, B., & Tibshirani, R. J. (1993). An introduction to the bootstrap. New
#' York: Chapman & Hall.
#'
#' @seealso \code{\link[boot]{boot}}, \code{\link[boot]{boot.ci}},
#'   \code{\link{mean_ci}}, \code{\link{median_ci}}, \code{\link{stat_ci}},
#'   \code{\link{describe_ci_all}}
#'
#' @export
describe_ci <- function(data, y = NULL, ..., stat = mean, replicates = 2000,
                        ci_level = 0.95, ci_type = "perc", parallel = FALSE, cores = NULL,
                        na.rm = TRUE, output = "tibble") {

  st <- deparse(substitute(stat))
  y <- deparse(substitute(y))
  dt <- data.table::as.data.table(data)

  if(is.numeric(data)){
    if(st == "mean") {
      description <- dt[[1]] %>% mean_ci(replicates = replicates, ci_type = ci_type,
                                         ci_level = ci_level, parallel = parallel, cores = cores, na.rm = na.rm)
      description <- data.table::data.table(description[1], description[2], description[3])
      names(description) <- c("lower", st, "upper")
    } else {
      description <- dt[[1]] %>%
        stat_ci(stat = stat, replicates = replicates, ci_type = ci_type,
                ci_level = ci_level, parallel = parallel, cores = cores, na.rm = na.rm)
      description <- data.table::data.table(description[1], description[2], description[3])
      names(description) <- c("lower", st, "upper")
    }
  } else if(!is.data.frame(data)) {
    stop("data must either be a numeric vector or data frame")
  } else if(is.data.frame(data)){
    if(!is.numeric(dt[[y]])){
      stop("y must be a numeric vector or column of a data frame")
    }
    if(!missing(...)){
      g <- gsub(" ", "", unlist(strsplit(deparse(substitute(list(...))), "[(,)]")))[-1]

      if(st == "mean") {
        description <- dt[,
                          .(measure = c("lower", st, "upper"),
                            value = mean_ci(get(y), replicates = replicates, ci_type = ci_type,
                                            ci_level = ci_level, parallel = parallel, cores = cores, na.rm = na.rm)),
                          by = eval(g)] %>% stats::na.omit() %>%
          data.table::dcast(formula = ... ~ measure, value.var = "value")
      } else {
        description <- dt[,
                          .(measure = c("lower", st, "upper"),
                            value = stat_ci(get(y), stat = stat, replicates = replicates, ci_type = ci_type,
                                            ci_level = ci_level, parallel = parallel, cores = cores, na.rm = na.rm)),
                          by = eval(g)] %>% stats::na.omit() %>%
          data.table::dcast(formula = ... ~ measure, value.var = "value")
      }
    } else {
      if(st == "mean") {
        description <- dt[[y]] %>% mean_ci(replicates = replicates, ci_type = ci_type,
                                           ci_level = ci_level, parallel = parallel, cores = cores, na.rm = na.rm)
        description <- data.table::data.table(description[1], description[2], description[3])
        names(description) <- c("lower", st, "upper")
      } else {
        description <- dt[[y]] %>%
          stat_ci(stat = stat, replicates = replicates, ci_type = ci_type,
                  ci_level = ci_level, parallel = parallel, cores = cores, na.rm = na.rm)
        description <- data.table::data.table(description[1], description[2], description[3])
        names(description) <- c("lower", st, "upper")
      }
    }
  }
  if(output == "tibble") {
    description <- tibble::as_tibble(description)
  }
  return(description)
}

# describe_ci_all ---------------------------------------------------------
#' @title
#' Confidence intervals for a summary statistic of numeric variables in a data
#' frame.
#'
#' @description describe_ci_all extends the functionality of
#'   \code{\link{describe_ci}} by using the power of \code{\link[purrr]{map}} to
#'   enable you to obtain confidence intervals for a chosen summary statistic
#'   for each numeric variable in a data frame split by any number of grouping
#'   variables. Like \code{\link{stat_ci}} you can specify any function that
#'   operates on a numeric variable and returns a single value (e.g. mean,
#'   median, sd, se, etc.). Calculations of confidence intervals for the mean
#'   are obtained based on refernce to the theoretical normal/gaussian
#'   distribution for speed, otherwise bootstrapping is used, with options for
#'   multicore machines to use parallel processing which can speed things up
#'   quite a bit for larger samples. \code{\link{stat_ci}} may be useful instead
#'   of describe_ci if you need to pass additional arguments to the chosen
#'   summary statistic function (which is what that function uses the ...
#'   argument for). To get confidence intervals for a single numeric variable in
#'   a data frame, use \code{\link{describe_ci}} instead.
#'
#' @importFrom dplyr select
#' @importFrom dplyr select_if
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @importFrom purrr map
#' @importFrom tidyselect everything
#' @importFrom data.table data.table
#' @importFrom data.table as.data.table
#' @importFrom data.table rbindlist
#' @importFrom tibble as_tibble
#'
#' @param data A data frame or tibble containing the numeric vectors to be
#'   described and any grouping variables ("...").
#'
#' @param ... If the data object is a data.frame, this special argument accepts
#'   any number of unquoted grouping variable names (also present in the data source)
#'   to use for subsetting, separated by commas (e.g. \code{group_var1,
#'   group_var2})
#'
#' @param stat the unquoted name (e.g. mean, not "mean") of a summary statistic
#'   function to calculate confidence intervals for. Only functions which return
#'   a single value and operate on numeric variables are currently supported.
#'
#' @param replicates The number of bootstrap replicates to use to construct
#'   confidence intervals for statistics other than the sample mean. Default is
#'   2,000, as recommended by Efron & Tibshirani (1993). For publications, or if
#'   you need more precise estimates, more replications (e.g. >= 5,000) are
#'   recommended. N.B. more replications will of course take longer to run. If
#'   you get the error: "estimated adjustment 'a' is NA" when ci_type is set to
#'   "bca" then try again with more replications.
#'
#' @param ci_level The confidence level to use for constructing confidence
#'   intervals. Default is set to \code{ci_level = 0.95} for 95 percent CIs.
#'
#' @param ci_type The type of confidence intervals to calculate from the
#'   bootstrap samples. Most of the options available in the underlying boot.ci
#'   function are implemented (except for studentized intervals): "norm" for an
#'   approximation based on the normal distribution, "perc" for percentile,
#'   "basic" for basic, and "bca" for bias-corrected and accelerated. Percentile
#'   intervals are the default since these are typically sufficient when working
#'   with large data sets (e.g. >= 100,000 rows of data) and are faster to
#'   calculate than BCa intervals. However, BCa intervals (the default for the
#'   more primitive \code{\link{stat_ci}} function) tend to provide the most
#'   accurate/least-biased results (Efron, 1987), particularly for small-medium
#'   sized samples, at the obvious cost of requiring more time to calculate. See
#'   \code{\link[boot]{boot.ci}} for details.
#'
#' @param parallel set to TRUE if you want to use multiple cores or FALSE if you
#'   don't (the default). Note that there is some processing overhead involved
#'   when operating in parallel so speed gains may not be very noticable for
#'   smaller samples (and may even take longer than sequential processing). Due
#'   to the nature of the underlying parallelization architecture, performance
#'   gains will likely be greater on non-Windows machines that can use the
#'   "multicore" implementation instead of "snow". For obvious reasons this
#'   option only works on machines with more than 1 logical processing core.
#'
#' @param cores If parallel is set to TRUE, this determines the number of cores
#'   to use. To see how many cores are available on your machine, use
#'   parallel::detectCores(). If cores is unspecified the number of available
#'   cores - 1 will be used by default.
#'
#' @param na.rm should missing values be removed before attempting to calculate
#'   the chosen statistic and confidence intervals? Default is TRUE.
#'
#' @param output "tibble" for tibble or "dt" for data.table. Tibble is used as
#'   the default output to facilitate subsequent use/modification of the output
#'   with the tidyverse collection of packages.
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gov.bc.ca}
#'
#' @examples
#'
#' #using a single core (sequential processing)
#' \dontrun{
#' describe_ci_all(pdata[1:1000, ], stat = median) #bootstrapped CIs for the median
#' }
#' describe_ci_all(pdata, stat = mean) #the default
#' describe_ci_all(pdata, high_low, stat = mean) #split by a grouping variable
#'
#' \dontrun{
#' #using multiple cores (parallel processing)
#' describe_ci_all(pdata[1:1000, ], stat = sd, parallel = TRUE, cores = 2)
#' }
#'
#' @references
#' Efron, B. (1987). Better bootstrap confidence intervals. Journal of the
#' American statistical Association, 82(397), 171-185.
#'
#' Efron, B., & Tibshirani, R. J. (1993). An introduction to the bootstrap. New
#' York: Chapman & Hall.
#'
#' @seealso \code{\link[boot]{boot}}, \code{\link[boot]{boot.ci}},
#'   \code{\link{mean_ci}}, \code{\link{median_ci}}, \code{\link{stat_ci}},
#'   \code{\link{describe_ci}}
#'
#' @export
describe_ci_all <- function(data, ..., stat = mean, replicates = 2000,
                            ci_level = 0.95, ci_type = "perc", parallel = FALSE, cores = NULL,
                            na.rm = TRUE, output = "tibble") {

  st <- deparse(substitute(stat))
  dscr_ci_all <- function(data, stat = mean, replicates = 2000,
                          ci_level = 0.95, ci_type = "perc", parallel = FALSE, cores = NULL,
                          na.rm = TRUE, output = "dt") {
    num_data <- dplyr::select_if(data, is.numeric)
    st_ci <- function(data, stat = mean, replicates = 2000,
                      ci_level = 0.95, ci_type = "perc", parallel = FALSE, cores = NULL,
                      na.rm = TRUE, output = "dt"){
      dt <- data.table::as.data.table(data)
      if(st == "mean") {
        description <- dt[[1]] %>% mean_ci
        description <- data.table::data.table(description[1], description[2], description[3])
        names(description) <- c("lower", st, "upper")
      } else {
        description <- dt[[1]] %>%
          stat_ci(stat = stat, replicates = replicates, ci_type = ci_type,
                  ci_level = ci_level, parallel = parallel, cores = cores, na.rm = na.rm)
        description <- data.table::data.table(description[1], description[2], description[3])
        names(description) <- c("lower", st, "upper")
      }
      return(description)
    }
    if(st == "mean"){
      out <- num_data %>%
        purrr::map(~st_ci(data = .x, stat = mean, replicates = replicates, ci_type = ci_type,
                          ci_level = ci_level, parallel = parallel, cores = cores, na.rm = na.rm,
                          output = "dt")) %>%
        data.table::rbindlist(use.names = TRUE, idcol = "variable", fill = TRUE)
    } else {
      out <- num_data %>%
        purrr::map(~st_ci(data = .x, stat = stat, replicates = replicates, ci_type = ci_type,
                          ci_level = ci_level, parallel = parallel, cores = cores, na.rm = na.rm,
                          output = "dt")) %>%
        data.table::rbindlist(use.names = TRUE, idcol = "variable", fill = TRUE)
    }
    return(out)
  }

  if(!missing(...)){
    description <- data %>%
      dplyr::group_by(...) %>%
      tidyr::nest() %>%
      dplyr::mutate(data = purrr::map(data, ~dscr_ci_all(data = .x, stat = stat, replicates = replicates,
                                                         ci_level = ci_level, ci_type = ci_type,
                                                         parallel = parallel, cores = cores, na.rm = na.rm
                                                         )
                                      )
                    ) %>%
      tidyr::unnest(data) %>% dplyr::select(variable, tidyselect::everything()) %>%
      dplyr::arrange(variable)
  } else {
    description <- dscr_ci_all(data, stat = stat, replicates = replicates,
                               ci_level = ci_level, ci_type = ci_type,
                               parallel = parallel, cores = cores, na.rm = na.rm)
  }
  if(output == "tibble") {
    description <- tibble::as_tibble(description)
  } else if(output == "dt"){
    description <- data.table::as.data.table(description)
  }
  return(description)
}
