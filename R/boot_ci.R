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

# boot_ci functions -------------------------------------------------------

# mean_ci ---------------------------------------------------------------
#' @title
#' Obtain a (bootstrapped) confidence interval for the mean of a numeric vector.
#'
#' @description mean_ci returns confidence intervals for the mean of a numeric
#'   vector. One might want to use bootstrapping to obtain robust estimates for
#'   a confidence interval of the mean if the sample size is small (e.g. n = 10)
#'   or calculate them from a theoretical normal distribution otherwise. Note
#'   that the usual calculation based on quantiles of the theoretical
#'   distribution can be obtained with this function using the default ci_type =
#'   "norm". This function provides a simplified user interface to the
#'   \code{\link[boot]{boot}} and \code{\link[boot]{boot.ci}} functions
#'   similarly to the \code{\link[simpleboot]{one.boot}} function but retains
#'   more of the boot package's functionality, most notably including options
#'   for parallelization. For convenience, when operating in parallel the user's
#'   operating system is automatically detected so that the appropriate
#'   parallelization engine is used (e.g. snow for Windows, multicore otherwise)
#'   by the parallel package. Since the mean and median are common descriptive
#'   statistics for which confidence intervals are estimated, these have their
#'   own dedicated functions. To obtain bootstrapped confidence intervals for
#'   other summary statistics use code{\link{stat_ci}} instead.
#'
#' @importFrom stats qnorm
#' @importFrom boot boot
#' @importFrom boot boot.ci
#' @importFrom parallel detectCores
#'
#' @param y A vector/variable (required).
#'
#' @param replicates The number of bootstrap replicates to use. Default is
#'   2,000, as recommended by Efron & Tibshirani (1993). For publications, or if
#'   you need more precise estimates, more replications (e.g. >= 5,000) are
#'   recommended. N.B. more replications will of course take longer to run. If
#'   you get the error: "estimated adjustment 'a' is NA" when ci_type is set to
#'   "bca" then try again with more replications.
#'
#' @param ci_level The confidence level to use for constructing confidence intervals.
#' Default is set to \code{ci_level = 0.95} for 95 percent CIs.
#'
#' @param ci_type The type of confidence intervals to calculate from the
#'   bootstrap samples. Most of the options available in the underlying boot.ci
#'   function are implemented (except for studentized intervals): "norm" for
#'   calculation based on a theoretical normal distribution, "perc" for
#'   percentile, "basic" for basic, and "bca" for bias-corrected and
#'   accelerated. See \code{\link[boot]{boot.ci}} for details regarding options
#'   other than "norm". Since the normal confidence intervals for the mean can
#'   be directly calculation based quantiles from the theoretical gaussian
#'   distribution this method is used for this unique case (CIs for the mean)
#'   instead of bootstrapping when the ci_type is set to "norm" (the default),
#'   since it is MUCH faster.
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
#'   parallel::detectCores()
#'
#' @param na.rm should missing values be removed before attempting to calculate
#'   the mean and confidence intervals? Default is TRUE.
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gov.bc.ca}
#'
#' @examples
#'
#' y1 <- rnorm(1:10000, 100, 10)
#'
#' #using a single core (sequential processing)
#' mean_ci(y1)
#'
#' mean_ci(y1, ci_type = "perc")
#'
#' #using multiple cores (parallel processing)
#' mean_ci(y1, parallel = TRUE, cores = 2, ci_type = "perc")
#'
#' @references
#' Efron, B., & Tibshirani, R. J. (1993). An introduction to the bootstrap. New
#' York: Chapman & Hall.
#'
#' @seealso \code{\link[boot]{boot}}, \code{\link[boot]{boot.ci}},
#'   \code{\link{median_ci}}, \code{\link{stat_ci}}, \code{\link{describe_ci}},
#'   \code{\link{describe_ci_all}}
#'
#' @export
mean_ci <- function(y, replicates = 2000, ci_level = 0.95, ci_type = "norm", parallel = FALSE, cores = NULL, na.rm = TRUE){
  if(na.rm == TRUE) {
    y <- na.omit(y)
  }

  if(ci_type == "norm") {
    mn <- mean(y)
    mean_lb <- mean(y, na.rm = T) - (abs(stats::qnorm((1-ci_level)/2))*se(y))
    mean_ub <- mean(y, na.rm = T) + (abs(stats::qnorm((1-ci_level)/2))*se(y))
    out <- c("lower" = mean_lb, "mean" = mn, "upper" = mean_ub)
    return(out)
  } else {


    bmean <- function(y, i) {
      return(mean(y[i], na.rm = TRUE))
    }
    ncores <- parallel::detectCores()
    OS <- Sys.info()['sysname']

    if(ncores > 1 && parallel == TRUE) {
      if (OS == "Windows" || OS == "windows"){
        if(missing(cores)){
          b <- boot::boot(y, bmean, R = replicates, parallel = "snow", ncpus = ncores - 1, simple = TRUE)
        } else {
          if(cores > 1) {
            b <- boot::boot(y, bmean, R = replicates, parallel = "snow", ncpus = cores, simple = TRUE)
          } else {
            b <- boot::boot(y, bmean, R = replicates)
          }
        }
      } else {
        if(missing(cores)){
          b <- boot::boot(y, bmean, R = replicates, parallel = "multicore", ncpus = ncores - 1, simple = TRUE)
        } else {
          b <- boot::boot(y, bmean, R = replicates, parallel = "multicore", ncpus = cores, simple = TRUE)
        }
      }
    } else {
      b <- boot::boot(y, bmean, R = replicates)
    }

    mn <- mean(y)

    if(ci_type == "bca"){
      bci <- boot::boot.ci(boot.out = b, type = "bca", conf = ci_level)
      mean_lb <- bci$bca[4]
      mean_ub <- bci$bca[5]
    } else if (ci_type == "perc") {
      bci <- boot::boot.ci(boot.out = b, type = "perc", conf = ci_level)
      mean_lb <- bci$percent[4]
      mean_ub <- bci$percent[5]
    } else if (ci_type == "basic") {
      bci <- boot::boot.ci(boot.out = b, type = "basic", conf = ci_level)
      mean_lb <- bci$basic[4]
      mean_ub <- bci$basic[5]
    }

    out <- c("lower" = mean_lb, "mean" = mn, "upper" = mean_ub)
    return(out)
  }
}

# median_ci ---------------------------------------------------------------
#' @title
#' Obtain a bootstrapped confidence interval for the median of a numeric vector.
#'
#' @description median_ci returns bootstrapped confidence intervals for the
#'   median of a numeric vector. This function provides a simplified user
#'   interface to the \code{\link[boot]{boot}} and \code{\link[boot]{boot.ci}}
#'   functions similarly to the \code{\link[simpleboot]{one.boot}} function but
#'   retains more of the boot package's functionality, most notably including
#'   options for parallelization. For convenience, when operating in parallel
#'   the user's operating system is automatically detected so that the
#'   appropriate parallelization engine is used (e.g. snow for Windows,
#'   multicore otherwise) by the parallel package. Since the mean and median are
#'   common descriptive statistics for which confidence intervals are estimated,
#'   these have their own dedicated functions. To obtain bootstrapped confidence
#'   intervals for other summary statistics use code{\link{stat_ci}} instead.
#'
#' @importFrom stats median
#' @importFrom boot boot
#' @importFrom boot boot.ci
#' @importFrom parallel detectCores
#'
#' @param y A vector/variable (required).
#'
#' @param replicates The number of bootstrap replicates to use. Default is
#'   2,000, as recommended by Efron & Tibshirani (1993). For publications, or if
#'   you need more precise estimates, more replications (e.g. >= 5,000) are
#'   recommended. N.B. more replications will of course take longer to run. If
#'   you get the error: "estimated adjustment 'a' is NA" when ci_type is set to
#'   "bca" then try again with more replications.
#'
#' @param ci_level The confidence level to use for constructing confidence intervals.
#' Default is set to \code{ci_level = 0.95} for 95 percent CIs.
#'
#' @param ci_type The type of confidence intervals to calculate from the
#'   bootstrap samples. Most of the options available in the underlying boot.ci
#'   function are implemented (except for studentized intervals): "norm" for an
#'   approximation based on the normal distribution, "perc" for percentile,
#'   "basic" for basic, and "bca" for bias-corrected and accelerated. BCa
#'   intervals are the default since these tend to provide the most
#'   accurate/least-biased results (Efron, 1987), however they require more time
#'   to calculate and may not be much better than the other methods for large
#'   sample sizes (e.g. >= 100,000 rows of data). See
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
#'   parallel::detectCores()
#'
#' @param na.rm should missing values be removed before attempting to calculate
#'   the median and confidence intervals? Default is TRUE.
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gov.bc.ca}
#'
#' @examples
#'
#' y1 <- rnorm(1:10000, 100, 10)
#'
#' #using a single core (sequential processing)
#' median_ci(y1, ci_type = "perc")
#'
#' #using multiple cores (parallel processing)
#' median_ci(y1, parallel = TRUE, cores = 2, ci_type = "perc")
#'
#' @references
#' Efron, B. (1987). Better bootstrap confidence intervals. Journal of the
#' American statistical Association, 82(397), 171-185.
#'
#' Efron, B., & Tibshirani, R. J. (1993). An introduction to the bootstrap. New
#' York: Chapman & Hall.
#'
#' @seealso \code{\link[boot]{boot}}, \code{\link[boot]{boot.ci}},
#'   \code{\link{mean_ci}}, \code{\link{stat_ci}}, \code{\link{describe_ci}},
#'   \code{\link{describe_ci_all}}
#'
#' @export
median_ci <- function(y, replicates = 2000, ci_level = 0.95, ci_type = "bca", parallel = FALSE, cores = NULL, na.rm = TRUE){
  if(na.rm == TRUE) {
    y <- na.omit(y)
  }

  bmed <- function(y, i) {
    return(stats::median(y[i]))
  }
  ncores <- parallel::detectCores()
  OS <- Sys.info()['sysname']

  if(ncores > 1 && parallel == TRUE) {
    if (OS == "Windows" || OS == "windows"){
      if(missing(cores)){
        b <- boot::boot(y, bmed, R = replicates, parallel = "snow", ncpus = ncores - 1, simple = TRUE)
      } else {
        if(cores > 1) {
          b <- boot::boot(y, bmed, R = replicates, parallel = "snow", ncpus = cores, simple = TRUE)
        } else {
          b <- boot::boot(y, bmed, R = replicates)
        }
      }
    } else {
      if(missing(cores)){
        b <- boot::boot(y, bmed, R = replicates, parallel = "multicore", ncpus = ncores - 1, simple = TRUE)
      } else {
        b <- boot::boot(y, bmed, R = replicates, parallel = "multicore", ncpus = cores, simple = TRUE)
      }
    }
  } else {
    b <- boot::boot(y, bmed, R = replicates)
  }

  p50 <- stats::median(y, na.rm = TRUE)

  if(ci_type == "bca"){
    bci <- boot::boot.ci(boot.out = b, type = "bca", conf = ci_level)
    p50_lb <- bci$bca[4]
    p50_ub <- bci$bca[5]
  } else if (ci_type == "norm") {
    bci <- boot::boot.ci(boot.out = b, type = "norm", conf = ci_level)
    p50_lb <- bci$normal[4]
    p50_ub <- bci$normal[5]
  } else if (ci_type == "perc") {
    bci <- boot::boot.ci(boot.out = b, type = "perc", conf = ci_level)
    p50_lb <- bci$percent[4]
    p50_ub <- bci$percent[5]
  } else if (ci_type == "basic") {
    bci <- boot::boot.ci(boot.out = b, type = "basic", conf = ci_level)
    p50_lb <- bci$basic[4]
    p50_ub <- bci$basic[5]
  }

  out <- c("lower" = p50_lb, "median" = p50, "upper" = p50_ub)
  return(out)
}


# stat_ci -----------------------------------------------------------------
#' @title
#' Obtain a bootstrapped confidence interval for a summary statistic of a numeric vector.
#'
#' @description stat_ci returns bootstrapped confidence intervals for a specific
#'   summary statistic for numeric vectors. This function provides a simplified
#'   user interface to the \code{\link[boot]{boot}} and
#'   \code{\link[boot]{boot.ci}} functions similarly to the
#'   \code{\link[simpleboot]{one.boot}} function but retains more of the boot
#'   package's functionality, most notably including options for
#'   parallelization. For convenience, when operating in parallel the user's
#'   operating system is automatically detected so that the appropriate
#'   parallelization engine is used (e.g. snow for Windows, multicore otherwise)
#'   by the parallel package. Confidence intervals for the mean or median can be
#'   obtained more easily using the convenience functions \code{\link{mean_ci}}
#'   & \code{\link{median_ci}}.
#'
#' @importFrom boot boot
#' @importFrom boot boot.ci
#' @importFrom parallel detectCores
#'
#' @param y A vector/variable (required).
#'
#' @param stat the unquoted name (e.g. mean, not "mean") of a summary statistic
#'   function to calculate confidence intervals for. Only functions which return
#'   a single value and operate on numeric variables are currently supported.
#'
#' @param ... any number of additional named arguments passed to stat function
#'   for further customization.
#'
#' @param replicates The number of bootstrap replicates to use. Default is
#'   2,000, as recommended by Efron & Tibshirani (1993). For publications, or if
#'   you need more precise estimates, more replications (e.g. >= 5,000) are
#'   recommended. N.B. more replications will of course take longer to run. If
#'   you get the error: "estimated adjustment 'a' is NA" when ci_type is set to
#'   "bca" then try again with more replications.
#'
#' @param ci_level The confidence level to use for constructing confidence intervals.
#' Default is set to \code{ci_level = 0.95} for 95 percent CIs.
#'
#' @param ci_type The type of confidence intervals to calculate from the
#'   bootstrap samples. Most of the options available in the underlying boot.ci
#'   function are implemented (except for studentized intervals): "norm" for an
#'   approximation based on the normal distribution, "perc" for percentile,
#'   "basic" for basic, and "bca" for bias-corrected and accelerated. BCa
#'   intervals are the default since these tend to provide the most
#'   accurate/least-biased results (Efron, 1987), however they require more time
#'   to calculate and may not be much better than the other methods for large
#'   sample sizes (e.g. >= 100,000 rows of data). See
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
#'   parallel::detectCores()
#'
#' @param na.rm should missing values be removed before attempting to calculate
#'   the chosen statistic and confidence intervals? Default is TRUE.
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gov.bc.ca}
#'
#' @examples
#'
#' y1 <- rnorm(1:10000, 100, 10)
#'
#' #using a single core (sequential processing)
#' stat_ci(y1, stat = sd, ci_type = "perc")
#'
#' #using multiple cores (parallel processing)
#' stat_ci(y1, stat = sd, parallel = TRUE, cores = 2, ci_type = "perc")
#'
#' @references
#' Efron, B. (1987). Better bootstrap confidence intervals. Journal of the
#' American statistical Association, 82(397), 171-185.
#'
#' Efron, B., & Tibshirani, R. J. (1993). An introduction to the bootstrap. New
#' York: Chapman & Hall.
#'
#' @seealso \code{\link[boot]{boot}}, \code{\link[boot]{boot.ci}},
#'   \code{\link{mean_ci}}, \code{\link{median_ci}}, \code{\link{describe_ci}},
#'   \code{\link{describe_ci_all}}
#'
#' @export
stat_ci <- function(y, stat, ..., replicates = 2000, ci_level = 0.95, ci_type = "bca", parallel = FALSE, cores = NULL, na.rm = TRUE){
  if(na.rm == TRUE) {
    y <- na.omit(y)
  }
  bstat <- function(y, i) {
    return(stat(y[i], ...))
  }
  ncores <- parallel::detectCores()
  OS <- Sys.info()['sysname']

  if(ncores > 1 && parallel == TRUE) {
    if (OS == "Windows" || OS == "windows"){
      if(missing(cores)){
        b <- boot::boot(y, bstat, R = replicates, parallel = "snow", ncpus = ncores - 1, simple = TRUE)
      } else {
        if(cores > 1) {
          b <- boot::boot(y, bstat, R = replicates, parallel = "snow", ncpus = cores, simple = TRUE)
        } else {
          b <- boot::boot(y, bstat, R = replicates)
        }
      }
    } else {
      if(missing(cores)){
        b <- boot::boot(y, bstat, R = replicates, parallel = "multicore", ncpus = ncores - 1, simple = TRUE)
      } else {
        b <- boot::boot(y, bstat, R = replicates, parallel = "multicore", ncpus = cores, simple = TRUE)
      }
    }
  } else {
    b <- boot::boot(y, bstat, R = replicates)
  }

  st <- stat(y, ...)

  if(ci_type == "bca"){
    bci <- boot::boot.ci(boot.out = b, type = "bca", conf = ci_level)
    st_lb <- bci$bca[4]
    st_ub <- bci$bca[5]
  } else if (ci_type == "norm") {
    bci <- boot::boot.ci(boot.out = b, type = "norm", conf = ci_level)
    st_lb <- bci$normal[4]
    st_ub <- bci$normal[5]
  } else if (ci_type == "perc") {
    bci <- boot::boot.ci(boot.out = b, type = "perc", conf = ci_level)
    st_lb <- bci$percent[4]
    st_ub <- bci$percent[5]
  } else if (ci_type == "basic") {
    bci <- boot::boot.ci(boot.out = b, type = "basic", conf = ci_level)
    st_lb <- bci$basic[4]
    st_ub <- bci$basic[5]
  }
  nms <- c("lower", deparse(substitute(stat)), "upper")
  out <- c(st_lb, st, st_ub)
  names(out) <- nms
  return(out)
}
