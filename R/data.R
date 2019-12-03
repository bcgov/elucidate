# practice dataset for the elucidate package-----------------------------------------------------------------
#' practice data for package testing and development
#'
#' A dataset containing randomly generated practice data of various classes for
#' testing, developing, and benchmarking elucidate package functions on a
#' realistic scale an analyst might encounter in "the wild".
#'
#' @format A data frame of 1,000,000 rows and 11 columns
#' \describe{
#'  \item{id}{a sequence of numbers labeling the row id}
#'  \item{d}{a sequence of dates (yyyy-mm-dd) starting January 1st, 2000}
#'  \item{x1}{random sample of values between 1-100}
#'  \item{x2}{random sample of values between 101-200}
#'  \item{x3}{random sample of values between 201-300}
#'  \item{y1}{random sample from a normal distribution with mean of 50 and standard deviation of 5}
#'  \item{y2}{random sample from a normal distribution with mean of 80 and standard deviation of 5}
#'  \item{y3}{random sample from a normal distribution with mean of 900 and standard deviation of 90}
#'  \item{high_low}{character vector indicating whether y3 is above = "high" or below = "low" the mean value of 900}
#'  \item{even}{logical vector indicating whether a row id is even = TRUE or odd = FALSE}
#'  \item{g2}{5 level factor of values "a", "b", "c", "d", and "e" which switches group labels every 10 rows}
#' }
#' @source "Randomly generated data"
"pdata"
