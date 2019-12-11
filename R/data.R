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

# practice dataset for the elucidate package-----------------------------------------------------------------
#' practice data for package testing and development
#'
#' A dataset containing randomly generated practice data of various classes for
#' testing and developing elucidate package functions.
#'
#' @format A data frame of 10,000 rows and 11 columns
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

