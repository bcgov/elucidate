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

# practice dataset for the elucidate package-----------------------------------------------------------------
#' practice data for package testing and development
#'
#' A dataset containing randomly generated practice data of various classes for
#' testing and developing elucidate package functions.
#'
#'
#' @format A data frame of 12,000 rows and 10 columns
#' \describe{
#'  \item{id}{a sequence of numbers from 1:1000 labelling the row id, repeats for each unique value of d}
#'  \item{d}{a sequence of dates (yyyy-mm-dd) ranging from 2008-01-01 to 2019-01-01 in 1 year increments}
#'  \item{g}{5 level factor of values "a", "b", "c", "d", and "e" which was assigned to ids randomly within d}
#'  \item{high_low}{character vector indicating whether y2 is above = "high" or below = "low" the mean value of 100}
#'  \item{even}{logical vector indicating whether a row id is even = TRUE or odd = FALSE}
#'  \item{y1}{random samples from a normal distribution within each level of g and d}
#'  \item{y2}{random sample from a normal distribution with mean of 100 and standard deviation of 10}
#'  \item{x1}{random sample of values between 1-100}
#'  \item{x2}{random sample of values between 101-200}
#'  \item{x3}{random sample of values between 201-300}
#' }
#' @source "Randomly generated data"
"pdata"

