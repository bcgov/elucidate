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

library(elucidate)
set.seed(1234)

test_that("median_ci works with defaults", {
  expect_equal(median_ci(mtcars$mpg),
               c("lower" = 15.9, "median" = 19.2, "upper" = 21.2),
               tolerance = 1)
})

test_that("median_ci with altered ci_level works", {
  expect_equal(median_ci(mtcars$mpg, ci_level = 0.8),
               c("lower" = 17.10, "median" = 19.2, "upper" = 20.5),
               tolerance = 1)
})


test_that("median_ci with altered ci_type works", {
  expect_equal(median_ci(mtcars$mpg, ci_type = "basic"),
               c("lower" = 17.0, "median" = 19.2, "upper" = 21.9),
               tolerance = 1)
})

test_that("median_ci with parallel processing works", {
  expect_equal(median_ci(mtcars$mpg, ci_type = "perc", parallel = TRUE, cores = 2),
               c("lower" = 16.5, "median" = 19.2, "upper" = 21.4),
               tolerance = 1)
})

test_that("median_ci with BCa ci_type returns bca.ci error when NA present", {
  expect_error(median_ci(c(1:10, NA), na.rm = FALSE), regexp = "*estimated adjustment 'w' is infinite*")
})
