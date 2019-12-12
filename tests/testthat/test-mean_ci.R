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

test_that("mean_ci works with defaults", {
  expect_equal(mean_ci(mtcars$mpg),
               c("lower" = 18.00243, "mean" = 20.09062, "upper" = 22.17882),
               tolerance = 0.00001)
})

test_that("mean_ci with altered ci_level works", {
  expect_equal(mean_ci(mtcars$mpg, ci_level = 0.8),
               c("lower" = 18.72523, "mean" = 20.09062, "upper" = 21.45602),
               tolerance = 0.00001)
})


test_that("mean_ci with bootsrapping works", {
  expect_equal(mean_ci(mtcars$mpg, ci_type = "perc"),
               c("lower" = 18.12539, "mean" = 20.09062, "upper" = 22.23125),
               tolerance = 0.1)
})

test_that("mean_ci with bootsrapping and parallel processing works", {
  expect_equal(mean_ci(mtcars$mpg, ci_type = "perc", parallel = TRUE, cores = 2),
               c("lower" = 18.12539, "mean" = 20.09062, "upper" = 22.23125),
               tolerance = 0.1)
})

test_that("mean_ci with na.rm = FALSE returns NA for mean", {
  expect_equal(mean_ci(c(1:10, NA, 11:20), na.rm = FALSE),
               c("lower" = 7.907211, "mean" = NA, "upper" = 13.092789),
               tolerance = 0.00001)
})
