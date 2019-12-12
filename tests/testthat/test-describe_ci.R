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

d <- pdata[1:100, ]

describe_ci(d, y1, stat = mean)

r1 <- tibble::tibble("lower" = 96.6, "mean" = 98.4, "upper" = 100.0)

r1b <- tibble::tibble("lower" = 97.3, "mean" = 98.4, "upper" = 99.6)

r2 <- tibble::tibble("lower" = 96.5, "median" = 97.8, "upper" = 101.0)

r3 <- tibble::tibble("lower" = 8.05, "sd" = 9.24, "upper" = 10.30)

test_that("describe_ci works for the mean", {
  expect_equivalent(as.data.frame(describe_ci(d, y1)), as.data.frame(r1), tolerance = 1)
})

test_that("describe_ci works with altered ci_level", {
  expect_equivalent(as.data.frame(describe_ci(d, y1, ci_level = 0.8)), as.data.frame(r1b), tolerance = 1)
})

test_that("describe_ci works for the median", {
  expect_equivalent(as.data.frame(describe_ci(d, y1, stat = median)), as.data.frame(r2), tolerance = 1)
})

test_that("describe_ci works for the standard deviation", {
  expect_equivalent(as.data.frame(describe_ci(d, y1, stat = sd)), as.data.frame(r3), tolerance = 1)
})

