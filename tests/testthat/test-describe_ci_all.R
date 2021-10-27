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

r1 <- tibble::tibble(
  "variable" = c("id", "y1", "y2", "x1", "x2", "x3"),
  "mean"  = c(50.50000, 98.43273, 98.88692, 47.55000, 148.42000, 254.29000),
  "lower" = c(44.81385, 96.62156, 97.20835, 41.62230, 142.79991, 248.40283),
  "upper" = c(56.18615, 100.24389, 100.56548, 53.47770, 154.04009, 260.17717))

r2 <- tibble::tibble(
  "variable" = c("id", "y1", "y2", "x1", "x2", "x3"),
  "median" = c(50.50000, 97.79197, 99.50010, 44.00000, 146.00000, 257.50000),
  "lower" = c(40.50000, 96.48603, 96.74080, 33.00000, 139.00000, 248.00000),
  "upper" = c(60.0000, 100.4147, 101.6341, 59.0000, 156.0000, 266.0000))

test_that("describe_ci_all works for the mean", {
  expect_equivalent(as.data.frame(describe_ci_all(d)), as.data.frame(r1), tolerance = 1)
})


test_that("describe_ci_all works for the median", {
  expect_equivalent(as.data.frame(describe_ci_all(d, stat = median)), as.data.frame(r2), tolerance = 1)
})


