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

d <- pdata[1:50,]

l <- list()

l[["date"]] <- tibble::tibble("variable" = "d",
                              "cases" = 50L,
                              "n" = 50L,
                              "na" = 0L,
                              "p_na" = as.numeric(0),
                              "n_unique" = 1L,
                              "start" = as.Date("2008-01-01"),
                              "end" = as.Date("2008-01-01"))

l[["factor"]] <- tibble::tibble("variable" = "g",
                                "cases" = 50L,
                                "n" = 50L,
                                "na" = 0L,
                                "p_na" = as.numeric(0),
                                "n_unique" = 5L,
                                "ordered" = FALSE,
                                "counts_tb" = "a_16, d_10, ..., c_8, e_7")

l[["character"]] <- tibble::tibble("variable" = "high_low",
                                   "cases" = 50L,
                                   "n" = 50L,
                                   "na" = 0L,
                                   "p_na" = as.numeric(0),
                                   "n_unique" = 2L,
                                   "min_chars" = 3L,
                                   "max_chars" = 4L,
                                   "counts_tb" = "low_28, high_22")

l[["logical"]] <- tibble::tibble("variable" = "even",
                                 "cases" = 50L,
                                 "n" = 50L,
                                 "na" = 0L,
                                 "p_na" = as.numeric(0),
                                 "n_TRUE" = 25,
                                 "n_FALSE" = 25,
                                 "p_TRUE" = 0.5)

l[["numeric"]] <- tibble::tibble("variable" = c("id", "y1", "y2", "x1", "x2", "x3"),
                                 "cases" = rep(50L, 6),
                                 "n" = rep(50L, 6),
                                 "na" = rep(0L, 6),
                                 "p_na" = rep(as.numeric(0), 6),
                                 "mean" = c(25.500, 98.258, 98.027, 50.320, 150.620, 254.800),
                                 "sd" = c(14.577, 9.049, 8.808, 29.848, 28.055, 31.905),
                                 "se" = c(2.062, 1.280, 1.246, 4.221, 3.968, 4.512),
                                 "p0" = c(1.000, 76.278, 80.066, 3.000, 101.000, 201.000),
                                 "p25" = c(13.250, 91.885, 92.408, 24.750, 125.750, 231.750),
                                 "p50" = c(25.500, 97.124, 98.642, 51.500, 149.000, 257.000),
                                 "p75" = c(37.750, 102.968, 104.488, 71.750, 177.750, 283.000),
                                 "p100" = c(50.000, 121.628, 117.927, 100.000, 199.000, 298.000),
                                 "skew" = c(0.000, 0.238, -0.023, 0.003, 0.081, -0.368),
                                 "kurt" = c(-1.200, 0.154, -0.458, -1.206, -1.191, -1.168))

test_that("describe_all works", {
  expect_equivalent(describe_all(d), l, tolerance = 0.1)
})
