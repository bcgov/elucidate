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

d <- pdata[1:50,]

res_num <- tibble::tibble("cases" = 50L,
             "n" = 50L,
             "na" = 0L,
             "p_na" = as.numeric(0),
             "mean" = 98.3,
             "sd" = 9.05,
             "se" = 1.28,
             "p0" = 76.3,
             "p25" = 91.9,
             "p50" = 97.1,
             "p75" = 103.0,
             "p100" = 122,
             "skew" = 0.238,
             "kurt" = 0.154)


res_num_g <- tibble::tibble("high_low" = c("high", "low"),
                    "cases" = c(22L, 28L),
                    "n" = c(22L, 28L),
                    "na" = c(0L, 0L),
                    "p_na" = as.numeric(c(0, 0)),
                    "mean" = c(96, 100),
                    "sd" = c(7.7, 9.74),
                    "se" = c(1.64, 1.84),
                    "p0" = c(76.3, 85.3),
                    "p25" = c(93.2, 91.6),
                    "p50" = c(96.5, 99.6),
                    "p75" = c(99.6, 108),
                    "p100" = c(109, 122),
                    "skew" = c(-0.611, 0.364),
                    "kurt" = c(0.862, -0.624))

res_f <- tibble::tibble("cases" = 50L,
                "n" = 50L,
                "na" = 0L,
                "p_na" = as.numeric(0),
                "n_unique" = 5L,
                "ordered" = FALSE,
                "v1_n" = "a_16",
                "v2_n" = "d_10",
                "v3_n" = "b_9",
                "v4_n" = "c_8",
                "v5_n" = "e_7")

res_c <- tibble::tibble("cases" = 50L,
                "n" = 50L,
                "na" = 0L,
                "p_na" = as.numeric(0),
                "n_unique" = 2L,
                "v1_n" = "low_28",
                "v2_n" = "high_22")

res_d <- tibble::tibble("cases" = 50L,
                "n" = 50L,
                "na" = 0L,
                "p_na" = as.numeric(0),
                "n_unique" = 1L,
                "start" = as.Date("2008-01-01"),
                "end" = as.Date("2008-01-01"))

res_l <- tibble::tibble("cases" = 50L,
                "n" = 50L,
                "na" = 0L,
                "p_na" = as.numeric(0),
                "n_TRUE" = 25,
                "n_FALSE" = 25,
                "p_TRUE" = 0.5,
                "p_FALSE" = 0.5)

test_that("describe works for numeric", {
  expect_equivalent(as.data.frame(describe(d, y1)), as.data.frame(res_num), tolerance = 0.1)
})

test_that("describe works with a grouping variable", {
  expect_equivalent(as.data.frame(describe(d, y1, high_low)), as.data.frame(res_num_g), tolerance = 0.1)
})

test_that("describe works for factor", {
  expect_equivalent(as.data.frame(describe(d, g)), as.data.frame(res_f), tolerance = 0.1)
})

test_that("describe works for character", {
  expect_equivalent(as.data.frame(describe(d, high_low)), as.data.frame(res_c), tolerance = 0.1)
})

test_that("describe works for date", {
  expect_equivalent(as.data.frame(describe(d, d)), as.data.frame(res_d), tolerance = 0.1)
})

test_that("describe works for logical", {
  expect_equivalent(as.data.frame(describe(d, even)), as.data.frame(res_l), tolerance = 0.1)
})

test_that("describe fails when y is not supplied by data is a data frame", {
  expect_error(describe(d))
})
