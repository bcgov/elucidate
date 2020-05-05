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
library(dplyr)

set.seed(1234)

d <- data.frame(id = rep(1:1000, times = 12)) %>%
  mutate(d = rep(lubridate::year(lubridate::ymd(stringr::str_c(as.character(seq(2008, 2019, 1)), "-01-01"))), each = 1000),
         x1 = sample(1:100, 12000, replace = TRUE),
         x2 = sample(101:200, 12000, replace = TRUE),
         x3 = sample(201:300, 12000, replace = TRUE),
         y2 = rnorm(n = 12000, mean = 100, sd = 10)) %>%
  mutate(high_low = if_else(y2 > 100, "high", "low"),
         even = if_else(id %% 2 == 0, TRUE, FALSE),
         g = as.factor(rep(sample(c(letters[1:5]), 1000, replace = TRUE), times = 12))) %>%
  arrange(d, g) %>%
  mutate(y1 = c(rnorm(216, 100, 10), rnorm(205, 100, 10), rnorm(185, 100, 10),
                rnorm(198, 100, 10), rnorm(196, 100, 10), rnorm(216, 110, 11),
                rnorm(205, 115, 10), rnorm(185, 115, 11), rnorm(198, 120, 10),
                rnorm(196, 120, 10), rnorm(216, 112, 9), rnorm(205, 115, 10),
                rnorm(185, 120, 10), rnorm(198, 130, 8), rnorm(196, 150, 9),
                rnorm(216, 120, 10), rnorm(205, 120, 10), rnorm(185, 130, 10),
                rnorm(198, 150, 10), rnorm(196, 130, 10), rnorm(216, 110, 10),
                rnorm(205, 120, 10), rnorm(185, 140, 10), rnorm(198, 150, 10),
                rnorm(196, 135, 10), rnorm(216, 120, 11), rnorm(205, 130, 11),
                rnorm(185, 150, 11), rnorm(198, 160, 11), rnorm(196, 145, 11),
                rnorm(216, 146, 11), rnorm(205, 160, 10), rnorm(185, 180, 12),
                rnorm(198, 190, 11), rnorm(196, 155, 12), rnorm(216, 160, 11),
                rnorm(205, 180, 11), rnorm(185, 210, 11), rnorm(198, 200, 11),
                rnorm(196, 118, 11), rnorm(216, 150, 12), rnorm(205, 185, 12),
                rnorm(185, 220, 12), rnorm(198, 210, 12), rnorm(196, 135, 12),
                rnorm(216, 170, 10), rnorm(205, 195, 10), rnorm(185, 250, 10),
                rnorm(198, 225, 10), rnorm(196, 145, 10), rnorm(216, 150, 13),
                rnorm(205, 200, 13), rnorm(185, 245, 13), rnorm(198, 220, 13),
                rnorm(196, 130, 12), rnorm(216, 160, 12), rnorm(205, 195, 12),
                rnorm(185, 260, 12), rnorm(198, 230, 12), rnorm(196, 150, 12))) %>%
  arrange(d, id) %>% as_tibble() %>%
  select(id, d, g, high_low, even, y1, y2, x1:x3) %>%
  mutate(d = lubridate::ymd(stringr::str_c(as.character(d), "-01-01"))) %>%
  slice(1:50)

res_num <- tibble::tibble("cases" = 50L,
             "n" = 50L,
             "na" = 0L,
             "p_na" = as.numeric(0),
             "mean" = 97.8,
             "sd" = 9,
             "se" = 1.27,
             "p0" = 75.4,
             "p25" = 91.3,
             "p50" = 99.0,
             "p75" = 102.0,
             "p100" = 121.0,
             "skew" = -0.033,
             "kurt" = 0.686)

res_num_g <- tibble::tibble("high_low" = c("high", "low"),
                    "cases" = c(23L, 27L),
                    "n" = c(23L, 27L),
                    "na" = c(0L, 0L),
                    "p_na" = as.numeric(c(0, 0)),
                    "mean" = c(96.7, 98.7),
                    "sd" = c(6.89, 10.5),
                    "se" = c(1.44, 2.02),
                    "p0" = c(79.5, 75.4),
                    "p25" = c(92.1, 92.3),
                    "p50" = c(97.7, 99.0),
                    "p75" = c(101.0, 104.0),
                    "p100" = c(108.0, 121.0),
                    "skew" = c(-0.495, -0.084),
                    "kurt" = c(0.352, 0.315))

res_f <- tibble::tibble("cases" = 50L,
                "n" = 50L,
                "na" = 0L,
                "p_na" = as.numeric(0),
                "n_unique" = 5L,
                "ordered" = FALSE,
                "v1_n" = "c_13",
                "v2_n" = "b_11",
                "v3_n" = "d_10",
                "v4_n" = "e_10",
                "v5_n" = "a_6")

res_c <- tibble::tibble("cases" = 50L,
                "n" = 50L,
                "na" = 0L,
                "p_na" = as.numeric(0),
                "n_unique" = 2L,
                "v1_n" = "low_27",
                "v2_n" = "high_23")

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
