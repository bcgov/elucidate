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
  slice(1:100)

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

