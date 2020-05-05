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

r1 <- tibble::tibble(
  "variable" = c("id", "y1", "y2", "x1", "x2", "x3"),
  "lower" = c(44.81385, 96.62156, 97.20835, 41.62230, 142.79991, 248.40283),
  "mean"  = c(50.50000, 98.43273, 98.88692, 47.55000, 148.42000, 254.29000),
  "upper" = c(56.18615, 100.24389, 100.56548, 53.47770, 154.04009, 260.17717))

r2 <- tibble::tibble(
  "variable" = c("id", "y1", "y2", "x1", "x2", "x3"),
  "lower" = c(40.50000, 96.48603, 96.74080, 33.00000, 139.00000, 248.00000),
  "median" = c(50.50000, 97.79197, 99.50010, 44.00000, 146.00000, 257.50000),
  "upper" = c(60.0000, 100.4147, 101.6341, 59.0000, 156.0000, 266.0000))

test_that("describe_ci_all works for the mean", {
  expect_equivalent(as.data.frame(describe_ci_all(d)), as.data.frame(r1), tolerance = 1)
})


test_that("describe_ci_all works for the median", {
  expect_equivalent(as.data.frame(describe_ci_all(d, stat = median)), as.data.frame(r2), tolerance = 1)
})


