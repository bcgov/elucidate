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

y <- c(1:100)

test_that("se works", {
  expect_equal(se(y), 2.901149, tolerance = 0.00001)
})

test_that("inv_quantile works", {
  expect_equal(inv_quantile(y = y, values = c(2, 25, 50, 75, 95), digits = 2), c(0.02, 0.25, 0.50, 0.75, 0.95), tolerance = 0.01)
})

test_that("mode_of_y works", {
  expect_equal(mode_of_y(d$y1), 80.89, tolerance = 0.1)
})

test_that("skewness type 1 works", {
  expect_equal(skewness(d$y1, type = 1), 0.0873031, tolerance = 0.1)
})

test_that("skewness type 2 works", {
  expect_equal(skewness(d$y1, type = 2), 0.08863826, tolerance = 0.1)
})

test_that("skewness type 3 works", {
  expect_equal(skewness(d$y1, type = 3), 0.08599684, tolerance = 0.1)
})

test_that("kurtosis type 1 works", {
  expect_equal(kurtosis(d$y1, type = 1), 0.03684228, tolerance = 0.1)
})

test_that("kurtosis type 2 works", {
  expect_equal(kurtosis(d$y1, type = 2), 0.1012398, tolerance = 0.1)
})

test_that("kurtosis type 3 works", {
  expect_equal(kurtosis(d$y1, type = 3), -0.02359089, tolerance = 0.1)
})

test_that("counts works with defaults", {
  expect_equal(counts(d$g), c("b_24", "c_24", "a_19", "e_19", "d_14"))
})

test_that("counts works with ascending order", {
  expect_equal(counts(d$g, order = "a"), c("d_14", "a_19", "e_19", "b_24", "c_24"))
})

test_that("counts works with altered n argument", {
  expect_equal(counts(d$g, n = 3), c("b_24", "c_24", "a_19"))
})

r1 <- list()
r1[["d"]] <- c("2008-01-01_100")
r1[["g"]] <- c("b_24", "c_24", "a_19", "e_19", "d_14")
r1[["high_low"]] <- c("high_50", "low_50")
r1[["even"]] <- c("FALSE_50", "TRUE_50")

test_that("counts works with descending order", {
  expect_equal(counts_all(d[, 2:5], order = "d"), r1)
})

r2 <- list()
r2[["d"]] <- c("2008-01-01_100")
r2[["g"]] <- c("d_14", "a_19", "e_19", "b_24", "c_24")
r2[["high_low"]] <- c("high_50", "low_50")
r2[["even"]] <- c("FALSE_50", "TRUE_50")

test_that("counts_all works with ascending order", {
  expect_equal(counts_all(d[, 2:5], order = "a"), r2)
})

test_that("static_to_dynamic works", {
  expect_visible(static_to_dynamic(d, caption = "Table 1"))
})

x <- mtcars
x$`Extra Column` <- rep(NA, length.out = nrow(mtcars)) #add an empty column
x[33:50,] <- NA #add some missing rows

test_that("wash_df works", {
  expect_equal(wash_df(x), tibble::as_tibble(mtcars))
})


old_values <- c(1:10)

new_values <- c("one", "two", "three", "four", "five",
                "six", "seven", "eight", "nine", "ten")

test_that("translate works", {
  expect_equal(translate(y = mtcars$cyl[1:10], old = old_values, new = new_values),
               c("six", "six", "four", "six", "eight", "six", "eight", "four", "four", "six"))
})


test_that("recode_errors works", {
  expect_equivalent(recode_errors(d$g[1:10], errors = c("e", "c")),
               factor(c("a", "d", NA, NA, NA, NA, NA, NA, NA, "d"),
                      levels = letters[1:5]))
})


test_that("grapes-ni-grapes works", {
  expect_equivalent((d$g[1:10] %ni% c("a", "e")),
                    c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE))
})
