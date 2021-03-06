# Copyright 2021 Province of British Columbia
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

test_that("se works", {
  expect_equal(se(d$y1), 0.92408, tolerance = 0.001)
})

test_that("inv_quantile works", {
  expect_equal(inv_quantile(y = d$x1, values = c(2, 25, 50, 75, 95), digits = 2), c(0.00, 0.26, 0.50, 0.79, 0.91), tolerance = 0.1)
})

test_that("mode works", {
  expect_equal(mode(d$g), "a", tolerance = 0.001)
})

test_that("skewness type 1 works", {
  expect_equal(skewness(d$y1, type = 1), -0.01506, tolerance = 0.00001)
})

test_that("skewness type 2 works", {
  expect_equal(skewness(d$y1, type = 2), -0.01529, tolerance = 0.00001)
})

test_that("skewness type 3 works", {
  expect_equal(skewness(d$y1, type = 3), -0.01483, tolerance = 0.00001)
})

test_that("kurtosis type 1 works", {
  expect_equal(kurtosis(d$y1, type = 1), -0.35955, tolerance = 0.00001)
})

test_that("kurtosis type 2 works", {
  expect_equal(kurtosis(d$y1, type = 2), -0.31571, tolerance = 0.00001)
})

test_that("kurtosis type 3 works", {
  expect_equal(kurtosis(d$y1, type = 3), -0.41210, tolerance = 0.00001)
})

test_that("counts works with defaults", {
  expect_equal(counts(d$g, na.rm = TRUE), c("a_24", "b_23", "d_23", "c_15", "e_15"))
})

test_that("counts works with ascending order", {
  expect_equal(counts(d$g, order = "a"), c("c_15", "e_15", "b_23", "d_23", "a_24"))
})

test_that("counts works with altered n argument", {
  expect_equal(counts(d$g, n = 3), c("a_24", "b_23", "d_23"))
})

r1 <- list()
r1[["d"]] <- c("2008-01-01_100")
r1[["g"]] <- c("a_24", "b_23", "d_23", "c_15", "e_15")
r1[["high_low"]] <- c("low_52", "high_48")
r1[["even"]] <- c("FALSE_50", "TRUE_50")

test_that("counts works with descending order", {
  expect_equal(counts_all(d[, 2:5], order = "d"), r1)
})

r2 <- list()
r2[["d"]] <- c("2008-01-01_100")
r2[["g"]] <- c("c_15", "e_15", "b_23", "d_23", "a_24")
r2[["high_low"]] <- c("high_48", "low_52")
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


test_that("recode_errors works with factors", {
  expect_equivalent(recode_errors(d$g[1:10], errors = c("e", "c")),
                    factor(c(NA, NA, "d", NA, "a", "a", "d", "b", NA, NA),
                           levels = letters[1:5]))
})

re_test_df <- d
re_test_df$d[re_test_df$d == as.Date("2008-01-01")] <- NA
re_test_df$high_low[re_test_df$high_low == "high"] <- NA
re_test_df$g[re_test_df$g == "a"] <- NA
re_test_df$x3[re_test_df$x3 %in% c(248, 250)] <- NA

test_that("recode_errors works for data frames", {
  expect_equivalent(recode_errors(d, errors = c("2008-01-01", "high", "a", 248, 250)), re_test_df)
})

test_that("grapes-ni-grapes works", {
  expect_equivalent((d$g[1:10] %ni% c("a", "e")),
                    c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE))
})
