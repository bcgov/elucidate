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

test_that("colour_options works", {
  expect_invisible(colour_options())
})

test_that("plot_density works", {
  expect_is(plot_density(mtcars, x = mpg), "gg")
})

test_that("interactive plot_density works", {
  expect_is(plot_density(mtcars, x = mpg, fill_var = cyl, fill_var_title = "# cyl",
                         interactive = T), "plotly")
})

test_that("customized plot_density works", {
  expect_is(plot_density(mtcars, x = mpg,
                         fill_var = am,
                         fill_var_order = c("1", "0"),
                         fill_var_labs = c("manual" = "0",
                                           "automatic" = "1"),
                         fill_var_values = c("blue4", "red4"),
                         fill_var_title = "transmission"), "gg")
})

test_that("plot_histogram works", {
  expect_is(plot_histogram(mtcars, x = mpg, fill = "blue"), "gg")
})

test_that("interactive plot_histogram works", {
  expect_is(plot_histogram(mtcars, x = mpg, fill = "blue", interactive = TRUE), "plotly")
})

test_that("plot_box works", {
  expect_is(plot_box(mtcars, y = mpg, x = cyl, fill = "blue"), "gg")
})

test_that("interactive plot_box works", {
  expect_is(plot_box(mtcars, y = mpg, x = cyl, interactive = TRUE), "plotly")
})

test_that("plot_violin works", {
  expect_is(plot_violin(mtcars, y = mpg, x = cyl, fill = "blue"), "gg")
})

test_that("interactive plot_violin works", {
  expect_is(plot_violin(mtcars, y = mpg, x = cyl, interactive = TRUE), "plotly")
})

test_that("plot_scatter works", {
  expect_is(plot_scatter(mtcars, y = mpg, x = cyl, fill = "blue"), "gg")
})

test_that("interactive plot_scatter works", {
  expect_is(plot_scatter(mtcars, y = mpg, x = cyl, interactive = TRUE), "plotly")
})

test_that("plot_stat_error works for the mean", {
  expect_is(plot_stat_error(mtcars, y = mpg, x = cyl, fill = "blue"), "gg")
})

test_that("plot_stat_error works for the median", {
  expect_is(plot_stat_error(mtcars, y = mpg, x = cyl, stat = "median", replicates = 500), "gg")
})
