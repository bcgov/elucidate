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
library(janitor)
library(dplyr)

set.seed(1234)

cars <- tibble::rownames_to_column(mtcars, "car")

a <- copies(cars, vs, am, gear, carb, filter = "dupes") %>%
  dplyr::select(vs, am, gear, carb, dupe_count = n_copies, everything()) %>%
  dplyr::arrange(vs, am, gear, carb) %>%
  elucidate::wash_df(clean_names = F, remove_empty = F)

b <- janitor::get_dupes(cars, vs, am, gear, carb) %>%
  elucidate::wash_df(clean_names = F, remove_empty = F)

test_that("copies() filters duplicated rows equivalently to janitor::get_dupes()", {
 expect_equal(a, b,
              tolerance = 1)
})


c <- copies(cars, vs, am, gear, carb, filter = "first") %>%
  dplyr::arrange(vs, am, gear, carb) %>%
  elucidate::wash_df(clean_names = F, remove_empty = F)

d <- dplyr::distinct(cars, vs, am, gear, carb, .keep_all = TRUE) %>%
  dplyr::arrange(vs, am, gear, carb) %>%
  elucidate::wash_df(clean_names = F, remove_empty = F) %>%
  tibble::as_tibble()

test_that("copies() filters distinct rows equivalently to dplyr::distinct()", {
  expect_equal(c, d,
               tolerance = 1)
})

