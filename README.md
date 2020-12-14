
<!-- README.md is generated from README.Rmd. Please edit that file -->

# elucidate <img src='man/figures/hex-elucidate.png' align="right" height="139" />

## Project Status

[![img](https://img.shields.io/badge/Lifecycle-Maturing-007EC6)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)

This package is currently maintained by [Craig
Hutton](https://craig.rbind.io/), a Data Scientist working with the
Research Branch of the [British Columbia Ministry of Social Development
& Poverty
Reduction](https://www2.gov.bc.ca/gov/content/governments/organizational-structure/ministries-organizations/ministries/social-development-poverty-reduction).

## Why `elucidate`?

`elucidate` provides a collection of convenience functions to make
exploratory data analysis in R easier and more accessible to
researchers:

  - Functions that help summarize data of multiple types (numeric,
    character strings, logicals, etc.) using descriptive statistics
    (`describe*`) split by any number of grouping variables with
    comparable performance to existing alternatives and enhanced
    customization via arguments.

  - Functions that make it easier to access the power of the
    [boot](https://cran.r-project.org/web/packages/boot/index.html)
    package to obtain confidence intervals for a variety of summary
    statistics (`*_ci`), with options for within-group calculations and
    parallelized computation to maximize performance.

  - Functions that help you visualize your data using the popular
    [ggplot2](https://ggplot2.tidyverse.org/) &
    [plotly](https://plot.ly/r/) packages more easily via a relatively
    simple API with sensible and well documented arguments rather than
    layers (`plot_*`).

  - Miscellaneous functions that fill in a few of the (rare) gaps in
    `R`’s statistical computing & data science toolkit, e.g. functions
    which are easy for experienced users to write on the fly but are
    challenging for `R` novices or researchers without programming
    experience to implement: the standard error of the mean (`se()`), an
    operator that returns the values that do not match a vector (`%ni%`,
    i.e. the opposite of `%in%`), the mode of a vector (`mode()`), etc.

Inspired by tidyverse naming conventions, the core functions of
`elucidate` are organized into sets that begin with a common root
(e.g. `describe*`, `plot_*`), since this enables the user to see them
all as suggestions as you are coding in R studio.

Drawing from similar inspiration, many elucidate functions are also
designed to accept a data object as the 1st argument and return a data
or plotting object (e.g. ggplot2 or plotly) so they are compatible with
the pipe operator from the
[magrittr](https://magrittr.tidyverse.org/reference/pipe.html) package
for easy integration into data processing pipelines. For convenience,
the pipe operator is also imported when elucidate is loaded.

The `static_to_dynamic()` function converts a data frame into an
interactive [datatable](https://rstudio.github.io/DT/) or
[reactable](https://glin.github.io/reactable/articles/cookbook/cookbook.html)
depending on sample size or preference. `static_to_dynamic()` also
converts ggplot2 objects (including `plot_*` outputs) into interactive
[plotly](https://plotly-r.com/) graphs.

## Installation

You can install the development version of elucidate from this
repository with:

``` r
# use the remotes package to install from a github repository

install.packages("remotes") #only run this 1st if you haven't installed remotes before

remotes::install_github("bcgov/elucidate")
```

The authors of `elucidate` acknowledge and express their gratitude to
the authors of the [tidyverse packages](https://www.tidyverse.org/),
[data.table](https://github.com/Rdatatable/data.table), and the
functions of other dependency packages which were used to build
`elucidate`, since without their effort and ingenuity `elucidate` would
mostly have remained a collection of ideas instead of functions.

## Usage

Use `describe()` to describe a single variable in a data frame of vector
of values:

``` r
library(elucidate)
#> 
#> Attaching package: 'elucidate'
#> The following object is masked from 'package:base':
#> 
#>     mode

#using a numeric vector as input
describe(data = rnorm(1:1000, 100, 5))
#> # A tibble: 1 x 14
#>   cases     n    na  p_na  mean    sd    se    p0   p25   p50   p75  p100  skew
#>   <int> <int> <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  1000  1000     0     0  100.  5.01 0.158  83.6  96.7  100.  104.  118. 0.015
#> # ... with 1 more variable: kurt <dbl>
```

To describe all variables in a data frame, use `describe_all()`:

``` r
#if more than one class of variable is summarized you'll get a list
#if only one class is present in the data, or only one is requested, you'll get a dataframe

describe_all(pdata) #all summary types in a list, not split by any grouping variables
#> $date
#> # A tibble: 1 x 8
#>   variable cases     n    na  p_na n_unique start      end       
#>   <chr>    <int> <int> <int> <dbl>    <int> <date>     <date>    
#> 1 d        12000 12000     0     0       12 2008-01-01 2019-01-01
#> 
#> $factor
#> # A tibble: 1 x 12
#>   variable cases     n    na  p_na n_unique ordered v1_n  v2_n  v3_n  v4_n 
#>   <chr>    <int> <int> <int> <dbl>    <int> <lgl>   <chr> <chr> <chr> <chr>
#> 1 g        12000 12000     0     0        5 FALSE   a_25~ b_24~ d_23~ e_23~
#> # ... with 1 more variable: v5_n <chr>
#> 
#> $character
#> # A tibble: 1 x 8
#>   variable cases     n    na  p_na n_unique v1_n      v2_n    
#>   <chr>    <int> <int> <int> <dbl>    <int> <chr>     <chr>   
#> 1 high_low 12000 12000     0     0        2 high_6045 low_5955
#> 
#> $logical
#> # A tibble: 1 x 9
#>   variable cases     n    na  p_na n_TRUE n_FALSE p_TRUE p_FALSE
#>   <chr>    <int> <int> <int> <dbl>  <dbl>   <dbl>  <dbl>   <dbl>
#> 1 even     12000 12000     0     0   6000    6000    0.5     0.5
#> 
#> $numeric
#> # A tibble: 6 x 15
#>   variable cases     n    na  p_na  mean    sd    se    p0   p25   p50   p75
#>   <chr>    <int> <int> <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 id       12000 12000     0     0 500.  289.  2.64    1   251.   500.  750.
#> 2 y1       12000 12000     0     0 154.   42.7 0.39   69.2 121.   145.  181.
#> 3 y2       12000 12000     0     0 100.   10.1 0.092  60.0  93.4  100.  107.
#> 4 x1       12000 12000     0     0  50.5  28.9 0.264   1    25     50    75 
#> 5 x2       12000 12000     0     0 151.   28.8 0.263 101   126    150   176 
#> 6 x3       12000 12000     0     0 250.   28.9 0.264 201   225    251   276 
#> # ... with 3 more variables: p100 <dbl>, skew <dbl>, kurt <dbl>
```

Only want to summarise certain variable classes? No problem, use the
“class” argument.

Would you like a description for each level of a grouping variable? Just
provide the unquoted name of the column to use for splitting.

``` r

pdata %>% 
  describe_all(high_low, #get a description for each level of the "high_low" column
               output = "dt", #you can ask for a tibble or data.table as output options
               class = "n") #describe numeric columns only
#>     variable high_low cases    n na p_na    mean      sd    se      p0     p25
#>  1:       id     high  6045 6045  0    0 494.339 288.067 3.705   1.000 245.000
#>  2:       id      low  5955 5955  0    0 506.754 289.205 3.748   1.000 256.000
#>  3:       x1     high  6045 6045  0    0  50.499  28.760 0.370   1.000  26.000
#>  4:       x1      low  5955 5955  0    0  50.490  29.117 0.377   1.000  25.000
#>  5:       x2     high  6045 6045  0    0 150.870  28.829 0.371 101.000 126.000
#>  6:       x2      low  5955 5955  0    0 150.429  28.812 0.373 101.000 125.000
#>  7:       x3     high  6045 6045  0    0 250.458  28.941 0.372 201.000 225.000
#>  8:       x3      low  5955 5955  0    0 250.537  28.791 0.373 201.000 225.000
#>  9:       y1     high  6045 6045  0    0 153.740  42.936 0.552  70.748 120.520
#> 10:       y1      low  5955 5955  0    0 153.669  42.511 0.551  69.224 121.375
#> 11:       y2     high  6045 6045  0    0 108.084   6.081 0.078 100.005 103.233
#> 12:       y2      low  5955 5955  0    0  91.980   6.179 0.080  59.963  88.417
#>         p50     p75     p100   skew   kurt
#>  1: 489.000 743.000 1000.000  0.031 -1.203
#>  2: 513.000 759.000 1000.000 -0.032 -1.194
#>  3:  50.000  75.000  100.000  0.009 -1.199
#>  4:  51.000  76.000  100.000  0.006 -1.217
#>  5: 151.000 176.000  200.000  0.003 -1.213
#>  6: 150.000 175.000  200.000  0.000 -1.206
#>  7: 250.000 276.000  300.000  0.017 -1.214
#>  8: 251.000 275.000  300.000 -0.004 -1.214
#>  9: 144.509 182.297  289.235  0.721 -0.230
#> 10: 145.085 179.503  288.781  0.758 -0.128
#> 11: 106.867 111.612  142.181  0.996  0.862
#> 12:  93.268  96.877   99.990 -1.055  1.125
```

Calculate confidence intervals for a sample mean, median or other
summary statistic using `describe_ci()`:

``` r
#confidence intervals for other statistics are obtained using bootstrapping
pdata[1:100, ] %>% 
  describe_ci(y1, stat = median) #bootstrapped median
#> # A tibble: 1 x 3
#>   lower median upper
#>   <dbl>  <dbl> <dbl>
#> 1  96.5   97.8  101.
```

Use the `plot_*` set of functions to easily generate customized ggplot2
visualizations:

``` r
data(mtcars)

#customized density plot
mtcars %>% plot_density(x = mpg,
                        colour_var = cyl, #assign a variable to colour
                        colour_var_order = c("6", "8", "4"), #reorder the levels of the colour variable
                        colour_var_labs = c("six" = "6", "eight" = "8"), #recode the colour variable labels
                        colour_var_values = c("blue3", "red3", "green3"), #change the colours from the ggplot2 defaults
                        colour_var_title = "# cylinders")
```

<img src="man/figures/README-plot-1.png" width="100%" />

``` r

#plot group means with SE error bars
mtcars %>% 
  plot_stat_error(y = mpg, x = cyl,
                  stat = "mean", error = "se",
                  alpha = 0.6, fill = "blue2")
```

<img src="man/figures/README-plot-2.png" width="100%" />

These examples only highlight a few of the many things `elucidate` can
do. Check out [this blog
post](https://craig.rbind.io/post/2020-12-07-asgr-3-0-exploring-data-with-elucidate/)
for a comprehensive guide.

## Development Notes

`elucidate` is still in the formative stage of development and
considerable changes may occur to it in the near future.

## Reporting an Issue

To report bugs/issues or request feature changes, open an
[issue](https://github.com/bcgov/elucidate/issues) for the package
GitHub repo. If raising an issue, *please provide a reproducible
example* ([reprex](https://www.tidyverse.org/help/)) of the problem
you’re encountering.

## Requesting Features/Changes

To suggest changes or code improvements, please submit a [pull
request](https://github.com/bcgov/elucidate/pulls).

## License

Copyright 2020 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the “License”); you may
not use this file except in compliance with the License. You may obtain
a copy of the License at

<http://www.apache.org/licenses/LICENSE-2.0>

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an “AS IS” BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
