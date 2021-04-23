## elucidate 0.0.0.9022 - April 22nd, 2021

* Added `plot_pie()` for building pie charts with `ggplot2::geom_bar()` & `ggplot2::coord_polar()`. Despite well founded criticisms of pie charts (e.g. https://www.data-to-viz.com/caveat/pie.html), sometimes our project stakeholders, bosses, clients, or graduate supervisors want to see them anyways, so `plot_pie()` aims to make producing them with ggplot2 a bit easier. To encourage limiting the number of slices users display in a pie chart, if the chosen fill variable (argument "fill_var") contains more than 5 unique values (leading to >5 pie slices), a warning is issued which urges the user to consider either lumping some slices together (via argument "lump_n") or using `plot_bar()` instead.

* Minor `plot_bar()` bug fixes. Changed "flip_coordinates" argument to `plot_stat_error()` to "coord_flip" to align with the underlying `ggplot2::coord_flip()` and `plot_bar()`.

* removed "font_options" argument from `plot_*` functions and replaced it with a constrained list of the three available options: "sans" = Arial, "serif" = Times New Roman, & "mono" = Courier New.

* Additional argument option matching checks to limit invalid inputs for arguments with only a few options via `match.arg()`.

* Corrected a bug where `recode_errors()` was not working when applied to `data.table` objects.

* Moved packages `htmltools` and `htmlwidgets` from "Imports" section to "Suggests" section of Description file.

## elucidate 0.0.0.9021 - April 20th, 2021

* Added `plot_bar()` for building bar plots using `ggplot2::geom_bar()`/`ggplot2::geom_col()` with options to allow easy sorting bars in order of decreasing or increasing counts (or proportions if position = "fill"), based on values of a y-axis variable if one is specified, or manually.

## elucidate 0.0.0.9020 - February 3rd, 2021

* Upgraded `describe()`, `mode()`, and the `counts*` function set to use the more efficient `Rfast::Table()` instead of `base::table()` for counting the unique values of a vector. We are trying to avoid adding any more dependencies to `elucidate` but the substantial performance improvements of using `Rfast::Table()` made using it worth the added dependency on `Rfast`. 

* Mean calculations in the the `skewness()` and `kurtosis()` functions now use the `sum()`/`length()` method of calculating the mean instead of `mean()` because for some reason (unknown to me) it runs slightly faster. 

* `describe()` and `describe_all()` now provide the minimum and maximum number of characters for string vector inputs, similarly to the `skimr` package. Their descriptive outputs for both character and factor variables have also been modified to provide up to the two most common unique values and two least common values with associated counts combined as a single string (similar to `skimr::skim()`) under a column called "counts_tb", for consistency with the identically named `counts_tb()` function. Descriptions of logical vectors now only provide the proportion of values that are `TRUE` instead of also providing the proportion of `FALSE` values, since the latter can very easily be calculated afterwards from the `p_TRUE` column in the output should the user want to know the proportion of values that are `FALSE`. 

* Upgraded `describe_all()` to use `.SD` in the `data.table` j argument to improve processing speed. 

* All calls to `stringr` package functions have been replaced with their lower-level equivalents in the `stringi` package, and the package dependencies have been updated to require `stringi` instead of `stringr`.

* Fixed a bug with `copies()` where the input data was being modified in the global environment when the filter argument was set to either "all" or "dupes". This was only happening in cases where the input data source was already a data.table due to the subsequent use of the `:=` operator. Now all modifications occur within the function execution environment as expected, regardless of the classes of the input data. 

## elucidate 0.0.0.9013 - November 30th, 2020

* Added a `NEWS.md` file to track changes to the package.

* Fixed a bug in the `copies()` function where the "sort_by_copies" argument would not work when no conditioning variables were specified. 

*	Updated `static_to_dynamic()` to use `reactable::reactable()` to render interactive versions of data frames instead of `DT::datatable()` when the number of rows in the input data frame are over 10,000. This allows you to still get a dynamic JavaScript-based version of the data frame for larger sample sizes where testing indicated that client-side/local processing would be very slow or fail using `DT::datatable()`. The new **"reactable"** argument also lets you render a `reactable` instead of a `datatable` for fewer than 10,000 rows if you prefer it to the `DT::datatable()` output, or if you find it the default `DT::datatable()` output loading too slowly on your machine (for <10,000 rows of data). The 10,000 row threshold is adjustable via the `reactable_threshold` argument. I am currently writing a  blog post to demonstrate the use of `static_to_dynamic()` and other elucidate package functions that will published at [craig.rbind.io](https://craig.rbind.io/post/) as soon as it is ready. For the reactable version to work, you will need shiny (version >= 1.5.0) and reactable (version >= 0.2.3) installed, although these are listed as suggested packages to limit the number of dependencies required to install elucidate.

* Renamed `mode_of_y()` to the more intuitive `mode()` and upgraded it to return the most common value (i.e. the mode) of a vector regardless of the input vector class (previously it only worked for numeric vectors). You can now also specify the number of "digits"" to use for rounding, omit missing values before calculation (consistent with other elucidate functions), and can use the "inv" argument to get the anti-mode instead (i.e. least common value). ***N.B.***  this function now conflicts with a base R function `mode()` with the same name that is a convenience shortcut used to specify the storage mode of an object. However, this conflict isn't anticipated to be much of an issue because that alternative function can still be accessed using the full function name = "`storage.mode()`". Moreover, in the 7+ years I've been working in R, I've never needed to use the `storage.mode()` function, but have often wanted an intuitive `mode()` function that gives the most common value, so I suspect most elucidate users would prefer `elucidate::mode()` the base R `mode()` that is just a redundant convenience shortcut to the `storage.mode()` function that they can still access without conflicts. 

* changed the default value of the "n" argument to `describe()` to 5 instead of "all" (which is still an option) since this tends to lead to nicer output.

* Fixed a bug in recode_errors() that caused it to fail when trying to recode a factor with a non-NA replacement value. This now works for vector inputs but not (yet) when multiple columns are operated on for a data frame input. 

## elucidate 0.0.0.9012 - October 6th, 2020

*	Added the `copies()` function written primarily using data.table that combines functionality of `unique(DT)`/`distinct()` and `janitor::get_dupes()`. Performance is substantially better than `get_dupes()` based on benchmarking with a 10,000,000 row resampled version of `pdata`.

*	Also added `consum()` for consecutive summation of binary or logical vectors and the `counts_tb()` and `counts_tb_all()` convenience function extensions for `counts()` and `counts_all()`.

*	Changed the default geom of `plot_stat_error()` to point instead of bar so that users have to go out of their way to create dynamite plots.

*	Corrected `wash_df()` documentation return description.

## elucidate 0.0.0.9011 - March 5th, 2020 

* Removed references to `grDevices::windowsFonts()` from the documentation to avoid linux compatibility issues.

## elucidate 0.0.0.9010 - March 3rd, 2020

*	Updated readme and made font options arg for plots available to Windows OS systems only

*	Package approved for public release by SDPR administration and published: https://github.com/bcgov/elucidate

*	Updated installation instructions to use `remotes::install_github(“bcgov/elucidate”)`

## elucidate 0.0.0.9009 - Dec. 12th, 2019

*	Updated licensing and other components to meet BG Gov R standards. 

* Created a readme file.

*	Updated pdata to make it a bit more realistic (e.g. 12 unique dates, y1 randomly sampled within g and d); 12,000 rows instead of 10,000.

* Added unit tests

*	Removed gapminder data package as a dependency

## elucidate 0.0.0.9008 - Dec. 10th, 2019

* Added the %ni% operator which returns the negative of the %in% operator, i.e. FALSE for matching values and TRUE for non-matching values instead of TRUE for matches and FALSE for non-matches

## elucidate 0.0.0.9007 - Dec. 9th, 2019 

* added `inv_quantile()` to calculate values of a vector `y` at different quantiles. Added convenience wrappers for calculating `skewness()` and `kurtosis()`.

* added bootstrapping-based confidence interval convenience functions `mean_ci()`, `median_ci()`, `stat_ci()`, `describe_ci()`, and `describe_ci_all()`.

* replaced `mcvals()` with `counts()`
 
*	upgraded `describe()` and `describe_all()` to use `data.table` for their underlying calculations and added multiple output types for variable classes including dates, factors, logicals, character strings. Also removed the mode from the list of summary statistics returned because it slowed overall performance by too much. Use `counts()` to get the most and least common values instead.

*	Direct interactive output option was removed for performance and since the plotly versions of boxplots and histograms didn’t seem to render without a commercial license when you have a large data set anyways. Interactive tables can still be generated from the outputs using `static_to_dynamic()`.

*	Added a 10,000 row version of pdata (practice data) for testing purposes

*	updated `plot_stat_error()` to use `data.table` for all error bars and `median_ci()` for median-derived confidence intervals

*	Also updated all `plot_*` functions to use && and || instead of & or | for speed

*	Removed simpleboot and glue as dependencies.

## elucidate 0.0.0.9006 - Oct. 16th, 2019

*	Made the function titles more concise.

*	Fixed a typo in the x parameter description for `plot_stat_error()`.

*	Removed the pop out window option from `colour_options()`.

*	Added utility functions `wash_df()`, `translate()`, & `recode_errors()`. For`wash_df()`, benchmarking indicates that vroom parser is not faster than the readr parser used by `wash_df()`.

  -	**Note:** `translate()` is similar to a left join but only for a vector pair. The results should be equivalent to left joins where the key (e.g. “by” arg) is a single column and only a single other column is added… therefore it can be viewed as a special case of a left join.

## elucidate 0.0.0.9005 - Oct. 11th, 2019

*	Fixed a `plot_scatter()` bug where splitting regression lines by colour_var didn't work, but spltting did work when variables were mapped to either shape or fill.

*	Added `plot_stat_error()`, which plots a statistic and confidence intervals or other uncertainty metric as error bars. This initial version of the function only plots the mean and has several options for the error bars (standard deviation, standard error, confidence interval).

## elucidate 0.0.0.9003 - Sept. 23rd, 2019

* Updated documentation, dependencies, & fixed documentation typos.

## elucidate 0.0.0.9002 - Sept. 19th, 2019

* fixed an issue from version 0.0.0.9000 where `describe_all()` output incorrectly displayed grouping variable names.

* Updated `describe()` and `describe_all()` to include the mode of y and interactive modes with `DT()` output. 

* added `mode_of_y()`, which returns the mode of a numeric vector & convenience function `static_to_dynamic()` which converts data frames into interactive DataTables with `DT::datatable()` and ggplot2 graphs into interactive plotly graphs with `plotly::ggplotly()`.

## elucidate 0.0.0.9001 - Sept. 16th, 2019

* reformatted `mcvals()` & `mcvals_all()`, removed `lcvals()` & `lcvals_all()`. Fixed issue from version 0.0.0.9000 with `describe_ci()` failing for sample sizes over 3,000.

## elucidate 0.0.0.9000 - July 16th, 2019

*	package created locally with initial definitions for functions `se()`, `mcvals()`, `lcvals()`, `mcvals_all()`, `lcvals_all()`, `describe()`, `describe_all()`, `describe_ci()`, and  `describe_ci_all()`

