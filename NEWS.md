# elucidate 0.1.0.9005 - January 23rd, 2022

* Fixed a bug which caused `describe()` and `describe_ci()` to fail when attempting to evaluate columns from data.table objects.

# elucidate 0.1.0.9004 - December 13th, 2021

* `describe()` and `describe_all()` now work for "POSIXct" and "POSIXlt" date/time class vectors in addition to the previously supported "Date" class vectors.

# elucidate 0.1.0.9003 - October 31st, 2021

* Fixed a bug that caused `plot_var()` to fail for ordered factors. 

* Fixed a bug that caused `plot_box()` to fail when the optional dot plot layer enhancement was enabled ("dots" = `TRUE`) but no variable was assigned to the x-axis.

* Fixed a bug that prevented `plot_raincloud()` from allowing users to adjust the fill colour of the box plots when the "box_plot" option is enabled.

* Updated the package readme and other documentation.

* Added a package vignette and continuous integration build checks for Windows, MacOS, and Linux.

* Added a "caption" argument to all `plot_*` functions which provides a shortcut for adding a caption to the bottom of the graph that is left-justified by default as per standard scientific communication practice (unlike the `ggplot2` default). All geom-specific `plot_*` functions (not `plot_var*` functions) now also have a "caption_hjust" argument that makes it easy to adjust the horizontal alignment of the caption by adding the appropriate `ggplot2::theme()` layer for you. 

* Changed the name of the `plot_pie()` "title_alignment" argument to "title_hjust" for consistency with other `plot_*` functions, which also now all have a "title_hjust" argument (except for the `plot_var*` functions). The elucidate default title alignment is now 0.5 for centre-justified.

* Rearranged the output of the `*ci()` functions such that the confidence interval lower and upper bound columns are after the summary statistic column.

* The magrittr pipe operator (`%>%`) is no longer imported when `elucidate` is loaded. With the advent of the base R pipe operator (`|>`) in R 4.1 it seemed no longer necessary for `elucidate` to also import a secondary pipe operator, especially since it is already loaded with other common packages like `dplyr`.

# elucidate 0.1.0.9002 - June 28th, 2021

* Fixed a bug that prevented `copies()` and `dupes()` from working correctly when the input data is a data.table. 

* Added citation file.

# elucidate 0.1.0.9001 - June 27th, 2021

* Fixed a bug with `plot_var()` that was preventing the combined box-and-whisker plots + violin plots produced by default for a mix of numeric and categorical variables from generating a plotly graph when the "interactive" argument was set to `TRUE`.

# elucidate 0.1.0.9000 - June 25th, 2021

## major update

### new functions

* Added `plot_var()`, `plot_var_all()`, `plot_var_pairs()`, and `plot_c()` to complete the `plot_*` function set. 

* `plot_var()` is an elucidate alternative to the flexible, but esoteric, `ggplot2::qplot()` function. It can also be thought of as a graphical version of the `describe()` function. `plot_var()` allows you to easily generate a ggplot2 graph for one or two numeric and/or categorical variables using one of four geom-specific `plot_*` functions in a class-appropriate manner, with some default enhancements and a restricted set of arguments:

  - One numeric (classes numeric/integer/date) variable will be graphed with `plot_density()`. By default, a normal density curve will be added as a dashed line. This can be disabled by setting the "dnorm" argument to `FALSE` or the "basic" argument to `TRUE`. 

  - One or two categorical (classes factor/character/logical) variable(s) will be graphed with `plot_bar()`.
  
  - Two numeric variables will be graphed with `plot_scatter()`. By default, a generalized additive model regression line and 95% confidence envelope will also be added to the scatter plot. These extras can be disabled by setting the "regression_line" argument to `FALSE` or the "basic" argument to `TRUE`. 
  
  - A mixture of numeric and categorical variables will be graphed with `plot_box()`. By Default, a `ggplot2::geom_violin()` layer will also be added to enable rapid detection of multi-modal distributions. This additional layer can be disabled by setting the "violin" argument to `FALSE` or the basic argument to `TRUE`.

  - You can also use the "group_var" argument to split any of the above plots by a categorical grouping variable from the same data source, which will be mapped to either fill or colour depending upon the `plot_*` function that is used.
  
  - Unlike the geom-named `plot_*` functions (`plot_density()`, `plot_bar()`, etc.), `plot_var()` also works with vectors (i.e. does not require a data frame input).
  
  - To aid beginners, `plot_var()` also has a "verbose" argument that (when set to `TRUE`) will print a message to the console telling you which input variable classes were detected, which `plot_*` function was used to generate the graph, and which arguments of that function were used by `plot_var()` to map input variables to the x-axis, y-axis, and the fill/colour aesthetics. 

* `plot_var_all()` is to `plot_var()` as `describe_all()` is to `describe()`, extending `plot_var()` to allow you to easily generate a graph of *each* variable in a data frame. Each of these "primary" variables can also be (optionally) plotted against a single named secondary variable (via the "var2" argument), and you can also use the same "group_var" argument to split each plot by a categorical grouping variable.

* `plot_var_pairs()` extends `plot_var_all()` further, by allowing you to graph all pairwise variable combinations in a data frame to produce a matrix of so-called "pair" plots, similar to the `graphics::pairs()` or `GGally::ggpairs()` function. These pair plots can also be split by a grouping variable via the "group_var" argument.

* Unlike existing alternative methods of producing plot matrices or lattices, `plot_var_all()` and `plot_var_pairs()` provide a "trelliscope" argument that can be used to combine plots into an interactive javascript display via the [trelliscopejs](https://ryanhafen.com/blog/trelliscopejs/) package, which is especially useful in cases where there are too many graphs to read on a single page. The default plot composition method uses the [patchwork](https://patchwork.data-imaginist.com/) package instead, which means that you can modify the multi-panel further with patchwork functions like `patchwork::plot_annotation()`.

* `plot_c()` allows you to use either `patchwork::wrap_plots()` or `trelliscopejs::trelliscope()` to combine multiple plots into a multi-panel static or interactive display via a logical "trelliscope" argument. Unlike alternatives functions for combining plots, you can pass `plot_c()` either (1) a set of unquoted plot object names or (2) a list of plots. The interactive trelliscope version should also work with plotly graphs.

### upgrades to existing functions

* Added a "dots" argument to `plot_box()` which allows you to overlay a `ggplot2::geom_dotplot()` layer over the box-and-whisker plot(s). Inclusion of the dotplot layer can reveal the presence of multimodal distributions (multiple peaks), which can't be detected using a box-and-whisker plot alone.

* `copies()`, `dupes()`, and all `plot_*` and `describe*` functions now allow you to specify variables using quoted ("x") or unquoted (x) column names (instead of just unquoted), e.g. `plot_density(data = pdata, x = "y1")` or `plot_density(data = pdata, x = y1)`. Note that you must use one format or the other and should not mix formats within the same function call for functions which accept grouping variable names via the special ellipsis argument (`...`), like the `describe*` set, or the function will fail with an error indicating that one or more of variables could not be found in the input data.  By also accepting column names as character strings, these functions should be easier to use in loops and other functions, while the unquoted names are easier for interactive use. 

### bug fixes

* Fixed a bug where plotly output of the `plot_box()` interactive mode was not dodging box plots when a variable was assigned to the "fill_var" or "colour_var" arguments. Now the boxes for each group will be separated, but a spurious warning message that "'layout' objects don't have these attributes: 'boxmode'" will be printed to the console. This warning message is a [documented bug](https://github.com/ropensci/plotly/issues/994) with the `plotly` package and the message can be safely ignored (unfortunately it cannot be suppressed though).
 
* updated `counts_tb()` and `counts_tb_all()` to omit empty rows in their outputs if the number of top and bottom values requested exceeds the number of unique values in the input data.

# elucidate 0.0.0.9026 - June 2nd, 2021

* Added a new plotting function, `plot_line()`, to make it easy to generate ggplot2 line graphs. Unlike a basic `ggplot2::geom_line()` layer, `plot_line()` will automatically check the input data to see if there are multiple values of the y-axis variable for each level of x-axis variable and any grouping variables mapped to line colour or line type, or used for faceting. In addition, `plot_line()` makes it easier for you to use a non-numeric/non-date variable on the x-axis by converting it to a factor with levels that can easily be rearranged using the "x_var_order_by_y" or "x_var_order" arguments. 

* Added "xbreaks" and "ybreaks" arguments as appropriate for `plot*` functions which accept numeric variables on the x and/or y axes. These arguments allow you to modify the x or y axis breaks via `ggplot2::scale_*_continuous()`. One exception to this rule is that "xbreaks" uses `ggplot2::scale_x_date()` instead if a date vector is mapped to the x-axis of `plot_line()`. You can also modify the x and y axis tick labels (for numeric variables) with "x_var_labs" and "y_var_labs" arguments.

* Modified the behaviour of all `plot*` function "xlim" and "ylim" arguments to use `ggplot2::coord_cartesian()` (or `ggplot2::coord_flip()` if "coord_flip" = TRUE for `plot_bar()` or `plot_raincloud()`) instead of `ggplot2::scale_*_continuous()` to zoom in on part of the graph when limiting the x or y axis range rather than dropping values. 

# elucidate 0.0.0.9025 - May 27th, 2021

* Added a new plotting function, `plot_raincloud()`, to make ggplot2 [rain cloud plots](https://wellcomeopenresearch.org/articles/4-63/v2) using the [gghalves](https://github.com/erocoar/gghalves) package. A "rain cloud" plot consists of a half violin plot on one side and scattered points on the other side of the plot, which looks like a cloud (half-violin plot portion) over rain drops (scatter plot portion) when the x and y axes are flipped (via the "coord_flip" argument or by adding a ggplot2::coord_flip() layer). `plot_raincloud()` also provides a "box_plot" and "box_*" set of arguments to allow you to add and customize a box-and-whisker plot if you wish. This combination of half-violin plot, box-and-whisker-plot, and scatter plot is one of the best ways to compare continuous variables across levels of a categorical variable.

* Added a "rug" argument to `plot_density()` and `plot_histogram()` that makes it easy to add rug lines to a density plot or histogram. Both of these plots also now have a "dnorm*" set of arguments that allow you to add normal/gaussian density curves to these plot types.

* All `plot_*` functions now have a `palette*` set of arguments which enable you to easily apply any of the five core `viridis` palettes to an elucidate-generated ggplot2 graph via `ggplot2::scale_fill_viridis()`. See [this section](https://craig.rbind.io/post/2021-05-17-asgr-3-1-data-visualization/#colourblind-friendly-palettes) of my recent blog post for examples of each palette option. All `plot_*` functions now use the "plasma" viridis palette as a default whenever a variable is mapped to the fill or colour aesthetics (via the "fill_var" or "colour_var" arguments). Since the viridis palettes [were designed](https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html#the-color-scales) to be colourblind-friendly, hopefully more of your graphs will now be easier for colourblind individuals to read. Note that you can still override these palettes with the "fill_var_values" or "colour_var_values" arguments as appropriate. See the `plot_*` function documentation for other minor changes.

# elucidate 0.0.0.9024 - May 22nd, 2021

* Fixed a bug that was causing `recode_errors()` to fail when a non-date/POSIX.ct value was passed to the "errors" argument and checked against a vector of dates. Now you can re-code erroneous values in date columns or vectors by simply specifying the erroneous values as character strings or numbers. For example, if a vector of dates named "date_vector" contains an erroneous value of "2099-01-01" that should be "2009-01-01", you can now fix it via `recode_errors(date_vector, errors = "2099-01-01", replacement = "2009-01-01")` instead of the previously required `recode_errors(date_vector, errors = as.Date("2099-01-01"), replacement = as.Date("2009-01-01"))`. If you are instead trying to re-code values of "." (or other arbitrary sting) as `NA` throughout a data frame, "df", via `recode_errors(df, errors = c(9999, "."))`, and the data frame happens to include a date column, the function will no longer fail with the error: "character string is not in a standard unambiguous format". 

* In the process of debugging `recode_errors()`, some inefficient for loops were also replaced with `lapply` and `data.table::.SD`, which should considerably reduce execution times and memory utilization when re-coding long vectors or large data frames. For example, benchmarking the new version of `recode_errors()` against the old version to re-code 3 erroneous values of mixed classes as `NA` using a resampled version of `pdata` with one million rows by nine columns suggests that the new changes led to over a three-fold reduction in execution time and memory utilization. 

# elucidate 0.0.0.9023 - May 20th, 2021

* Added a convenience shortcut for `copies(filter = "dupes", sort_by_copies = TRUE)` as a new function, `dupes()`, which makes checking data frames for duplicated rows even easier. 

* Reverted `describe()`, `describe_all()`, `mode()`, and the `counts*` function set to using `base::table()` instead of the faster `Rfast::Table()` because of an [ongoing incompatibility issue](https://github.com/RfastOfficial/Rfast/issues/26) between `Rfast` and `dplyr` that appears to [cause R studio to hang](https://github.com/RfastOfficial/Rfast/issues/16) for extended periods. We may switch back to `Rfast::Table()` after the issue has been resolved by the `Rfast` developers. 

* Added a "sep" argument to the `counts_*` function set, `describe()`, and `describe_all()` that allows the user to change the separator string pasted between unique values and their counts in the output. The ability to change the separator string also enables the user to obtain the intended results from `counts_tb()` or `counts_tb_all()` in cases where the input data has values containing an underscore by allowing them to change it to a string that is not already present in the data. 

* The `stringi` package has been removed as a dependency.

# elucidate 0.0.0.9022 - April 22nd, 2021

* Added `plot_pie()` for building pie charts with `ggplot2::geom_bar()` & `ggplot2::coord_polar()`. Despite well founded criticisms of pie charts (e.g. https://www.data-to-viz.com/caveat/pie.html), sometimes our project stakeholders, bosses, clients, or graduate supervisors want to see them anyway, so `plot_pie()` aims to make producing them with ggplot2 a bit easier. To encourage limiting the number of slices users display in a pie chart, if the chosen fill variable (argument "fill_var") contains more than 5 unique values (leading to >5 pie slices), a warning is issued which urges the user to consider either lumping some slices together (via argument "lump_n") or using `plot_bar()` instead.

* Minor `plot_bar()` bug fixes. Changed "flip_coordinates" argument to `plot_stat_error()` to "coord_flip" to align with the underlying `ggplot2::coord_flip()` and `plot_bar()`.

* removed "font_options" argument from `plot_*` functions and replaced it with a constrained list of the three available options: "sans" = Arial, "serif" = Times New Roman, & "mono" = Courier New.

* Additional argument option matching checks to limit invalid inputs for arguments with only a few options via `match.arg()`.

* Corrected a bug where `recode_errors()` was not working when applied to `data.table` objects.

* Moved packages `htmltools` and `htmlwidgets` from "Imports" section to "Suggests" section of Description file.

# elucidate 0.0.0.9021 - April 20th, 2021

* Added `plot_bar()` for building bar plots using `ggplot2::geom_bar()`/`ggplot2::geom_col()` with options to allow easy sorting bars in order of decreasing or increasing counts (or proportions if position = "fill"), based on values of a y-axis variable if one is specified, or manually.

# elucidate 0.0.0.9020 - February 3rd, 2021

* Upgraded `describe()`, `mode()`, and the `counts*` function set to use the more efficient `Rfast::Table()` instead of `base::table()` for counting the unique values of a vector. We are trying to avoid adding any more dependencies to `elucidate` but the substantial performance improvements of using `Rfast::Table()` made using it worth the added dependency on `Rfast`. 

* Mean calculations in the the `skewness()` and `kurtosis()` functions now use the `sum()`/`length()` method of calculating the mean instead of `mean()` because for some reason (unknown to me) it runs slightly faster. 

* `describe()` and `describe_all()` now provide the minimum and maximum number of characters for string vector inputs, similarly to the `skimr` package. Their descriptive outputs for both character and factor variables have also been modified to provide up to the two most common unique values and two least common values with associated counts combined as a single string (similar to `skimr::skim()`) under a column called "counts_tb", for consistency with the identically named `counts_tb()` function. Descriptions of logical vectors now only provide the proportion of values that are `TRUE` instead of also providing the proportion of `FALSE` values, since the latter can very easily be calculated afterwards from the `p_TRUE` column in the output should the user want to know the proportion of values that are `FALSE`. 

* Upgraded `describe_all()` to use `.SD` in the `data.table` j argument to improve processing speed. 

* All calls to `stringr` package functions have been replaced with their lower-level equivalents in the `stringi` package, and the package dependencies have been updated to require `stringi` instead of `stringr`.

* Fixed a bug with `copies()` where the input data was being modified in the global environment when the filter argument was set to either "all" or "dupes". This was only happening in cases where the input data source was already a data.table due to the subsequent use of the `:=` operator. Now all modifications occur within the function execution environment as expected, regardless of the classes of the input data. 

# elucidate 0.0.0.9013 - November 30th, 2020

* Added a `NEWS.md` file to track changes to the package.

* Fixed a bug in the `copies()` function where the "sort_by_copies" argument would not work when no conditioning variables were specified. 

*	Updated `static_to_dynamic()` to use `reactable::reactable()` to render interactive versions of data frames instead of `DT::datatable()` when the number of rows in the input data frame are over 10,000. This allows you to still get a dynamic JavaScript-based version of the data frame for larger sample sizes where testing indicated that client-side/local processing would be very slow or fail using `DT::datatable()`. The new **"reactable"** argument also lets you render a `reactable` instead of a `datatable` for fewer than 10,000 rows if you prefer it to the `DT::datatable()` output, or if you find it the default `DT::datatable()` output loading too slowly on your machine (for <10,000 rows of data). The 10,000 row threshold is adjustable via the `reactable_threshold` argument. I am currently writing a  blog post to demonstrate the use of `static_to_dynamic()` and other elucidate package functions that will published at [craig.rbind.io](https://craig.rbind.io/post/) as soon as it is ready. For the reactable version to work, you will need shiny (version >= 1.5.0) and reactable (version >= 0.2.3) installed, although these are listed as suggested packages to limit the number of dependencies required to install elucidate.

* Renamed `mode_of_y()` to the more intuitive `mode()` and upgraded it to return the most common value (i.e. the mode) of a vector regardless of the input vector class (previously it only worked for numeric vectors). You can now also specify the number of "digits"" to use for rounding, omit missing values before calculation (consistent with other elucidate functions), and can use the "inv" argument to get the anti-mode instead (i.e. least common value). ***N.B.***  this function now conflicts with a base R function `mode()` with the same name that is a convenience shortcut used to specify the storage mode of an object. However, this conflict isn't anticipated to be much of an issue because that alternative function can still be accessed using the full function name = "`storage.mode()`". Moreover, in the 7+ years I've been working in R, I've never needed to use the `storage.mode()` function, but have often wanted an intuitive `mode()` function that gives the most common value, so I suspect most elucidate users would prefer `elucidate::mode()` the base R `mode()` that is just a redundant convenience shortcut to the `storage.mode()` function that they can still access without conflicts. 

* changed the default value of the "n" argument to `describe()` to 5 instead of "all" (which is still an option) since this tends to lead to nicer output.

* Fixed a bug in recode_errors() that caused it to fail when trying to recode a factor with a non-NA replacement value. This now works for vector inputs but not (yet) when multiple columns are operated on for a data frame input. 

# elucidate 0.0.0.9012 - October 6th, 2020

*	Added the `copies()` function written primarily using data.table that combines functionality of `unique(DT)`/`distinct()` and `janitor::get_dupes()`. Performance is substantially better than `get_dupes()` based on benchmarking with a 10,000,000 row resampled version of `pdata`.

*	Also added `consum()` for consecutive summation of binary or logical vectors and the `counts_tb()` and `counts_tb_all()` convenience function extensions for `counts()` and `counts_all()`.

*	Changed the default geom of `plot_stat_error()` to point instead of bar so that users have to go out of their way to create dynamite plots.

*	Corrected `wash_df()` documentation return description.

# elucidate 0.0.0.9011 - March 5th, 2020 

* Removed references to `grDevices::windowsFonts()` from the documentation to avoid linux compatibility issues.

# elucidate 0.0.0.9010 - March 3rd, 2020

*	Updated readme and made the font options argument for plots available to Windows OS systems only.

*	Package approved for public release by SDPR administration and published: https://github.com/bcgov/elucidate

*	Updated installation instructions to use `remotes::install_github(“bcgov/elucidate”)`

# elucidate 0.0.0.9009 - Dec. 12th, 2019

*	Updated licensing and other components to meet BG Gov R standards. 

* Created a readme file.

*	Updated pdata to make it a bit more realistic (e.g. 12 unique dates, y1 randomly sampled within g and d); 12,000 rows instead of 10,000.

* Added unit tests

*	Removed gapminder data package as a dependency

# elucidate 0.0.0.9008 - Dec. 10th, 2019

* Added the `%ni%` operator which returns the negative of the `%in%` operator, i.e. FALSE for matching values and TRUE for non-matching values instead of TRUE for matches and FALSE for non-matches

# elucidate 0.0.0.9007 - Dec. 9th, 2019 

* added `inv_quantile()` to calculate values of a vector `y` at different quantiles. Added convenience wrappers for calculating `skewness()` and `kurtosis()`.

* added bootstrapping-based confidence interval convenience functions `mean_ci()`, `median_ci()`, `stat_ci()`, `describe_ci()`, and `describe_ci_all()`.

* replaced `mcvals()` with `counts()`
 
*	upgraded `describe()` and `describe_all()` to use `data.table` for their underlying calculations and added multiple output types for variable classes including dates, factors, logicals, character strings. Also removed the mode from the list of summary statistics returned because it slowed overall performance by too much. Use `counts()` to get the most and least common values instead.

*	Direct interactive output option was removed for performance and since the plotly versions of boxplots and histograms didn’t seem to render without a commercial license when you have a large data set anyway. Interactive tables can still be generated from the outputs using `static_to_dynamic()`.

*	Added a 10,000 row version of pdata (practice data) for testing purposes

*	updated `plot_stat_error()` to use `data.table` for all error bars and `median_ci()` for median-derived confidence intervals

*	Also updated all `plot_*` functions to use && and || instead of & or | for speed

*	Removed simpleboot and glue as dependencies.

# elucidate 0.0.0.9006 - Oct. 16th, 2019

*	Made the function titles more concise.

*	Fixed a typo in the x parameter description for `plot_stat_error()`.

*	Removed the pop out window option from `colour_options()`.

*	Added utility functions `wash_df()`, `translate()`, & `recode_errors()`. For`wash_df()`, benchmarking indicates that vroom parser is not faster than the readr parser used by `wash_df()`.

  -	**Note:** `translate()` is similar to a left join but only for a vector pair. The results should be equivalent to left joins where the key (e.g. “by” argument) is a single column and only a single other column is added… therefore it can be viewed as a special case of a left join.

# elucidate 0.0.0.9005 - Oct. 11th, 2019

*	Fixed a `plot_scatter()` bug where splitting regression lines by colour_var didn't work, but splitting did work when variables were mapped to either shape or fill.

*	Added `plot_stat_error()`, which plots a statistic and confidence intervals or other uncertainty metric as error bars. This initial version of the function only plots the mean and has several options for the error bars (standard deviation, standard error, confidence interval).

# elucidate 0.0.0.9003 - Sept. 23rd, 2019

* Updated documentation, dependencies, & fixed documentation typos.

# elucidate 0.0.0.9002 - Sept. 19th, 2019

* fixed an issue from version 0.0.0.9000 where `describe_all()` output incorrectly displayed grouping variable names.

* Updated `describe()` and `describe_all()` to include the mode of y and interactive modes with `DT()` output. 

* added `mode_of_y()`, which returns the mode of a numeric vector & convenience function `static_to_dynamic()` which converts data frames into interactive DataTables with `DT::datatable()` and ggplot2 graphs into interactive plotly graphs with `plotly::ggplotly()`.

# elucidate 0.0.0.9001 - Sept. 16th, 2019

* reformatted `mcvals()` & `mcvals_all()`, removed `lcvals()` & `lcvals_all()`. Fixed issue from version 0.0.0.9000 with `describe_ci()` failing for sample sizes over 3,000.

# elucidate 0.0.0.9000 - July 16th, 2019

*	package created locally with initial definitions for functions `se()`, `mcvals()`, `lcvals()`, `mcvals_all()`, `lcvals_all()`, `describe()`, `describe_all()`, `describe_ci()`, and  `describe_ci_all()`

