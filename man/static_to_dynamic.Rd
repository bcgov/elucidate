% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/miscellaneous.R
\name{static_to_dynamic}
\alias{static_to_dynamic}
\title{Convert a static data frame or ggplot object to a dynamic form to
facilitate interactive data exploration.}
\usage{
static_to_dynamic(
  static_object,
  caption = NULL,
  reactable = FALSE,
  reactable_threshold = 10000,
  group_by = NULL,
  reactable_stripe_colour = "#e4e2e9",
  reactable_highlight_colour = "#a28adb",
  reactable_selected_colour = "#aaaadb"
)
}
\arguments{
\item{static_object}{A data frame, tibble, or ggplot2 object.}

\item{caption}{Add a caption/title to the dynamic table/figure.}

\item{reactable}{Affects data frame inputs only. overrides the row-limit to
convert a dataframe to reactable() format instead of datatable() format
even when there are fewer rows than reactable_threshold. You might want to
do this to take advantage of custom highlighting/stripe colours or grouping
via the "group_by" argument. Alternatively, you could set
reactable_threshold to 0 and achieve the same effect.}

\item{reactable_threshold}{Affects data frame inputs only. Determines the
threshold for the number of rows in a input data frame beyond which a
reactable() is generated as output instead of a datatable().}

\item{group_by}{If the input is a data frame and reactable is TRUE and/or the
number of rows exceeds the reactable_threshold, this allows you to group the
reactable output by input columns, which can be specified using a character
vector.}

\item{reactable_stripe_colour}{If the input is a data frame and reactable is
TRUE and/or the number of rows exceeds the reactable_threshold, this allows
you to change the row striping colour (specified using a hexcode or base R
colour name). Use \code{elucidate::colour_options()} to see which colour options
are available.}

\item{reactable_highlight_colour}{If the input is a data frame and reactable is
TRUE and/or the number of rows exceeds the reactable_threshold, this allows
you to change the row highlight colour (specified using a hexcode or base R
colour name). Use \code{elucidate::colour_options()} to see which colour options
are available.}

\item{reactable_selected_colour}{If the input is a data frame and reactable
is TRUE and/or the number of rows exceeds the reactable_threshold, this
allows you to change the background colour of selected rows (specified
using a hexcode or base R colour name). Use \code{elucidate::colour_options()}
to see which colour options are available.}
}
\value{
If a data frame or tibble was provided, the output will be a
DataTables or reactable html widget (according to criteria specified
above). If a ggplot object was provided, the output will be a plotly html
widget.

Unique features of the DataTable-derived output for data frames include
Excel-like cell editing and filling, the ability to rearrange and/or
selectively hide columns, and convenience buttons for printing or
downloading the table to Excel, csv, or PDF format. Unfortunately the
client-side version of DataTables doesn't perform well for larger datasets.
In such cases (>10,000 rows) a reactable() is returned instead, which also
has some nice unique functions including stripes that are more clearly
visible, row highlighting when hovering, the ability to selectively
highlight rows to facilitate visual comparisons, and options to modify the
highlight, stripe, and background colours. The reactable, but not datatable
version, also lets you group the output by variables of interest so they can
be selectively collapsed or expanded. Both output versions allow you to do
search-based filtering of rows for some or all of the columns.
}
\description{
Uses \code{\link[plotly]{ggplotly}} or
\code{\link[DT]{datatable}} to convert a ggplot2 object or data
frame/tibble to a dynamic & interactive form. The input object is a
dataframe with over 10,000 rows (by default, this threshold is adjustable),
\code{\link[reactable]{reactable}} will be used instead of
\code{\link[DT]{datatable}}, because the client-side version of
\code{\link[DT]{datatable}} (implemented here) doesn't perform well or may
crash your R studio session for larger datasets than this. Note that
\code{ggplot2} graphs rendered by \code{plot_raincloud()} and \code{plot_pie()} currently
cannot be properly converted to plotly format because of incompatibility
with \code{\link[plotly]{ggplotly}}.
}
\examples{

\dontrun{
data(mtcars)
library(ggplot2)

p1 <- ggplot(mtcars, aes(x = mpg)) + geom_histogram(binwidth = 1)
static_to_dynamic(p1)

static_to_dynamic(mtcars)

static_to_dynamic(mtcars, reactable = TRUE)

static_to_dynamic(mtcars, reactable = TRUE, group_by = "cyl")

static_to_dynamic(mtcars, reactable = TRUE,
                       reactable_hightlight_colour = "lightgreen")
}

}
\seealso{
\code{\link[DT]{datatable}}, \code{\link[plotly]{ggplotly}}
}
\author{
Craig P. Hutton, \email{Craig.Hutton@gov.bc.ca}
}
