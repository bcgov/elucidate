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

# plot functions ------------------------------------------------------------
# colour options --------------------------------------------------------
#' @title
#' Display the base R colour options.
#'
#' @description display the base R colour options in the active graphics
#'   plotting device or printed to a PDF file. This makes it easier to see what
#'   the color options are in conjunction with their R labels to make it easier
#'   to specify custom colours for plots.
#'
#' @importFrom grDevices pdf
#' @importFrom grDevices col2rgb
#' @importFrom grDevices dev.off
#' @importFrom graphics image
#' @importFrom graphics mtext
#' @importFrom graphics par
#' @importFrom graphics text
#'
#' @param print_to_pdf IF TRUE, output is exported to a PDF in the working
#'   directory instead of displayed in an R graphics device.
#'
#' @param pdf_name The name of the PDF file. Default is
#'   "base_r_colour_options.pdf". Must end in .pdf. Ignored if print_to_pdf =
#'   FALSE.
#'
#' @author Craig P. Hutton, \email{Craig.Hutton@@gov.bc.ca}
#'
#' @examples
#' \dontrun{
#' #default settings. Prints to the active graphics device.
#' colour_options(print_to_pdf = FALSE) #the default
#' colour_options(print_to_pdf = TRUE) #print to a single page PDF file in your working directory.
#'}
#' @references
#' Code was adapted from http://bc.bojanorama.pl/2013/04/r-color-reference-sheet/
#'
#' @export
colour_options <- function(print_to_pdf = FALSE, pdf_name = "base_r_colour_options.pdf") {
  #adapted from http://bc.bojanorama.pl/2013/04/r-color-reference-sheet/
  if(print_to_pdf == TRUE) {
    grDevices::pdf(pdf_name, paper="a4r", width=11.6, height=8.2, onefile=TRUE)
    mat.1 <- matrix(1:660, 60, 11)
    colors <- colors()[mat.1]
    op <- par(mar=c(.1, .1, 2, .1))
    image(1:11, 1:60, t(mat.1), col=colors, axes=FALSE, ann=FALSE)
    txtcol <- ifelse(apply(col2rgb(colors), 2, mean) < 70, "white", "black")
    text( as.numeric(col(mat.1)), as.numeric(row(mat.1)), colors, cex=.8, col=txtcol)
    mtext("Base r colour options. see ?colours() for more info.", 3, cex=1.5)
    par(op)
    dev.off()
  } else {
    mat.1 <- matrix(1:660, 60, 11)
    colors <- colors()[mat.1]
    op <- par(mar=c(.1, .1, 2, .1))
    image(1:11, 1:60, t(mat.1), col=colors, axes=FALSE, ann=FALSE)
    txtcol <- ifelse(apply(col2rgb(colors), 2, mean) < 70, "white", "black")
    text( as.numeric(col(mat.1)), as.numeric(row(mat.1)), colors, cex=.8, col=txtcol)
    mtext("Base r colour options. see ?colours() for more info.", 3, cex=1.5)
    par(op)
  }
}

# plot_density -------------------------------------------------------
#' @title
#'
#' Generate a density plot.
#'
#' @description Easily generate a density plot of a numeric variable using
#'   ggplot2 with a simplified customization interface for common modifications
#'   with static (ggplot) and interactive (plotly) output options. The static
#'   output is useful for producing static reports (e.g. for manuscripts) and is
#'   readily customized further using ggplot2 syntax. The interactive output is
#'   helpful for exploring the data and producing dynamic html reports.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom forcats fct_relevel
#' @importFrom forcats fct_recode
#' @importFrom rlang !!!
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_density
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_fill_grey
#' @importFrom ggplot2 lims
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 theme_grey
#' @importFrom ggplot2 theme_gray
#' @importFrom ggplot2 theme_light
#' @importFrom ggplot2 theme_dark
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 element_text
#' @importFrom plotly ggplotly
#' @importFrom utils browseURL
#'
#' @param data A data frame or tibble containing the dependent measure "x" and
#'   any grouping variables.
#'
#' @param x The numeric variable you want a density plot of.
#'
#' @param ... graphical parameters (not associated with variables) to be passed
#'   to \code{\link[ggplot2]{geom_density}}, e.g. colour or fill, to be applied
#'   to all curves. To see some of the available options in a web browser, set
#'   the aesthetic_options argument to TRUE.
#'
#' @param fill_var Use if you want to assign a variable to the density curve
#'   fill colour, e.g. fill_var = grouping_variable. Produces separate curves
#'   for each level of the fill variable. See \code{\link[ggplot2]{aes}} for
#'   details.
#'
#' @param colour_var Use if you want to assign a variable to the density curve
#'   outline colour, e.g. colour_var = grouping_variable. Produces separate
#'   curves for each level of the colour variable. See
#'   \code{\link[ggplot2]{aes}} for details.
#'
#' @param xlab Specify/overwrite the x-axis label using a character string, e.g.
#'   "x-axis label"
#'
#' @param ylab Specify/overwrite the y-axis label using a character string, e.g.
#'   "y-axis label"
#'
#' @param title Add a main title to the plot using a character string, e.g.
#'   "Density graph of X"
#'
#' @param fill_var_title If a variable has been assigned to fill using fill_var,
#'   this allows you to modify the variable label in the plot legend.
#'
#' @param colour_var_title If a variable has been assigned to colour using
#'   colour_var, this allows you to modify the variable label in the plot
#'   legend.
#'
#' @param xlim specify the x-axis limits, e.g. xlim = c(lower_limit,
#'   upper_limit). Use NA for the existing minimum or maximum value of x, e.g.
#'   the default is xlim = c(NA, NA)
#'
#' @param transform_x Would you like to transform the x axis? (TRUE or FALSE)
#'
#' @param x_transformation If transform_x = TRUE, this determines the
#'   transformation to be applied. Common choices include "log10" (the default),
#'   "log2", "sqrt", or "exp". See \code{\link[ggplot2]{scale_continuous}} for
#'   details.
#'
#' @param fill_var_order If a variable has been assigned to fill using fill_var,
#'   this allows you to modify the order of the variable groups, e.g. fill_var =
#'   grouping_variable, fill_var_order = c("group_2", "group_1"). See
#'   \code{\link[forcats]{fct_relevel}} for details.
#'
#' @param colour_var_order If a variable has been assigned to colour using
#'   colour_var, this allows you to modify the order of the variable groups,
#'   e.g. colour_var = grouping_variable, fill_var_order = c("group_2",
#'   "group_1"). See \code{\link[forcats]{fct_relevel}} for details.
#'
#' @param fill_var_labs If a variable has been assigned to fill using fill_var,
#'   this allows you to modify the labels of the variable groups, e.g. fill_var
#'   = grouping_variable, fill_var_labs = c("group_1_new_label" =
#'   "group_1_old_label", "group_2_new_label" = "group_2_old_label"). See
#'   \code{\link[forcats]{fct_recode}} for details.
#'
#' @param colour_var_labs If a variable has been assigned to colour using
#'   colour_var, this allows you to modify the labels of the variable groups,
#'   e.g. colour_var = grouping_variable, colour_var_labs =
#'   c("group_1_new_label" = "group_1_old_label", "group_2_new_label" =
#'   "group_2_old_label"). See \code{\link[forcats]{fct_recode}} for details.
#'
#' @param fill_var_values If a variable has been assigned to fill using
#'   fill_var, this allows you to modify the colours assigned to the fill of
#'   each of the variable groups, e.g. fill_var = grouping_variable,
#'   fill_var_values = c("blue", "red"). See
#'   \code{\link[ggplot2]{scale_fill_manual}} for details. For the colour
#'   options available in base R, see \code{\link[elucidate]{colour_options}}.
#'
#' @param colour_var_values If a variable has been assigned to colour using
#'   colour_var, this allows you to modify the colours assigned to the outline
#'   of each of the variable groups, e.g. colour_var = grouping_variable,
#'   colour_var_values = c("blue", "red"). See
#'   \code{\link[ggplot2]{scale_fill_manual}} for details. For the colour
#'   options available in base R, see \code{\link[elucidate]{colour_options}}.
#'
#' @param alpha This adjusts the transparency/opacity of the graphical
#'   components of the plot, ranging from 0 = 100% transparent to 1 = 100%
#'   opaque.
#'
#' @param greyscale Set to TRUE if you want the plot converted to greyscale.
#'
#' @param theme Adjusts the theme using 1 of 6 predefined "complete" theme
#'   templates provided by ggplot2. Currenlty supported options are: "classic"
#'   (the elucidate default), "bw", "grey" (the ggplot2 default), "light",
#'   "dark", & "minimal". See \code{\link[ggplot2]{theme_classic}} for more
#'   information.
#'
#' @param text_size This controls the size of all plot text. Default = 14.
#'
#' @param font This controls the font of all plot text. Default = "sans"
#'   (Arial). Other options include "serif" (Times New Roman) and "mono" (Courier
#'   New).
#'
#' @param facet_var Use if you want separate plots for each level of a grouping
#'   variable (i.e. a facetted plot), e.g. facet_var = grouping_variable. See
#'   \code{\link[ggplot2]{facet_wrap}} for details.
#'
#' @param facet_var_order If a variable has been assigned for facetting using
#'   facet_var, this allows you to modify the order of the variable groups, e.g.
#'   facet_var = grouping_variable, facet_var_order = c("group_2", "group_1").
#'   See \code{\link[forcats]{fct_relevel}} for details.
#'
#' @param facet_var_labs If a variable has been assigned for facetting using
#'   facet_var, this allows you to modify the labels of the variable groups
#'   which will appear in the facet strips, e.g. facet_var = grouping_variable,
#'   facet_var_labs = c("group_1_new_label" = "group_1_old_label",
#'   "group_2_new_label" = "group_2_old_label"). See
#'   \code{\link[forcats]{fct_recode}} for details.
#'
#' @param facet_var_strip_position If a variable has been assigned for facetting
#'   using facet_var, this allows you to modify the position of the facet strip
#'   labels. Sensible options include "top" (the default) or "bottom".
#'
#' @param facet_var_text_bold If a variable has been assigned for facetting
#'   using facet_var, this allows you to use boldface (TRUE/default or FALSE)
#'   for the facet strip label text.
#'
#' @param legend_position This allows you to modify the legend position.
#'   Options include "right" (the default), "left", "top", & "bottom".
#'
#' @param omit_legend Set to TRUE if you want to remove/omit the legends.
#'
#' @param interactive Determines whether a static ggplot object or an interactive html
#'   plotly object is returned. See \code{\link[plotly]{ggplotly}} for details.
#'
#' @param aesthetic_options If set to TRUE, opens a web browser to the tidyverse
#'   online aesthetic options vignette.
#'
#' @return A ggplot object or plotly object depending on whether static or
#'   interactive output was requested.
#'
#' @author Craig P. Hutton, \email{Craig.Hutton@@gov.bc.ca}
#'
#' @examples
#' data(mtcars) #load the mtcars data
#'
#' library(magrittr)
#'
#' mtcars %>% plot_density(x = mpg)
#'
#' \dontrun{
#' mtcars %>%
#'   plot_density(x = mpg, transform_x = T, x_transformation = "log2")
#'
#' mtcars %>%
#'   plot_density(x = mpg, transform_x = T, x_transformation = "log10") #default transformation
#'
#' mtcars %>%
#'   plot_density(x = mpg, transform_x = T, x_transformation = "sqrt") #default transformation
#'
#' mtcars %>% plot_density(x = mpg, fill_var = cyl, fill_var_labs = c("four" = "4"))
#'
#' mtcars %>% plot_density(x = mpg, fill_var = cyl, fill_var_title = "# cyl",
#'                         interactive = T)
#'
#' mtcars %>% plot_density(x = mpg,
#'                         fill_var = am,
#'                         fill_var_order = c("1", "0"),
#'                         fill_var_labs = c("manual" = "0",
#'                                           "automatic" = "1"),
#'                         fill_var_values = c("blue4", "red4"),
#'                         fill_var_title = "transmission")
#'
#' mtcars %>% plot_density(x = mpg,
#'                         colour_var = cyl,
#'                         colour_var_order = c("6", "8", "4"),
#'                         colour_var_labs = c("six" = "6", "eight" = "8"),
#'                         colour_var_values = c("blue3", "red3", "green3"),
#'                         colour_var_title = "# cylinders")
#'
#' #interactive version
#'
#' mtcars %>% describe(mpg, fill_var = cyl, interactive = T)
#' }
#'
#'
#' @references
#' Wickham, H. (2016). ggplot2: elegant graphics for data analysis. New York, N.Y.: Springer-Verlag.
#'
#' @seealso \code{\link[ggplot2]{geom_density}}, \code{\link[plotly]{ggplotly}}
#'
#' @export
plot_density <- function(data, x, #essential parameters
                         ..., #non-variable aesthetic specification
                         fill_var = NULL, colour_var = NULL, #grouping variable aesthetic mappings
                         xlab = NULL, ylab = NULL, title = NULL, fill_var_title = NULL, colour_var_title = NULL, #titles
                         xlim = c(NA, NA), transform_x = FALSE, x_transformation = "log10",#control the x axis limits and scaling
                         fill_var_order = NULL, colour_var_order = NULL, #modify grouping variable level order
                         fill_var_labs = NULL, colour_var_labs = NULL, #modify grouping variable labels
                         fill_var_values = NULL, colour_var_values = NULL, #manual colour specification
                         alpha = 0.6, greyscale = FALSE, #control transparency, convert to greyscale
                         theme = "classic", text_size = 14, font = c("sans", "serif", "mono"),#theme options
                         facet_var = NULL, facet_var_order = NULL, facet_var_labs = NULL, #facet options
                         facet_var_strip_position = c("top", "bottom"), facet_var_text_bold = TRUE, #facet aesthetic customization
                         legend_position = c("right", "left", "top", "bottom"), omit_legend = FALSE, #legend position
                         interactive = FALSE, aesthetic_options = FALSE) {#output format
  font <- match.arg(font)
  facet_var_strip_position <- match.arg(facet_var_strip_position)
  legend_position <- match.arg(legend_position)

  #fill variable recoding
  if(!missing(fill_var)){
    data <- data %>%
      dplyr::mutate({{fill_var}} := as.character({{fill_var}}))
  }
  if(!missing(fill_var) && !missing(fill_var_order)){
    data <- data %>%
      dplyr::mutate({{fill_var}} := forcats::fct_relevel({{fill_var}}, levels = !!!fill_var_order))
  }
  if(!missing(fill_var) && !missing(fill_var_labs)){
    data <- data %>%
      dplyr::mutate({{fill_var}} := forcats::fct_recode({{fill_var}}, !!!fill_var_labs))
  }

  #colour variable recoding
  if(!missing(colour_var)){
    data <- data %>%
      dplyr::mutate({{colour_var}} := as.character({{colour_var}}))
  }
  if(!missing(colour_var) && !missing(colour_var_order)){
    data <- data %>%
      dplyr::mutate({{colour_var}} := forcats::fct_relevel({{colour_var}}, levels = !!!colour_var_order))
  }
  if(!missing(colour_var) && !missing(colour_var_labs)){
    data <- data %>%
      dplyr::mutate({{colour_var}} := forcats::fct_recode({{colour_var}}, !!!colour_var_labs))
  }

  #facet label recoding
  if(!missing(facet_var)){
    data <- data %>%
      dplyr::mutate({{facet_var}} := as.character({{facet_var}}))
  }
  if(!missing(facet_var) && !missing(facet_var_order)){
    data <- data %>%
      dplyr::mutate({{facet_var}} := forcats::fct_relevel({{facet_var}}, levels = !!!facet_var_order))
  }
  if(!missing(facet_var) && !missing(facet_var_labs)){
    data <- data %>%
      dplyr::mutate({{facet_var}} := forcats::fct_recode({{facet_var}}, !!!facet_var_labs))
  }

  #setup foundational plotting object layer
  p <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = {{x}}, fill = {{fill_var}}, colour = {{colour_var}}))

  #add the geom layer
  p <- p +
    ggplot2::geom_density(alpha = alpha, ...)

  #modification of the colour or fill values
  if(!missing(fill_var_values)){
    p <- p +
      ggplot2::scale_fill_manual(values = fill_var_values)
  }
  if(!missing(colour_var_values)){
    p <- p +
      ggplot2::scale_colour_manual(values = colour_var_values)
  }

  #modification of x-axis limits
  if(!missing(xlim) && transform_x == FALSE){
    p <- p + ggplot2::lims(x = xlim)
  } else if (missing(xlim) && transform_x == TRUE){
    p <- p + ggplot2::scale_x_continuous(limits = c(NA, NA), trans = x_transformation)
  } else if (!missing(xlim) && transform_x  == TRUE){
    p <- p + ggplot2::scale_x_continuous(limits = c(xlim[1], xlim[2]), trans = x_transformation)
  }

  if(!missing(xlab)){
    p <- p + ggplot2::labs(x = xlab)
  }
  if(!missing(ylab)){
    p <- p + ggplot2::labs(y = ylab)
  }
  if(!missing(fill_var_title)){
    p <- p + ggplot2::labs(fill = fill_var_title)
  }
  if(!missing(colour_var_title)){
    p <- p + ggplot2::labs(colour = colour_var_title)
  }
  if(greyscale == TRUE){
    p <- p + ggplot2::scale_fill_grey()
  }
  if(!missing(title)){
    p <- p + ggplot2::ggtitle(title)
  }
  if(theme == "classic"){
    p <- p + ggplot2::theme_classic(base_size = text_size, base_family = font)
  } else if (theme == "bw"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "b & w"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "black and white"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "black & white"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "grey"){
    p <- p + ggplot2::theme_grey(base_size = text_size, base_family = font)
  } else if (theme == "gray"){
    p <- p + ggplot2::theme_gray(base_size = text_size, base_family = font)
  } else if (theme == "light"){
    p <- p + ggplot2::theme_light(base_size = text_size, base_family = font)
  } else if (theme == "dark"){
    p <- p + ggplot2::theme_dark(base_size = text_size, base_family = font)
  } else if (theme == "minimal"){
    p <- p + ggplot2::theme_minimal(base_size = text_size, base_family = font)
  }
  if(omit_legend == TRUE){
    p <- p + ggplot2::theme(legend.position = "none")
  }
  if(legend_position != "right"){
    p <- p + ggplot2::theme(legend.position = legend_position)
  }
  if(!missing(facet_var)){
    p <- p + ggplot2::facet_wrap(vars({{facet_var}}), strip.position = facet_var_strip_position)
  }
  if(!missing(facet_var) && facet_var_text_bold == TRUE){
    p <- p + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
  }
  if(aesthetic_options == TRUE){
    utils::browseURL("https://ggplot2.tidyverse.org/articles/ggplot2-specs.html")
  }
  if(interactive == TRUE){
    return(plotly::ggplotly(p))
  }
  if(interactive == FALSE){
    return(p)
  }
}
# plot_histogram -----------------------------------------------------
#' @title
#'
#' Generate a histogram.
#'
#' @description Easily generate a histogram of a variable using
#'   ggplot2 with a simplified customization interface for common modifications
#'   with static (ggplot) and interactive (plotly) output options. The static
#'   output is useful for producing static reports (e.g. for manuscripts) and is
#'   readily customized further using ggplot2 syntax. The interactive output is
#'   helpful for exploring the data and producing dynamic html reports.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom forcats fct_relevel
#' @importFrom forcats fct_recode
#' @importFrom rlang !!!
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_histogram
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_fill_grey
#' @importFrom ggplot2 lims
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 theme_grey
#' @importFrom ggplot2 theme_gray
#' @importFrom ggplot2 theme_light
#' @importFrom ggplot2 theme_dark
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 element_text
#' @importFrom plotly ggplotly
#' @importFrom utils browseURL
#'
#' @param data A data frame or tibble containing the dependent measure "x" and
#'   any grouping variables.
#'
#' @param x The numeric variable you want a histogram of.
#'
#' @param ... graphical parameters (not associated with variables) to be passed
#'   to \code{\link[ggplot2]{geom_histogram}}, e.g. colour or fill, to be applied
#'   to all bars. To see some of the available options in a web browser, set
#'   the aesthetic_options argument to TRUE.
#'
#' @param binwidth Determines the aggregation level for bin construction in
#'   units of x. Default = 1.
#'
#' @param bins Instead of specifying the binsize using binwidth, you can specify
#'   a total number of bins to display. If you choose to do so, set binwidth =
#'   NULL.
#'
#' @param position Determines how bars are organized when a grouping variable is
#'   assigned to fill or colour. Valid options include "identity" (the
#'   default),"stack", and "dodge"
#'
#' @param stat Determines how the y-axis is constructed. Typically one would not
#'   change this from the default of "bin" for a histogram, although "count" is
#'   another sensible option. See \code{\link[ggplot2]{stat_bin}} &
#'   \code{\link[ggplot2]{stat_count}} for details.
#'
#' @param na.rm If set to TRUE (the default), missing values will be omitted
#'   before plotting.
#'
#' @param fill_var Use if you want to assign a variable to the histogram bar
#'   fill colour, e.g. fill_var = grouping_variable. Produces separate sets of
#'   bars for each level of the fill variable. See \code{\link[ggplot2]{aes}}
#'   for details.
#'
#' @param colour_var Use if you want to assign a variable to the histogram bar
#'   outline colour, e.g. colour_var = grouping_variable. Produces separate sets
#'   of bars for each level of the colour variable. See
#'   \code{\link[ggplot2]{aes}} for details.
#'
#' @param xlab Specify/overwrite the x-axis label using a character string, e.g.
#'   "x-axis label"
#'
#' @param ylab Specify/overwrite the y-axis label using a character string, e.g.
#'   "y-axis label"
#'
#' @param title Add a main title to the plot using a character string, e.g.
#'   "Distribution of X"
#'
#' @param fill_var_title If a variable has been assigned to fill using fill_var,
#'   this allows you to modify the variable label in the plot legend.
#'
#' @param colour_var_title If a variable has been assigned to colour using
#'   colour_var, this allows you to modify the variable label in the plot
#'   legend.
#'
#' @param xlim specify the x-axis limits, e.g. xlim = c(lower_limit,
#'   upper_limit). Use NA for the existing minimum or maximum value of x, e.g.
#'   the default is xlim = c(NA, NA)
#'
#' @param transform_x Would you like to transform the x axis? (TRUE or FALSE)
#'
#' @param x_transformation If transform_x = TRUE, this determines the
#'   transformation to be applied. Common choices include "log10" (the default),
#'   "log2", "sqrt", or "exp". See \code{\link[ggplot2]{scale_continuous}} for
#'   details.
#'
#' @param fill_var_order If a variable has been assigned to fill using fill_var,
#'   this allows you to modify the order of the variable groups, e.g. fill_var =
#'   grouping_variable, fill_var_order = c("group_2", "group_1"). See
#'   \code{\link[forcats]{fct_relevel}} for details.
#'
#' @param colour_var_order If a variable has been assigned to colour using
#'   colour_var, this allows you to modify the order of the variable groups,
#'   e.g. colour_var = grouping_variable, fill_var_order = c("group_2",
#'   "group_1"). See \code{\link[forcats]{fct_relevel}} for details.
#'
#' @param fill_var_labs If a variable has been assigned to fill using fill_var,
#'   this allows you to modify the labels of the variable groups, e.g. fill_var
#'   = grouping_variable, fill_var_labs = c("group_1_new_label" =
#'   "group_1_old_label", "group_2_new_label" = "group_2_old_label"). See
#'   \code{\link[forcats]{fct_recode}} for details.
#'
#' @param colour_var_labs If a variable has been assigned to colour using
#'   colour_var, this allows you to modify the labels of the variable groups,
#'   e.g. colour_var = grouping_variable, colour_var_labs =
#'   c("group_1_new_label" = "group_1_old_label", "group_2_new_label" =
#'   "group_2_old_label"). See \code{\link[forcats]{fct_recode}} for details.
#'
#' @param fill_var_values If a variable has been assigned to fill using
#'   fill_var, this allows you to modify the colours assigned to the fill of
#'   each of the variable groups, e.g. fill_var = grouping_variable,
#'   fill_var_values = c("blue", "red"). See
#'   \code{\link[ggplot2]{scale_fill_manual}} for details. For the colour
#'   options available in base R, see \code{\link[elucidate]{colour_options}}.
#'
#' @param colour_var_values If a variable has been assigned to colour using
#'   colour_var, this allows you to modify the colours assigned to the outline
#'   of each of the variable groups, e.g. colour_var = grouping_variable,
#'   colour_var_values = c("blue", "red"). See
#'   \code{\link[ggplot2]{scale_fill_manual}} for details. For the colour
#'   options available in base R, see \code{\link[elucidate]{colour_options}}.
#'
#' @param alpha This adjusts the transparency/opacity of the graphical
#'   components of the plot, ranging from 0 = 100% transparent to 1 = 100%
#'   opaque.
#'
#' @param greyscale Set to TRUE if you want the plot converted to greyscale.
#'
#' @param theme Adjusts the theme using 1 of 6 predefined "complete" theme
#'   templates provided by ggplot2. Currenlty supported options are: "classic"
#'   (the elucidate default), "bw", "grey" (the ggplot2 default), "light",
#'   "dark", & "minimal". See \code{\link[ggplot2]{theme_classic}} for more
#'   information.
#'
#' @param text_size This controls the size of all plot text. Default = 14.
#'
#' @param font This controls the font of all plot text. Default = "sans"
#'   (Arial). Other options include "serif" (Times New Roman) and "mono" (Courier
#'   New).
#'
#' @param facet_var Use if you want separate plots for each level of a grouping
#'   variable (i.e. a facetted plot), e.g. facet_var = grouping_variable. See
#'   \code{\link[ggplot2]{facet_wrap}} for details.
#'
#' @param facet_var_order If a variable has been assigned for facetting using
#'   facet_var, this allows you to modify the order of the variable groups, e.g.
#'   facet_var = grouping_variable, facet_var_order = c("group_2", "group_1").
#'   See \code{\link[forcats]{fct_relevel}} for details.
#'
#' @param facet_var_labs If a variable has been assigned for facetting using
#'   facet_var, this allows you to modify the labels of the variable groups
#'   which will appear in the facet strips, e.g. facet_var = grouping_variable,
#'   facet_var_labs = c("group_1_new_label" = "group_1_old_label",
#'   "group_2_new_label" = "group_2_old_label"). See
#'   \code{\link[forcats]{fct_recode}} for details.
#'
#' @param facet_var_strip_position If a variable has been assigned for facetting
#'   using facet_var, this allows you to modify the position of the facet strip
#'   labels. Sensible options include "top" (the default) or "bottom".
#'
#' @param facet_var_text_bold If a variable has been assigned for facetting
#'   using facet_var, this allows you to use boldface (TRUE/default or FALSE)
#'   for the facet strip label text.
#'
#' @param legend_position This allows you to modify the legend position.
#'   Options include "right" (the default), "left", "top", & "bottom".
#'
#' @param omit_legend Set to TRUE if you want to remove/omit the legends.
#'
#' @param interactive Determines whether a static ggplot object or an interactive html
#'   plotly object is returned. See \code{\link[plotly]{ggplotly}} for details.
#'
#' @param aesthetic_options If set to TRUE, opens a web browser to the tidyverse
#'   online aesthetic options vignette.
#'
#' @return A ggplot object or plotly object depending on whether static or
#'   interactive output was requested.
#'
#' @author Craig P. Hutton, \email{Craig.Hutton@@gov.bc.ca}
#'
#' @examples
#' data(mtcars) #load the mtcars data
#'
#' library(magrittr)
#'
#' mtcars %>% plot_histogram(x = mpg, fill = "blue")
#'
#' \dontrun{
#' mtcars %>% plot_histogram(x = mpg, fill = "blue")
#'
#' mtcars %>% plot_histogram(x = mpg, fill = "blue", colour = "black")
#'
#' mtcars %>% plot_histogram(x = mpg, colour_var = cyl, fill = "white")
#'
#' mtcars %>% plot_histogram(x = mpg, fill_var = cyl, position = "identity") #default in elucidate
#'
#' mtcars %>% plot_histogram(x = mpg, fill_var = cyl, position = "dodge", binwidth = 1)
#'
#' mtcars %>% plot_histogram(x = mpg, fill_var = cyl, position = "stack") #default in ggplot2
#'
#' mtcars %>% plot_histogram(x = mpg, fill = "blue", binwidth = 5)
#'
#' mtcars %>% plot_histogram(x = mpg, fill = "blue", binwidth = NULL, bins = 30) #default in ggplot2
#'
#' mtcars %>% plot_histogram(x = mpg, fill = "blue", interactive = T)
#'
#' mtcars %>% plot_histogram(x = mpg, fill_var = cyl, binwidth = 5, interactive = T)
#' }
#'
#'
#' @references
#' Wickham, H. (2016). ggplot2: elegant graphics for data analysis. New York, N.Y.: Springer-Verlag.
#'
#' @seealso \code{\link[ggplot2]{geom_histogram}}, \code{\link[plotly]{ggplotly}}
#'
#' @export
plot_histogram <- function(data, x, #essential parameters
                           ..., #non-variable aestheic specification
                           binwidth = 1, bins = NULL, #geom specific customization params
                           position = c("identity", "stack", "dodge"),
                           stat = c("bin", "count"), na.rm = TRUE, #geom specific customization params.
                           fill_var = NULL, colour_var = NULL, #grouping variable aesthetic mappings
                           xlab = NULL, ylab = NULL, title = NULL, fill_var_title = NULL, colour_var_title = NULL, #titles
                           xlim = c(NA, NA), transform_x = FALSE, x_transformation = "log10",#control the x axis limits and scaling
                           fill_var_order = NULL, colour_var_order = NULL, #modify grouping variable level order
                           fill_var_labs = NULL, colour_var_labs = NULL, #modify grouping variable labels
                           fill_var_values = NULL, colour_var_values = NULL, #manual colour specification
                           alpha = 0.6, greyscale = FALSE, #control transparency, convert to greyscale
                           theme = "classic", text_size = 14, font = c("sans", "serif", "mono"),#theme options
                           facet_var = NULL, facet_var_order = NULL, facet_var_labs = NULL, #facet options
                           facet_var_strip_position = c("top", "bottom"), facet_var_text_bold = TRUE, #facet aesthetic customization
                           legend_position = c("right", "left", "top", "bottom"), omit_legend = FALSE, #legend position
                           interactive = FALSE, aesthetic_options = FALSE) {#output format

  position <- match.arg(position)
  stat <- match.arg(stat)
  font <- match.arg(font)
  legend_position <- match.arg(legend_position)
  facet_var_strip_position <- match.arg(facet_var_strip_position)

  #fill variable recoding
  if(!missing(fill_var)){
    data <- data %>%
      dplyr::mutate({{fill_var}} := as.character({{fill_var}}))
  }
  if(!missing(fill_var) && !missing(fill_var_order)){
    data <- data %>%
      dplyr::mutate({{fill_var}} := forcats::fct_relevel({{fill_var}}, levels = !!!fill_var_order))
  }
  if(!missing(fill_var) && !missing(fill_var_labs)){
    data <- data %>%
      dplyr::mutate({{fill_var}} := forcats::fct_recode({{fill_var}}, !!!fill_var_labs))
  }

  #colour variable recoding
  if(!missing(colour_var)){
    data <- data %>%
      dplyr::mutate({{colour_var}} := as.character({{colour_var}}))
  }
  if(!missing(colour_var) && !missing(colour_var_order)){
    data <- data %>%
      dplyr::mutate({{colour_var}} := forcats::fct_relevel({{colour_var}}, levels = !!!colour_var_order))
  }
  if(!missing(colour_var) && !missing(colour_var_labs)){
    data <- data %>%
      dplyr::mutate({{colour_var}} := forcats::fct_recode({{colour_var}}, !!!colour_var_labs))
  }

  #facet label recoding
  if(!missing(facet_var)){
    data <- data %>%
      dplyr::mutate({{facet_var}} := as.character({{facet_var}}))
  }
  if(!missing(facet_var) && !missing(facet_var_order)){
    data <- data %>%
      dplyr::mutate({{facet_var}} := forcats::fct_relevel({{facet_var}}, levels = !!!facet_var_order))
  }
  if(!missing(facet_var) && !missing(facet_var_labs)){
    data <- data %>%
      dplyr::mutate({{facet_var}} := forcats::fct_recode({{facet_var}}, !!!facet_var_labs))
  }

  #setup foundational plotting object layer
  p <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = {{x}}, fill = {{fill_var}}, colour = {{colour_var}}))

  #add the geom layer
  p <- p +
    ggplot2::geom_histogram(alpha = alpha, binwidth = binwidth, bins = bins,
                            position = position, stat = stat, na.rm = na.rm, ...)

  #modification of the colour or fill values
  if(!missing(fill_var_values)){
    p <- p +
      ggplot2::scale_fill_manual(values = fill_var_values)
  }
  if(!missing(colour_var_values)){
    p <- p +
      ggplot2::scale_colour_manual(values = colour_var_values)
  }

  #modification of x-axis limits
  if(!missing(xlim) && transform_x == FALSE){
    p <- p + ggplot2::lims(x = xlim)
  } else if (missing(xlim) && transform_x == TRUE){
    p <- p + ggplot2::scale_x_continuous(limits = c(NA, NA), trans = x_transformation)
  } else if (!missing(xlim) && transform_x  == TRUE){
    p <- p + ggplot2::scale_x_continuous(limits = c(xlim[1], xlim[2]), trans = x_transformation)
  }

  if(!missing(xlab)){
    p <- p + ggplot2::labs(x = xlab)
  }
  if(!missing(ylab)){
    p <- p + ggplot2::labs(y = xlab)
  }
  if(!missing(fill_var_title)){
    p <- p + ggplot2::labs(fill = fill_var_title)
  }
  if(!missing(colour_var_title)){
    p <- p + ggplot2::labs(colour = colour_var_title)
  }
  if(greyscale == TRUE){
    p <- p + ggplot2::scale_fill_grey()
  }
  if(!missing(title)){
    p <- p + ggplot2::ggtitle(title)
  }
  if(theme == "classic"){
    p <- p + ggplot2::theme_classic(base_size = text_size, base_family = font)
  } else if (theme == "bw"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "b & w"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "black and white"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "black && white"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "grey"){
    p <- p + ggplot2::theme_grey(base_size = text_size, base_family = font)
  } else if (theme == "gray"){
    p <- p + ggplot2::theme_gray(base_size = text_size, base_family = font)
  } else if (theme == "light"){
    p <- p + ggplot2::theme_light(base_size = text_size, base_family = font)
  } else if (theme == "dark"){
    p <- p + ggplot2::theme_dark(base_size = text_size, base_family = font)
  } else if (theme == "minimal"){
    p <- p + ggplot2::theme_minimal(base_size = text_size, base_family = font)
  }
  if(omit_legend == TRUE){
    p <- p + ggplot2::theme(legend.position = "none")
  }
  if(legend_position != "right"){
    p <- p + ggplot2::theme(legend.position = legend_position)
  }
  if(!missing(facet_var)){
    p <- p + ggplot2::facet_wrap(vars({{facet_var}}), strip.position = facet_var_strip_position)
  }
  if(!missing(facet_var) && facet_var_text_bold == TRUE){
    p <- p + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
  }
  if(aesthetic_options == TRUE){
    utils::browseURL("https://ggplot2.tidyverse.org/articles/ggplot2-specs.html")
  }
  if(interactive == TRUE){
    return(plotly::ggplotly(p))
  }
  if(interactive == FALSE){
    return(p)
  }
}

# start of plot_box ----------------------------------------------------------------
#' @title
#'
#' Generate a box-and-whisker plot.
#'
#' @description Easily generate box-and-whisker plots using ggplot2 with a
#'   simplified customization interface for common modifications with static
#'   (ggplot) and interactive (plotly) output options. The static output is
#'   useful for producing static reports (e.g. for manuscripts) and is readily
#'   customized further using ggplot2 syntax. The interactive output is helpful
#'   for exploring the data and producing dynamic html reports.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom forcats fct_relevel
#' @importFrom forcats fct_recode
#' @importFrom rlang !!!
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_fill_grey
#' @importFrom ggplot2 lims
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 theme_grey
#' @importFrom ggplot2 theme_gray
#' @importFrom ggplot2 theme_light
#' @importFrom ggplot2 theme_dark
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 element_text
#' @importFrom plotly ggplotly
#' @importFrom utils browseURL
#'
#' @param data A data frame or tibble containing the dependent measure "y" and
#'   any grouping variables.
#'
#' @param y A numeric variable you want to obtain boxplots for.
#'
#' @param x A categorical variable you want to obtain separate boxplots of y for.
#'
#' @param ... graphical parameters (not associated with variables) to be passed
#'   to \code{\link[ggplot2]{geom_boxplot}}, e.g. colour or fill, to be applied
#'   to all bars. To see some of the available options in a web browser, set the
#'   aesthetic_options argument to TRUE.
#'
#' @param fill_var Use if you want to assign a variable to the box fill colour,
#'   e.g. fill_var = grouping_variable. Produces separate sets of boxes for each
#'   level of the fill variable. See \code{\link[ggplot2]{aes}} for details.
#'
#' @param colour_var Use if you want to assign a variable to the box outline
#'   colour, e.g. colour_var = grouping_variable. Produces separate sets of
#'   boxes for each level of the colour variable. See \code{\link[ggplot2]{aes}}
#'   for details.
#'
#' @param xlab Specify/overwrite the x-axis label using a character string, e.g.
#'   "x-axis label"
#'
#' @param ylab Specify/overwrite the y-axis label using a character string, e.g.
#'   "y-axis label"
#'
#' @param title Add a main title to the plot using a character string, e.g.
#'   "boxplots graph of y for each group of x"
#'
#' @param fill_var_title If a variable has been assigned to fill using fill_var,
#'   this allows you to modify the variable label in the plot legend.
#'
#' @param colour_var_title If a variable has been assigned to colour using
#'   colour_var, this allows you to modify the variable label in the plot
#'   legend.
#'
#' @param ylim specify the y-axis limits, e.g. ylim = c(lower_limit,
#'   upper_limit). Use NA for the existing minimum or maximum value of y, e.g.
#'   the default is ylim = c(NA, NA)
#'
#' @param transform_y Would you like to transform the y axis? (TRUE or FALSE)
#'
#' @param y_transformation If transform_y = TRUE, this determines the
#'   transformation to be applied. Common choices include "log10" (the default),
#'   "log2", "sqrt", or "exp". See \code{\link[ggplot2]{scale_continuous}} for
#'   details.
#'
#' @param x_var_order If a variable has been assigned to x, this allows you to
#'   modify the order of the variable groups, e.g. x = grouping_variable,
#'   x_var_order = c("group_2", "group_1"). See
#'   \code{\link[forcats]{fct_relevel}} for details.
#'
#' @param fill_var_order If a variable has been assigned to fill using fill_var,
#'   this allows you to modify the order of the variable groups, e.g. fill_var =
#'   grouping_variable, fill_var_order = c("group_2", "group_1"). See
#'   \code{\link[forcats]{fct_relevel}} for details.
#'
#' @param colour_var_order If a variable has been assigned to colour using
#'   colour_var, this allows you to modify the order of the variable groups,
#'   e.g. colour_var = grouping_variable, fill_var_order = c("group_2",
#'   "group_1"). See \code{\link[forcats]{fct_relevel}} for details.
#'
#' @param x_var_labs If a variable has been assigned to x, this allows you to
#'   modify the labels of the variable groups, e.g. x = grouping_variable,
#'   x_var_labs = c("group_1_new_label" = "group_1_old_label",
#'   "group_2_new_label" = "group_2_old_label"). See
#'   \code{\link[forcats]{fct_recode}} for details.
#'
#' @param fill_var_labs If a variable has been assigned to fill using fill_var,
#'   this allows you to modify the labels of the variable groups, e.g. fill_var
#'   = grouping_variable, fill_var_labs = c("group_1_new_label" =
#'   "group_1_old_label", "group_2_new_label" = "group_2_old_label"). See
#'   \code{\link[forcats]{fct_recode}} for details.
#'
#' @param colour_var_labs If a variable has been assigned to colour using
#'   colour_var, this allows you to modify the labels of the variable groups,
#'   e.g. colour_var = grouping_variable, colour_var_labs =
#'   c("group_1_new_label" = "group_1_old_label", "group_2_new_label" =
#'   "group_2_old_label"). See \code{\link[forcats]{fct_recode}} for details.
#'
#' @param fill_var_values If a variable has been assigned to fill using
#'   fill_var, this allows you to modify the colours assigned to the fill of
#'   each of the variable groups, e.g. fill_var = grouping_variable,
#'   fill_var_values = c("blue", "red"). See
#'   \code{\link[ggplot2]{scale_fill_manual}} for details. For the colour
#'   options available in base R, see \code{\link[elucidate]{colour_options}}.
#'
#' @param colour_var_values If a variable has been assigned to colour using
#'   colour_var, this allows you to modify the colours assigned to the outline
#'   of each of the variable groups, e.g. colour_var = grouping_variable,
#'   colour_var_values = c("blue", "red"). See
#'   \code{\link[ggplot2]{scale_fill_manual}} for details. For the colour
#'   options available in base R, see \code{\link[elucidate]{colour_options}}.
#'
#' @param alpha This adjusts the transparency/opacity of the graphical
#'   components of the plot, ranging from 0 = 100% transparent to 1 = 100%
#'   opaque.
#'
#' @param greyscale Set to TRUE if you want the plot converted to greyscale.
#'
#' @param theme Adjusts the theme using 1 of 6 predefined "complete" theme
#'   templates provided by ggplot2. Currenlty supported options are: "classic"
#'   (the elucidate default), "bw", "grey" (the ggplot2 default), "light",
#'   "dark", & "minimal". See \code{\link[ggplot2]{theme_classic}} for more
#'   information.
#'
#' @param text_size This controls the size of all plot text. Default = 14.
#'
#' @param font This controls the font of all plot text. Default = "sans"
#'   (Arial). Other options include "serif" (Times New Roman) and "mono" (Courier
#'   New).
#'
#' @param facet_var Use if you want separate plots for each level of a grouping
#'   variable (i.e. a facetted plot), e.g. facet_var = grouping_variable. See
#'   \code{\link[ggplot2]{facet_wrap}} for details.
#'
#' @param facet_var_order If a variable has been assigned for facetting using
#'   facet_var, this allows you to modify the order of the variable groups, e.g.
#'   facet_var = grouping_variable, facet_var_order = c("group_2", "group_1").
#'   See \code{\link[forcats]{fct_relevel}} for details.
#'
#' @param facet_var_labs If a variable has been assigned for facetting using
#'   facet_var, this allows you to modify the labels of the variable groups
#'   which will appear in the facet strips, e.g. facet_var = grouping_variable,
#'   facet_var_labs = c("group_1_new_label" = "group_1_old_label",
#'   "group_2_new_label" = "group_2_old_label"). See
#'   \code{\link[forcats]{fct_recode}} for details.
#'
#' @param facet_var_strip_position If a variable has been assigned for facetting
#'   using facet_var, this allows you to modify the position of the facet strip
#'   labels. Sensible options include "top" (the default) or "bottom".
#'
#' @param facet_var_text_bold If a variable has been assigned for facetting
#'   using facet_var, this allows you to use boldface (TRUE/default or FALSE)
#'   for the facet strip label text.
#'
#' @param legend_position This allows you to modify the legend position.
#'   Options include "right" (the default), "left", "top", & "bottom".
#'
#' @param omit_legend Set to TRUE if you want to remove/omit the legends.
#'
#' @param interactive Determines whether a static ggplot object or an interactive html
#'   plotly object is returned. See \code{\link[plotly]{ggplotly}} for details.
#'
#' @param aesthetic_options If set to TRUE, opens a web browser to the tidyverse
#'   online aesthetic options vignette.
#'
#' @return A ggplot object or plotly object depending on whether static or
#'   interactive output was requested.
#'
#' @author Craig P. Hutton, \email{Craig.Hutton@@gov.bc.ca}
#'
#' @examples
#' data(mtcars) #load the mtcars data
#'
#' library(magrittr)
#'
#' mtcars %>% plot_box(y = mpg, x = cyl, fill = "blue")
#'
#' \dontrun{
#' mtcars %>%
#'  plot_box(x = cyl, y = hp,
#'           xlab = "# of cylinders",
#'           ylab = "horsepower",
#'           fill_var = am,
#'           fill_var_title = "transmission",
#'           fill_var_labs = c("manual" = "0", "automatic" = "1"),
#'           fill_var_values = c("blue", "red"),
#'           theme = "bw")
#'
#' #modifying fill doesn't work as well for the interactive version of a boxplot
#' mtcars %>%
#'  plot_box(x = cyl, y = hp,
#'           xlab = "# of cylinders",
#'           ylab = "horsepower",
#'           fill_var = am,
#'           fill_var_title = "transmission",
#'           fill_var_labs = c("manual" = "0", "automatic" = "1"),
#'           fill_var_values = c("blue", "red"),
#'           theme = "bw",
#'           interactive = T)
#'
#' #using colour works better for the interactive version
#' mtcars %>%
#'  plot_box(x = cyl, y = hp,
#'           xlab = "# of cylinders",
#'           ylab = "horsepower",
#'           colour_var = am,
#'           colour_var_title = "transmission",
#'           colour_var_labs = c("manual" = "0", "automatic" = "1"),
#'           colour_var_values = c("blue", "red"),
#'           theme = "bw", interactive = T)
#' }
#'
#'
#' @references
#' Wickham, H. (2016). ggplot2: elegant graphics for data analysis. New York, N.Y.: Springer-Verlag.
#'
#' @seealso \code{\link[ggplot2]{geom_boxplot}}, \code{\link[plotly]{ggplotly}}
#'
#' @export
plot_box <- function(data, y,#essential parameters
                     x = NULL,
                     ..., #geom-specific customization see ?geom_boxplot for details
                     fill_var = NULL, colour_var = NULL, #grouping variable aesthetic mappings
                     xlab = NULL, ylab = NULL, title = NULL,
                     fill_var_title = NULL, colour_var_title = NULL, #titles
                     ylim = c(NA, NA), transform_y = FALSE, y_transformation = "log10", #control the y axis limits and scaling
                     x_var_order = NULL, x_var_labs = NULL,
                     fill_var_order = NULL, colour_var_order = NULL, #modify grouping variable level order
                     fill_var_labs = NULL, colour_var_labs = NULL, #modify grouping variable labels
                     fill_var_values = NULL, colour_var_values = NULL, #manual colour specification
                     alpha = 0.6, greyscale = FALSE, #control transparency, convert to greyscale
                     theme = "classic", text_size = 14, font = c("sans", "serif", "mono"), #theme options
                     facet_var = NULL, facet_var_order = NULL, facet_var_labs = NULL, #facet options
                     facet_var_strip_position = c("top", "bottom"), facet_var_text_bold = TRUE, #facet aesthetic customization
                     legend_position = c("right", "left", "top", "bottom"), omit_legend = FALSE, #legend position
                     interactive = FALSE, aesthetic_options = FALSE) {#output format

  font <- match.arg(font)
  legend_position <- match.arg(legend_position)
  facet_var_strip_position <- match.arg(facet_var_strip_position)

  #x-variable recoding
  if(!missing(x)){
    data <- data %>%
      dplyr::mutate({{x}} := as.character({{x}}))
  }
  if(!missing(x_var_order)){
    data <- data %>%
      dplyr::mutate({{x}} := forcats::fct_relevel({{x}}, levels = !!!x_var_order))
  }
  if(!missing(x_var_labs)){
    data <- data %>%
      dplyr::mutate({{x}} := forcats::fct_recode({{x}}, !!!x_var_labs))
  }

  #fill variable recoding
  if(!missing(fill_var)){
    data <- data %>%
      dplyr::mutate({{fill_var}} := as.character({{fill_var}}))
  }
  if(!missing(fill_var) & !missing(fill_var_order)){
    data <- data %>%
      dplyr::mutate({{fill_var}} := forcats::fct_relevel({{fill_var}}, levels = !!!fill_var_order))
  }
  if(!missing(fill_var) & !missing(fill_var_labs)){
    data <- data %>%
      dplyr::mutate({{fill_var}} := forcats::fct_recode({{fill_var}}, !!!fill_var_labs))
  }

  #colour variable recoding
  if(!missing(colour_var)){
    data <- data %>%
      dplyr::mutate({{colour_var}} := as.character({{colour_var}}))
  }
  if(!missing(colour_var) & !missing(colour_var_order)){
    data <- data %>%
      dplyr::mutate({{colour_var}} := forcats::fct_relevel({{colour_var}}, levels = !!!colour_var_order))
  }
  if(!missing(colour_var) & !missing(colour_var_labs)){
    data <- data %>%
      dplyr::mutate({{colour_var}} := forcats::fct_recode({{colour_var}}, !!!colour_var_labs))
  }

  #facet label recoding
  if(!missing(facet_var)){
    data <- data %>%
      dplyr::mutate({{facet_var}} := as.character({{facet_var}}))
  }
  if(!missing(facet_var) & !missing(facet_var_order)){
    data <- data %>%
      dplyr::mutate({{facet_var}} := forcats::fct_relevel({{facet_var}}, levels = !!!facet_var_order))
  }
  if(!missing(facet_var) & !missing(facet_var_labs)){
    data <- data %>%
      dplyr::mutate({{facet_var}} := forcats::fct_recode({{facet_var}}, !!!facet_var_labs))
  }

  #setup foundational plotting object layer
  p <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = {{x}}, y = {{y}}, fill = {{fill_var}}, colour = {{colour_var}}))

  #add the geom layer
  p <- p +
    ggplot2::geom_boxplot(alpha = alpha, ...)

  #modification of the colour or fill values
  if (!missing(fill_var) & missing(colour_var)){
    if(!missing(fill_var_values)){
      p <- p +
        ggplot2::scale_fill_manual(values = fill_var_values)
    }
  } else if(missing(fill_var) & !missing(colour_var)) {
    if(!missing(colour_var_values)){
      p <- p +
        ggplot2::scale_colour_manual(values = colour_var_values)
    }
  } else if(!missing(fill_var) & !missing(colour_var)) {
    if(!missing(fill_var_values) & missing(colour_var_values)){
      p <- p +
        ggplot2::scale_fill_manual(values = fill_var_values)
    } else if(missing(fill_var_values) & !missing(colour_var_values)) {
      p <- p +
        ggplot2::scale_colour_manual(values = colour_var_values)
    } else if(!missing(fill_var_values) & !missing(colour_var_values)) {
      p <- p +
        ggplot2::scale_fill_manual(values = fill_var_values) +
        ggplot2::scale_colour_manual(values = colour_var_values)
    }
  }

  #modification of y-axis limits & transformations
  if(!missing(ylim) & transform_y == FALSE){
    p <- p + ggplot2::lims(y = ylim)
  } else if (missing(ylim) & transform_y == TRUE){
    p <- p + ggplot2::scale_y_continuous(limits = c(NA, NA), trans = y_transformation)
  } else if (!missing(ylim) & transform_y  == TRUE){
    p <- p + ggplot2::scale_y_continuous(limits = c(ylim[1], ylim[2]), trans = y_transformation)
  }

  #modification of axis labels
  if(!missing(xlab)){
    p <- p + ggplot2::labs(x = xlab)
  }
  if(!missing(ylab)){
    p <- p + ggplot2::labs(y = ylab)
  }
  if(!missing(fill_var_title)){
    p <- p + ggplot2::labs(fill = fill_var_title)
  }
  if(!missing(colour_var_title)){
    p <- p + ggplot2::labs(color = colour_var_title)
  }

  if(greyscale == TRUE){
    p <- p + ggplot2::scale_fill_grey()
  }
  if(!missing(title)){
    p <- p + ggplot2::ggtitle(title)
  }
  if(theme == "classic"){
    p <- p + ggplot2::theme_classic(base_size = text_size, base_family = font)
  } else if (theme == "bw"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "b & w"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "black and white"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "black & white"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "grey"){
    p <- p + ggplot2::theme_grey(base_size = text_size, base_family = font)
  } else if (theme == "gray"){
    p <- p + ggplot2::theme_gray(base_size = text_size, base_family = font)
  } else if (theme == "light"){
    p <- p + ggplot2::theme_light(base_size = text_size, base_family = font)
  } else if (theme == "dark"){
    p <- p + ggplot2::theme_dark(base_size = text_size, base_family = font)
  } else if (theme == "minimal"){
    p <- p + ggplot2::theme_minimal(base_size = text_size, base_family = font)
  }
  if(omit_legend == TRUE){
    p <- p + ggplot2::theme(legend.position = "none")
  }
  if(legend_position != "right"){
    p <- p + ggplot2::theme(legend.position = legend_position)
  }
  if(!missing(facet_var)){
    p <- p + ggplot2::facet_wrap(vars({{facet_var}}), strip.position = facet_var_strip_position)
  }
  if(!missing(facet_var) & facet_var_text_bold == TRUE){
    p <- p + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
  }
  if(aesthetic_options == TRUE){
    utils::browseURL("https://ggplot2.tidyverse.org/articles/ggplot2-specs.html")
  }
  if(interactive == TRUE){
    return(plotly::ggplotly(p))
  }
  if(interactive == FALSE){
    return(p)
  }
}

# start of plot_violin -------------------------------------------------------
#' @title
#'
#' Generate a violin plot.
#'
#' @description Easily generate violin plots using ggplot2 with a simplified
#'   customization interface for common modifications with static (ggplot) and
#'   interactive (plotly) output options. The static output is useful for
#'   producing static reports (e.g. for manuscripts) and is readily customized
#'   further using ggplot2 syntax. The interactive output is helpful for
#'   exploring the data and producing dynamic html reports.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom forcats fct_relevel
#' @importFrom forcats fct_recode
#' @importFrom rlang !!!
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_violin
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_fill_grey
#' @importFrom ggplot2 lims
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 theme_grey
#' @importFrom ggplot2 theme_gray
#' @importFrom ggplot2 theme_light
#' @importFrom ggplot2 theme_dark
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 element_text
#' @importFrom plotly ggplotly
#' @importFrom utils browseURL
#'
#' @param data A data frame or tibble containing the dependent measure "y" and
#'   any grouping variables.
#'
#' @param y A numeric variable you want to obtain violin plots for.
#'
#' @param x A categorical variable you want to obtain separate violin plots of y for.
#'
#' @param ... graphical parameters (not associated with variables) to be passed
#'   to \code{\link[ggplot2]{geom_violin}}, e.g. colour or fill, to be applied
#'   to all bars. To see some of the available options in a web browser, set the
#'   aesthetic_options argument to TRUE. An option unique to geom_violin is
#'   draw_quantiles, which adds horizonal lines for the specified quantiles,
#'   e.g. draw_quantiles = c(0.25, 0.5, 0.75) would add lines for the 25th,
#'   50th, and 75th percentiles (similar to a boxplot).
#'
#' @param fill_var Use if you want to assign a variable to the violin fill
#'   colour, e.g. fill_var = grouping_variable. Produces separate sets of bars
#'   for each level of the fill variable. See \code{\link[ggplot2]{aes}} for
#'   details.
#'
#' @param colour_var Use if you want to assign a variable to the violin
#'   outline colour, e.g. colour_var = grouping_variable. Produces separate sets
#'   of bars for each level of the colour variable. See
#'   \code{\link[ggplot2]{aes}} for details.
#'
#' @param xlab Specify/overwrite the x-axis label using a character string, e.g.
#'   "x-axis label"
#'
#' @param ylab Specify/overwrite the y-axis label using a character string, e.g.
#'   "y-axis label"
#'
#' @param title Add a main title to the plot using a character string, e.g.
#'   "Violin graph of X"
#'
#' @param fill_var_title If a variable has been assigned to fill using fill_var,
#'   this allows you to modify the variable label in the plot legend.
#'
#' @param colour_var_title If a variable has been assigned to colour using
#'   colour_var, this allows you to modify the variable label in the plot
#'   legend.
#'
#' @param ylim specify the y-axis limits, e.g. ylim = c(lower_limit,
#'   upper_limit). Use NA for the existing minimum or maximum value of y, e.g.
#'   the default is ylim = c(NA, NA)
#'
#' @param transform_y Would you like to transform the y axis? (TRUE or FALSE)
#'
#' @param y_transformation If transform_y = TRUE, this determines the
#'   transformation to be applied. Common choices include "log10" (the default),
#'   "log2", "sqrt", or "exp". See \code{\link[ggplot2]{scale_continuous}} for
#'   details.
#'
#' @param x_var_order If a variable has been assigned to x, this allows you to
#'   modify the order of the variable groups, e.g. x = grouping_variable,
#'   x_var_order = c("group_2", "group_1"). See
#'   \code{\link[forcats]{fct_relevel}} for details.
#'
#' @param fill_var_order If a variable has been assigned to fill using fill_var,
#'   this allows you to modify the order of the variable groups, e.g. fill_var =
#'   grouping_variable, fill_var_order = c("group_2", "group_1"). See
#'   \code{\link[forcats]{fct_relevel}} for details.
#'
#' @param colour_var_order If a variable has been assigned to colour using
#'   colour_var, this allows you to modify the order of the variable groups,
#'   e.g. colour_var = grouping_variable, fill_var_order = c("group_2",
#'   "group_1"). See \code{\link[forcats]{fct_relevel}} for details.
#'
#' @param x_var_labs If a variable has been assigned to x, this allows you to
#'   modify the labels of the variable groups, e.g. x = grouping_variable,
#'   x_var_labs = c("group_1_new_label" = "group_1_old_label",
#'   "group_2_new_label" = "group_2_old_label"). See
#'   \code{\link[forcats]{fct_recode}} for details.
#'
#' @param fill_var_labs If a variable has been assigned to fill using fill_var,
#'   this allows you to modify the labels of the variable groups, e.g. fill_var
#'   = grouping_variable, fill_var_labs = c("group_1_new_label" =
#'   "group_1_old_label", "group_2_new_label" = "group_2_old_label"). See
#'   \code{\link[forcats]{fct_recode}} for details.
#'
#' @param colour_var_labs If a variable has been assigned to colour using
#'   colour_var, this allows you to modify the labels of the variable groups,
#'   e.g. colour_var = grouping_variable, colour_var_labs =
#'   c("group_1_new_label" = "group_1_old_label", "group_2_new_label" =
#'   "group_2_old_label"). See \code{\link[forcats]{fct_recode}} for details.
#'
#' @param fill_var_values If a variable has been assigned to fill using
#'   fill_var, this allows you to modify the colours assigned to the fill of
#'   each of the variable groups, e.g. fill_var = grouping_variable,
#'   fill_var_values = c("blue", "red"). See
#'   \code{\link[ggplot2]{scale_fill_manual}} for details. For the colour
#'   options available in base R, see \code{\link[elucidate]{colour_options}}.
#'
#' @param colour_var_values If a variable has been assigned to colour using
#'   colour_var, this allows you to modify the colours assigned to the outline
#'   of each of the variable groups, e.g. colour_var = grouping_variable,
#'   colour_var_values = c("blue", "red"). See
#'   \code{\link[ggplot2]{scale_fill_manual}} for details. For the colour
#'   options available in base R, see \code{\link[elucidate]{colour_options}}.
#'
#' @param alpha This adjusts the transparency/opacity of the graphical
#'   components of the plot, ranging from 0 = 100% transparent to 1 = 100%
#'   opaque.
#'
#' @param greyscale Set to TRUE if you want the plot converted to greyscale.
#'
#' @param theme Adjusts the theme using 1 of 6 predefined "complete" theme
#'   templates provided by ggplot2. Currenlty supported options are: "classic"
#'   (the elucidate default), "bw", "grey" (the ggplot2 default), "light",
#'   "dark", & "minimal". See \code{\link[ggplot2]{theme_classic}} for more
#'   information.
#'
#' @param text_size This controls the size of all plot text. Default = 14.
#'
#' @param font This controls the font of all plot text. Default = "sans"
#'   (Arial). Other options include "serif" (Times New Roman) and "mono" (Courier
#'   New).
#'
#' @param facet_var Use if you want separate plots for each level of a grouping
#'   variable (i.e. a facetted plot), e.g. facet_var = grouping_variable. See
#'   \code{\link[ggplot2]{facet_wrap}} for details.
#'
#' @param facet_var_order If a variable has been assigned for facetting using
#'   facet_var, this allows you to modify the order of the variable groups, e.g.
#'   facet_var = grouping_variable, facet_var_order = c("group_2", "group_1").
#'   See \code{\link[forcats]{fct_relevel}} for details.
#'
#' @param facet_var_labs If a variable has been assigned for facetting using
#'   facet_var, this allows you to modify the labels of the variable groups
#'   which will appear in the facet strips, e.g. facet_var = grouping_variable,
#'   facet_var_labs = c("group_1_new_label" = "group_1_old_label",
#'   "group_2_new_label" = "group_2_old_label"). See
#'   \code{\link[forcats]{fct_recode}} for details.
#'
#' @param facet_var_strip_position If a variable has been assigned for facetting
#'   using facet_var, this allows you to modify the position of the facet strip
#'   labels. Sensible options include "top" (the default) or "bottom".
#'
#' @param facet_var_text_bold If a variable has been assigned for facetting
#'   using facet_var, this allows you to use boldface (TRUE/default or FALSE)
#'   for the facet strip label text.
#'
#' @param legend_position This allows you to modify the legend position.
#'   Options include "right" (the default), "left", "top", & "bottom".
#'
#' @param omit_legend Set to TRUE if you want to remove/omit the legends.
#'
#' @param interactive Determines whether a static ggplot object or an interactive html
#'   plotly object is returned. See \code{\link[plotly]{ggplotly}} for details.
#'
#' @param aesthetic_options If set to TRUE, opens a web browser to the tidyverse
#'   online aesthetic options vignette.
#'
#' @return A ggplot object or plotly object depending on whether static or
#'   interactive output was requested.
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gov.bc.ca}
#'
#' @examples
#' data(mtcars) #load the mtcars data
#'
#' library(magrittr)
#'
#' mtcars %>% plot_violin(y = mpg, x = cyl, fill = "blue")
#' mtcars %>% plot_violin(y = mpg, x = cyl, fill = "blue", draw_quantiles = c(0.25, 0.5, 0.75))
#'
#' \dontrun{
#' mtcars %>%
#'  plot_violin(x = cyl, y = hp,
#'           xlab = "# of cylinders",
#'           ylab = "horsepower",
#'           fill_var = am,
#'           fill_var_title = "transmission",
#'           fill_var_labs = c("manual" = "0", "automatic" = "1"),
#'           fill_var_values = c("blue", "red"),
#'           theme = "bw")
#'
#' #modifying fill doesn't work as well for the interactive version of a boxplot
#' mtcars %>%
#'  plot_violin(x = cyl, y = hp,
#'           xlab = "# of cylinders",
#'           ylab = "horsepower",
#'           fill_var = am,
#'           fill_var_title = "transmission",
#'           fill_var_labs = c("manual" = "0", "automatic" = "1"),
#'           fill_var_values = c("blue", "red"),
#'           theme = "bw",
#'           interactive = T)
#'
#' #using colour works better for the interactive version
#' mtcars %>%
#'  plot_violin(x = cyl, y = hp,
#'           xlab = "# of cylinders",
#'           ylab = "horsepower",
#'           colour_var = am,
#'           colour_var_title = "transmission",
#'           colour_var_labs = c("manual" = "0", "automatic" = "1"),
#'           colour_var_values = c("blue", "red"),
#'           theme = "bw", interactive = T)
#' }
#'
#'
#' @references
#' Wickham, H. (2016). ggplot2: elegant graphics for data analysis. New York, N.Y.: Springer-Verlag.
#'
#' @seealso \code{\link[ggplot2]{geom_violin}}, \code{\link[plotly]{ggplotly}}
#'
#' @export
plot_violin <- function(data, y,#essential parameters
                        x = NULL,
                        ..., #geom-specific customization see ?geom_violin for details
                        fill_var = NULL, colour_var = NULL, #grouping variable aesthetic mappings
                        xlab = NULL, ylab = NULL, title = NULL,
                        fill_var_title = NULL, colour_var_title = NULL, #titles
                        ylim = c(NA, NA), transform_y = FALSE, y_transformation = "log10", #control the y axis limits and scaling
                        x_var_order = NULL, x_var_labs = NULL,
                        fill_var_order = NULL, colour_var_order = NULL, #modify grouping variable level order
                        fill_var_labs = NULL, colour_var_labs = NULL, #modify grouping variable labels
                        fill_var_values = NULL, colour_var_values = NULL, #manual colour specification
                        alpha = 0.6, greyscale = FALSE, #control transparency, convert to greyscale
                        theme = "classic", text_size = 14, font = c("sans", "serif", "mono"),#theme options
                        facet_var = NULL, facet_var_order = NULL, facet_var_labs = NULL, #facet options
                        facet_var_strip_position = c("top", "bottom"), facet_var_text_bold = TRUE, #facet aesthetic customization
                        legend_position = c("right", "left", "top", "bottom"), omit_legend = FALSE, #legend position
                        interactive = FALSE, aesthetic_options = FALSE) {#output format

  font <- match.arg(font)
  legend_position <- match.arg(legend_position)
  facet_var_strip_position <- match.arg(facet_var_strip_position)

  #x-variable recoding
  if(!missing(x)){
    data <- data %>%
      dplyr::mutate({{x}} := as.character({{x}}))
  }
  if(!missing(x_var_order)){
    data <- data %>%
      dplyr::mutate({{x}} := forcats::fct_relevel({{x}}, levels = !!!x_var_order))
  }
  if(!missing(x_var_labs)){
    data <- data %>%
      dplyr::mutate({{x}} := forcats::fct_recode({{x}}, !!!x_var_labs))
  }

  #fill variable recoding
  if(!missing(fill_var)){
    data <- data %>%
      dplyr::mutate({{fill_var}} := as.character({{fill_var}}))
  }
  if(!missing(fill_var) & !missing(fill_var_order)){
    data <- data %>%
      dplyr::mutate({{fill_var}} := forcats::fct_relevel({{fill_var}}, levels = !!!fill_var_order))
  }
  if(!missing(fill_var) & !missing(fill_var_labs)){
    data <- data %>%
      dplyr::mutate({{fill_var}} := forcats::fct_recode({{fill_var}}, !!!fill_var_labs))
  }

  #colour variable recoding
  if(!missing(colour_var)){
    data <- data %>%
      dplyr::mutate({{colour_var}} := as.character({{colour_var}}))
  }
  if(!missing(colour_var) & !missing(colour_var_order)){
    data <- data %>%
      dplyr::mutate({{colour_var}} := forcats::fct_relevel({{colour_var}}, levels = !!!colour_var_order))
  }
  if(!missing(colour_var) & !missing(colour_var_labs)){
    data <- data %>%
      dplyr::mutate({{colour_var}} := forcats::fct_recode({{colour_var}}, !!!colour_var_labs))
  }

  #facet label recoding
  if(!missing(facet_var)){
    data <- data %>%
      dplyr::mutate({{facet_var}} := as.character({{facet_var}}))
  }
  if(!missing(facet_var) & !missing(facet_var_order)){
    data <- data %>%
      dplyr::mutate({{facet_var}} := forcats::fct_relevel({{facet_var}}, levels = !!!facet_var_order))
  }
  if(!missing(facet_var) & !missing(facet_var_labs)){
    data <- data %>%
      dplyr::mutate({{facet_var}} := forcats::fct_recode({{facet_var}}, !!!facet_var_labs))
  }

  #setup foundational plotting object layer
  p <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = {{x}}, y = {{y}}, fill = {{fill_var}}, colour = {{colour_var}}))

  #add the geom layer
  p <- p +
    ggplot2::geom_violin(alpha = alpha, ...)

  #modification of the colour or fill values
  if (!missing(fill_var) & missing(colour_var)){
    if(!missing(fill_var_values)){
      p <- p +
        ggplot2::scale_fill_manual(values = fill_var_values)
    }
  } else if(missing(fill_var) & !missing(colour_var)) {
    if(!missing(colour_var_values)){
      p <- p +
        ggplot2::scale_colour_manual(values = colour_var_values)
    }
  } else if(!missing(fill_var) & !missing(colour_var)) {
    if(!missing(fill_var_values) & missing(colour_var_values)){
      p <- p +
        ggplot2::scale_fill_manual(values = fill_var_values)
    } else if(missing(fill_var_values) & !missing(colour_var_values)) {
      p <- p +
        ggplot2::scale_colour_manual(values = colour_var_values)
    } else if(!missing(fill_var_values) & !missing(colour_var_values)) {
      p <- p +
        ggplot2::scale_fill_manual(values = fill_var_values) +
        ggplot2::scale_colour_manual(values = colour_var_values)
    }
  }

  #modification of y-axis limits & transformations
  if(!missing(ylim) & transform_y == FALSE){
    p <- p + ggplot2::lims(y = ylim)
  } else if (missing(ylim) & transform_y == TRUE){
    p <- p + ggplot2::scale_y_continuous(limits = c(NA, NA), trans = y_transformation)
  } else if (!missing(ylim) & transform_y  == TRUE){
    p <- p + ggplot2::scale_y_continuous(limits = c(ylim[1], ylim[2]), trans = y_transformation)
  }

  #modification of axis labels
  if(!missing(xlab)){
    p <- p + ggplot2::labs(x = xlab)
  }
  if(!missing(ylab)){
    p <- p + ggplot2::labs(y = ylab)
  }
  if(!missing(fill_var_title)){
    p <- p + ggplot2::labs(fill = fill_var_title)
  }
  if(!missing(colour_var_title)){
    p <- p + ggplot2::labs(color = colour_var_title)
  }

  if(greyscale == TRUE){
    p <- p + ggplot2::scale_fill_grey()
  }
  if(!missing(title)){
    p <- p + ggplot2::ggtitle(title)
  }
  if(theme == "classic"){
    p <- p + ggplot2::theme_classic(base_size = text_size, base_family = font)
  } else if (theme == "bw"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "b & w"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "black and white"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "black & white"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "grey"){
    p <- p + ggplot2::theme_grey(base_size = text_size, base_family = font)
  } else if (theme == "gray"){
    p <- p + ggplot2::theme_gray(base_size = text_size, base_family = font)
  } else if (theme == "light"){
    p <- p + ggplot2::theme_light(base_size = text_size, base_family = font)
  } else if (theme == "dark"){
    p <- p + ggplot2::theme_dark(base_size = text_size, base_family = font)
  } else if (theme == "minimal"){
    p <- p + ggplot2::theme_minimal(base_size = text_size, base_family = font)
  }
  if(omit_legend == TRUE){
    p <- p + ggplot2::theme(legend.position = "none")
  }
  if(legend_position != "right"){
    p <- p + ggplot2::theme(legend.position = legend_position)
  }
  if(!missing(facet_var)){
    p <- p + ggplot2::facet_wrap(vars({{facet_var}}), strip.position = facet_var_strip_position)
  }
  if(!missing(facet_var) & facet_var_text_bold == TRUE){
    p <- p + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
  }
  if(aesthetic_options == TRUE){
    utils::browseURL("https://ggplot2.tidyverse.org/articles/ggplot2-specs.html")
  }
  if(interactive == TRUE){
    return(plotly::ggplotly(p))
  }
  if(interactive == FALSE){
    return(p)
  }
}

# start of plot_scatter ------------------------------------------------------------
#' @title
#'
#' Generate a scatterplot.
#'
#' @description Easily generate scatterplots using ggplot2 with a simplified
#'   customization interface for common modifications with static (ggplot) and
#'   interactive (plotly) output options. The static output is useful for
#'   producing static reports (e.g. for manuscripts) and is readily customized
#'   further using ggplot2 syntax. The interactive output is helpful for
#'   exploring the data and producing dynamic html reports.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom forcats fct_relevel
#' @importFrom forcats fct_recode
#' @importFrom rlang !!!
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_jitter
#' @importFrom ggplot2 stat_smooth
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 scale_shape_manual
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 scale_size_continuous
#' @importFrom ggplot2 scale_fill_grey
#' @importFrom ggplot2 lims
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 theme_grey
#' @importFrom ggplot2 theme_gray
#' @importFrom ggplot2 theme_light
#' @importFrom ggplot2 theme_dark
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 element_text
#' @importFrom plotly ggplotly
#' @importFrom utils browseURL
#'
#' @param data A data frame or tibble containing the dependent measure "y", the
#'   independent measure "x", and any grouping variables or covariates.
#'
#' @param y A numeric variable you want to plot against x.
#'
#' @param x A numeric variable you want to plot against y.
#'
#' @param ... graphical parameters (not associated with variables) to be passed
#'   to \code{\link[ggplot2]{geom_point}}, e.g. colour or fill, to be applied
#'   to all bars. To see some of the available options in a web browser, set the
#'   aesthetic_options argument to TRUE.
#'
#' @param jitter Set to TRUE to slightly offset overlapping points in random
#'   directions. See \code{\link[ggplot2]{geom_jitter}} for details.
#'
#' @param fill_var Use if you want to assign a variable to the point fill
#'   colour, e.g. fill_var = grouping_variable. Produces separate sets of points
#'   for each level of the fill variable. See \code{\link[ggplot2]{aes}} for
#'   details. Note: for geom_point, fill_var and fill only affect shapes 21-24.
#'   To split the data by a variable based on colour, it is therefore easier to
#'   use colour_var for this particular plot geometry.
#'
#' @param colour_var Use if you want to assign a variable to the point outline
#'   colour, e.g. colour_var = grouping_variable. Produces separate sets of
#'   points for each level of the colour variable. See
#'   \code{\link[ggplot2]{aes}} for details.
#'
#' @param shape_var Use if you want to assign a variable to the point shape,
#'   e.g. shape_var = grouping_variable. Produces separate sets of points for
#'   each level of the shape variable. See \code{\link[ggplot2]{aes}} for
#'   details.
#'
#' @param size_var Use if you want to assign a continuous variable to the point
#'   size, e.g. size_var = covariate. Adjusts point sizes according to the value
#'   of the covariate. See \code{\link[ggplot2]{aes}} for details.
#'
#' @param ylab Specify/overwrite the y-axis label using a character string, e.g.
#'   "y-axis label"
#'
#' @param xlab Specify/overwrite the x-axis label using a character string, e.g.
#'   "x-axis label"
#'
#' @param title Add a main title to the plot using a character string, e.g.
#'   "scatterplot of y as a function of x"
#'
#' @param fill_var_title If a variable has been assigned to fill using fill_var,
#'   this allows you to modify the variable label in the plot legend.
#'
#' @param colour_var_title If a variable has been assigned to colour using
#'   colour_var, this allows you to modify the variable label in the plot
#'   legend.
#'
#' @param shape_var_title If a variable has been assigned to shape using
#'   shape_var, this allows you to modify the variable label in the plot
#'   legend.
#'
#' @param size_var_title If a variable has been assigned to shape using
#'   shape_var, this allows you to modify the variable label in the plot
#'   legend.
#'
#' @param ylim specify the y-axis limits, e.g. ylim = c(lower_limit,
#'   upper_limit). Use NA for the existing minimum or maximum value of y, e.g.
#'   the default is ylim = c(NA, NA)
#'
#' @param transform_y Would you like to transform the y axis? (TRUE or FALSE)
#'
#' @param y_transformation If transform_y = TRUE, this determines the
#'   transformation to be applied. Common choices include "log10" (the default),
#'   "log2", "sqrt", or "exp". See \code{\link[ggplot2]{scale_continuous}} for
#'   details.
#'
#' @param xlim specify the x-axis limits, e.g. xlim = c(lower_limit,
#'   upper_limit). Use NA for the existing minimum or maximum value of x, e.g.
#'   the default is xlim = c(NA, NA)
#'
#' @param transform_x Would you like to transform the x axis? (TRUE or FALSE)
#'
#' @param x_transformation If transform_x = TRUE, this determines the
#'   transformation to be applied. Common choices include "log10" (the default),
#'   "log2", "sqrt", or "exp". See \code{\link[ggplot2]{scale_continuous}} for
#'   details.
#'
#' @param size_lim specify the size scale limits, e.g. size_lim = c(lower_limit,
#'   upper_limit). Use NA for the existing minimum or maximum value of x, e.g.
#'   the default is size_lim = c(NA, NA)
#'
#' @param transform_size Would you like to transform the size scale? (TRUE or FALSE)
#'
#' @param size_transformation If transform_size = TRUE, this determines the
#'   transformation to be applied. Common choices include "log10" (the default),
#'   "log2", "sqrt", or "exp". See \code{\link[ggplot2]{scale_continuous}} for
#'   details.
#'
#' @param fill_var_order If a variable has been assigned to fill using fill_var,
#'   this allows you to modify the order of the variable groups, e.g. fill_var =
#'   grouping_variable, fill_var_order = c("group_2", "group_1"). See
#'   \code{\link[forcats]{fct_relevel}} for details.
#'
#' @param colour_var_order If a variable has been assigned to colour using
#'   colour_var, this allows you to modify the order of the variable groups,
#'   e.g. colour_var = grouping_variable, fill_var_order = c("group_2",
#'   "group_1"). See \code{\link[forcats]{fct_relevel}} for details.
#'
#' @param shape_var_order If a variable has been assigned to shape using
#'   shape_var, this allows you to modify the order of the variable groups,
#'   e.g. shape_var = grouping_variable, shape_var_order = c("group_2",
#'   "group_1"). See \code{\link[forcats]{fct_relevel}} for details.
#'
#' @param fill_var_labs If a variable has been assigned to fill using fill_var,
#'   this allows you to modify the labels of the variable groups, e.g. fill_var
#'   = grouping_variable, fill_var_labs = c("group_1_new_label" =
#'   "group_1_old_label", "group_2_new_label" = "group_2_old_label"). See
#'   \code{\link[forcats]{fct_recode}} for details.
#'
#' @param colour_var_labs If a variable has been assigned to colour using
#'   colour_var, this allows you to modify the labels of the variable groups,
#'   e.g. colour_var = grouping_variable, colour_var_labs =
#'   c("group_1_new_label" = "group_1_old_label", "group_2_new_label" =
#'   "group_2_old_label"). See \code{\link[forcats]{fct_recode}} for details.
#'
#' @param shape_var_labs If a variable has been assigned to shape using
#'   shape_var, this allows you to modify the labels of the variable groups,
#'   e.g. shape_var = grouping_variable, shape_var_labs =
#'   c("group_1_new_label" = "group_1_old_label", "group_2_new_label" =
#'   "group_2_old_label"). See \code{\link[forcats]{fct_recode}} for details.
#'
#' @param fill_var_values If a variable has been assigned to fill using
#'   fill_var, this allows you to modify the colours assigned to the fill of
#'   each of the variable groups, e.g. fill_var = grouping_variable,
#'   fill_var_values = c("blue", "red"). See
#'   \code{\link[ggplot2]{scale_fill_manual}} for details. For the colour
#'   options available in base R, see \code{\link[elucidate]{colour_options}}.
#'
#' @param colour_var_values If a variable has been assigned to colour using
#'   colour_var, this allows you to modify the colours assigned to the outline
#'   of each of the variable groups, e.g. colour_var = grouping_variable,
#'   colour_var_values = c("blue", "red"). See
#'   \code{\link[ggplot2]{scale_fill_manual}} for details. For the colour
#'   options available in base R, see \code{\link[elucidate]{colour_options}}.
#'
#' @param shape_var_values If a variable has been assigned to shape using
#'   shape_var, this allows you to modify the shapes assigned to each of the
#'   variable groups, e.g. colour_var = grouping_variable, shape_var_values =
#'   c(1, 2). See \code{\link[ggplot2]{scale_shape_manual}} for details.
#'   For the available shape options, set aesthetic_options = TRUE.
#'
#' @param regression_line Set to TRUE if you want to add a regression line to
#'   the plot.
#'
#' @param regression_method This determines the type of regression line to use.
#'   Common options are "lm", "loess", & "gam". "gam" is the default, which fits
#'   a generalized additive model using a smoothing term for x. This method has
#'   a longer run time, but typically provides a better fit to the data than
#'   other options and uses an optimization algorithm to determine the optimal
#'   wiggliness of the line. If the relationship between y and x is linear, the
#'   output will be equivalent to fitting a linear model. See
#'   \code{\link[ggplot2]{stat_smooth}} and \code{\link[mgcv]{gam}} for details.
#'
#' @param regression_formula Specify a formula to use with the chosen regression
#'   method, using the formula() interface. see
#'   \code{\link[ggplot2]{stat_smooth}} and \code{\link[stats]{formula}} for
#'   details. Useful for plotting polynomials, e.g. regression_formula = y ~
#'   poly(x, 2).
#'
#' @param regression_se Add a confidence envelope for the regression line? (TRUE
#'   or FALSE)
#'
#' @param ci_level Confidence level for the regression_line confidence envelope.
#'   Default is 0.95.
#'
#' @param regression_geom Not typically modified. See
#'   \code{\link[ggplot2]{stat_smooth}} for more information
#'
#' @param regression_line_size Adjusts the thickness/size of regression lines
#'
#' @param regression_line_colour Adjusts the colour of regression lines
#'
#' @param regression_line_type Adjusts the linetype of regression lines, e.g.
#'   "solid", "dashed", etc.
#'
#' @param regression_alpha Adjusts the transparency of the regression confidence
#'   envelope.
#'
#' @param regression_line_full_range Set to TRUE if you want regression lines to
#'   extend beyond the limits of the data to cover the full range of the plot.
#'
#' @param regression_method_args Additional arguments you would like passed to
#'   the regression_method modeling function, see method.args in
#'   \code{\link[ggplot2]{stat_smooth}}.
#'
#' @param loess_span Affects the wiggliness of the loess regression line. See
#'   \code{\link[ggplot2]{stat_smooth}} for details.
#'
#' @param alpha This adjusts the transparency/opacity of the graphical
#'   components of the plot, ranging from 0 = 100% transparent to 1 = 100%
#'   opaque.
#'
#' @param greyscale Set to TRUE if you want the plot converted to greyscale.
#'
#' @param theme Adjusts the theme using 1 of 6 predefined "complete" theme
#'   templates provided by ggplot2. Currenlty supported options are: "classic"
#'   (the elucidate default), "bw", "grey" (the ggplot2 default), "light",
#'   "dark", & "minimal". See \code{\link[ggplot2]{theme_classic}} for more
#'   information.
#'
#' @param text_size This controls the size of all plot text. Default = 14.
#'
#' @param font This controls the font of all plot text. Default = "sans"
#'   (Arial). Other options include "serif" (Times New Roman) and "mono" (Courier
#'   New).
#'
#' @param facet_var Use if you want separate plots for each level of a grouping
#'   variable (i.e. a facetted plot), e.g. facet_var = grouping_variable. See
#'   \code{\link[ggplot2]{facet_wrap}} for details.
#'
#' @param facet_var_order If a variable has been assigned for facetting using
#'   facet_var, this allows you to modify the order of the variable groups, e.g.
#'   facet_var = grouping_variable, facet_var_order = c("group_2", "group_1").
#'   See \code{\link[forcats]{fct_relevel}} for details.
#'
#' @param facet_var_labs If a variable has been assigned for facetting using
#'   facet_var, this allows you to modify the labels of the variable groups
#'   which will appear in the facet strips, e.g. facet_var = grouping_variable,
#'   facet_var_labs = c("group_1_new_label" = "group_1_old_label",
#'   "group_2_new_label" = "group_2_old_label"). See
#'   \code{\link[forcats]{fct_recode}} for details.
#'
#' @param facet_var_strip_position If a variable has been assigned for facetting
#'   using facet_var, this allows you to modify the position of the facet strip
#'   labels. Sensible options include "top" (the default) or "bottom".
#'
#' @param facet_var_text_bold If a variable has been assigned for facetting
#'   using facet_var, this allows you to use boldface (TRUE/default or FALSE)
#'   for the facet strip label text.
#'
#' @param legend_position This allows you to modify the legend position.
#'   Options include "right" (the default), "left", "top", & "bottom".
#'
#' @param omit_legend Set to TRUE if you want to remove/omit the legends.
#'
#' @param interactive Determines whether a static ggplot object or an interactive html
#'   plotly object is returned. See \code{\link[plotly]{ggplotly}} for details.
#'
#' @param aesthetic_options If set to TRUE, opens a web browser to the tidyverse
#'   online aesthetic options vignette.
#'
#' @return A ggplot object or plotly object depending on whether static or
#'   interactive output was requested.
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gov.bc.ca}
#'
#' @examples
#' data(mtcars) #load the mtcars data
#'
#' library(magrittr)
#'
#' mtcars %>% plot_scatter(y = mpg, x = hp, colour = "blue")
#'
#' \dontrun{
#' mtcars %>%
#'   plot_scatter(y = mpg, x = hp)
#'
#' mtcars %>%
#'   plot_scatter(y = mpg, x = hp, fill_var = cyl)
#'
#' mtcars %>%
#'   plot_scatter(y = mpg, x = hp, fill_var = cyl, shape = 21, size = 2)
#'
#' mtcars %>%
#'   plot_scatter(y = mpg, x = hp, colour_var = cyl, shape_var = am, size = 4)
#'
#' mtcars %>%
#'   plot_scatter(y = mpg, x = hp, colour = "blue",
#'                shape_var_labs = c("manual" = "0", "automatic" = "1"),
#'                shape_var = am, theme = "bw")
#'
#' #map colour, shape, and size to different variables
#' windows()
#'
#' mtcars %>%
#'   plot_scatter(y = mpg, x = hp,
#'                colour_var = cyl, shape_var = am, size_var = wt)
#'
#' #map colour and shape to a common variable
#' mtcars %>%
#'   plot_scatter(y = mpg, x = hp, shape_var = cyl, colour_var = cyl)
#'
#' mtcars %>%
#'   plot_scatter(y = mpg, x = hp, shape_var = cyl, colour_var = cyl)
#'
#' #add a regression line
#'
#' #linear
#' mtcars %>%
#'   plot_scatter(y = mpg, x = hp,
#'                regression_line = T, regression_method = "lm")
#'
#' #change the regression line colour
#' mtcars %>%
#'   plot_scatter(y = mpg, x = hp,
#'                regression_line = T, regression_method = "lm",
#'                regression_line_colour = "green")
#'
#' #add standard error envelope
#' mtcars %>%
#'   plot_scatter(y = mpg, x = hp,
#'                regression_line = T, regression_method = "lm", regression_se = T)
#'
#' #adjust standard error envelope transparency
#' mtcars %>%
#'   plot_scatter(y = mpg, x = hp,
#'                regression_line = T, regression_method = "lm", regression_se = T,
#'                regression_alpha = 0.8) #default is 0.5
#'
#'
#' #split by a grouping variable
#' mtcars %>%
#'   plot_scatter(y = mpg, x = hp, colour_var = cyl,
#'                regression_line = T, regression_method = "lm")
#'
#'
#' #fit a polynomial regression line by specifying a regression_formula = formula()
#' mtcars %>%
#'   plot_scatter(y = mpg, x = hp,
#'                regression_line = T, regression_method = "lm", regression_se = T,
#'                regression_formula = y ~ poly(x, 2))
#'
#' mtcars %>%
#'   plot_scatter(y = mpg, x = hp, shape_var = cyl, colour_var = cyl,
#'                regression_line = T, regression_method = "lm",
#'                regression_formula = y ~ poly(x, 3))
#'
#'
#' #fit a non-linear regression line using locally(-weighted) scatterplot smoothing (loess)
#' mtcars %>%
#'   plot_scatter(y = mpg, x = hp,
#'                regression_line = T, regression_se = T,
#'                regression_method = "loess")
#'
#'
#' #fit a non-linear regression line using locally(-weighted) scatterplot smoothing (loess)
#' #& also adjust the span (default = 0.75).
#' #This controls how much of the data is used for the weighted smoothing.
#'
#' mtcars %>%
#'   plot_scatter(y = mpg, x = hp,
#'                regression_line = T, regression_se = T,
#'                regression_method = "loess", loess_span = 0.3)
#'
#' #fit a non-linear regression line using a generalized additive model (gam), the default
#' mtcars %>%
#'   plot_scatter(y = mpg, x = hp,
#'                regression_line = T, regression_se = T,
#'                regression_method = "gam")
#'
#' #use a dashed regression line instead
#' mtcars %>%
#'   plot_scatter(y = mpg, x = hp,
#'                regression_line = T, regression_se = T, regression_line_type = "dashed")
#'
#' #more complex example with overplotting
#' pdata %>%
#'   plot_scatter(y = y1, x = d, colour_var = g,
#'                regression_line = T)
#'
#' #option 1 for dealing with overplotting: add jittering to offset overlappping points
#'  pdata %>%
#'  plot_scatter(y = y1, x = d, colour_var = g,
#'                jitter = T,
#'                regression_line = T)
#'
#' #option 2: make overlapping values more transparent
#'  pdata %>%
#'   plot_scatter(y = y1, x = d, colour_var = g,
#'                alpha = 0.2,
#'                regression_line = T)
#'
#' #option 3: do both and make it interactive
#' pdata %>%
#'  plot_scatter(y = y1, x = d, colour_var = g,
#'               jitter = T, alpha = 0.2,
#'               regression_line = T, interactive = TRUE)
#'
#' #add a faceting variable
#' windows()
#' pdata %>%
#'  plot_scatter(y = y1, x = d,
#'               colour = "black", shape = 21, fill = "green4",
#'               jitter = T, size = 4, alpha = 0.1,
#'               regression_line = T, regression_se = T,
#'               facet_var = g,
#'               ylab = "outcome",
#'               theme = "bw")
#'
#' #open a web page with details on the aesthetic options for ggplot2
#' mtcars %>%
#'  plot_scatter(y = mpg, x = hp, aesthetic_options = T)
#' }
#'
#'
#' @references
#' Wickham, H. (2016). ggplot2: elegant graphics for data analysis. New York, N.Y.: Springer-Verlag.
#'
#' @seealso \code{\link[ggplot2]{geom_point}},
#'   \code{\link[ggplot2]{geom_jitter}}, \code{\link[ggplot2]{stat_smooth}},
#'   \code{\link[plotly]{ggplotly}}
#'
#' @export
plot_scatter <- function(data, y, x,#essential parameters
                         jitter = FALSE,
                         ..., #non-variable geom customization see ?geom_point for details
                         #grouping variable aesthetic mappings
                         fill_var = NULL, colour_var = NULL,
                         shape_var = NULL, size_var = NULL,

                         ylab = NULL, xlab = NULL, title = NULL, #titles
                         fill_var_title = NULL, colour_var_title = NULL, #titles
                         shape_var_title = NULL, size_var_title = NULL, #titles

                         #axis scale limits and transformations
                         ylim = c(NA, NA), transform_y = FALSE, y_transformation = "log10",
                         xlim = c(NA, NA), transform_x = FALSE, x_transformation = "log10",
                         size_lim = c(NA, NA), transform_size = FALSE, size_transformation = "log10",

                         #aesthetic variable mapping customization options
                         fill_var_order = NULL, colour_var_order = NULL, shape_var_order = NULL,
                         fill_var_labs = NULL, colour_var_labs = NULL, shape_var_labs = NULL,
                         fill_var_values = NULL, colour_var_values = NULL, shape_var_values = NULL,

                         #regression line options
                         regression_line = FALSE, regression_method = "gam",
                         regression_formula = NULL, regression_se = FALSE, ci_level = 0.95,
                         regression_geom = "smooth", regression_line_size = 1, regression_line_colour = NULL,
                         regression_alpha = 0.5, regression_line_type = 1, regression_line_full_range = FALSE,
                         regression_method_args = NULL, loess_span = 0.75,

                         alpha = 0.6, greyscale = FALSE, #control transparency, convert to greyscale
                         theme = "classic", text_size = 14, font = c("sans", "serif", "mono"), #theme options
                         facet_var = NULL, facet_var_order = NULL, facet_var_labs = NULL, #facet options
                         facet_var_strip_position = c("top", "bottom"), facet_var_text_bold = TRUE, #facet aesthetic customization
                         legend_position = c("right", "left", "top", "bottom"), omit_legend = FALSE, #legend position
                         interactive = FALSE, aesthetic_options = FALSE) {#output format

  font <- match.arg(font)
  legend_position <- match.arg(legend_position)
  facet_var_strip_position <- match.arg(facet_var_strip_position)

  #fill variable recoding
  if(!missing(fill_var)){
    data <- data %>%
      dplyr::mutate({{fill_var}} := as.character({{fill_var}}))
  }
  if(!missing(fill_var) & !missing(fill_var_order)){
    data <- data %>%
      dplyr::mutate({{fill_var}} := forcats::fct_relevel({{fill_var}}, levels = !!!fill_var_order))
  }
  if(!missing(fill_var) & !missing(fill_var_labs)){
    data <- data %>%
      dplyr::mutate({{fill_var}} := forcats::fct_recode({{fill_var}}, !!!fill_var_labs))
  }

  #colour variable recoding
  if(!missing(colour_var)){
    data <- data %>%
      dplyr::mutate({{colour_var}} := as.character({{colour_var}}))
  }
  if(!missing(colour_var) & !missing(colour_var_order)){
    data <- data %>%
      dplyr::mutate({{colour_var}} := forcats::fct_relevel({{colour_var}}, levels = !!!colour_var_order))
  }
  if(!missing(colour_var) & !missing(colour_var_labs)){
    data <- data %>%
      dplyr::mutate({{colour_var}} := forcats::fct_recode({{colour_var}}, !!!colour_var_labs))
  }

  #shape variable recoding
  if(!missing(shape_var)){
    data <- data %>%
      dplyr::mutate({{shape_var}} := as.character({{shape_var}}))
  }
  if(!missing(shape_var) & !missing(shape_var_order)){
    data <- data %>%
      dplyr::mutate({{shape_var}} := forcats::fct_relevel({{shape_var}}, levels = !!!shape_var_order))
  }
  if(!missing(shape_var) & !missing(shape_var_labs)){
    data <- data %>%
      dplyr::mutate({{shape_var}} := forcats::fct_recode({{shape_var}}, !!!shape_var_labs))
  }

  #facet label recoding
  if(!missing(facet_var)){
    data <- data %>%
      dplyr::mutate({{facet_var}} := as.character({{facet_var}}))
  }
  if(!missing(facet_var) & !missing(facet_var_order)){
    data <- data %>%
      dplyr::mutate({{facet_var}} := forcats::fct_relevel({{facet_var}}, levels = !!!facet_var_order))
  }
  if(!missing(facet_var) & !missing(facet_var_labs)){
    data <- data %>%
      dplyr::mutate({{facet_var}} := forcats::fct_recode({{facet_var}}, !!!facet_var_labs))
  }

  #core plotting layer
  if(jitter == FALSE){
    p <- data %>%
      ggplot2::ggplot(ggplot2::aes(x = {{x}}, y = {{y}},
                          colour = {{colour_var}}, fill = {{fill_var}},
                          shape = {{shape_var}}, size = {{size_var}})) +
      ggplot2::geom_point(alpha = alpha, ...)

  } else if(jitter == TRUE){
    p <- data %>%
      ggplot2::ggplot(ggplot2::aes(x = {{x}}, y = {{y}},
                          colour = {{colour_var}}, fill = {{fill_var}},
                          shape = {{shape_var}}, size = {{size_var}})) +
      ggplot2::geom_jitter(alpha = alpha, ...)
  }

  if(!missing(fill_var)){
    warning("For ggplot2 scatterplots, fill argument only works for point shapes 21-24.\nSpecify the point shape using the shape argument.\nNote: For scatterplots, mapping a grouping variable to colour using colour_var works for all point shapes.")
  }

  #modification of the colour, fill, or shape values
  if (!missing(fill_var) & !missing(fill_var_values)){
    p <- p +
      ggplot2::scale_fill_manual(values = fill_var_values)
  }
  if (!missing(colour_var) & !missing(colour_var_values)){
    p <- p +
      ggplot2::scale_colour_manual(values = colour_var_values)
  }
  if (!missing(shape_var) & !missing(shape_var_values)){
    p <- p +
      ggplot2::scale_shape_manual(values = shape_var_values)
    if(interactive == FALSE){
      warning("Custom shape value assignments may not appear in shape legend when viewed in the R studio plots panel.\nIf this happens, try expanding the plot with the zoom button, view it in interactive mode with plotly,\n or print it to a separate graphics window")
    }
  }

  #regression line options
  if(!missing(regression_line_colour)){
    if(regression_line == TRUE & missing(regression_formula)){
      if(regression_method != "gam"){
        p <- p +
          ggplot2::stat_smooth(method = regression_method,
                               se = regression_se, alpha = regression_alpha,
                               linetype = regression_line_type, colour = regression_line_colour,
                               size = regression_line_size, fullrange = regression_line_full_range,
                               geom = regression_geom, level = ci_level, span = loess_span,
                               method.args = regression_method_args)
      } else if(regression_method == "gam"){
        p <- p +
          ggplot2::stat_smooth(method = regression_method, formula = y ~ s(x),
                               se = regression_se, alpha = regression_alpha,
                               linetype = regression_line_type, colour = regression_line_colour,
                               size = regression_line_size, fullrange = regression_line_full_range,
                               geom = regression_geom, level = CI_level, span = loess_span,
                               method.args = regression_method_args)
      }
    } else if(regression_line == TRUE & !missing(regression_formula)){
      p <- p +
        ggplot2::stat_smooth(method = regression_method, formula = regression_formula,
                             se = regression_se, alpha = regression_alpha,
                             linetype = regression_line_type, colour = regression_line_colour,
                             size = regression_line_size, fullrange = regression_line_full_range,
                             geom = regression_geom, level = ci_level, span = loess_span,
                             method.args = regression_method_args)
    }
  } else if (missing(regression_line_colour)){
    if(regression_line == TRUE & missing(regression_formula)){
      if(regression_method != "gam"){
        p <- p +
          ggplot2::stat_smooth(method = regression_method,
                               se = regression_se, alpha = regression_alpha,
                               linetype = regression_line_type,
                               size = regression_line_size, fullrange = regression_line_full_range,
                               geom = regression_geom, level = ci_level, span = loess_span,
                               method.args = regression_method_args)
      } else if(regression_method == "gam"){
        p <- p +
          ggplot2::stat_smooth(method = regression_method, formula = y ~ s(x),
                               se = regression_se, alpha = regression_alpha,
                               linetype = regression_line_type,
                               size = regression_line_size, fullrange = regression_line_full_range,
                               geom = regression_geom, level = ci_level, span = loess_span,
                               method.args = regression_method_args)
      }
    } else if(regression_line == TRUE & !missing(regression_formula)){
      p <- p +
        ggplot2::stat_smooth(method = regression_method, formula = regression_formula,
                             se = regression_se, alpha = regression_alpha,
                             linetype = regression_line_type,
                             size = regression_line_size, fullrange = regression_line_full_range,
                             geom = regression_geom, level = ci_level, span = loess_span,
                             method.args = regression_method_args)
    }
  }
  #modification of x-axis limits & transformations
  if(!missing(xlim) & transform_x == FALSE){
    p <- p + ggplot2::lims(x = xlim)
  } else if (missing(xlim) & transform_x == TRUE){
    p <- p + ggplot2::scale_x_continuous(limits = c(NA, NA), trans = x_transformation)
  } else if (!missing(xlim) & transform_x  == TRUE){
    p <- p + ggplot2::scale_x_continuous(limits = c(xlim[1], xlim[2]), trans = x_transformation)
  }

  #modification of y-axis limits & transformations
  if(!missing(ylim) & transform_y == FALSE){
    p <- p + ggplot2::lims(y = ylim)
  } else if (missing(ylim) & transform_y == TRUE){
    p <- p + ggplot2::scale_y_continuous(limits = c(NA, NA), trans = y_transformation)
  } else if (!missing(ylim) & transform_y  == TRUE){
    p <- p + ggplot2::scale_y_continuous(limits = c(ylim[1], ylim[2]), trans = y_transformation)
  }

  #modification of (continuous) size variable limits & transformations
  if(!missing(size_var) & !missing(size_lim) & transform_size == FALSE){
    if(is.numeric(size_var)){
      p <- p + ggplot2::scale_size_continuous(limits = c(size_lim[1], size_lim[2]))
    } else {
      message("size variable must be of numeric class to modify limits or apply transformations")
    }
  } else if (!missing(size_var) & missing(size_lim) & transform_size == TRUE){
    if(is.numeric(size_var)){
      p <- p + ggplot2::scale_size_continuous(limits = c(NA, NA), trans = size_transformation)
    } else {
      message("size variable must be of numeric class to modify limits or apply transformations")
    }
  } else if (!missing(size_var) & !missing(size_lim) & transform_size  == TRUE){
    if(is.numeric(size_var)){
      p <- p + ggplot2::scale_size_continuous(limits = c(size_lim[1], size_lim[2]), trans = size_transformation)
    } else {
      message("size variable must be of numeric class to modify limits or apply transformations")
    }
  }

  #modification of axis labels
  if(!missing(xlab)){
    p <- p + ggplot2::labs(x = xlab)
  }
  if(!missing(ylab)){
    p <- p + ggplot2::labs(y = ylab)
  }
  if(!missing(fill_var_title)){
    p <- p + ggplot2::labs(fill = fill_var_title)
  }
  if(!missing(colour_var_title)){
    p <- p + ggplot2::labs(color = colour_var_title)
  }
  if(!missing(shape_var_title)){
    p <- p + ggplot2::labs(shape = shape_var_title)
  }
  if(!missing(size_var_title)){
    p <- p + ggplot2::labs(size = size_var_title)
  }

  if(greyscale == TRUE){
    p <- p + ggplot2::scale_fill_grey()
  }
  if(!missing(title)){
    p <- p + ggplot2::ggtitle(title)
  }
  if(theme == "classic"){
    p <- p + ggplot2::theme_classic(base_size = text_size, base_family = font)
  } else if (theme == "bw"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "b & w"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "black and white"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "black & white"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "grey"){
    p <- p + ggplot2::theme_grey(base_size = text_size, base_family = font)
  } else if (theme == "gray"){
    p <- p + ggplot2::theme_gray(base_size = text_size, base_family = font)
  } else if (theme == "light"){
    p <- p + ggplot2::theme_light(base_size = text_size, base_family = font)
  } else if (theme == "dark"){
    p <- p + ggplot2::theme_dark(base_size = text_size, base_family = font)
  } else if (theme == "minimal"){
    p <- p + ggplot2::theme_minimal(base_size = text_size, base_family = font)
  }
  if(omit_legend == TRUE){
    p <- p + ggplot2::theme(legend.position = "none")
  }
  if(legend_position != "right"){
    p <- p + ggplot2::theme(legend.position = legend_position)
  }
  if(!missing(facet_var)){
    p <- p + ggplot2::facet_wrap(vars({{facet_var}}), strip.position = facet_var_strip_position)
  }
  if(!missing(facet_var) & facet_var_text_bold == TRUE){
    p <- p + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
  }
  if(aesthetic_options == TRUE){
    utils::browseURL("https://ggplot2.tidyverse.org/articles/ggplot2-specs.html")
  }
  if(interactive == TRUE){
    return(plotly::ggplotly(p))
  }
  if(interactive == FALSE){
    return(p)
  }
}


# start of plot_bar -------------------------------------------------------
#' @title
#'
#' Generate a bar plot.
#'
#' @description Easily generate bar plots using ggplot2 with a simplified
#'   customization interface for common modifications with static (ggplot) and
#'   interactive (plotly) output options. The static output is useful for
#'   producing static reports (e.g. for manuscripts) and is readily customized
#'   further using ggplot2 syntax. The interactive output is helpful for
#'   exploring the data and producing dynamic html reports. To plot a bar graph
#'   of sample means or medians and error bars, see
#'   \code{\link{plot_stat_error}} instead.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom forcats fct_relevel
#' @importFrom forcats fct_recode
#' @importFrom forcats fct_reorder
#' @importFrom forcats fct_infreq
#' @importFrom forcats fct_rev
#' @importFrom rlang !!!
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_fill_grey
#' @importFrom ggplot2 lims
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 theme_grey
#' @importFrom ggplot2 theme_gray
#' @importFrom ggplot2 theme_light
#' @importFrom ggplot2 theme_dark
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 position_dodge2
#' @importFrom ggplot2 element_text
#' @importFrom plotly ggplotly
#' @importFrom utils browseURL
#'
#' @param data A data frame or tibble containing at least one categorical
#'   variable.
#'
#' @param x A categorical variable you want to obtain separate bar plots for. If
#'   you want to plot all bars on top of each other (position = "fill" or
#'   position = "stack") to form a single banded bar leave "x" blank and assign
#'   a variable to either fill_var or colour_var instead. N.B. failing to assign
#'   a variable to x will also remove x-axis ticks and labels.
#'
#' @param y A numeric variable containing the values you would like plotted on
#'   the y-axis. If y is not specified, then the stat = "count" option will be
#'   used for \code{\link[ggplot2]{geom_bar}} and the counts of the variable(s)
#'   assigned to x, fill_var, and/or colour_var will be plotted on the y-axis.
#'
#' @param ... graphical parameters (not associated with variables) to be passed
#'   to \code{\link[ggplot2]{geom_bar}}, e.g. colour or fill, to be applied
#'   to all bars. To see some of the available options in a web browser, set the
#'   aesthetic_options argument to TRUE.
#'
#' @param width Adjusts the width of the bars (default = 0.85).
#'
#' @param position Determines how bars are arranged relative to one another when
#'   a grouping variable is assigned to either fill_var or colour_var. The
#'   default, "dodge", uses \code{\link[ggplot2]{position_dodge2}} to arrange
#'   bars side-by-side; "stack" places the bars on top of each other; "fill"
#'   also stacks bars but additionally converts y-axis from counts to
#'   proportions (assuming y argument is unspecified).
#'
#' @param dodge_padding If position = "dodge", this controls the gap width
#'   between adjacent bars (default = 0.1). To eliminate the gap, set this to 0.
#'   To overlay bars use a negative value e.g. -0.5. See
#'   \code{\link[ggplot2]{position_dodge2}} for details.
#'
#' @param fill_var Use if you want to assign a variable to the bar fill colour,
#'   e.g. fill_var = grouping_variable. Produces separate sets of bars for each
#'   level of the fill variable. See \code{\link[ggplot2]{aes}} for details.
#'
#' @param colour_var Use if you want to assign a variable to the bar outline
#'   colour, e.g. colour_var = grouping_variable. Produces separate sets of
#'   bars for each level of the colour variable. See \code{\link[ggplot2]{aes}}
#'   for details.
#'
#' @param xlab Specify/overwrite the x-axis label using a character string, e.g.
#'   "x-axis label"
#'
#' @param ylab Specify/overwrite the y-axis label using a character string, e.g.
#'   "y-axis label"
#'
#' @param title Add a main title to the plot using a character string, e.g.
#'   "bar plots of y for each group of x"
#'
#' @param fill_var_title If a variable has been assigned to fill using fill_var,
#'   this allows you to modify the variable label in the plot legend.
#'
#' @param colour_var_title If a variable has been assigned to colour using
#'   colour_var, this allows you to modify the variable label in the plot
#'   legend.
#'
#' @param ylim specify the y-axis limits, e.g. ylim = c(lower_limit,
#'   upper_limit). Use NA for the existing minimum or maximum value of y, e.g.
#'   the default is ylim = c(NA, NA)
#'
#' @param transform_y Would you like to transform the y axis? (TRUE or FALSE)
#'
#' @param y_transformation If transform_y = TRUE, this determines the
#'   transformation to be applied. Common choices include "log10" (the default),
#'   "log2", "sqrt", or "exp". See \code{\link[ggplot2]{scale_continuous}} for
#'   details.
#'
#' @param x_var_order_by_y If a variable has been assigned to x, this allows you
#'   to sort the bars in order of increasing/ascending ("i" or "a") or
#'   decreasing ("d") value of y. If no variable is assigned to y, then the
#'   sorting occurs based on relative counts (position = "dodge" or position =
#'   "stack") or proportions (position = "fill").
#'
#' @param x_var_order If a variable has been assigned to x, this allows you to
#'   manually modify the order of the variable groups, e.g. x =
#'   grouping_variable, x_var_order = c("group_2", "group_1"). See
#'   \code{\link[forcats]{fct_relevel}} for details.
#'
#' @param fill_var_order_by_y If a variable has been assigned to fill_var, this
#'   allows you to sort the bars in order of increasing/ascending ("i" or "a")
#'   or decreasing ("d") value of y. If no variable is assigned to y, then the
#'   sorting occurs based on relative counts (position = "dodge" or position =
#'   "stack") or proportions (position = "fill").
#'
#' @param fill_var_order If a variable has been assigned to fill using fill_var,
#'   this allows you to modify the order of the variable groups, e.g. fill_var =
#'   grouping_variable, fill_var_order = c("group_2", "group_1"). See
#'   \code{\link[forcats]{fct_relevel}} for details.
#'
#' @param colour_var_order_by_y If a variable has been assigned to colour_var,
#'   this allows you to sort the bars in order of increasing/ascending ("i" or
#'   "a") or decreasing ("d") value of y. If no variable is assigned to y, then
#'   the sorting occurs based on relative counts (position = "dodge" or position
#'   = "stack") or proportions (position = "fill").
#'
#' @param colour_var_order If a variable has been assigned to colour using
#'   colour_var, this allows you to modify the order of the variable groups,
#'   e.g. colour_var = grouping_variable, fill_var_order = c("group_2",
#'   "group_1"). See \code{\link[forcats]{fct_relevel}} for details.
#'
#' @param x_var_labs If a variable has been assigned to x, this allows you to
#'   modify the labels of the variable groups, e.g. x = grouping_variable,
#'   x_var_labs = c("group_1_new_label" = "group_1_old_label",
#'   "group_2_new_label" = "group_2_old_label"). See
#'   \code{\link[forcats]{fct_recode}} for details.
#'
#' @param fill_var_labs If a variable has been assigned to fill using fill_var,
#'   this allows you to modify the labels of the variable groups, e.g. fill_var
#'   = grouping_variable, fill_var_labs = c("group_1_new_label" =
#'   "group_1_old_label", "group_2_new_label" = "group_2_old_label"). See
#'   \code{\link[forcats]{fct_recode}} for details.
#'
#' @param colour_var_labs If a variable has been assigned to colour using
#'   colour_var, this allows you to modify the labels of the variable groups,
#'   e.g. colour_var = grouping_variable, colour_var_labs =
#'   c("group_1_new_label" = "group_1_old_label", "group_2_new_label" =
#'   "group_2_old_label"). See \code{\link[forcats]{fct_recode}} for details.
#'
#' @param fill_var_values If a variable has been assigned to fill using
#'   fill_var, this allows you to modify the colours assigned to the fill of
#'   each of the variable groups, e.g. fill_var = grouping_variable,
#'   fill_var_values = c("blue", "red"). See
#'   \code{\link[ggplot2]{scale_fill_manual}} for details. For the colour
#'   options available in base R, see \code{\link[elucidate]{colour_options}}.
#'
#' @param colour_var_values If a variable has been assigned to colour using
#'   colour_var, this allows you to modify the colours assigned to the outline
#'   of each of the variable groups, e.g. colour_var = grouping_variable,
#'   colour_var_values = c("blue", "red"). See
#'   \code{\link[ggplot2]{scale_fill_manual}} for details. For the colour
#'   options available in base R, see \code{\link[elucidate]{colour_options}}.
#'
#' @param alpha This adjusts the transparency/opacity of the graphical
#'   components of the plot, ranging from 0 = 100% transparent to 1 = 100%
#'   opaque.
#'
#' @param greyscale Set to TRUE if you want the plot converted to greyscale.
#'
#' @param coord_flip Flips the x and y axes. See
#'   \code{\link[ggplot2]{coord_flip}} for details.
#'
#' @param theme Adjusts the theme using 1 of 6 predefined "complete" theme
#'   templates provided by ggplot2. Currenlty supported options are: "classic"
#'   (the elucidate default), "bw", "grey" (the ggplot2 default), "light",
#'   "dark", & "minimal". See \code{\link[ggplot2]{theme_classic}} for more
#'   information.
#'
#' @param text_size This controls the size of all plot text. Default = 14.
#'
#' @param font This controls the font of all plot text. Default = "sans"
#'   (Arial). Other options include "serif" (Times New Roman) and "mono" (Courier
#'   New).
#'
#' @param facet_var Use if you want separate plots for each level of a grouping
#'   variable (i.e. a facetted plot), e.g. facet_var = grouping_variable. See
#'   \code{\link[ggplot2]{facet_wrap}} for details.
#'
#' @param facet_var_order If a variable has been assigned for facetting using
#'   facet_var, this allows you to modify the order of the variable groups, e.g.
#'   facet_var = grouping_variable, facet_var_order = c("group_2", "group_1").
#'   See \code{\link[forcats]{fct_relevel}} for details.
#'
#' @param facet_var_labs If a variable has been assigned for facetting using
#'   facet_var, this allows you to modify the labels of the variable groups
#'   which will appear in the facet strips, e.g. facet_var = grouping_variable,
#'   facet_var_labs = c("group_1_new_label" = "group_1_old_label",
#'   "group_2_new_label" = "group_2_old_label"). See
#'   \code{\link[forcats]{fct_recode}} for details.
#'
#' @param facet_var_strip_position If a variable has been assigned for facetting
#'   using facet_var, this allows you to modify the position of the facet strip
#'   labels. Sensible options include "top" (the default) or "bottom".
#'
#' @param facet_var_text_bold If a variable has been assigned for facetting
#'   using facet_var, this allows you to use boldface (TRUE/default or FALSE)
#'   for the facet strip label text.
#'
#' @param legend_position This allows you to modify the legend position.
#'   Options include "right" (the default), "left", "top", & "bottom".
#'
#' @param omit_legend Set to TRUE if you want to remove/omit the legends.
#'
#' @param interactive Determines whether a static ggplot object or an interactive html
#'   plotly object is returned. See \code{\link[plotly]{ggplotly}} for details.
#'
#' @param aesthetic_options If set to TRUE, opens a web browser to the tidyverse
#'   online aesthetic options vignette.
#'
#' @return A ggplot object or plotly object depending on whether static or
#'   interactive output was requested.
#'
#' @author Craig P. Hutton, \email{Craig.Hutton@@gov.bc.ca}
#'
#' @examples
#'
#' #plotting group counts
#'
#' plot_bar(pdata,
#'          x = g,
#'          xlab = "group",
#'          fill_var = high_low,
#'          colour = "black",
#'          fill_var_values = c("blue2", "red2"))
#'
#' #plotting specific values on the y-axis, e.g. a grouped summary statistic
#'
#' library(dplyr)
#'
#' grouped_y1_max <- pdata %>%
#'    group_by(g) %>%
#'    summarise(y1_max = max(y1), .groups = "drop")
#'
#' grouped_y1_max %>%
#'   plot_bar(x = g, y = y1_max,
#'            xlab = "group", ylab = "y1 maximum value",
#'            x_var_order_by_y = "i", #order levels of x by increasing y value
#'            fill = "blue2")
#'
#' @references
#' Wickham, H. (2016). ggplot2: elegant graphics for data analysis. New York, N.Y.: Springer-Verlag.
#'
#' @seealso \code{\link[ggplot2]{geom_bar}}, \code{\link[plotly]{ggplotly}},
#'   \code{\link{plot_stat_error}}
#'
#' @export
plot_bar <- function(data, x = NULL,
                     y = NULL, #required for stat = "identity"
                     ..., #geom-specific customization see ?geom_bar for details
                     width = 0.85,
                     position = c("dodge", "fill", "stack"),
                     dodge_padding = 0.1,
                     fill_var = NULL, colour_var = NULL, #grouping variable aesthetic mappings
                     xlab = NULL, ylab = NULL, title = NULL,
                     fill_var_title = NULL, colour_var_title = NULL, #titles
                     ylim = c(NA, NA), transform_y = FALSE, y_transformation = "log10", #control the y axis limits and scaling
                     x_var_order_by_y = NULL, x_var_order = NULL,
                     fill_var_order_by_y = NULL, fill_var_order = NULL,
                     colour_var_order_by_y = NULL, colour_var_order = NULL, #modify grouping variable level order
                     x_var_labs = NULL, fill_var_labs = NULL, colour_var_labs = NULL, #modify grouping variable labels
                     fill_var_values = NULL, colour_var_values = NULL, #manual colour specification
                     alpha = 0.6, greyscale = FALSE, #control transparency, convert to greyscale
                     coord_flip = FALSE,
                     theme = "classic", text_size = 14, font = c("sans", "serif", "mono"), #theme options
                     facet_var = NULL, facet_var_order = NULL, facet_var_labs = NULL, #facet options
                     facet_var_strip_position = c("top", "bottom"), facet_var_text_bold = TRUE, #facet aesthetic customization
                     legend_position = c("right", "left", "top", "bottom"), omit_legend = FALSE, #legend position
                     interactive = FALSE, aesthetic_options = FALSE) {#output format

  if(missing(x) && missing(y) && missing(fill_var) && missing(colour_var)) {
    stop('At least one of "x", "y", "fill_var", or "colour_var" must be specified.')
  }
  if(!missing(x_var_order_by_y)) {
   if(x_var_order_by_y != "d" && x_var_order_by_y != "a" && x_var_order_by_y != "i"){
     stop('"x_var_order_by_y" should be one of "d", "a", or "i"')
   }
  }
  if(!missing(fill_var_order_by_y)) {
    if(fill_var_order_by_y != "d" && fill_var_order_by_y != "a" && fill_var_order_by_y != "i"){
      stop('"fill_var_order_by_y" should be one of "d", "a", or "i"')
    }
  }
  if(!missing(colour_var_order_by_y)) {
    if(colour_var_order_by_y != "d" && colour_var_order_by_y != "a" && colour_var_order_by_y != "i"){
      stop('"colour_var_order_by_y" should be one of "d", "a", or "i"')
    }
  }
  position <- match.arg(position)
  font <- match.arg(font)
  legend_position <- match.arg(legend_position)
  facet_var_strip_position <- match.arg(facet_var_strip_position)

  #x-variable recoding
  if(!missing(x)){
    data <- data %>%
      dplyr::mutate({{x}} := as.character({{x}}))
  }
  if(!missing(x) && !missing(x_var_order_by_y)) {
    if(!missing(y)) {
      if(x_var_order_by_y == "d") {
        data <- data %>%
          dplyr::mutate({{x}} := forcats::fct_reorder({{x}}, {{y}}, .desc = TRUE))
      } else {
        data <- data %>%
          dplyr::mutate({{x}} := forcats::fct_reorder({{x}}, {{y}}, .desc = FALSE))
      }
    } else {
      if(x_var_order_by_y == "d") {
        data <- data %>%
          dplyr::mutate({{x}} := forcats::fct_infreq({{x}}))
      } else {
        data <- data %>%
          dplyr::mutate({{x}} := forcats::fct_rev(forcats::fct_infreq({{x}})))
      }
    }
  }
  if(!missing(x) && !missing(x_var_order)){
    data <- data %>%
      dplyr::mutate({{x}} := forcats::fct_relevel({{x}}, levels = !!!x_var_order))
  }
  if(!missing(x) && !missing(x_var_labs)){
    data <- data %>%
      dplyr::mutate({{x}} := forcats::fct_recode({{x}}, !!!x_var_labs))
  }

  #fill variable recoding
  if(!missing(fill_var)){
    data <- data %>%
      dplyr::mutate({{fill_var}} := as.character({{fill_var}}))
  }
  if(!missing(fill_var) && !missing(fill_var_order_by_y)) {
    if(!missing(y)) {
      if(fill_var_order_by_y == "d") {
        data <- data %>%
          dplyr::mutate({{fill_var}} := forcats::fct_reorder({{fill_var}}, {{y}}, .desc = TRUE))
      } else {
        data <- data %>%
          dplyr::mutate({{fill_var}} := forcats::fct_reorder({{fill_var}}, {{y}}, .desc = FALSE))
      }
    } else {
      if(fill_var_order_by_y == "d") {
        data <- data %>%
          dplyr::mutate({{fill_var}} := forcats::fct_infreq({{fill_var}}))
      } else {
        data <- data %>%
          dplyr::mutate({{fill_var}} := forcats::fct_rev(forcats::fct_infreq({{fill_var}})))
      }
    }
  }
  if(!missing(fill_var) && !missing(fill_var_order)){
    data <- data %>%
      dplyr::mutate({{fill_var}} := forcats::fct_relevel({{fill_var}}, levels = !!!fill_var_order))
  }
  if(!missing(fill_var) && !missing(fill_var_labs)){
    data <- data %>%
      dplyr::mutate({{fill_var}} := forcats::fct_recode({{fill_var}}, !!!fill_var_labs))
  }

  #colour variable recoding
  if(!missing(colour_var)){
    data <- data %>%
      dplyr::mutate({{colour_var}} := as.character({{colour_var}}))
  }
  if(!missing(colour_var) && !missing(colour_var_order_by_y)) {
    if(!missing(y)) {
      if(colour_var_order_by_y == "d") {
        data <- data %>%
          dplyr::mutate({{colour_var}} := forcats::fct_reorder({{colour_var}}, {{y}}, .desc = TRUE))
      } else {
        data <- data %>%
          dplyr::mutate({{colour_var}} := forcats::fct_reorder({{colour_var}}, {{y}}, .desc = FALSE))
      }
    } else {
      if(colour_var_order_by_y == "d") {
        data <- data %>%
          dplyr::mutate({{colour_var}} := forcats::fct_infreq({{colour_var}}))
      } else {
        data <- data %>%
          dplyr::mutate({{colour_var}} := forcats::fct_rev(forcats::fct_infreq({{colour_var}})))
      }
    }
  }
  if(!missing(colour_var) & !missing(colour_var_order)){
    data <- data %>%
      dplyr::mutate({{colour_var}} := forcats::fct_relevel({{colour_var}}, levels = !!!colour_var_order))
  }
  if(!missing(colour_var) & !missing(colour_var_labs)){
    data <- data %>%
      dplyr::mutate({{colour_var}} := forcats::fct_recode({{colour_var}}, !!!colour_var_labs))
  }

  #facet label recoding
  if(!missing(facet_var)){
    data <- data %>%
      dplyr::mutate({{facet_var}} := as.character({{facet_var}}))
  }
  if(!missing(facet_var) & !missing(facet_var_order)){
    data <- data %>%
      dplyr::mutate({{facet_var}} := forcats::fct_relevel({{facet_var}}, levels = !!!facet_var_order))
  }
  if(!missing(facet_var) & !missing(facet_var_labs)){
    data <- data %>%
      dplyr::mutate({{facet_var}} := forcats::fct_recode({{facet_var}}, !!!facet_var_labs))
  }

  #setup foundational plotting object layer
  if(!missing(y) && !missing(x)) {
    p <- data %>%
      ggplot2::ggplot(ggplot2::aes(x = {{x}}, y = {{y}}, fill = {{fill_var}}, colour = {{colour_var}}))
  } else if (!missing(x)) {
    p <- data %>%
      ggplot2::ggplot(ggplot2::aes(x = {{x}}, fill = {{fill_var}}, colour = {{colour_var}}))
  } else if (!missing(y) && missing(x)) {
    p <- data %>%
      ggplot2::ggplot(ggplot2::aes(y = {{y}}, x = 1, fill = {{fill_var}}, colour = {{colour_var}}))
  } else {
    p <- data %>%
      ggplot2::ggplot(ggplot2::aes(x = 1, fill = {{fill_var}}, colour = {{colour_var}}))
  }

  #add the geom layer
  if(missing(y)) {
    if(position == "dodge") {
      p <- p +
        ggplot2::geom_bar(alpha = alpha, stat = "count", width = width, position = ggplot2::position_dodge2(padding = dodge_padding), ...)
    } else {
      p <- p +
        ggplot2::geom_bar(alpha = alpha, stat = "count", width = width,  position = position, ...)
    }
  } else {
    if(position == "dodge") {
      p <- p +
        ggplot2::geom_bar(alpha = alpha, stat = "identity", width = width,  position = ggplot2::position_dodge2(padding = dodge_padding), ...)
    } else {
      p <- p +
        ggplot2::geom_bar(alpha = alpha, stat = "identity", width = width,  position = position, ...)
    }

  }

  #modification of the colour or fill values
  if (!missing(fill_var) & missing(colour_var)){
    if(!missing(fill_var_values)){
      p <- p +
        ggplot2::scale_fill_manual(values = fill_var_values)
    }
  } else if(missing(fill_var) & !missing(colour_var)) {
    if(!missing(colour_var_values)){
      p <- p +
        ggplot2::scale_colour_manual(values = colour_var_values)
    }
  } else if(!missing(fill_var) & !missing(colour_var)) {
    if(!missing(fill_var_values) & missing(colour_var_values)){
      p <- p +
        ggplot2::scale_fill_manual(values = fill_var_values)
    } else if(missing(fill_var_values) & !missing(colour_var_values)) {
      p <- p +
        ggplot2::scale_colour_manual(values = colour_var_values)
    } else if(!missing(fill_var_values) & !missing(colour_var_values)) {
      p <- p +
        ggplot2::scale_fill_manual(values = fill_var_values) +
        ggplot2::scale_colour_manual(values = colour_var_values)
    }
  }

  #modification of y-axis limits & transformations
  if(!missing(ylim) && transform_y == FALSE){
    p <- p + ggplot2::lims(y = ylim)
  } else if (!missing(ylim) && transform_y == TRUE){
    p <- p + ggplot2::scale_y_continuous(limits = c(NA, NA), trans = y_transformation)
  } else if (!missing(ylim) && transform_y  == TRUE){
    p <- p + ggplot2::scale_y_continuous(limits = c(ylim[1], ylim[2]), trans = y_transformation)
  }

  #modification of axis labels
  if(!missing(xlab)){
    p <- p + ggplot2::labs(x = xlab)
  } else if(missing(x)) {
    p <- p + ggplot2::labs(x = NULL)
  }
  if(missing(x)) {
    p <- p + scale_x_continuous(breaks = NULL)
  }
  if(!missing(ylab)){
    p <- p + ggplot2::labs(y = ylab)
  } else if (missing(y) && position == "fill") {
    p <- p + ggplot2::labs(y = "proportion of total count")
  }
  if(!missing(fill_var_title)){
    p <- p + ggplot2::labs(fill = fill_var_title)
  }
  if(!missing(colour_var_title)){
    p <- p + ggplot2::labs(color = colour_var_title)
  }

  if(coord_flip == TRUE) {
    p <- p + ggplot2::coord_flip()
  }

  if(greyscale == TRUE){
    p <- p + ggplot2::scale_fill_grey()
  }
  if(!missing(title)){
    p <- p + ggplot2::ggtitle(title)
  }
  if(theme == "classic"){
    p <- p + ggplot2::theme_classic(base_size = text_size, base_family = font)
  } else if (theme == "bw"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "b & w"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "black and white"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "black & white"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "grey"){
    p <- p + ggplot2::theme_grey(base_size = text_size, base_family = font)
  } else if (theme == "gray"){
    p <- p + ggplot2::theme_gray(base_size = text_size, base_family = font)
  } else if (theme == "light"){
    p <- p + ggplot2::theme_light(base_size = text_size, base_family = font)
  } else if (theme == "dark"){
    p <- p + ggplot2::theme_dark(base_size = text_size, base_family = font)
  } else if (theme == "minimal"){
    p <- p + ggplot2::theme_minimal(base_size = text_size, base_family = font)
  }
  if(omit_legend == TRUE){
    p <- p + ggplot2::theme(legend.position = "none")
  }
  if(legend_position != "right"){
    p <- p + ggplot2::theme(legend.position = legend_position)
  }
  if(!missing(facet_var)){
    p <- p + ggplot2::facet_wrap(vars({{facet_var}}), strip.position = facet_var_strip_position)
  }
  if(!missing(facet_var) & facet_var_text_bold == TRUE){
    p <- p + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
  }
  if(aesthetic_options == TRUE){
    utils::browseURL("https://ggplot2.tidyverse.org/articles/ggplot2-specs.html")
  }
  if(interactive == TRUE){
    return(plotly::ggplotly(p))
  }
  if(interactive == FALSE){
    return(p)
  }
}


# start of plot_stat_error -------------------------------------------------------
#' @title
#'
#' Plot a sample mean or median +/- error bars.
#'
#' @description Easily generate plots of a sample mean or median +/- error bars
#'   using ggplot2 with a simplified customization interface with static
#'   (ggplot) and interactive (plotly) output options. The static output is
#'   useful for producing static reports (e.g. for manuscripts) and is readily
#'   customized further using ggplot2 syntax. The interactive output is helpful
#'   for exploring the data and producing dynamic html reports.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom forcats fct_relevel
#' @importFrom forcats fct_recode
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_errorbar
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 position_dodge
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 scale_fill_grey
#' @importFrom ggplot2 lims
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 theme_grey
#' @importFrom ggplot2 theme_gray
#' @importFrom ggplot2 theme_light
#' @importFrom ggplot2 theme_dark
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 position_dodge
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @importFrom stats quantile
#' @importFrom stats qnorm
#' @importFrom stats var
#' @importFrom scales percent
#' @importFrom plotly ggplotly
#' @importFrom utils browseURL
#'
#' @param data A data frame or tibble containing the dependent measure "y", the
#'   independent measure "x" (optional), and any grouping variables or covariates.
#'
#' @param y A numeric variable you want to plot against x.
#'
#' @param x A categorical variable you want to plot against y.
#'
#' @param geom Determines whether the chosen summary statistic is displayed
#'   using \code{\link[ggplot2]{geom_bar}} (geom = "bar"; the default) or
#'   \code{\link[ggplot2]{geom_point}} (geom = "point")
#'
#' @param stat The summary statistic to use for plotting bars/points. Options
#'   are "mean" (the default) or "median".
#'
#' @param error The statistic to use for the error bars. When stat = "mean",
#'   available options include se (standard error), sd (standard deviation),
#'   var(variance), and ci (confidence interval; the default). When stat =
#'   "median", options include "quartile" (lower bound = 25th percentile & upper
#'   bound = 75th percentile), or "ci". See below for more details on confidence
#'   intervals. With respect to CIs, when stat = "mean", CIs are calculated
#'   directly from a normal distribution based on the standard error using
#'   \code{\link[stats]{qnorm}}. In contrast, bootstrapped CIs of the specified
#'   type are returned via \code{\link{median_ci}} when stat = "median".
#'
#' @param ci_level The confidence level to use for constructing confidence
#'   intervals. Default is set to \code{ci_level = 0.95} for 95 percent CIs.
#'
#' @param ci_type The type of confidence intervals to calculate from the
#'   bootstrap samples when stat = "median" and error = "ci". Most of the
#'   options available in the underlying boot.ci function are implemented
#'   (except for studentized intervals): "norm" for an approximation based on
#'   the normal distribution, "perc" for percentile, "basic" for basic, and
#'   "bca" for bias-corrected and accelerated. Percentile intervals are the
#'   default since these are typically sufficient when working with large data
#'   sets (e.g. >= 100,000 rows of data) and are faster to calculate than BCa
#'   intervals. However, BCa intervals (the default for the more primitive
#'   \code{\link{median_ci}} function) tend to provide the most
#'   accurate/least-biased results (Efron, 1987), particularly for small-medium
#'   sized samples, at the obvious cost of requiring more time to calculate. See
#'   \code{\link[boot]{boot.ci}} for details.
#'
#' @param replicates The number of bootstrap replicates to use for calculating
#'   bootstrapped CIs when stat = "median" and error = "ci". Default is 2,000,
#'   as recommended by Efron & Tibshirani (1993). For publications, or if you
#'   need more precise estimates, more replications (e.g. >= 5,000) are
#'   recommended. N.B. more replications will of course take longer to run. If
#'   you get the error: "estimated adjustment 'a' is NA" then try again with
#'   more replications.
#'
#' @param parallel set to TRUE if you want to use multiple cores or FALSE if you
#'   don't (the default). Note that there is some processing overhead involved
#'   when operating in parallel so speed gains may not be very noticable for
#'   smaller samples (and may even take longer than sequential processing). Due
#'   to the nature of the underlying parallelization architecture, performance
#'   gains will likely be greater on non-Windows machines that can use the
#'   "multicore" implementation instead of "snow". For obvious reasons this
#'   option only works on machines with more than 1 logical processing core.
#'
#' @param cores If parallel is set to TRUE, this determines the number of cores
#'   to use. To see how many cores are available on your machine, use
#'   parallel::detectCores()
#'
#' @param ylab Specify/overwrite the y-axis label using a character string, e.g.
#'   "y-axis label". If not specified, this label will reflect a combination of
#'   the chosen statistic, y variable, and error bar options.
#'
#' @param xlab Specify/overwrite the x-axis label using a character string, e.g.
#'   "x-axis label". If no variable is assigned to x, then this label will be
#'   omitted.
#'
#' @param title Add a main title to the plot using a character string, e.g.
#'   "scatterplot of y as a function of x"
#'
#' @param ... graphical parameters (not associated with variables) to be passed
#'   to \code{\link[ggplot2]{geom_point}} or \code{\link[ggplot2]{geom_bar}}
#'   according to which base geometry is specified (via the geom argument), e.g.
#'   colour, fill, or transparency (e.g. alpha = 0.6) to be applied to all
#'   cases. To see some of the available options in a web browser, set the
#'   aesthetic_options argument to TRUE. Exceptions to this are the width of the
#'   bar graphs which is controlled using the b_width argument, and the shape &
#'   size of points for geom = "point", which are controlled using p_size &
#'   p_shape (see below).
#'
#' @param ylim specify the y-axis limits, e.g. ylim = c(lower_limit,
#'   upper_limit). Use NA for the existing minimum or maximum value of y, e.g.
#'   the default is ylim = c(NA, NA)
#'
#' @param transform_y Would you like to transform the y axis? (TRUE or FALSE)
#'
#' @param y_transformation If transform_y = TRUE, this determines the
#'   transformation to be applied. Common choices include "log10" (the default),
#'   "log2", "sqrt", or "exp". See \code{\link[ggplot2]{scale_continuous}} for
#'   details.
#'
#' @param x_var_order If a variable has been assigned to x, this allows you to
#'   modify the order of the variable groups, e.g. x = grouping_variable,
#'   x_var_order = c("group_2", "group_1"). See
#'   \code{\link[forcats]{fct_relevel}} for details.
#'
#' @param x_var_labs If a variable has been assigned to x, this allows you to
#'   modify the labels of the variable groups, e.g. x = grouping_variable,
#'   x_var_labs = c("group_1_new_label" = "group_1_old_label",
#'   "group_2_new_label" = "group_2_old_label"). See
#'   \code{\link[forcats]{fct_recode}} for details.
#'
#' @param fill_var Use if you want to assign a variable to the point fill
#'   colour, e.g. fill_var = grouping_variable. Produces separate sets of points
#'   for each level of the fill variable. See \code{\link[ggplot2]{aes}} for
#'   details. Note: for geom = "point", fill_var and fill only affect shapes
#'   21-24 (21 is the default). To split the data by a variable based on colour,
#'   it is therefore easier to use colour_var for this particular plot geometry.
#'
#' @param fill_var_order If a variable has been assigned to fill using fill_var,
#'   this allows you to modify the order of the variable groups, e.g. fill_var =
#'   grouping_variable, fill_var_order = c("group_2", "group_1"). See
#'   \code{\link[forcats]{fct_relevel}} for details.
#'
#' @param fill_var_values If a variable has been assigned to fill using
#'   fill_var, this allows you to modify the colours assigned to the fill of
#'   each of the variable groups, e.g. fill_var = grouping_variable,
#'   fill_var_values = c("blue", "red"). See
#'   \code{\link[ggplot2]{scale_fill_manual}} for details. For the colour
#'   options available in base R, see \code{\link[elucidate]{colour_options}}.
#'
#' @param fill_var_labs If a variable has been assigned to fill using fill_var,
#'   this allows you to modify the labels of the variable groups, e.g. fill_var
#'   = grouping_variable, fill_var_labs = c("group_1_new_label" =
#'   "group_1_old_label", "group_2_new_label" = "group_2_old_label"). See
#'   \code{\link[forcats]{fct_recode}} for details.
#'
#' @param fill_var_title If a variable has been assigned to fill using fill_var,
#'   this allows you to modify the variable label in the plot legend.
#'
#' @param colour_var Use if you want to assign a variable to the point outline
#'   colour, e.g. colour_var = grouping_variable. Produces separate sets of
#'   points for each level of the colour variable. See
#'   \code{\link[ggplot2]{aes}} for details.
#'
#' @param colour_var_order If a variable has been assigned to colour using
#'   colour_var, this allows you to modify the order of the variable groups,
#'   e.g. colour_var = grouping_variable, fill_var_order = c("group_2",
#'   "group_1"). See \code{\link[forcats]{fct_relevel}} for details.
#'
#' @param colour_var_values If a variable has been assigned to colour using
#'   colour_var, this allows you to modify the colours assigned to the outline
#'   of each of the variable groups, e.g. colour_var = grouping_variable,
#'   colour_var_values = c("blue", "red"). See
#'   \code{\link[ggplot2]{scale_fill_manual}} for details. For the colour
#'   options available in base R, see \code{\link[elucidate]{colour_options}}.
#'
#' @param colour_var_labs If a variable has been assigned to colour using
#'   colour_var, this allows you to modify the labels of the variable groups,
#'   e.g. colour_var = grouping_variable, colour_var_labs =
#'   c("group_1_new_label" = "group_1_old_label", "group_2_new_label" =
#'   "group_2_old_label"). See \code{\link[forcats]{fct_recode}} for details.
#'
#' @param colour_var_title If a variable has been assigned to colour using
#'   colour_var, this allows you to modify the variable label in the plot
#'   legend.
#'
#' @param greyscale Set to TRUE if you want the plot converted to greyscale.
#'
#' @param b_width The width of the bars when geom = "bar". Affects the spacing
#'   between adjacent bars, where a value of 1 specifies that there should be no
#'   space between adjacent bars. Default is 0.75.
#'
#' @param p_size The size of the points when geom = "point", default = 2.
#'
#' @param p_shape The shape of the points when geom = "point". The default is
#'   21, which is a circle like shape 1 (the ggplot2 default), but unlike the
#'   latter also has a fill parameter.
#'
#' @param dodge_width Determines the amount by which to jitter points and error
#'   bars when variables are mapped/assigned to x, fill, or colour. To disable
#'   jittering set this to 0.
#'
#' @param eb_size Controls the thickness of error bar lines. Default = 0.3.
#'
#' @param eb_width Controls the width of error bar endpoint lines. Default =
#'   0.2.
#'
#' @param eb_alpha This adjusts the transparency/opacity of the graphical
#'   components of the error bars, ranging from 0 = 100 percent transparent to 1
#'   = 100% percent opaque.
#'
#' @param eb_linetype Controls the error bar line type. Default = 1 or "solid".
#'
#' @param eb_colour Controls the colour of error bar lines. If unspecified,
#'   will either be "black" for all lines, or differ by groups if colour_var is
#'   specified.
#'
#' @param add_lines Would you like to connect the estimated statistics (the
#'   points or ends of the bars) with lines (TRUE/FALSE)? Default = FALSE.
#'   Requires that a varable is assigned to x. Particularly useful for showing
#'   changes over time between groups (assigned to either fill_var or
#'   colour_var). If only one of fill_var or colour_var are specified then lines
#'   will be split according to the same variable mapping. If neither or both of
#'   these parameters are specified and you want the lines split by a grouping
#'   variable, assign the chosen variable to line_group.
#'
#' @param line_alpha This adjusts the transparency/opacity of the lines
#'   connecting each (groupwise) estimate, ranging from 0 = 100 percent
#'   transparent to 1 = 100 percent opaque.
#'
#' @param line_group Determines which variable to split the connecting lines on.
#'   Only required if (2 different) variabes are already assigned to both
#'   fill_var and colour_var.
#'
#' @param line_colour Controls the colour of connection lines. If unspecified,
#'   will either be "black" for all lines, or differ by groups if colour_var is
#'   specified.
#'
#' @param linetype Controls the connection line type. Default = 1 or "solid".
#'
#' @param line_size Controls the thickness of the connection lines. Default =
#'   0.5.
#'
#' @param theme Adjusts the theme using 1 of 6 predefined "complete" theme
#'   templates provided by ggplot2. Currenlty supported options are: "classic"
#'   (the elucidate default), "bw", "grey" (the ggplot2 default), "light",
#'   "dark", & "minimal". See \code{\link[ggplot2]{theme_classic}} for more
#'   information.
#'
#' @param text_size This controls the size of all plot text. Default = 14.
#'
#' @param font This controls the font of all plot text. Default = "sans"
#'   (Arial). Other options include "serif" (Times New Roman) and "mono" (Courier
#'   New).
#'
#' @param coord_flip Set to TRUE (default = FALSE) if you want to swap the
#'   x and y axes. See \code{\link[ggplot2]{coord_flip}} for more information.
#'
#' @param omit_legend Set to TRUE if you want to remove/omit the legends.
#'
#' @param legend_position This allows you to modify the legend position.
#'   Options include "right" (the default), "left", "top", & "bottom".
#'
#' @param facet_var Use if you want separate plots for each level of a grouping
#'   variable (i.e. a facetted plot), e.g. facet_var = grouping_variable. See
#'   \code{\link[ggplot2]{facet_wrap}} for details.
#'
#' @param facet_var_order If a variable has been assigned for facetting using
#'   facet_var, this allows you to modify the order of the variable groups, e.g.
#'   facet_var = grouping_variable, facet_var_order = c("group_2", "group_1").
#'   See \code{\link[forcats]{fct_relevel}} for details.
#'
#' @param facet_var_labs If a variable has been assigned for facetting using
#'   facet_var, this allows you to modify the labels of the variable groups
#'   which will appear in the facet strips, e.g. facet_var = grouping_variable,
#'   facet_var_labs = c("group_1_new_label" = "group_1_old_label",
#'   "group_2_new_label" = "group_2_old_label"). See
#'   \code{\link[forcats]{fct_recode}} for details.
#'
#' @param facet_var_strip_position If a variable has been assigned for facetting
#'   using facet_var, this allows you to modify the position of the facet strip
#'   labels. Sensible options include "top" (the default) or "bottom".
#'
#' @param facet_var_text_bold If a variable has been assigned for facetting
#'   using facet_var, this allows you to use boldface (TRUE/default or FALSE)
#'   for the facet strip label text.
#'
#' @param print_stats Set to TRUE (the default is FALSE) if you would like the
#'   values used for plotting and info on the y variable sample size(s) &
#'   missing values to be printed as the function is executed. To save/extract
#'   these values, use output = "ps" instead.
#'
#' @param aesthetic_options If set to TRUE, opens a web browser to the tidyverse
#'   online aesthetic options vignette.
#'
#' @param interactive Determines whether a static ggplot object or an
#'   interactive html plotly object is returned. See
#'   \code{\link[plotly]{ggplotly}} for details.
#'
#' @param output Set to "p" if only want the ggplot or plotly object (depending
#'   on whether interactive = T or F) to be returned. Set to "ps" if you would
#'   instead like a list to be returned containing both the "plot" as a
#'   ggplot2/plotly object and "stats" used to produce it as a tibble. This
#'   allows you to extract/save the values for subsequent reporting or
#'   utilization.
#'
#' @param na.rm This determines whether missing values (NAs) should be removed
#'   before attempting to calculate the summary statistics used for plotting.
#'
#' @return A ggplot object or plotly object depending on whether static or
#'   interactive output was requested. This is returned as a standalone object
#'   if output = "p", or as the "plot" component of a list also containing the
#'   underlying "stats" as a 2nd component if output = "ps.
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gov.bc.ca}
#'
#' @examples
#' data(mtcars) #load the mtcars data
#'
#' library(magrittr)
#'
#' mtcars %>% plot_stat_error(y = mpg, x = cyl, colour = "blue")
#'
#' mtcars %>% plot_stat_error(y = mpg, x = cyl, colour = "blue", geom = "point")
#'
#' \dontrun{
#'
#' pdata %>%
#'   plot_stat_error(y = y1, x = d, colour_var = g, print_stats = T,
#'                   geom = "point", p_size = 3,
#'                   add_lines = T,
#'                   dodge_width = 0,
#'                   alpha = 0.6)
#'
#' pdata %>%
#'  plot_stat_error(y = y1, x = g, coord_flip = T,
#'                  fill_var = g, geom = "point", eb_size = 0.6,
#'                  alpha = 0.6)
#'
#' pdata %>%
#'   plot_stat_error(y = y1, x = g, fill = "blue", alpha = 0.6,
#'                   stat = "median", error = "quartile")
#'
#' pdata %>%
#'   plot_stat_error(y = y1, x = g, fill = "blue", alpha = 0.6,
#'                   stat = "median", error = "ci")
#'
#'
#' pdata %>%
#'   plot_stat_error(y = y1, x = g, fill = "blue", alpha = 0.6,
#'                   stat = "mean", error = "ci", ci_level = 0.8, interactive = T)
#'
#' #when output = "ps" the plot is stored as the 1st element of a
#' #list
#'
#' out <- pdata %>%
#'   plot_stat_error(y = y1, x = g, fill = "blue", alpha = 0.6,
#'                   stat = "mean", error = "ci", output = "ps")
#'
#' out$plot #print the plot to the appropriate active graphics device
#'
#' out$stats #print the descriptive summary table with the values used for plotting to the console
#'
#' }
#'
#'
#' @references
#' Wickham, H. (2016). ggplot2: elegant graphics for data analysis. New York, N.Y.: Springer-Verlag.
#'
#' Altman, D. G., & Bland, J. M. (2005). Standard deviations and standard
#' errors. Bmj, 331(7521), 903.
#'
#' Efron, B. (1987). Better bootstrap confidence intervals. Journal of the
#' American statistical Association, 82(397), 171-185.
#'
#' Efron, B., & Tibshirani, R. J. (1993). An introduction to the bootstrap. New
#' York: Chapman & Hall.
#'
#' @seealso \code{\link[ggplot2]{geom_point}}, \code{\link[ggplot2]{geom_line}},
#'   \code{\link[ggplot2]{geom_bar}}, \code{\link[plotly]{ggplotly}},
#'   \code{\link[base]{mean}}, \code{\link[stats]{sd}}, \code{\link[stats]{quantile}},
#'   \code{\link[boot]{boot.ci}}, \code{\link{median_ci}},
#'
#' @export
plot_stat_error <- function(data, y, x = NULL, geom = c("point", "bar"), stat = c("mean", "median"),
                            error = c("ci", "sd", "var", "quartile"),
                            ci_level = 0.95, ci_type = c("perc","bca", "norm", "basic"),
                            replicates = 2000, parallel = FALSE, cores = NULL,
                            xlab = NULL, ylab = NULL, title = NULL, ...,
                            ylim = c(NA, NA), transform_y = FALSE, y_transformation = "log10",
                            x_var_order = NULL, x_var_labs = NULL,
                            fill_var = NULL, fill_var_order = NULL, fill_var_values = NULL,
                            fill_var_labs = NULL, fill_var_title = NULL,
                            colour_var = NULL, colour_var_order = NULL, colour_var_values = NULL,
                            colour_var_labs = NULL, colour_var_title = NULL, greyscale = FALSE,
                            b_width = 0.75, p_size = 2, p_shape = 21,
                            dodge_width = 0.9, eb_size = 0.3, eb_width = 0.2, eb_alpha = 1,
                            eb_linetype = 1, eb_colour = NULL,
                            add_lines = F, line_alpha = 0.75, line_group = NULL,
                            line_colour = NULL, linetype = 1, line_size = 0.5,
                            theme = "classic", text_size = 14, font = c("sans", "serif", "mono"),
                            coord_flip = FALSE, omit_legend = FALSE,
                            legend_position = c("right", "left", "bottom", "top"),
                            facet_var = NULL, facet_var_order = NULL, facet_var_labs = NULL,
                            facet_var_strip_position = c("top", "bottom"),
                            facet_var_text_bold = TRUE,
                            print_stats = F, aesthetic_options = FALSE,
                            output = "p", interactive = FALSE, na.rm = TRUE){

  geom <- match.arg(geom)
  stat <- match.arg(stat)
  error <- match.arg(error)
  ci_type <- match.arg(ci_type)
  font <- match.arg(font)
  legend_position <- match.arg(legend_position)
  facet_var_strip_position <- match.arg(facet_var_strip_position)

  #x-variable recoding
  if(!missing(x)){
    data <- data %>%
      dplyr::mutate({{x}} := as.character({{x}}))
  }
  if(!missing(x_var_order)){
    data <- data %>%
      dplyr::mutate({{x}} := forcats::fct_relevel({{x}}, levels = !!!x_var_order))
  }
  if(!missing(x_var_labs)){
    data <- data %>%
      dplyr::mutate({{x}} := forcats::fct_recode({{x}}, !!!x_var_labs))
  }

  #fill variable recoding
  if(!missing(fill_var)){
    data <- data %>%
      dplyr::mutate({{fill_var}} := as.character({{fill_var}}))
  }
  if(!missing(fill_var) && !missing(fill_var_order)){
    data <- data %>%
      dplyr::mutate({{fill_var}} := forcats::fct_relevel({{fill_var}}, levels = !!!fill_var_order))
  }
  if(!missing(fill_var) && !missing(fill_var_labs)){
    data <- data %>%
      dplyr::mutate({{fill_var}} := forcats::fct_recode({{fill_var}}, !!!fill_var_labs))
  }

  #colour variable recoding
  if(!missing(colour_var)){
    data <- data %>%
      dplyr::mutate({{colour_var}} := as.character({{colour_var}}))
  }
  if(!missing(colour_var) && !missing(colour_var_order)){
    data <- data %>%
      dplyr::mutate({{colour_var}} := forcats::fct_relevel({{colour_var}}, levels = !!!colour_var_order))
  }
  if(!missing(colour_var) && !missing(colour_var_labs)){
    data <- data %>%
      dplyr::mutate({{colour_var}} := forcats::fct_recode({{colour_var}}, !!!colour_var_labs))
  }

  #facet label recoding
  if(!missing(facet_var)){
    data <- data %>%
      dplyr::mutate({{facet_var}} := as.character({{facet_var}}))
  }
  if(!missing(facet_var) && !missing(facet_var_order)){
    data <- data %>%
      dplyr::mutate({{facet_var}} := forcats::fct_relevel({{facet_var}}, levels = !!!facet_var_order))
  }
  if(!missing(facet_var) && !missing(facet_var_labs)){
    data <- data %>%
      dplyr::mutate({{facet_var}} := forcats::fct_recode({{facet_var}}, !!!facet_var_labs))
  }
  DT <- data.table::as.data.table(data)
  ST <- deparse(substitute(stat))
  Y <- deparse(substitute(y))

  if(!is.numeric(DT[[Y]])){
    stop("y must be a numeric vector or column of a data frame")
  }

  #grouping options
  if (!missing(x) && missing(fill_var) && missing(colour_var) && missing(facet_var)) {
    G <- deparse(substitute(x))
  } else if (missing(x) && !missing(fill_var) && missing(colour_var) && missing(facet_var)) {
    G <- deparse(substitute(fill_var))
  } else if (missing(x) && missing(fill_var) && !missing(colour_var) && missing(facet_var)) {
    G <- deparse(substitute(colour_var))
  } else if (missing(x) && missing(fill_var) && missing(colour_var) && !missing(facet_var)) {
    G <- deparse(substitute(facet_var))
  } else if (!missing(x) && !missing(fill_var) && missing(colour_var) && missing(facet_var)) {
    G <- c(deparse(substitute(x)), deparse(substitute(fill_var)))
  } else if (!missing(x) && missing(fill_var) && !missing(colour_var) && missing(facet_var)) {
    G <- c(deparse(substitute(x)), deparse(substitute(colour_var)))
  } else if (!missing(x) && missing(fill_var) && missing(colour_var) && !missing(facet_var)) {
    G <- c(deparse(substitute(x)), deparse(substitute(facet_var)))
  } else if (missing(x) && !missing(fill_var) && !missing(colour_var) && missing(facet_var)) {
    G <- c(deparse(substitute(fill_var)), deparse(substitute(colour_var)))
  } else if (missing(x) && !missing(fill_var) && missing(colour_var) && !missing(facet_var)) {
    G <- c(deparse(substitute(fill_var)), deparse(substitute(facet_var)))
  } else if (missing(x) && missing(fill_var) && !missing(colour_var) && !missing(facet_var)) {
    G <- c(deparse(substitute(colour_var)), deparse(substitute(facet_var)))
  } else if (!missing(x) && !missing(fill_var) && !missing(colour_var) && missing(facet_var)) {
    G <- c(deparse(substitute(fill_var)), deparse(substitute(colour_var)))
  } else if (!missing(x) && !missing(fill_var) && missing(colour_var) && !missing(facet_var)) {
    G <- c(deparse(substitute(x)), deparse(substitute(fill_var)), deparse(substitute(facet_var)))
  } else if (!missing(x) && missing(fill_var) && !missing(colour_var) && !missing(facet_var)) {
    G <- c(deparse(substitute(x)), deparse(substitute(colour_var)), deparse(substitute(facet_var)))
  } else if (missing(x) && !missing(fill_var) && !missing(colour_var) && !missing(facet_var)) {
    G <- c(deparse(substitute(fill_var)), deparse(substitute(colour_var)), deparse(substitute(facet_var)))
  } else if (!missing(x) && !missing(fill_var) && !missing(colour_var) && !missing(facet_var)) {
    G <- c(deparse(substitute(x)), deparse(substitute(colour_var)), deparse(substitute(fill_var)), deparse(substitute(facet_var)))
  }

  #produce the descriptive stats & plot

  if(stat == "mean" && error == "se"){
    if(!missing(x) || !missing(colour_var) || !missing(fill_var) || !missing(facet_var)){
      desc <- DT[, .(cases = .N,
                     n = sum(!is.na(get(Y))),
                     na = sum(is.na(get(Y))),
                     p_na = round(sum(is.na(get(Y)))/length(get(Y)), 3),
                     mean = round(sum(get(Y), na.rm = na.rm)/length(na.omit(get(Y))), 3),
                     se = round(se(get(Y), na.rm = na.rm), 3)),
                 by = eval(G)] %>%
        tibble::as_tibble()

    } else {
      desc <- DT[, .(cases = .N,
                     n = sum(!is.na(get(Y))),
                     na = sum(is.na(get(Y))),
                     p_na = round(sum(is.na(get(Y)))/length(get(Y)), 3),
                     mean = round(sum(get(Y), na.rm = na.rm)/length(na.omit(get(Y))), 3),
                     se = round(se(get(Y), na.rm = na.rm), 3))] %>%
        tibble::as_tibble()
    }
    if (print_stats == T){
      print(desc, n = Inf)
    }

    if(missing(x)){
      p <- desc %>%
        ggplot2::ggplot(ggplot2::aes(y = mean, x = 1, fill = {{fill_var}}, colour = {{colour_var}}))

    } else if(!missing(x)){
      p <- desc %>%
        ggplot2::ggplot(ggplot2::aes(y = mean, x = {{x}}, fill = {{fill_var}}, colour = {{colour_var}}))
    }

    if (geom == "bar") {
      p  <- p + ggplot2::geom_bar(position = ggplot2::position_dodge(dodge_width), width = b_width,
                                  stat = "identity", ...)
    } else if (geom == "point"){
      p  <- p + ggplot2::geom_point(position = ggplot2::position_dodge(dodge_width),
                                    size = p_size, shape = p_shape, ...)
    }

    if (missing(eb_colour)){
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se),
                                      size = eb_size, width = eb_width, alpha = eb_alpha,
                                      linetype = eb_linetype,
                                      position = ggplot2::position_dodge(dodge_width))
      p
    } else if (!missing(eb_colour) && missing(colour_var)){
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se),
                                      size = eb_size, width = eb_width, alpha = eb_alpha,
                                      linetype = eb_linetype, colour = eb_colour,
                                      position = ggplot2::position_dodge(dodge_width))
      p
    } else if (!missing(eb_colour) && !missing(colour_var)){
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se, group = {{colour_var}}),
                                      size = eb_size, width = eb_width, alpha = eb_alpha,
                                      linetype = eb_linetype, colour = eb_colour,
                                      position = ggplot2::position_dodge(dodge_width))
      p
    }

    if(!missing(ylab)){
      p <- p + ggplot2::labs(y = ylab)
    } else if(missing(ylab)){
      p <- p + ggplot2::labs(y = paste0("mean ", Y, " \u00B1 SE"))
    }

  } else if(stat == "mean" && error == "sd"){
    if(!missing(x) || !missing(colour_var) || !missing(fill_var) || !missing(facet_var)){
      desc <- DT[, .(cases = .N,
                     n = sum(!is.na(get(Y))),
                     na = sum(is.na(get(Y))),
                     p_na = round(sum(is.na(get(Y)))/length(get(Y)), 3),
                     mean = round(sum(get(Y), na.rm = na.rm)/length(na.omit(get(Y))), 3),
                     sd = round(stats::sd(get(Y), na.rm = na.rm), 3)),
                 by = eval(G)] %>%
        tibble::as_tibble()

    } else {
      desc <- DT[, .(cases = .N,
                     n = sum(!is.na(get(Y))),
                     na = sum(is.na(get(Y))),
                     p_na = round(sum(is.na(get(Y)))/length(get(Y)), 3),
                     mean = round(sum(get(Y), na.rm = na.rm)/length(na.omit(get(Y))), 3),
                     sd = round(stats::sd(get(Y), na.rm = na.rm), 3))] %>%
        tibble::as_tibble()
    }
    if (print_stats == T){
      print(desc, n = Inf)
    }

    if(missing(x)){
      p <- desc %>%
        ggplot2::ggplot(ggplot2::aes(y = mean, x = 1, fill = {{fill_var}}, colour = {{colour_var}}))

    } else if(!missing(x)){
      p <- desc %>%
        ggplot2::ggplot(ggplot2::aes(y = mean, x = {{x}}, fill = {{fill_var}}, colour = {{colour_var}}))
    }

    if (geom == "bar") {
      p  <- p + ggplot2::geom_bar(position = ggplot2::position_dodge(dodge_width), width = b_width,
                                  stat = "identity", ...)
    } else if (geom == "point"){
      p  <- p + ggplot2::geom_point(position = ggplot2::position_dodge(dodge_width),
                                    size = p_size, shape = p_shape, ...)
    }

    if (missing(eb_colour)){
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - sd, ymax = mean + sd),
                                      size = eb_size, width = eb_width, alpha = eb_alpha,
                                      linetype = eb_linetype,
                                      position = ggplot2::position_dodge(dodge_width))
      p
    } else if (!missing(eb_colour) && missing(colour_var)) {
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - sd, ymax = mean + sd),
                                      size = eb_size, width = eb_width, alpha = eb_alpha,
                                      linetype = eb_linetype, colour = eb_colour,
                                      position = ggplot2::position_dodge(dodge_width))
      p
    } else if (!missing(eb_colour) && !missing(colour_var)) {
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - sd, ymax = mean + sd, group = {{colour_var}}),
                                      size = eb_size, width = eb_width, alpha = eb_alpha,
                                      linetype = eb_linetype, colour = eb_colour,
                                      position = ggplot2::position_dodge(dodge_width))
      p
    }

    if(!missing(ylab)){
      p <- p + ggplot2::labs(y = ylab)
    } else if(missing(ylab)){
      p <- p + ggplot2::labs(y = paste0("mean ", Y, " \u00B1 s"))
    }

  } else if(stat == "mean" && error == "var"){
    if(!missing(x) || !missing(colour_var) || !missing(fill_var) || !missing(facet_var)){
      desc <- DT[, .(cases = .N,
                     n = sum(!is.na(get(Y))),
                     na = sum(is.na(get(Y))),
                     p_na = round(sum(is.na(get(Y)))/length(get(Y)), 3),
                     mean = round(sum(get(Y), na.rm = na.rm)/length(na.omit(get(Y))), 3),
                     var = round(stats::var(get(Y), na.rm = na.rm), 3)),
                 by = eval(G)] %>%
        tibble::as_tibble()

    } else {
      desc <- DT[, .(cases = .N,
                     n = sum(!is.na(get(Y))),
                     na = sum(is.na(get(Y))),
                     p_na = round(sum(is.na(get(Y)))/length(get(Y)), 3),
                     mean = round(sum(get(Y), na.rm = na.rm)/length(na.omit(get(Y))), 3),
                     var = round(stats::var(get(Y), na.rm = na.rm), 3))] %>%
        tibble::as_tibble()
    }
    if (print_stats == T){
      print(desc, n = Inf)
    }

    if(missing(x)){
      p <- desc %>%
        ggplot2::ggplot(ggplot2::aes(y = mean, x = 1, fill = {{fill_var}}, colour = {{colour_var}}))

    } else if(!missing(x)){
      p <- desc %>%
        ggplot2::ggplot(ggplot2::aes(y = mean, x = {{x}}, fill = {{fill_var}}, colour = {{colour_var}}))
    }

    if (geom == "bar") {
      p  <- p + ggplot2::geom_bar(position = ggplot2::position_dodge(dodge_width), width = b_width,
                                  stat = "identity", ...)
    } else if (geom == "point"){
      p  <- p + ggplot2::geom_point(position = ggplot2::position_dodge(dodge_width),
                                    size = p_size, shape = p_shape, ...)
    }

    if (missing(eb_colour)){
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - var, ymax = mean + var),
                                      size = eb_size, width = eb_width, alpha = eb_alpha,
                                      linetype = eb_linetype,
                                      position = ggplot2::position_dodge(dodge_width))
      p
    } else if (!missing(eb_colour) && missing(colour_var)) {
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - var, ymax = mean + var),
                                      size = eb_size, width = eb_width, alpha = eb_alpha,
                                      linetype = eb_linetype, colour = eb_colour,
                                      position = ggplot2::position_dodge(dodge_width))
      p
    } else if (!missing(eb_colour) && !missing(colour_var)) {
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - var, ymax = mean + var, group = {{colour_var}}),
                                      size = eb_size, width = eb_width, alpha = eb_alpha,
                                      linetype = eb_linetype, colour = eb_colour,
                                      position = ggplot2::position_dodge(dodge_width))
      p
    }

    if(!missing(ylab)){
      p <- p + ggplot2::labs(y = ylab)
    } else if(missing(ylab)){
      p <- p + ggplot2::labs(y = paste0("mean ", Y, " \u00B1 s\u00B2"))
    }

  } else if(stat == "mean" & error == "ci"){
    if(!missing(x) || !missing(colour_var) || !missing(fill_var) || !missing(facet_var)){
      desc <- DT[, .(cases = .N,
                     n = sum(!is.na(get(Y))),
                     na = sum(is.na(get(Y))),
                     p_na = round(sum(is.na(get(Y)))/length(get(Y)), 3),
                     lower = round(mean(get(Y), na.rm = T) - (abs(stats::qnorm((1-ci_level)/2))*se(get(Y))), 3),
                     mean = round(sum(get(Y), na.rm = na.rm)/length(na.omit(get(Y))), 3),
                     upper = round(mean(get(Y), na.rm = T) + (abs(stats::qnorm((1-ci_level)/2))*se(get(Y))), 3)),
                 by = eval(G)] %>%
        tibble::as_tibble()

    } else {
      desc <- DT[, .(cases = .N,
                     n = sum(!is.na(get(Y))),
                     na = sum(is.na(get(Y))),
                     p_na = round(sum(is.na(get(Y)))/length(get(Y)), 3),
                     lower = round(mean(get(Y), na.rm = T) - (abs(stats::qnorm((1-ci_level)/2))*se(get(Y))), 3),
                     mean = round(sum(get(Y), na.rm = na.rm)/length(na.omit(get(Y))), 3),
                     upper = round(mean(get(Y), na.rm = T) + (abs(stats::qnorm((1-ci_level)/2))*se(get(Y))), 3))] %>%
        tibble::as_tibble()
    }
    if (print_stats == T){
      print(desc, n = Inf)
    }

    if(missing(x)){
      p <- desc %>%
        ggplot2::ggplot(ggplot2::aes(y = mean, x = 1, fill = {{fill_var}}, colour = {{colour_var}}))

    } else if(!missing(x)){
      p <- desc %>%
        ggplot2::ggplot(ggplot2::aes(y = mean, x = {{x}}, fill = {{fill_var}}, colour = {{colour_var}}))
    }

    if (geom == "bar") {
      p  <- p + ggplot2::geom_bar(position = ggplot2::position_dodge(dodge_width),
                                  width = b_width,
                                  stat = "identity", ...)
    } else if (geom == "point"){
      p  <- p + ggplot2::geom_point(position = ggplot2::position_dodge(dodge_width),
                                    size = p_size, shape = p_shape, ...)
    }
    if (missing(eb_colour)) {
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
                                      position = ggplot2::position_dodge(dodge_width),
                                      size = eb_size, width = eb_width, alpha = eb_alpha,
                                      linetype = eb_linetype)
      p
    } else if (!missing(eb_colour) && missing(colour_var)) {
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
                                      position = ggplot2::position_dodge(dodge_width),
                                      size = eb_size, width = eb_width, alpha = eb_alpha,
                                      linetype = eb_linetype, colour = eb_colour)
      p
    } else if (!missing(eb_colour) && !missing(colour_var)) {
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper, group = {{colour_var}}),
                                      position = ggplot2::position_dodge(dodge_width),
                                      size = eb_size, width = eb_width, alpha = eb_alpha,
                                      linetype = eb_linetype, colour = eb_colour)
      p
    }
    if(!missing(ylab)){
      p <- p + ggplot2::labs(y = ylab)
    } else if(missing(ylab)){
      p <- p + ggplot2::labs(y = paste0("mean ", Y, " \u00B1 ", scales::percent(ci_level),  " CI"))
    }

  } else if(stat == "median" && error == "quartile"){
    if(!missing(x) || !missing(colour_var) || !missing(fill_var) || !missing(facet_var)){
      desc <- DT[, .(cases = .N,
                     n = sum(!is.na(get(Y))),
                     na = sum(is.na(get(Y))),
                     p_na = round(sum(is.na(get(Y)))/length(get(Y)), 3),
                     p25 = round(stats::quantile(get(Y), probs = 0.25, na.rm = na.rm), 3),
                     p50 = round(stats::quantile(get(Y), probs = 0.50, na.rm = na.rm), 3),
                     p75 = round(stats::quantile(get(Y), probs = 0.75, na.rm = na.rm), 3)),
                 by = eval(G)] %>%
        tibble::as_tibble()

    } else {
      desc <- DT[, .(cases = .N,
                     n = sum(!is.na(get(Y))),
                     na = sum(is.na(get(Y))),
                     p_na = round(sum(is.na(get(Y)))/length(get(Y)), 3),
                     p25 = round(stats::quantile(get(Y), probs = 0.25, na.rm = na.rm), 3),
                     p50 = round(stats::quantile(get(Y), probs = 0.50, na.rm = na.rm), 3),
                     p75 = round(stats::quantile(get(Y), probs = 0.75, na.rm = na.rm), 3))] %>%
        tibble::as_tibble()
    }
    if (print_stats == T){
      print(desc, n = Inf)
    }
    if(missing(x)){
      p <- desc %>%
        ggplot2::ggplot(ggplot2::aes(y = p50, x = 1, fill = {{fill_var}}, colour = {{colour_var}}))

    } else if(!missing(x)){
      p <- desc %>%
        ggplot2::ggplot(ggplot2::aes(y = p50, x = {{x}}, fill = {{fill_var}}, colour = {{colour_var}}))
    }

    if (geom == "bar") {
      p  <- p + ggplot2::geom_bar(position = ggplot2::position_dodge(dodge_width), width = b_width,
                                  stat = "identity", ...)
    } else if (geom == "point"){
      p  <- p + ggplot2::geom_point(position = ggplot2::position_dodge(dodge_width),
                                    size = p_size, shape = p_shape, ...)
    }
    if (missing(eb_colour)) {
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = p25, ymax = p75),
                                      size = eb_size, width = eb_width, alpha = eb_alpha,
                                      linetype = eb_linetype,
                                      position = ggplot2::position_dodge(dodge_width))
      p
    } else if (!missing(eb_colour) && missing(colour_var)) {
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = p25, ymax = p75),
                                      size = eb_size, width = eb_width, alpha = eb_alpha,
                                      linetype = eb_linetype, colour = eb_colour,
                                      position = ggplot2::position_dodge(dodge_width))
      p
    } else if (!missing(eb_colour) && !missing(colour_var)) {
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = p25, ymax = p75, group = {{colour_var}}),
                                      size = eb_size, width = eb_width, alpha = eb_alpha,
                                      linetype = eb_linetype, colour = eb_colour,
                                      position = ggplot2::position_dodge(dodge_width))
      p
    }
    if(!missing(ylab)){
      p <- p + ggplot2::labs(y = ylab)
    } else if(missing(ylab)){
      p <- p + ggplot2::labs(y = paste0("median ", Y, " \u00B1 quartile"))
    }
  } else if(stat == "median" & error == "ci"){
    if(!missing(x) || !missing(fill_var) || !missing(colour_var) || !missing(facet_var)){
      desc1 <- DT[, .(cases = .N,
                      n = sum(!is.na(get(Y))),
                      na = sum(is.na(get(Y))),
                      p_na = round(sum(is.na(get(Y)))/length(get(Y)), 3)),
                  by = eval(G)] %>%
        tibble::as_tibble()
      desc2 <- DT[,
                  .(measure = c("lower", "median", "upper"),
                    value = median_ci(get(Y), replicates = replicates, ci_type = ci_type,
                                      ci_level = ci_level, parallel = parallel, cores = cores)),
                  by = eval(G)] %>%
        stats::na.omit() %>%
        data.table::dcast(formula = ... ~ measure, value.var = "value") %>%
        tibble::as_tibble()
      suppressMessages(
        desc <- dplyr::left_join(desc1, desc2)
      )
    } else {
      desc1 <- DT[, .(cases = .N,
                      n = sum(!is.na(get(Y))),
                      na = sum(is.na(get(Y))),
                      p_na = round(sum(is.na(get(Y)))/length(get(Y)), 3))] %>%
        tibble::as_tibble()
      desc2 <- DT[,
                  .(measure = c("lower", "median", "upper"),
                    value = median_ci(get(Y), replicates = replicates, ci_type = ci_type,
                                      ci_level = ci_level, parallel = parallel, cores = cores))] %>%
        stats::na.omit() %>%
        data.table::dcast(formula = ... ~ measure, value.var = "value") %>%
        tibble::as_tibble() %>% .[,-1]
      desc <- dplyr::bind_cols(desc1, desc2)
    }
    if (print_stats == T){
      print(desc, n = Inf)
    }
    if(missing(x)){
      p <- desc %>%
        ggplot2::ggplot(ggplot2::aes(y = median, x = 1, fill = {{fill_var}}, colour = {{colour_var}}))

    } else if(!missing(x)){
      p <- desc %>%
        ggplot2::ggplot(ggplot2::aes(y = median, x = {{x}}, fill = {{fill_var}}, colour = {{colour_var}}))
    }

    if (geom == "bar") {
      p  <- p + ggplot2::geom_bar(position = ggplot2::position_dodge(dodge_width), width = b_width,
                                  stat = "identity", ...)
    } else if (geom == "point"){
      p  <- p + ggplot2::geom_point(position = ggplot2::position_dodge(dodge_width),
                                    size = p_size, shape = p_shape, ...)
    }
    if (missing(eb_colour)) {
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
                                      size = eb_size, width = eb_width, alpha = eb_alpha,
                                      linetype = eb_linetype,
                                      position = ggplot2::position_dodge(dodge_width))
      p
    } else if (!missing(eb_colour) && missing(colour_var)) {
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
                                      size = eb_size, width = eb_width, alpha = eb_alpha,
                                      linetype = eb_linetype, colour = eb_colour,
                                      position = ggplot2::position_dodge(dodge_width))
      p
    } else if (!missing(eb_colour) && !missing(colour_var)) {
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper, group = {{colour_var}}),
                                      size = eb_size, width = eb_width, alpha = eb_alpha,
                                      linetype = eb_linetype, colour = eb_colour,
                                      position = ggplot2::position_dodge(dodge_width))
      p
    }
    if(!missing(ylab)){
      p <- p + ggplot2::labs(y = ylab)
    } else if(missing(ylab)){
      if(ci_type == "bca"){
        ci_tp <- "BCa"
      } else if(ci_type == "perc") {
        ci_tp <- "percentile"
      } else if(ci_type == "norm") {
        ci_tp <- "normal"
      } else if(ci_type == "basic") {
        ci_tp <- "basic"
      }
      p <- p + ggplot2::labs(y = paste0("median ", Y, " \u00B1 ",
                                        scales::percent(ci_level),  " ", ci_tp,
                                        " CI\n(", scales::comma(replicates)," replicates)"))
    }
  }

  #change the values of the fill or colour variables
  if (!missing(fill_var) && !missing(fill_var_values)){
    p <- p +
      ggplot2::scale_fill_manual(values = fill_var_values)
  }
  if (!missing(colour_var) && !missing(colour_var_values)){
    p <- p +
      ggplot2::scale_colour_manual(values = colour_var_values)
  }
  #modification of axis labels
  y_name <-  data %>% dplyr::select({{y}}) %>% names
  if(!missing(xlab)){
    p <- p + ggplot2::labs(x = xlab)
  }
  if(!missing(fill_var_title)){
    p <- p + ggplot2::labs(fill = fill_var_title)
  }
  if(!missing(colour_var_title)){
    p <- p + ggplot2::labs(color = colour_var_title)
  }
  if(!missing(title)){
    p <- p + ggplot2::ggtitle(title)
  }

  #themes
  if(theme == "classic"){
    p <- p + ggplot2::theme_classic(base_size = text_size, base_family = font)
  } else if (theme == "bw"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "b & w"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "black and white"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "black & white"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "grey"){
    p <- p + ggplot2::theme_grey(base_size = text_size, base_family = font)
  } else if (theme == "gray"){
    p <- p + ggplot2::theme_gray(base_size = text_size, base_family = font)
  } else if (theme == "light"){
    p <- p + ggplot2::theme_light(base_size = text_size, base_family = font)
  } else if (theme == "dark"){
    p <- p + ggplot2::theme_dark(base_size = text_size, base_family = font)
  } else if (theme == "minimal"){
    p <- p + ggplot2::theme_minimal(base_size = text_size, base_family = font)
  }

  #lines
  if(add_lines == T && missing(x)){
    errorCondition("a variable must be assigned to x to connect statistical estimates with lines")
  } else if (add_lines == T && !missing(x)) {
    if (missing(fill_var) && missing(colour_var)){
      if (!missing(line_colour)) {
        p <- p + ggplot2::geom_line(aes(group = 1), position = ggplot2::position_dodge(dodge_width),
                                    colour = line_colour, linetype = linetype, size = line_size,
                                    alpha = line_alpha)
      } else {
        p <- p + ggplot2::geom_line(aes(group = 1), position = ggplot2::position_dodge(dodge_width),
                                    linetype = linetype, size = line_size,
                                    alpha = line_alpha)
      }

    } else if (!missing(fill_var) && missing(colour_var)){
      if (!missing(line_colour)) {
        p <- p + ggplot2::geom_line(aes(group = {{fill_var}}), position = ggplot2::position_dodge(dodge_width),
                                    colour = line_colour, linetype = linetype, size = line_size,
                                    alpha = line_alpha)
      } else {
        p <- p + ggplot2::geom_line(aes(group = {{fill_var}}), position = ggplot2::position_dodge(dodge_width),
                                    linetype = linetype, size = line_size,
                                    alpha = line_alpha)
      }
    } else if (missing(fill_var) && !missing(colour_var)){
      if (!missing(line_colour)) {
        p <- p + ggplot2::geom_line(aes(group = {{colour_var}}), position = ggplot2::position_dodge(dodge_width),
                                    colour = line_colour, linetype = linetype, size = line_size,
                                    alpha = line_alpha)
      } else {
        p <- p + ggplot2::geom_line(aes(group = {{colour_var}}, colour = {{colour_var}}),
                                    position = ggplot2::position_dodge(dodge_width),
                                    linetype = linetype, size = line_size,
                                    alpha = line_alpha)
      }
    } else if (!missing(fill_var) && !missing(colour_var) && !missing(line_group)){
      if (!missing(line_colour)) {
        p <- p + ggplot2::geom_line(aes(group = {{line_group}}), position = ggplot2::position_dodge(dodge_width),
                                    colour = line_colour, linetype = linetype, size = line_size,
                                    alpha = line_alpha)
      } else {
        p <- p + ggplot2::geom_line(aes(group = {{line_group}}, colour = {{line_group}}),
                                    position = ggplot2::position_dodge(dodge_width),
                                    colour = line_colour, linetype = linetype, size = line_size,
                                    alpha = line_alpha)
      }
    } else if (!missing(fill_var) && !missing(colour_var) && missing(line_group)){
      errorCondition("When variables are assigned to both fill and colour, specify which to use for splitting lines using line_group")
    }
  }

  #modification of y-axis limits & transformations
  if(!missing(ylim) & transform_y == FALSE){
    p <- p + ggplot2::lims(y = ylim)
  } else if (missing(ylim) & transform_y == TRUE){
    p <- p + ggplot2::scale_y_continuous(limits = c(NA, NA), trans = y_transformation)
  } else if (!missing(ylim) & transform_y  == TRUE){
    p <- p + ggplot2::scale_y_continuous(limits = c(ylim[1], ylim[2]), trans = y_transformation)
  }

  #misc
  if(greyscale == TRUE){
    p <- p + ggplot2::scale_fill_grey()
  }

  if(coord_flip == F){
    if(missing(x)){
      p <- p + ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                              axis.text.x = ggplot2::element_blank(),
                              axis.ticks.x = ggplot2::element_blank())
    }
  } else if (coord_flip == T){
    p <- p + ggplot2::coord_flip()
    if(missing(x)){
      p <- p + ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                              axis.text.y = ggplot2::element_blank(),
                              axis.ticks.y = ggplot2::element_blank())
    }
  }
  if(legend_position != "right"){
    p <- p + ggplot2::theme(legend.position = legend_position)
  }

  if(omit_legend == TRUE){
    p <- p + ggplot2::theme(legend.position = "none")
  }

  if(aesthetic_options == TRUE){
    utils::browseURL("https://ggplot2.tidyverse.org/articles/ggplot2-specs.html")
  }

  #facets
  if(!missing(facet_var)){
    p <- p + ggplot2::facet_wrap(vars({{facet_var}}), strip.position = facet_var_strip_position)
  }
  if(!missing(facet_var) && facet_var_text_bold == TRUE){
    p <- p + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
  }

  if(output == "p"){
    if(interactive == TRUE){
      return(plotly::ggplotly(p))
    }
    if(interactive == FALSE){
      return(p)
    }
  } else if (output == "ps"){
    if(interactive == TRUE){
      p <- plotly::ggplotly(p)
      out <- list("plot" = plotly::ggplotly(p), "stats" = desc)
      return(out)
    }
    if(interactive == FALSE){
      out <- list("plot" = p, "stats" = desc)
      return(out)
    }
  } else {
    errorCondition("output argument must be set to either \"p\" for the plot only or \"ps\" for a list containing the plot and the stats used to produce it")
  }
}


# start of plot_pie -------------------------------------------------------
#' @title
#'
#' Generate a pie chart.
#'
#' @description Easily generate pie charts, AKA bar charts with polar
#'   coordinates, using ggplot2 with a simplified customization interface for
#'   common modifications. Pie charts are rarely the most effective way of
#'   visualizing data (especially when >5 groups are being compared), but that
#'   doesn't mean there shouldn't be an easy way to build one with ggplot2 in
#'   case your project stakeholders ask.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr count
#' @importFrom dplyr arrange
#' @importFrom dplyr summarise
#' @importFrom forcats fct_relevel
#' @importFrom forcats fct_recode
#' @importFrom forcats fct_reorder
#' @importFrom forcats fct_infreq
#' @importFrom forcats fct_rev
#' @importFrom forcats fct_lump_n
#' @importFrom rlang !!!
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 coord_polar
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_fill_grey
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 theme_void
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 margin
#' @importFrom ggplot2 element_text
#' @importFrom utils browseURL
#' @importFrom data.table uniqueN
#'
#' @param data A data frame or tibble containing at least one categorical
#'   variable.
#'
#' @param fill_var A categorical variable to assign to the slice fill colour,
#'   e.g. fill_var = grouping_variable. Produces separate slices each
#'   level of the fill variable. See \code{\link[ggplot2]{aes}} for details.
#'
#' @param y A numeric variable containing values to be used for calculating pie
#'   slice sizes. If y is not specified, then pie slice sizes will be based on
#'   the relative frequency of fill_var categories. If y is specified, then the
#'   slices will represent fractions of the sum of the y-variable under each
#'   category of fill_var.
#'
#' @param ... graphical parameters (not associated with variables) to be passed
#'   to \code{\link[ggplot2]{geom_bar}}, e.g. colour (affects slice outlines),
#'   to be applied to all slices. To see some of the available options in a web
#'   browser, set the aesthetic_options argument to TRUE.
#'
#' @param title Add a main title to the plot using a character string, e.g.
#'   "pie chart title"
#'
#' @param title_alignment Left-to-right alignment of the main plot title.
#'   Accepts values from 0 (far left) to 1 (far right). Default is 0.5 (centre).
#'
#' @param fill_var_order_by_y This allows you to sort the slices of the chart in
#'   order of increasing/ascending ("i" or "a") or decreasing ("d") value of y.
#'   If no variable is assigned to y, then the sorting occurs based on the
#'   frequencies of the fill_var categories.
#'
#' @param fill_var_order This allows you to manually modify the order of the
#'   fill variable groups, e.g. fill_var = grouping_variable, fill_var_order =
#'   c("group_2", "group_1"). See \code{\link[forcats]{fct_relevel}} for
#'   details.
#'
#' @param fill_var_labs This allows you to modify the labels of the fill
#'   variable groups, e.g. fill_var = grouping_variable, fill_var_labs =
#'   c("group_1_new_label" = "group_1_old_label", "group_2_new_label" =
#'   "group_2_old_label"). See \code{\link[forcats]{fct_recode}} for details.
#'
#' @param fill_var_values This allows you to modify the colours assigned to the
#'   fill of each of the fill variable groups, e.g. fill_var =
#'   grouping_variable, fill_var_values = c("blue", "red"). See
#'   \code{\link[ggplot2]{scale_fill_manual}} for details. For the colour
#'   options available in base R, see \code{\link[elucidate]{colour_options}}.
#'
#' @param fill_var_title this allows you to modify the fill variable label in
#'   the plot legend.
#'
#' @param slice_text Adds text with slice percentages ("pct"), total
#'   counts/values ("tot"), or fill_var group labels ("grp") to the middle of
#'   each slice.
#'
#' @param slice_text_prefix Adds a prefix string to slice_text labels separated by a
#'   single space, e.g. if your slices represent monetary totals (via a
#'   y-variable), then you might set this to "$"
#'
#' @param slice_text_suffix Adds a suffix string to slice_text labels separated by a
#'   single space, e.g. if your slices represent percentages (e.g. slice_text =
#'   "pct"), then you may want to set this to "%"
#'
#' @param slice_text_colour Controls slice text font colour. For the colour
#'   options available in base R, see \code{\link[elucidate]{colour_options}}.
#'
#' @param slice_text_size Controls the slice text font size.
#'
#' @param slice_text_custom Use this to specify custom slice text labels instead
#'   of using one of the convenience options provided by the slice_text
#'   argument. Must be a character vector of length equal to the number of
#'   slices (fill_var categories).
#'
#' @param round_n If slice_text = "pct" or "tot" this allows you to round the
#'   values to n significant digits. See the \code{\link{round}} "n" argument
#'   documentation for details.
#'
#' @param lump_n If there are so many fill_var categories that you find the plot
#'   difficult to interpret, you can use this to lump/combine the least common
#'   categories together into an "other" category. Simply specify the number of
#'   categories you want to retain and the rest will be lumped together. See
#'   \code{\link[forcats]{fct_lump_n}} for details. If "y" is specified, then
#'   the relative proportions of the fill_var group totals for the y variable
#'   will be used to determine which are the top n categories to retain (i.e.
#'   largest slices).
#'
#' @param lump_lab If lump_n is used, this allows you to change the label of the
#'   "other" category.
#'
#' @param facet_var Use if you want separate pie charts for each level of a
#'   grouping variable (i.e. a facetted plot), e.g. facet_var =
#'   grouping_variable. See \code{\link[ggplot2]{facet_wrap}} for details.
#'
#' @param facet_var_order If a variable has been assigned for facetting using
#'   facet_var, this allows you to modify the order of the variable groups, e.g.
#'   facet_var = grouping_variable, facet_var_order = c("group_2", "group_1").
#'   See \code{\link[forcats]{fct_relevel}} for details.
#'
#' @param facet_var_labs If a variable has been assigned for facetting using
#'   facet_var, this allows you to modify the labels of the variable groups
#'   which will appear in the facet strips, e.g. facet_var = grouping_variable,
#'   facet_var_labs = c("group_1_new_label" = "group_1_old_label",
#'   "group_2_new_label" = "group_2_old_label"). See
#'   \code{\link[forcats]{fct_recode}} for details.
#'
#' @param facet_var_strip_position If a variable has been assigned for facetting
#'   using facet_var, this allows you to modify the position of the facet strip
#'   labels. Sensible options include "top" (the default) or "bottom".
#'
#' @param facet_var_text_bold If a variable has been assigned for facetting
#'   using facet_var, this allows you to use boldface (TRUE/default or FALSE)
#'   for the facet strip label text.
#'
#' @param greyscale Set to TRUE if you want the plot converted to greyscale.
#'
#' @param text_size This controls the size of all plot text. Default = 14.
#'
#' @param font This controls the font of all plot text. Default = "sans"
#'   (Arial). Other options include "serif" (Times New Roman) and "mono"
#'   (Courier New).
#'
#' @param legend_position This allows you to modify the legend position.
#'   Options include "right" (the default), "left", "top", & "bottom".
#'
#' @param omit_legend Set to TRUE if you want to remove/omit the legends.
#'
#' @param aesthetic_options If set to TRUE, opens a web browser to the tidyverse
#'   online aesthetic options vignette.
#'
#' @return A ggplot pie chart.
#'
#' @author Craig P. Hutton, \email{Craig.Hutton@@gov.bc.ca}
#'
#' @examples
#'
#' plot_pie(mtcars,
#'          fill_var = cyl,
#'          slice_text = "pct",
#'          slice_text_suffix = "%",
#'          colour = "white",
#'          round_n = 2)
#'
#' @references
#' Wickham, H. (2016). ggplot2: elegant graphics for data analysis. New York, N.Y.: Springer-Verlag.
#'
#' @seealso \code{\link[ggplot2]{geom_bar}}, \code{\link[plotly]{ggplotly}},
#'   \code{\link{plot_stat_error}}
#'
#' @export
plot_pie <- function(data,
                     fill_var,
                     y = NULL,
                     ..., #geom-specific customization see ?geom_bar for details
                     title = NULL, title_alignment = 0.5,
                     fill_var_order_by_y = NULL,
                     fill_var_order = NULL,
                     fill_var_labs = NULL,
                     fill_var_values = NULL,
                     fill_var_title = NULL,
                     slice_text = NULL,
                     slice_text_prefix = "",
                     slice_text_suffix = "",
                     slice_text_colour = "black",
                     slice_text_size = 4,
                     slice_text_custom = NULL,
                     round_n = NULL,
                     lump_n = NULL,
                     lump_lab = NULL,
                     facet_var = NULL, facet_var_order = NULL, facet_var_labs = NULL,
                     facet_var_strip_position = c("top", "bottom"), facet_var_text_bold = TRUE,
                     greyscale = FALSE, #control transparency, convert to greyscale
                     text_size = 14, font = c("sans", "serif", "mono"), #theme options
                     legend_position = c("right", "left", "bottom", "top"), omit_legend = FALSE, #legend position
                     aesthetic_options = FALSE) {#output format

  if(!is.numeric(title_alignment)) {
    stop('"title_alignment" must be a number between 0 (left) and 1 (right)')
  }

  if(!missing(fill_var_order_by_y)) {
    if(fill_var_order_by_y != "d" && fill_var_order_by_y != "a" && fill_var_order_by_y != "i"){
      stop('"fill_var_order_by_y" should be one of "d", "a", or "i"')
    }
  }
  if(!missing(slice_text)) {
    if(slice_text != "pct" && slice_text != "tot" && slice_text != "grp"){
      stop('"slice_text" should be one of "pct", "tot", or "grp"')
    }
  }
  facet_var_strip_position <- match.arg(facet_var_strip_position)
  legend_position <- match.arg(legend_position)
  font <- match.arg(font)

  #fill variable recoding
  if(!missing(fill_var)){
    data <- dplyr::mutate(data, {{fill_var}} := as.character({{fill_var}}))
  } else {
    stop("A categorical variable in the data frame source must be assigned to fill_var!")
  }

  if(!missing(lump_n)) {
    if(!missing(y)) {
      if(!missing(lump_lab)) {
        data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_lump_n({{fill_var}}, w = {{y}}, n = lump_n, other_level = lump_lab))
      } else {
        data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_lump_n({{fill_var}}, w = {{y}}, n = lump_n, other_level = "other"))
      }
    } else {
      if(!missing(lump_lab)) {
        data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_lump_n({{fill_var}}, n = lump_n, other_level = lump_lab))
      } else {
        data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_lump_n({{fill_var}}, n = lump_n, other_level = "other"))
      }
    }
  }

  if(data.table::uniqueN(data[[deparse(substitute(fill_var))]]) > 5) {
    warning('Pie charts tend to be difficult to read with more than 5 slices.\n  Consider lumping infrequent categories together with "lump_n" or using plot_bar() to visualize these data.')
  }
  if(!missing(fill_var_order_by_y)) {
    if(!missing(y)) {
      if(fill_var_order_by_y == "d") {
        data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_reorder({{fill_var}}, {{y}}, .desc = TRUE))
      } else {
        data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_reorder({{fill_var}}, {{y}}, .desc = FALSE))
      }
    } else {
      if(fill_var_order_by_y == "d") {
        data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_infreq({{fill_var}}))
      } else {
        data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_rev(forcats::fct_infreq({{fill_var}})))
      }
    }
  }

  if(!missing(fill_var_order)){
    data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_relevel({{fill_var}}, levels = !!!fill_var_order))
  }
  if(!missing(fill_var_labs)){
    data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_recode({{fill_var}}, !!!fill_var_labs))
  }

  #facet label recoding
  if(!missing(facet_var)){
    data <- dplyr::mutate(data, {{facet_var}} := as.character({{facet_var}}))
  }
  if(!missing(facet_var) && !missing(facet_var_order)){
    data <- dplyr::mutate(data, {{facet_var}} := forcats::fct_relevel({{facet_var}}, levels = !!!facet_var_order))
  }
  if(!missing(facet_var) && !missing(facet_var_labs)){
    data <- dplyr::mutate(data, {{facet_var}} := forcats::fct_recode({{facet_var}}, !!!facet_var_labs))
  }

  #produce the descriptive stats & plot
  if(missing(y)) {
    if(missing(facet_var)) {
      plot_data <- dplyr::group_by(data, {{fill_var}})
      plot_data <- dplyr::count(plot_data, name = "n_")
      plot_data <- dplyr::ungroup(plot_data)
      plot_data <- dplyr::arrange(plot_data, rev({{fill_var}}))
      plot_data <- dplyr::mutate(plot_data, p_ = n_/sum(n_), st_coords = cumsum(p_) - 0.5*p_)
    } else {
      plot_data <- dplyr::group_by(data, {{fill_var}}, {{facet_var}})
      plot_data <- dplyr::count(plot_data, name = "n_")
      plot_data <- dplyr::ungroup(plot_data)
      plot_data <- dplyr::arrange(plot_data, rev({{fill_var}}))
      plot_data <- dplyr::group_by(plot_data, {{facet_var}})
      plot_data <- dplyr::mutate(plot_data, p_ = n_/sum(n_), st_coords = cumsum(p_) - 0.5*p_)
      plot_data <- dplyr::ungroup(plot_data)
    }
  } else {
    if(missing(facet_var)) {
      plot_data <- dplyr::group_by(data, {{fill_var}})
      plot_data <- dplyr::summarise(plot_data, ytot = sum({{y}}, na.rm = TRUE), .groups = "drop")
      plot_data <- dplyr::ungroup(plot_data)
      plot_data <- dplyr::arrange(plot_data, rev({{fill_var}}))
      plot_data <- dplyr::mutate(plot_data, p_ = ytot/sum(ytot), st_coords = cumsum(p_) - 0.5*p_)
    } else {
      plot_data <- dplyr::group_by(data, {{fill_var}}, {{facet_var}})
      plot_data <- dplyr::summarise(plot_data, ytot = sum({{y}}, na.rm = TRUE), .groups = "drop")
      plot_data <- dplyr::arrange(plot_data, rev({{fill_var}}))
      plot_data <- dplyr::group_by(plot_data, {{facet_var}})
      plot_data <- dplyr::mutate(plot_data, p_ = ytot/sum(ytot), st_coords = cumsum(p_) - 0.5*p_)
      plot_data <- dplyr::ungroup(plot_data)
    }
  }
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(y = p_, x = "", fill = {{fill_var}})) +
    ggplot2::geom_bar(stat = "identity", position = "stack", width = 1, ...) +
    ggplot2::coord_polar("y", start= 0)

  if(!missing(slice_text) && !missing(slice_text_custom)) {
    stop('only one of "slice_text" or "slice_text_custom" should be specified')
  } else if(!missing(slice_text) && missing(slice_text_custom)) {
    if(slice_text == "pct") {
      if(!missing(round_n)) {
        p <- p + ggplot2::geom_text(aes(x=1, y = st_coords,
                                        label=paste(slice_text_prefix, round(p_*100, round_n), slice_text_suffix)),
                                    colour = slice_text_colour, size = slice_text_size)
      } else {
        p <- p + ggplot2::geom_text(aes(x=1, y = st_coords,
                                        label=paste(slice_text_prefix, p_*100, slice_text_suffix)),
                                    colour = slice_text_colour, size = slice_text_size)
      }
    } else if (slice_text == "tot") {
      if(missing(y)) {
        if(!missing(round_n)) {
          p <- p + ggplot2::geom_text(aes(x=1, y = st_coords,
                                          label=paste(slice_text_prefix, round(n_, round_n), slice_text_suffix)),
                                      colour = slice_text_colour, size = slice_text_size)
        } else {
          p <- p + ggplot2::geom_text(aes(x=1, y = st_coords,
                                          label=paste(slice_text_prefix, n_), slice_text_suffix),
                                      colour = slice_text_colour, size = slice_text_size)
        }
      } else {
        if(!missing(round_n)) {
          p <- p + ggplot2::geom_text(aes(x=1, y = st_coords,
                                          label=paste(slice_text_prefix, round(ytot, round_n), slice_text_suffix)),
                                      colour = slice_text_colour, size = slice_text_size)
        } else {
          p <- p + ggplot2::geom_text(aes(x=1, y = st_coords,
                                          label=paste(slice_text_prefix, ytot, slice_text_suffix)),
                                      colour = slice_text_colour, size = slice_text_size)
        }
      }
    } else if (slice_text == "grp") {
      p <- p + ggplot2::geom_text(aes(x=1, y = st_coords, label=unique(data[[deparse(substitute(fill_var))]])),
                                  colour = slice_text_colour, size = slice_text_size)
    }
  } else if(missing(slice_text) && !missing(slice_text_custom)) {
    if(length(slice_text_custom) != data.table::uniqueN(data[[deparse(substitute(fill_var))]])) {
      stop('"slice_text_custom" must be a character vector with as many values as there are levels of "fill_var"')
    }
    p <- p + ggplot2::geom_text(aes(x=1, y = st_coords, label=slice_text_custom),
                                colour = slice_text_colour, size = slice_text_size)
  }

  #apply theme
  p <- p + ggplot2::theme_void(base_size = text_size, base_family = font)
  p <- p + ggplot2::theme(plot.margin = ggplot2::margin(0.5, 0.5, 0.5, 0.5, "cm"))

  #legend options
  if(omit_legend == TRUE){
    p <- p + ggplot2::theme(legend.position = "none")
  }
  if(legend_position != "right"){
    p <- p + ggplot2::theme(legend.position = legend_position)
  }
  #convert to greyscale
  if(greyscale == TRUE){
    p <- p + ggplot2::scale_fill_grey()
  }
  #title
  if(!missing(title)){
    p <- p + ggplot2::ggtitle(title)
    p <- p + ggplot2::theme(plot.title = ggplot2::element_text(hjust = title_alignment))
  }
  if(!missing(fill_var_title)){
    p <- p + ggplot2::labs(fill = fill_var_title)
  }
  #customize fill colours
  if (!missing(fill_var) && !missing(fill_var_values)){
    p <- p +
      ggplot2::scale_fill_manual(values = fill_var_values)
  }
  #facets
  if(!missing(facet_var)){
    p <- p + ggplot2::facet_wrap(vars({{facet_var}}), strip.position = facet_var_strip_position)
  }
  if(!missing(facet_var) && facet_var_text_bold == TRUE){
    p <- p + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
  }
  #misc
  if(aesthetic_options == TRUE){
    utils::browseURL("https://ggplot2.tidyverse.org/articles/ggplot2-specs.html")
  }
  return(p)
}
