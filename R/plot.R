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
# See the License for the specific language governing permissions and limitations under the License

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
#' Code was adapted from \url{http://bc.bojanorama.pl/2013/04/r-color-reference-sheet/}
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

# plot_density --------------------------------------------------------
#' @title
#'
#' Generate a density plot.
#'
#' @description Easily generate a density plot of a numeric variable using
#'   ggplot2 with a simplified customization interface for common modifications
#'   with static (ggplot) and interactive (plotly) output options. The static
#'   output is useful for producing static reports (e.g. for manuscripts) and is
#'   readily customized further using ggplot2 syntax. The interactive output is
#'   helpful for exploring the data and producing dynamic html reports. See
#'   \href{https://craig.rbind.io/post/2021-05-17-asgr-3-1-data-visualization/}{this
#'    blog post} for an introduction to ggplot2.
#'
#' @importFrom dplyr mutate
#' @importFrom forcats fct_relevel
#' @importFrom forcats fct_recode
#' @importFrom rlang !!!
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_density
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_fill_grey
#' @importFrom ggplot2 lims
#' @importFrom ggplot2 labs
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
#' @importFrom ggplot2 waiver
#' @importFrom ggplot2 coord_cartesian
#' @importFrom plotly ggplotly
#' @importFrom utils browseURL
#'
#' @param data A data frame or tibble containing the dependent measure "x" and
#'   any grouping variables.
#'
#' @param x The name of a numeric variable you want a density plot of (quoted or
#'   unquoted), e.g. x = "variable" or x = variable.
#'
#' @param ... graphical parameters (not associated with variables) to be passed
#'   to \code{\link[ggplot2]{geom_density}}, e.g. colour or fill, to be applied
#'   to all curves. See the ggplot2 aesthetic specifications
#'   \href{https://ggplot2.tidyverse.org/articles/ggplot2-specs.html}{vignette}
#'   for options.
#'
#' @param fill_var Use if you want to assign a variable to the density curve
#'   fill colour, e.g. fill_var = "grouping_variable" or fill_var =
#'   grouping_variable. Produces separate curves for each level of the fill
#'   variable. See \code{\link[ggplot2]{aes}} for details.
#'
#' @param colour_var Use if you want to assign a variable to the density curve
#'   outline colour, e.g. colour_var = "grouping_variable" or colour_var =
#'   grouping_variable. Produces separate curves for each level of the colour
#'   variable. See \code{\link[ggplot2]{aes}} for details.
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
#' @param xlim Specify the x-axis limits, e.g. xlim = c(lower_limit,
#'   upper_limit). Use NA for the existing minimum or maximum value of x, e.g.
#'   the default is xlim = c(NA, NA).
#'
#' @param xbreaks This allows you to change the break points to use for tick marks
#'   via a numeric vector. \code{\link{seq}} is particularly useful here. See
#'   \code{\link[ggplot2]{scale_x_continuous}} for details. If xbreaks is
#'   specified, then xlim should be also.
#'
#' @param transform_x Would you like to transform the x axis? (TRUE or FALSE)
#'
#' @param x_transformation If transform_x = TRUE, this determines the
#'   transformation to be applied. Common choices include "log10" (the default),
#'   "log2", "sqrt", or "exp". See \code{\link[ggplot2]{scale_continuous}} for
#'   details.
#'
#' @param x_var_labs Allows you to modify the labels displayed with the x-axis
#'   tick marks. See \code{\link[ggplot2]{scale_x_continuous}} for details.
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
#'   options available in base R, see \code{\link{colour_options}}.
#'
#' @param palette If a variable is assigned to fill_var or colour_var, this
#'   determines which
#'   \href{https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html}{viridis
#'    colour palette} to use. Options include "plasma" or "C" (default), "magma"
#'   or "A", "inferno" or "B", "viridis" or "D", and "cividis" or "E". See
#'   \href{https://craig.rbind.io/post/2021-05-17-asgr-3-1-data-visualization/#colourblind-friendly-palettes}{this
#'    link} for examples. You can override these colour palettes with
#'   fill_var_values or colour_var_values.
#'
#' @param palette_direction Choose "d2l" for dark to light (default) or "l2d"
#'   for light to dark.
#'
#' @param palette_begin Value between 0 and 1 that determines where along the
#'   full range of the chosen colour palette's spectrum to begin sampling
#'   colours. See \code{\link[ggplot2]{scale_fill_viridis_d}} for details.
#'
#' @param palette_end Value between 0 and 1 that determines where along the full
#'   range of the chosen colour palette's spectrum to end sampling colours. See
#'   \code{\link[ggplot2]{scale_fill_viridis_d}} for details.
#'
#' @param alpha This adjusts the transparency/opacity of the density plot(s),
#'   with acceptable values ranging from 0 = 100% transparent to 1 = 100%
#'   opaque.
#'
#' @param greyscale Set to TRUE if you want the plot converted to greyscale.
#'
#' @param line_size THis modifies the thickness of the density curve.
#'
#' @param rug Set this to TRUE to add rug lines to the bottom of the plot.
#'
#' @param rug_colour Determines the colour of the rug lines (if rug = TRUE).
#'
#' @param rug_alpha This adjusts the transparency/opacity of the rug lines (if
#'   rug = TRUE) with valid values ranging from 0 = 100% transparent to 1 = 100%
#'   opaque.
#'
#' @param dnorm Set this to TRUE to add a normal/Gaussian density curve to the
#'   plot. Ignored if x is a date vector or "transform_x" = TRUE.
#'
#' @param dnorm_colour Determines the colour of the normal density curve (if dnorm = TRUE).
#'
#' @param dnorm_line_type The type of line to use to draw the normal density
#'   curve (if dnorm = TRUE). Options include "dashed" (default), "solid",
#'   "dotted", "dotdash", "longdash", and "twodash".
#'
#' @param dnorm_line_size Adjusts the thickness of the normal density curve (if
#'   dnorm = TRUE).
#'
#' @param dnorm_alpha This adjusts the transparency/opacity of the normal
#'   density curve (if dnorm = TRUE) with valid values ranging from 0 = 100%
#'   transparent to 1 = 100% opaque.
#'
#' @param theme Adjusts the theme using 1 of 6 predefined "complete" theme
#'   templates provided by ggplot2. Currently supported options are: "classic"
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
#'   variable (i.e. a facetted plot), e.g. facet_var = "grouping_variable" or
#'   facet_var = grouping_variable. See \code{\link[ggplot2]{facet_wrap}} for
#'   details.
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
#' \donttest{
#' mtcars %>%
#'   plot_density(x = mpg, transform_x = TRUE, x_transformation = "log2")
#'
#' mtcars %>%
#'   plot_density(x = mpg, transform_x = TRUE, x_transformation = "log10") #default transformation
#'
#' mtcars %>%
#'   plot_density(x = mpg, transform_x = TRUE, x_transformation = "sqrt") #default transformation
#'
#' mtcars %>% plot_density(x = mpg, fill_var = cyl, fill_var_labs = c("four" = "4"))
#'
#' mtcars %>% plot_density(x = mpg, fill_var = cyl, fill_var_title = "# cyl",
#'                         interactive = TRUE)
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
#' mtcars %>% plot_density(mpg, fill_var = cyl, interactive = TRUE)
#' }
#'
#'
#' @references
#' Wickham, H. (2016). ggplot2: elegant graphics for data analysis. New York, N.Y.: Springer-Verlag.
#'
#' @seealso \code{\link[ggplot2]{geom_density}}, \code{\link[stats]{dnorm}},
#'   \code{\link[ggplot2]{geom_line}}, \code{\link[ggplot2]{geom_rug}},
#'   \code{\link[plotly]{ggplotly}}
#'
#' @export
plot_density <- function(data, x, #essential parameters
                         ..., #non-variable aesthetic specification
                         fill_var = NULL, colour_var = NULL, #grouping variable aesthetic mappings
                         xlab = NULL, ylab = NULL, title = NULL, fill_var_title = NULL, colour_var_title = NULL, #titles
                         xlim = c(NA, NA), xbreaks = ggplot2::waiver(), #control the x axis limits and scaling
                         transform_x = FALSE, x_transformation = "log10", x_var_labs = ggplot2::waiver(),
                         fill_var_order = NULL, colour_var_order = NULL, #modify grouping variable level order
                         fill_var_labs = NULL, colour_var_labs = NULL, #modify grouping variable labels
                         fill_var_values = NULL, colour_var_values = NULL, #manual colour specification
                         palette = c("plasma", "C", "magma", "A", "inferno", "B", "viridis", "D", "cividis", "E"), #viridis colour palettes
                         palette_direction = c("d2l", "l2d"), palette_begin = 0, palette_end = 1, #viridis colour palette options
                         alpha = 0.6, greyscale = FALSE, #control transparency, convert to greyscale
                         line_size = 1.1, rug = FALSE, rug_colour = "black", rug_alpha = 0.8,
                         dnorm = FALSE, dnorm_colour = "black",
                         dnorm_line_type = c("dashed", "solid", "dotted", "dotdash", "longdash", "twodash"),
                         dnorm_line_size = 1.1, dnorm_alpha = 0.6,
                         theme = c("classic", "bw", "grey", "light", "dark", "minimal"),
                         text_size = 14, font = c("sans", "serif", "mono"),#theme options
                         facet_var = NULL, facet_var_order = NULL, facet_var_labs = NULL, #facet options
                         facet_var_strip_position = c("top", "bottom"), facet_var_text_bold = TRUE, #facet aesthetic customization
                         legend_position = c("right", "left", "top", "bottom"), omit_legend = FALSE, #legend position
                         interactive = FALSE, aesthetic_options = FALSE) {#output format

  theme <- match.arg(theme)
  font <- match.arg(font)
  facet_var_strip_position <- match.arg(facet_var_strip_position)
  legend_position <- match.arg(legend_position)
  dnorm_line_type <- match.arg(dnorm_line_type)
  palette <- match.arg(palette)
  palette_direction <- match.arg(palette_direction)
  palette_direction <- ifelse(palette_direction == "d2l", 1, -1)

  .classes <- class(data)
  if("data.frame" %ni% .classes) {
    stop("Input data must be a data.table, tibble, or data.frame.")
  }

  if(is.error(class(data[[x]]))) {
    x <- deparse(substitute(x))
  } else if(!is.character(x) || length(x) > 1){
    stop('If specified, `x` must be a single symbol or character string',
         '\n representing a column in the input data frame supplied to the `data` argument.')
  }
  if(!missing(fill_var)) {
    if(is.error(class(data[[fill_var]]))) {
      fill_var <- deparse(substitute(fill_var))
    }else if(!is.character(fill_var) || length(fill_var) > 1){
      stop('If specified, `fill_var` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }
  if(!missing(colour_var)) {
    if(is.error(class(data[[colour_var]]))) {
      colour_var <- deparse(substitute(colour_var))
    }else if(!is.character(colour_var) || length(colour_var) > 1){
      stop('If specified, `colour_var` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }
  if(!missing(facet_var)) {
    if(is.error(class(data[[facet_var]]))) {
      facet_var <- deparse(substitute(facet_var))
    } else if(!is.character(facet_var) || length(facet_var) > 1){
      stop('If specified, `facet_var` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }

  #fill variable recoding
  if(!missing(fill_var)){
    data <- dplyr::mutate(data, {{fill_var}} := as.character(.data[[fill_var]]))
  }
  if(!missing(fill_var) && !missing(fill_var_order)){
    data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_relevel(.data[[fill_var]], levels = !!!fill_var_order))
  }
  if(!missing(fill_var) && !missing(fill_var_labs)){
    data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_recode(.data[[fill_var]], !!!fill_var_labs))
  }

  #colour variable recoding
  if(!missing(colour_var)){
    data <- dplyr::mutate(data, {{colour_var}} := as.character(.data[[colour_var]]))
  }
  if(!missing(colour_var) && !missing(colour_var_order)){
    data <- dplyr::mutate(data, {{colour_var}} := forcats::fct_relevel(.data[[colour_var]], levels = !!!colour_var_order))
  }
  if(!missing(colour_var) && !missing(colour_var_labs)){
    data <- dplyr::mutate(data, {{colour_var}} := forcats::fct_recode(.data[[colour_var]], !!!colour_var_labs))
  }

  #facet label recoding
  if(!missing(facet_var)){
    data <- dplyr::mutate(data, {{facet_var}} := as.character(.data[[facet_var]]))
  }
  if(!missing(facet_var) && !missing(facet_var_order)){
    data <- dplyr::mutate(data, {{facet_var}} := forcats::fct_relevel(.data[[facet_var]], levels = !!!facet_var_order))
  }
  if(!missing(facet_var) && !missing(facet_var_labs)){
    data <- dplyr::mutate(data, {{facet_var}} := forcats::fct_recode(.data[[facet_var]], !!!facet_var_labs))
  }

  #normal density curves
  if(dnorm == TRUE && class(data[[x]]) != "Date" && transform_x == FALSE) {
    if("data.table" %ni% class(data)) {
      data <- data.table::as.data.table(data)
    } else {
      data <- data.table::as.data.table(as.data.frame(data))
      #this is conversion and reversal is necessary to prevent subsequent
      #modification of the original data source in the global environment when the
      #input is already a data.table due to the use of the := operator below.
    }
    if(!is.numeric(data[[x]])){
      stop("x must be a numeric vector or column of a data frame to calculate normal density curve")
    }

    #grouping options
    if (!missing(fill_var) && missing(colour_var) && missing(facet_var)) {
      G <- fill_var
    } else if (missing(fill_var) && !missing(colour_var) && missing(facet_var)) {
      G <- colour_var
    } else if (missing(fill_var) && missing(colour_var) && !missing(facet_var)) {
      G <- facet_var
    } else if (!missing(fill_var) && !missing(colour_var) && missing(facet_var)) {
      G <- c(fill_var, colour_var)
    } else if (!missing(fill_var) && missing(colour_var) && !missing(facet_var)) {
      G <- c(fill_var, facet_var)
    } else if (missing(fill_var) && !missing(colour_var) && !missing(facet_var)) {
      G <- c(colour_var, facet_var)
    } else if (!missing(fill_var) && !missing(colour_var) && !missing(facet_var)) {
      G <- c(fill_var, colour_var, facet_var)
    }

    #calculate the normal density curves
    if(!missing(colour_var) || !missing(fill_var) || !missing(facet_var)){
      data[, dnorm_y := dnorm(get(x), mean = mean(get(x), na.rm = TRUE), sd = sd(get(x), na.rm = TRUE)),
           by = eval(G)]
      data <- tibble::as_tibble(data)

    } else {
      data[, dnorm_y := dnorm(get(x), mean = mean(get(x), na.rm = TRUE), sd = sd(get(x), na.rm = TRUE))]
      data <- tibble::as_tibble(data)
    }
  }

  #setup foundational plotting object layer
  p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, fill = fill_var, colour = colour_var))

  #add the geom layers
  p <- p + ggplot2::geom_density(alpha = alpha, size = line_size, ...)

  if(rug == TRUE) {
    if(missing(colour_var)) {
      p <- p + ggplot2::geom_rug(colour = rug_colour, alpha = rug_alpha)
    } else {
      p <- p + ggplot2::geom_rug(alpha = rug_alpha)
    }
  }

  if(dnorm == TRUE && class(data[[x]]) != "Date" && transform_x == FALSE) {
    if(!missing(colour_var)) {
      p <- p + ggplot2::geom_line(ggplot2::aes_string(y = "dnorm_y"),
                                  linetype = dnorm_line_type, size = dnorm_line_size, alpha = dnorm_alpha)
    } else {
      p <- p + ggplot2::geom_line(ggplot2::aes_string(y = "dnorm_y"),
                                  linetype = dnorm_line_type, size = dnorm_line_size, colour = dnorm_colour, alpha = dnorm_alpha)
    }
  }

  #modification of the colour or fill values
  if (!missing(fill_var) && missing(colour_var)){
    if(!missing(fill_var_values)){
      p <- p + ggplot2::scale_fill_manual(values = fill_var_values)
    } else {
      p <- p + ggplot2::scale_fill_viridis_d(begin = palette_begin, end = palette_end,
                                             option = palette, direction = palette_direction)
    }
  } else if(missing(fill_var) && !missing(colour_var)) {
    if(!missing(colour_var_values)){
      p <- p + ggplot2::scale_colour_manual(values = colour_var_values)
    } else {
      p <- p + ggplot2::scale_colour_viridis_d(begin = palette_begin, end = palette_end,
                                               option = palette, direction = palette_direction)
    }
  } else if(!missing(fill_var) && !missing(colour_var)) {
    if(!missing(fill_var_values) && missing(colour_var_values)){
      p <- p +
        ggplot2::scale_fill_manual(values = fill_var_values) +
        ggplot2::scale_colour_viridis_d(begin = palette_begin, end = palette_end,
                                        option = palette, direction = palette_direction)
    } else if(missing(fill_var_values) && !missing(colour_var_values)) {
      p <- p +
        ggplot2::scale_colour_manual(values = colour_var_values) +
        ggplot2::scale_fill_viridis_d(begin = palette_begin, end = palette_end,
                                      option = palette, direction = palette_direction)
    } else if(!missing(fill_var_values) && !missing(colour_var_values)) {
      p <- p +
        ggplot2::scale_fill_manual(values = fill_var_values) +
        ggplot2::scale_colour_manual(values = colour_var_values)
    } else {
      p <- p +
        ggplot2::scale_colour_viridis_d(begin = palette_begin, end = palette_end,
                                        option = palette, direction = palette_direction) +
        ggplot2::scale_fill_viridis_d(begin = palette_begin, end = palette_end,
                                      option = palette, direction = palette_direction)
    }
  }

  #convert to greyscale
  if(greyscale == TRUE){
    p <- p + ggplot2::scale_fill_grey()
  }

  #themes
  if(theme == "classic"){
    p <- p + ggplot2::theme_classic(base_size = text_size, base_family = font)
  } else if (theme == "bw"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "grey"){
    p <- p + ggplot2::theme_grey(base_size = text_size, base_family = font)
  } else if (theme == "light"){
    p <- p + ggplot2::theme_light(base_size = text_size, base_family = font)
  } else if (theme == "dark"){
    p <- p + ggplot2::theme_dark(base_size = text_size, base_family = font)
  } else if (theme == "minimal"){
    p <- p + ggplot2::theme_minimal(base_size = text_size, base_family = font)
  }

  #modification of the x-axis limits, breaks, and transformations
  if(!missing(xlim)) {
    p <- p + ggplot2::coord_cartesian(xlim = c(xlim[1], xlim[2]))
  }

  if(class(x_var_labs) != "waiver") {
    p <- p + ggplot2::scale_x_continuous(labels = x_var_labs)
  } else if(transform_x == FALSE && class(xbreaks) != "waiver"){
    p <- p + ggplot2::scale_x_continuous(breaks = xbreaks, labels = x_var_labs)
  } else if (transform_x == TRUE){
    p <- p + ggplot2::scale_x_continuous(trans = x_transformation, breaks = xbreaks, labels = x_var_labs)
  }

  #labels
  if(!missing(xlab)){
    p <- p + ggplot2::labs(x = xlab)
  }
  if(!missing(ylab)){
    p <- p + ggplot2::labs(y = ylab)
  } else if(dnorm == TRUE) {
    p <- p + ggplot2::labs(y = "density")
  }
  if(!missing(fill_var_title)){
    p <- p + ggplot2::labs(fill = fill_var_title)
  }
  if(!missing(colour_var_title)){
    p <- p + ggplot2::labs(colour = colour_var_title)
  }
  if(!missing(title)){
    p <- p + ggplot2::labs(title = title)
  }

  #misc
  if(omit_legend == TRUE){
    p <- p + ggplot2::theme(legend.position = "none")
  }
  if(legend_position != "right"){
    p <- p + ggplot2::theme(legend.position = legend_position)
  }
  if(!missing(facet_var)){
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[facet_var]]), strip.position = facet_var_strip_position)
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

# plot_histogram ----------------------------------------------------------
#' @title
#'
#' Generate a histogram.
#'
#' @description Easily generate a histogram of a variable using ggplot2 with a
#'   simplified customization interface for common modifications with static
#'   (ggplot) and interactive (plotly) output options. The static output is
#'   useful for producing static reports (e.g. for manuscripts) and is readily
#'   customized further using ggplot2 syntax. The interactive output is helpful
#'   for exploring the data and producing dynamic html reports. See
#'   \href{https://craig.rbind.io/post/2021-05-17-asgr-3-1-data-visualization/}{this
#'    blog post} for an introduction to ggplot2.
#'
#' @importFrom dplyr mutate
#' @importFrom forcats fct_relevel
#' @importFrom forcats fct_recode
#' @importFrom rlang !!!
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_histogram
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_fill_grey
#' @importFrom ggplot2 lims
#' @importFrom ggplot2 labs
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
#' @importFrom ggplot2 sec_axis
#' @importFrom ggplot2 waiver
#' @importFrom ggplot2 coord_cartesian
#' @importFrom plotly ggplotly
#' @importFrom utils browseURL
#'
#' @param data A data frame or tibble containing the dependent measure "x" and
#'   any grouping variables.
#'
#' @param x The name of a numeric variable you want a histogram of (quoted or
#'   unquoted), e.g. x = "variable" or x = variable.
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
#'   fill colour, e.g. fill_var = "grouping_variable" or fill_var =
#'   grouping_variable. Produces separate sets of bars for each level of the
#'   fill variable. See \code{\link[ggplot2]{aes}} for details.
#'
#' @param colour_var Use if you want to assign a variable to the histogram bar
#'   outline colour, e.g. colour_var = "grouping_variable" or colour_var =
#'   grouping_variable. Produces separate sets of bars for each level of the
#'   colour variable. See \code{\link[ggplot2]{aes}} for details.
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
#' @param xlim Specify the x-axis limits, e.g. xlim = c(lower_limit,
#'   upper_limit). Use NA for the existing minimum or maximum value of x, e.g.
#'   the default is xlim = c(NA, NA).
#'
#' @param xbreaks This allows you to change the break points to use for tick marks
#'   via a numeric vector. \code{\link{seq}} is particularly useful here. See
#'   \code{\link[ggplot2]{scale_x_continuous}} for details. If xbreaks is
#'   specified, then xlim should be also.
#'
#' @param transform_x Would you like to transform the x axis? (TRUE or FALSE)
#'
#' @param x_transformation If transform_x = TRUE, this determines the
#'   transformation to be applied. Common choices include "log10" (the default),
#'   "log2", "sqrt", or "exp". See \code{\link[ggplot2]{scale_continuous}} for
#'   details.
#'
#' @param x_var_labs Allows you to modify the labels displayed with the x-axis
#'   tick marks. See \code{\link[ggplot2]{scale_x_continuous}} for details.
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
#' @param palette If a variable is assigned to fill_var or colour_var, this
#'   determines which
#'   \href{https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html}{viridis
#'    colour palette} to use. Options include "plasma" or "C" (default), "magma"
#'   or "A", "inferno" or "B", "viridis" or "D", and "cividis" or "E". See
#'   \href{https://craig.rbind.io/post/2021-05-17-asgr-3-1-data-visualization/#colourblind-friendly-palettes}{this
#'    link} for examples. You can override these colour palettes with
#'   fill_var_values or colour_var_values.
#'
#' @param palette_direction Choose "d2l" for dark to light (default) or "l2d"
#'   for light to dark.
#'
#' @param palette_begin Value between 0 and 1 that determines where along the
#'   full range of the chosen colour palette's spectrum to begin sampling
#'   colours. See \code{\link[ggplot2]{scale_fill_viridis_d}} for details.
#'
#' @param palette_end Value between 0 and 1 that determines where along the full
#'   range of the chosen colour palette's spectrum to end sampling colours. See
#'   \code{\link[ggplot2]{scale_fill_viridis_d}} for details.
#'
#' @param alpha This adjusts the transparency/opacity of the histogram(s),
#'   ranging from 0 = 100% transparent to 1 = 100% opaque.
#'
#' @param greyscale Set to TRUE if you want the plot converted to greyscale.
#'
#' @param line_size THis modifies the thickness of the histogram bar outlines.
#'
#' @param rug Set this to TRUE to add rug lines to the bottom of the plot.
#'
#' @param rug_colour Determines the colour of the rug lines (if rug = TRUE).
#'
#' @param rug_alpha This adjusts the transparency/opacity of the rug lines (if
#'   rug = TRUE) with valid values ranging from 0 = 100% transparent to 1 = 100%
#'   opaque.
#'
#' @param dnorm Set this to TRUE to add a normal/Gaussian density curve to the
#'   plot. Ignored if x is a date vector or "transform_x" = TRUE.
#'
#' @param dnorm_colour Determines the colour of the normal density curve (if dnorm = TRUE).
#'
#' @param dnorm_line_type The type of line to use to draw the normal density
#'   curve (if dnorm = TRUE). Options include "dashed" (default), "solid",
#'   "dotted", "dotdash", "longdash", and "twodash".
#'
#' @param dnorm_line_size Adjusts the thickness of the normal density curve (if
#'   dnorm = TRUE).
#'
#' @param dnorm_alpha This adjusts the transparency/opacity of the normal
#'   density curve (if dnorm = TRUE) with valid values ranging from 0 = 100%
#'   transparent to 1 = 100% opaque.
#'
#' @param dnorm_y_axis Set this to FALSE to omit the secondary y-axis that is
#'   drawn if dnorm = TRUE to show the scale for the normal density curve.
#'
#' @param theme Adjusts the theme using 1 of 6 predefined "complete" theme
#'   templates provided by ggplot2. Currently supported options are: "classic"
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
#'   variable (i.e. a facetted plot), e.g. facet_var = "grouping_variable" or
#'   facet_var = grouping_variable. See \code{\link[ggplot2]{facet_wrap}} for
#'   details.
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
#' \donttest{
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
#' mtcars %>% plot_histogram(x = mpg, fill = "blue", interactive = TRUE)
#'
#' mtcars %>% plot_histogram(x = mpg, fill_var = cyl, binwidth = 5, interactive = TRUE)
#' }
#'
#' @references
#' Wickham, H. (2016). ggplot2: elegant graphics for data analysis. New York, N.Y.: Springer-Verlag.
#'
#' @seealso \code{\link[ggplot2]{geom_histogram}}, \code{\link[stats]{dnorm}},
#'   \code{\link[ggplot2]{geom_line}}, \code{\link[plotly]{ggplotly}}
#'
#' @export
plot_histogram <- function(data, x, #essential parameters
                           ..., #non-variable aestheic specification
                           binwidth = NULL, bins = 30, #geom specific customization params
                           position = c("identity", "stack", "dodge"),
                           stat = c("bin", "count"), na.rm = TRUE, #geom specific customization params.
                           fill_var = NULL, colour_var = NULL, #grouping variable aesthetic mappings
                           xlab = NULL, ylab = NULL, title = NULL, fill_var_title = NULL, colour_var_title = NULL, #titles
                           xlim = c(NA, NA), xbreaks = ggplot2::waiver(), #control the x axis limits and scaling
                           transform_x = FALSE, x_transformation = "log10", x_var_labs = ggplot2::waiver(),
                           fill_var_order = NULL, colour_var_order = NULL, #modify grouping variable level order
                           fill_var_labs = NULL, colour_var_labs = NULL, #modify grouping variable labels
                           fill_var_values = NULL, colour_var_values = NULL, #manual colour specification
                           palette = c("plasma", "C", "magma", "A", "inferno", "B", "viridis", "D", "cividis", "E"), #viridis colour palettes
                           palette_direction = c("d2l", "l2d"), palette_begin = 0, palette_end = 1, #viridis colour palette options
                           alpha = 0.6, greyscale = FALSE, #control transparency, convert to greyscale
                           line_size = 1.1, rug = FALSE, rug_colour = "black", rug_alpha = 0.8,
                           dnorm = FALSE, dnorm_colour = "black",
                           dnorm_line_type = c("dashed", "solid", "dotted", "dotdash", "longdash", "twodash"),
                           dnorm_line_size = 1.1, dnorm_alpha = 0.6, dnorm_y_axis = TRUE,
                           theme = c("classic", "bw", "grey", "light", "dark", "minimal"),
                           text_size = 14, font = c("sans", "serif", "mono"),#theme options
                           facet_var = NULL, facet_var_order = NULL, facet_var_labs = NULL, #facet options
                           facet_var_strip_position = c("top", "bottom"), facet_var_text_bold = TRUE, #facet aesthetic customization
                           legend_position = c("right", "left", "top", "bottom"), omit_legend = FALSE, #legend position
                           interactive = FALSE, aesthetic_options = FALSE) {#output format

  theme <- match.arg(theme)
  position <- match.arg(position)
  stat <- match.arg(stat)
  font <- match.arg(font)
  legend_position <- match.arg(legend_position)
  facet_var_strip_position <- match.arg(facet_var_strip_position)
  dnorm_line_type <- match.arg(dnorm_line_type)
  palette <- match.arg(palette)
  palette_direction <- match.arg(palette_direction)
  palette_direction <- ifelse(palette_direction == "d2l", 1, -1)

  if(is.error(class(data[[x]]))) {
    x <- deparse(substitute(x))
  } else if(!is.character(x) || length(x) > 1){
    stop('If specified, `x` must be a single symbol or character string',
         '\n representing a column in the input data frame supplied to the `data` argument.')
  }
  if(!missing(fill_var)) {
    if(is.error(class(data[[fill_var]]))) {
      fill_var <- deparse(substitute(fill_var))
    }else if(!is.character(fill_var) || length(fill_var) > 1){
      stop('If specified, `fill_var` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }
  if(!missing(colour_var)) {
    if(is.error(class(data[[colour_var]]))) {
      colour_var <- deparse(substitute(colour_var))
    }else if(!is.character(colour_var) || length(colour_var) > 1){
      stop('If specified, `colour_var` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }
  if(!missing(facet_var)) {
    if(is.error(class(data[[facet_var]]))) {
      facet_var <- deparse(substitute(facet_var))
    } else if(!is.character(facet_var) || length(facet_var) > 1){
      stop('If specified, `facet_var` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }

  #fill variable recoding
  if(!missing(fill_var)){
    data <- dplyr::mutate(data, {{fill_var}} := as.character(.data[[fill_var]]))
  }
  if(!missing(fill_var) && !missing(fill_var_order)){
    data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_relevel(.data[[fill_var]], levels = !!!fill_var_order))
  }
  if(!missing(fill_var) && !missing(fill_var_labs)){
    data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_recode(.data[[fill_var]], !!!fill_var_labs))
  }

  #colour variable recoding
  if(!missing(colour_var)){
    data <- dplyr::mutate(data, {{colour_var}} := as.character(.data[[colour_var]]))
  }
  if(!missing(colour_var) && !missing(colour_var_order)){
    data <- dplyr::mutate(data, {{colour_var}} := forcats::fct_relevel(.data[[colour_var]], levels = !!!colour_var_order))
  }
  if(!missing(colour_var) && !missing(colour_var_labs)){
    data <- dplyr::mutate(data, {{colour_var}} := forcats::fct_recode(.data[[colour_var]], !!!colour_var_labs))
  }

  #facet label recoding
  if(!missing(facet_var)){
    data <- dplyr::mutate(data, {{facet_var}} := as.character(.data[[facet_var]]))
  }
  if(!missing(facet_var) && !missing(facet_var_order)){
    data <- dplyr::mutate(data, {{facet_var}} := forcats::fct_relevel(.data[[facet_var]], levels = !!!facet_var_order))
  }
  if(!missing(facet_var) && !missing(facet_var_labs)){
    data <- dplyr::mutate(data, {{facet_var}} := forcats::fct_recode(.data[[facet_var]], !!!facet_var_labs))
  }

  #normal density curves
  if(dnorm == TRUE && class(data[[x]]) != "Date" && transform_x == FALSE) {
    if(!missing(bins) || missing(binwidth)) {
      stop('plot_histogram() dnorm curve drawing functionality is currently incompatible with the "bins" argument, use "binwidth" to adjust bin sizes instead.')
    }
    if("data.table" %ni% class(data)) {
      data <- data.table::as.data.table(data)
    } else {
      data <- data.table::as.data.table(as.data.frame(data))
      #this is conversion and reversal is necessary to prevent subsequent
      #modification of the original data source in the global environment when the
      #input is already a data.table due to the use of the := operator below.
    }

    if(!is.numeric(data[[x]])){
      stop("x must be a numeric vector or column of a data frame to calculate normal density curve")
    }

    #grouping options
    if (!missing(fill_var) && missing(colour_var) && missing(facet_var)) {
      G <- fill_var
    } else if (missing(fill_var) && !missing(colour_var) && missing(facet_var)) {
      G <- colour_var
    } else if (missing(fill_var) && missing(colour_var) && !missing(facet_var)) {
      G <- facet_var
    } else if (!missing(fill_var) && !missing(colour_var) && missing(facet_var)) {
      G <- c(fill_var, colour_var)
    } else if (!missing(fill_var) && missing(colour_var) && !missing(facet_var)) {
      G <- c(fill_var, facet_var)
    } else if (missing(fill_var) && !missing(colour_var) && !missing(facet_var)) {
      G <- c(colour_var, facet_var)
    } else if (!missing(fill_var) && !missing(colour_var) && !missing(facet_var)) {
      G <- c(fill_var, colour_var, facet_var)
    }

    bw <- binwidth #define bin width to be used in the histogram
    if(!missing(colour_var) || !missing(fill_var) || !missing(facet_var)){
      n_obs <- mean(data[, .(n = sum(!is.na(get(x)))), by = eval(G)][["n"]], na.rm = TRUE)
    } else {
      n_obs <- sum(!is.na(data[[x]])) #count of non-missing observations
    }
    #define a function that returns the normal density curve scaled by the bin width
    #and number of observations
    scaled_dnorm <- function(x) {
      dnorm(x, mean = mean(x), sd = sd(x)) * (bw * n_obs)
    }
    #calculate the normal density curves
    if(!missing(colour_var) || !missing(fill_var) || !missing(facet_var)){
      data[, dnorm_y := scaled_dnorm(get(x)),
           by = eval(G)]
      data <- tibble::as_tibble(data)

    } else {
      data <- data[, dnorm_y := scaled_dnorm(get(x))]
      data <- tibble::as_tibble(data)
    }
  }

  #setup foundational plotting object layer
  p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, fill = fill_var, colour = colour_var))

  #add the geom layers
  p <- p +
    ggplot2::geom_histogram(alpha = alpha, binwidth = binwidth, bins = bins,
                            position = position, stat = stat, na.rm = na.rm, size = line_size, ...)

  if(rug == TRUE) {
    if(missing(colour_var)) {
      p <- p + ggplot2::geom_rug(colour = rug_colour, alpha = rug_alpha)
    } else {
      p <- p + ggplot2::geom_rug(alpha = rug_alpha)
    }
  }

  if(dnorm == TRUE && class(data[[x]]) != "Date" && transform_x == FALSE) {
    if(!missing(colour_var)) {
      p <- p + ggplot2::geom_line(aes_string(y = "dnorm_y"),
                                  linetype = dnorm_line_type, size = dnorm_line_size, alpha = dnorm_alpha)
    } else {
      p <- p + ggplot2::geom_line(aes_string(y = "dnorm_y"),
                                  linetype = dnorm_line_type, size = dnorm_line_size, colour = dnorm_colour, alpha = dnorm_alpha)
    }
  }

  #modification of the colour or fill values
  if (!missing(fill_var) && missing(colour_var)){
    if(!missing(fill_var_values)){
      p <- p +
        ggplot2::scale_fill_manual(values = fill_var_values)
    } else {
      p <- p +
        ggplot2::scale_fill_viridis_d(begin = palette_begin, end = palette_end,
                                      option = palette, direction = palette_direction)
    }
  } else if(missing(fill_var) && !missing(colour_var)) {
    if(!missing(colour_var_values)){
      p <- p +
        ggplot2::scale_colour_manual(values = colour_var_values)
    } else {
      p <- p +
        ggplot2::scale_colour_viridis_d(begin = palette_begin, end = palette_end,
                                        option = palette, direction = palette_direction)
    }
  } else if(!missing(fill_var) && !missing(colour_var)) {
    if(!missing(fill_var_values) && missing(colour_var_values)){
      p <- p +
        ggplot2::scale_fill_manual(values = fill_var_values) +
        ggplot2::scale_colour_viridis_d(begin = palette_begin, end = palette_end,
                                        option = palette, direction = palette_direction)
    } else if(missing(fill_var_values) && !missing(colour_var_values)) {
      p <- p +
        ggplot2::scale_colour_manual(values = colour_var_values) +
        ggplot2::scale_fill_viridis_d(begin = palette_begin, end = palette_end,
                                      option = palette, direction = palette_direction)
    } else if(!missing(fill_var_values) && !missing(colour_var_values)) {
      p <- p +
        ggplot2::scale_fill_manual(values = fill_var_values) +
        ggplot2::scale_colour_manual(values = colour_var_values)
    } else {
      p <- p +
        ggplot2::scale_colour_viridis_d(begin = palette_begin, end = palette_end,
                                        option = palette, direction = palette_direction) +
        ggplot2::scale_fill_viridis_d(begin = palette_begin, end = palette_end,
                                      option = palette, direction = palette_direction)
    }
  }

  #convert to greyscale
  if(greyscale == TRUE){
    p <- p + ggplot2::scale_fill_grey()
  }

  #themes
  if(theme == "classic"){
    p <- p + ggplot2::theme_classic(base_size = text_size, base_family = font)
  } else if (theme == "bw"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "grey"){
    p <- p + ggplot2::theme_grey(base_size = text_size, base_family = font)
  } else if (theme == "light"){
    p <- p + ggplot2::theme_light(base_size = text_size, base_family = font)
  } else if (theme == "dark"){
    p <- p + ggplot2::theme_dark(base_size = text_size, base_family = font)
  } else if (theme == "minimal"){
    p <- p + ggplot2::theme_minimal(base_size = text_size, base_family = font)
  }

  #modification of x-axis limits
  if(!missing(xlim)) {
    p <- p + ggplot2::coord_cartesian(xlim = c(xlim[1], xlim[2]))
  }
  if(class(x_var_labs) != "waiver") {
    p <- p + ggplot2::scale_x_continuous(labels = x_var_labs)
  } else if(transform_x == FALSE && class(xbreaks) != "waiver"){
    p <- p + ggplot2::scale_x_continuous(breaks = xbreaks, labels = x_var_labs)
  } else if (transform_x == TRUE){
    p <- p + ggplot2::scale_x_continuous(trans = x_transformation, breaks = xbreaks, labels = x_var_labs)
  }

  #labels
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
  if(!missing(title)){
    p <- p + ggplot2::labs(title = title)
  }

  #misc
  if(omit_legend == TRUE){
    p <- p + ggplot2::theme(legend.position = "none")
  }
  if(legend_position != "right"){
    p <- p + ggplot2::theme(legend.position = legend_position)
  }

  if(dnorm == TRUE && dnorm_y_axis == TRUE) {
    p <- p + scale_y_continuous(sec.axis = ggplot2::sec_axis(trans = ~./(bw*n_obs), name = "density"))
    if(!missing(colour_var)) {
      p <- p + ggplot2::theme(axis.ticks.y.right = ggplot2::element_line(colour = "black"),
                              axis.text.y.right = ggplot2::element_text(colour = "black"),
                              axis.title.y.right = ggplot2::element_text(colour = "black"))
    } else {
      p <- p + ggplot2::theme(axis.ticks.y.right = ggplot2::element_line(colour = dnorm_colour),
                              axis.text.y.right = ggplot2::element_text(colour = dnorm_colour),
                              axis.title.y.right = ggplot2::element_text(colour = dnorm_colour))
    }
  }

  if(!missing(facet_var)){
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[facet_var]]), strip.position = facet_var_strip_position)
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

# plot_box ------------------------------------------------------------
#' @title
#'
#' Generate a box-and-whisker plot.
#'
#' @description Easily generate box-and-whisker plots using ggplot2 with a
#'   simplified customization interface for common modifications with static
#'   (ggplot) and interactive (plotly) output options. The static output is
#'   useful for producing static reports (e.g. for manuscripts) and is readily
#'   customized further using ggplot2 syntax. The interactive output is helpful
#'   for exploring the data and producing dynamic html reports. See
#'   \href{https://craig.rbind.io/post/2021-05-17-asgr-3-1-data-visualization/}{this
#'    blog post} for an introduction to ggplot2.
#'
#' @importFrom dplyr mutate
#' @importFrom forcats fct_relevel
#' @importFrom forcats fct_recode
#' @importFrom rlang !!!
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_fill_grey
#' @importFrom ggplot2 lims
#' @importFrom ggplot2 labs
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
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 waiver
#' @importFrom ggplot2 coord_cartesian
#' @importFrom plotly ggplotly
#' @importFrom plotly layout
#' @importFrom utils browseURL
#'
#' @param data A data frame or tibble containing the dependent measure "y" and
#'   any grouping variables.
#'
#' @param y The name of a numeric variable you want boxplots for (quoted or
#'   unquoted), e.g. y = "variable" or y = variable.
#'
#' @param x A categorical variable you want to obtain separate boxplots of y
#'   for, e.g. x = "grouping_variable" or x = grouping_variable.
#'
#' @param ... graphical parameters (not associated with variables) to be passed
#'   to \code{\link[ggplot2]{geom_boxplot}}, e.g. colour or fill, to be applied
#'   to all bars. To see some of the available options in a web browser, set the
#'   aesthetic_options argument to TRUE.
#'
#' @param fill_var Use if you want to assign a variable to the box fill colour,
#'   e.g. fill_var = "grouping_variable" or fill_var = grouping_variable.
#'   Produces separate sets of boxes for each level of the fill variable. See
#'   \code{\link[ggplot2]{aes}} for details.
#'
#' @param colour_var Use if you want to assign a variable to the box outline
#'   colour, e.g. colour_var = "grouping_variable" or colour_var =
#'   grouping_variable. Produces separate sets of boxes for each level of the
#'   colour variable. See \code{\link[ggplot2]{aes}} for details.
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
#'   the default is ylim = c(NA, NA).
#'
#' @param ybreaks This allows you to change the break points to use for tick
#'   marks on the y-axis. \code{\link{seq}} is particularly useful here. See
#'   \code{\link[ggplot2]{scale_y_continuous}} for details. If ybreaks is
#'   specified, then ylim should be also.
#'
#' @param transform_y Would you like to transform the y axis? (TRUE or FALSE)
#'
#' @param y_transformation If transform_y = TRUE, this determines the
#'   transformation to be applied. Common choices include "log10" (the default),
#'   "log2", "sqrt", or "exp". See \code{\link[ggplot2]{scale_continuous}} for
#'   details.
#'
#' @param y_var_labs Allows you to modify the labels displayed with the y-axis
#'   tick marks. See \code{\link[ggplot2]{scale_continuous}} for details.
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
#' @param palette If a variable is assigned to fill_var or colour_var, this
#'   determines which
#'   \href{https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html}{viridis
#'    colour palette} to use. Options include "plasma" or "C" (default), "magma"
#'   or "A", "inferno" or "B", "viridis" or "D", and "cividis" or "E". See
#'   \href{https://craig.rbind.io/post/2021-05-17-asgr-3-1-data-visualization/#colourblind-friendly-palettes}{this
#'    link} for examples. You can override these colour palettes with
#'   fill_var_values or colour_var_values.
#'
#' @param palette_direction Choose "d2l" for dark to light (default) or "l2d"
#'   for light to dark.
#'
#' @param palette_begin Value between 0 and 1 that determines where along the
#'   full range of the chosen colour palette's spectrum to begin sampling
#'   colours. See \code{\link[ggplot2]{scale_fill_viridis_d}} for details.
#'
#' @param palette_end Value between 0 and 1 that determines where along the full
#'   range of the chosen colour palette's spectrum to end sampling colours. See
#'   \code{\link[ggplot2]{scale_fill_viridis_d}} for details.
#'
#' @param alpha This adjusts the transparency/opacity of the box-and-whisker
#'   plots(s), ranging from 0 = 100% transparent to 1 = 100% opaque.
#'
#' @param greyscale Set to TRUE if you want the plot converted to greyscale.
#'
#' @param line_size Controls the thickness of the box plot lines.
#'
#' @param dots Set to TRUE if you want to add a dotplot layer over the
#'   box-and-whisker plot(s).
#'
#' @param dots_colour Controls the colour of the dots in the dotplot layer.
#'   Ignored if "dots" = FALSE or if a variable is assigned to "colour_var".
#'
#' @param dots_alpha This adjusts the transparency/opacity of the graphical
#'   components of the dotplot layer if "dots" = TRUE, with acceptable values
#'   ranging from 0 = 100% transparent to 1 = 100% opaque.
#'
#' @param dots_binwidth Controls the binwidth to use for the dotplot layer if
#'   dots = TRUE. See \code{\link[ggplot2]{geom_dotplot}} for details.
#'
#' @param theme Adjusts the theme using 1 of 6 predefined "complete" theme
#'   templates provided by ggplot2. Currently supported options are: "classic"
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
#'   variable (i.e. a facetted plot), e.g. facet_var = "grouping_variable" or
#'   facet_var = grouping_variable. See \code{\link[ggplot2]{facet_wrap}} for
#'   details.
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
#' @param interactive Determines whether a static ggplot object or an
#'   interactive html plotly object is returned. See
#'   \code{\link[plotly]{ggplotly}} for details. Note that if a variable is
#'   assigned to "fill_var" or "colour_var", activating interactive/plotly mode
#'   will cause a spurious warning message about 'layout' objects not having a
#'   'boxmode' attribute to be printed to the console. This is a
#'   \href{https://github.com/ropensci/plotly/issues/994}{documented bug} with
#'   plotly that can be safely ignored, although unfortunately the message
#'   cannot be suppressed.
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
#' \donttest{
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
#'           interactive = TRUE)
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
#'           theme = "bw", interactive = TRUE)
#' }
#'
#'
#' @references
#' Wickham, H. (2016). ggplot2: elegant graphics for data analysis. New York, N.Y.: Springer-Verlag.
#'
#' @seealso \code{\link[ggplot2]{geom_boxplot}}, \code{\link[ggplot2]{geom_dotplot}}, \code{\link[plotly]{ggplotly}}
#'
#' @export
plot_box <- function(data, y,#essential parameters
                     x = NULL,
                     ..., #geom-specific customization see ?geom_boxplot for details
                     fill_var = NULL, colour_var = NULL, #grouping variable aesthetic mappings
                     xlab = NULL, ylab = NULL, title = NULL,
                     fill_var_title = NULL, colour_var_title = NULL, #titles
                     ylim = c(NA, NA), ybreaks = ggplot2::waiver(), #control the y axis limits and scaling
                     transform_y = FALSE, y_transformation = "log10", y_var_labs = ggplot2::waiver(),
                     x_var_order = NULL, x_var_labs = NULL,
                     fill_var_order = NULL, colour_var_order = NULL, #modify grouping variable level order
                     fill_var_labs = NULL, colour_var_labs = NULL, #modify grouping variable labels
                     fill_var_values = NULL, colour_var_values = NULL, #manual colour specification
                     palette = c("plasma", "C", "magma", "A", "inferno", "B", "viridis", "D", "cividis", "E"), #viridis colour palettes
                     palette_direction = c("d2l", "l2d"), palette_begin = 0, palette_end = 1, #viridis colour palette options
                     alpha = 0.6, greyscale = FALSE, #control transparency, convert to greyscale
                     line_size = 1, dots = FALSE, dots_colour = "black", dots_alpha = 0.4, dots_binwidth = 0.9,
                     theme = c("classic", "bw", "grey", "light", "dark", "minimal"),
                     text_size = 14, font = c("sans", "serif", "mono"), #theme options
                     facet_var = NULL, facet_var_order = NULL, facet_var_labs = NULL, #facet options
                     facet_var_strip_position = c("top", "bottom"), facet_var_text_bold = TRUE, #facet aesthetic customization
                     legend_position = c("right", "left", "top", "bottom"), omit_legend = FALSE, #legend position
                     interactive = FALSE, aesthetic_options = FALSE) {#output format

  theme <- match.arg(theme)
  font <- match.arg(font)
  legend_position <- match.arg(legend_position)
  facet_var_strip_position <- match.arg(facet_var_strip_position)
  palette <- match.arg(palette)
  palette_direction <- match.arg(palette_direction)
  palette_direction <- ifelse(palette_direction == "d2l", 1, -1)

  if(is.error(class(data[[y]]))) {
    y <- deparse(substitute(y))
  } else if(!is.character(y) || length(y) > 1){
    stop('If specified, `y` must be a single symbol or character string',
         '\n representing a column in the input data frame supplied to the `data` argument.')
  }
  if(!missing(x)) {
    if(is.error(class(data[[x]]))) {
      x <- deparse(substitute(x))
    } else if(!is.character(x) || length(x) > 1){
      stop('If specified, `x` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }
  if(!missing(fill_var)) {
    if(is.error(class(data[[fill_var]]))) {
      fill_var <- deparse(substitute(fill_var))
    }else if(!is.character(fill_var) || length(fill_var) > 1){
      stop('If specified, `fill_var` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }
  if(!missing(colour_var)) {
    if(is.error(class(data[[colour_var]]))) {
      colour_var <- deparse(substitute(colour_var))
    }else if(!is.character(colour_var) || length(colour_var) > 1){
      stop('If specified, `colour_var` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }
  if(!missing(facet_var)) {
    if(is.error(class(data[[facet_var]]))) {
      facet_var <- deparse(substitute(facet_var))
    } else if(!is.character(facet_var) || length(facet_var) > 1){
      stop('If specified, `facet_var` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }

  #x-variable recoding
  if(!missing(x)){
    data <- dplyr::mutate(data, {{x}} := as.character(.data[[x]]))
  }
  if(!missing(x_var_order)){
    data <- dplyr::mutate(data, {{x}} := forcats::fct_relevel(.data[[x]], levels = !!!x_var_order))
  }
  if(!missing(x_var_labs)){
    data <- dplyr::mutate(data, {{x}} := forcats::fct_recode(.data[[x]], !!!x_var_labs))
  }

  #fill variable recoding
  if(!missing(fill_var)){
    data <- dplyr::mutate(data, {{fill_var}} := as.character(.data[[fill_var]]))
  }
  if(!missing(fill_var) && !missing(fill_var_order)){
    data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_relevel(.data[[fill_var]], levels = !!!fill_var_order))
  }
  if(!missing(fill_var) && !missing(fill_var_labs)){
    data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_recode(.data[[fill_var]], !!!fill_var_labs))
  }

  #colour variable recoding
  if(!missing(colour_var)){
    data <- dplyr::mutate(data, {{colour_var}} := as.character(.data[[colour_var]]))
  }
  if(!missing(colour_var) && !missing(colour_var_order)){
    data <- dplyr::mutate(data, {{colour_var}} := forcats::fct_relevel(.data[[colour_var]], levels = !!!colour_var_order))
  }
  if(!missing(colour_var) && !missing(colour_var_labs)){
    data <- dplyr::mutate(data, {{colour_var}} := forcats::fct_recode(.data[[colour_var]], !!!colour_var_labs))
  }

  #facet label recoding
  if(!missing(facet_var)){
    data <- dplyr::mutate(data, {{facet_var}} := as.character(.data[[facet_var]]))
  }
  if(!missing(facet_var) && !missing(facet_var_order)){
    data <- dplyr::mutate(data, {{facet_var}} := forcats::fct_relevel(.data[[facet_var]], levels = !!!facet_var_order))
  }
  if(!missing(facet_var) && !missing(facet_var_labs)){
    data <- dplyr::mutate(data, {{facet_var}} := forcats::fct_recode(.data[[facet_var]], !!!facet_var_labs))
  }

  #setup foundational plotting object layer
  p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y, fill = fill_var, colour = colour_var))

  #add the geom layer
  p <- p + ggplot2::geom_boxplot(alpha = alpha, size = line_size, ...)

  #add a dotplot layer if enabled
  if(dots == TRUE) {
    if(missing(colour_var)) {
      p <- p + ggplot2::geom_dotplot(binaxis='y', stackdir='center',
                                     alpha = dots_alpha, binwidth = dots_binwidth)
    } else {
      p <- p + ggplot2::geom_dotplot(binaxis='y', stackdir='center',
                                     colour = dots_colour, alpha = dots_alpha, binwidth = dots_binwidth)
    }
  }

  #modification of the colour or fill values
  if (!missing(fill_var) && missing(colour_var)){
    if(!missing(fill_var_values)){
      p <- p +
        ggplot2::scale_fill_manual(values = fill_var_values)
    } else {
      p <- p +
        ggplot2::scale_fill_viridis_d(begin = palette_begin, end = palette_end,
                                      option = palette, direction = palette_direction)
    }
  } else if(missing(fill_var) && !missing(colour_var)) {
    if(!missing(colour_var_values)){
      p <- p +
        ggplot2::scale_colour_manual(values = colour_var_values)
    } else {
      p <- p +
        ggplot2::scale_colour_viridis_d(begin = palette_begin, end = palette_end,
                                        option = palette, direction = palette_direction)
    }
  } else if(!missing(fill_var) && !missing(colour_var)) {
    if(!missing(fill_var_values) && missing(colour_var_values)){
      p <- p +
        ggplot2::scale_fill_manual(values = fill_var_values) +
        ggplot2::scale_colour_viridis_d(begin = palette_begin, end = palette_end,
                                        option = palette, direction = palette_direction)
    } else if(missing(fill_var_values) && !missing(colour_var_values)) {
      p <- p +
        ggplot2::scale_colour_manual(values = colour_var_values) +
        ggplot2::scale_fill_viridis_d(begin = palette_begin, end = palette_end,
                                      option = palette, direction = palette_direction)
    } else if(!missing(fill_var_values) && !missing(colour_var_values)) {
      p <- p +
        ggplot2::scale_fill_manual(values = fill_var_values) +
        ggplot2::scale_colour_manual(values = colour_var_values)
    } else {
      p <- p +
        ggplot2::scale_colour_viridis_d(begin = palette_begin, end = palette_end,
                                        option = palette, direction = palette_direction) +
        ggplot2::scale_fill_viridis_d(begin = palette_begin, end = palette_end,
                                      option = palette, direction = palette_direction)
    }
  }

  #convert to greyscale
  if(greyscale == TRUE){
    p <- p + ggplot2::scale_fill_grey()
  }

  #themes
  if(theme == "classic"){
    p <- p + ggplot2::theme_classic(base_size = text_size, base_family = font)
  } else if (theme == "bw"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "grey"){
    p <- p + ggplot2::theme_grey(base_size = text_size, base_family = font)
  } else if (theme == "light"){
    p <- p + ggplot2::theme_light(base_size = text_size, base_family = font)
  } else if (theme == "dark"){
    p <- p + ggplot2::theme_dark(base_size = text_size, base_family = font)
  } else if (theme == "minimal"){
    p <- p + ggplot2::theme_minimal(base_size = text_size, base_family = font)
  }

  #modification of axis limits & transformations
  if(!missing(ylim)) {
    p <- p + ggplot2::coord_cartesian(ylim = c(ylim[1], ylim[2]))
  }
  if(class(y_var_labs) != "waiver") {
    p <- p + ggplot2::scale_y_continuous(labels = y_var_labs)
  } else if(transform_y == FALSE && class(ybreaks) != "waiver"){
    p <- p + ggplot2::scale_y_continuous(breaks = ybreaks, labels = y_var_labs)
  } else if (transform_y == TRUE){
    p <- p + ggplot2::scale_y_continuous(trans = y_transformation, breaks = ybreaks, labels = y_var_labs)
  }

  #labels
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
  if(!missing(title)){
    p <- p + ggplot2::labs(title = title)
  }
  if(missing(x)){
    p <- p + ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                            axis.text.x = ggplot2::element_blank(),
                            axis.ticks.x = ggplot2::element_blank())
  }

  #misc
  if(omit_legend == TRUE){
    p <- p + ggplot2::theme(legend.position = "none")
  }
  if(legend_position != "right"){
    p <- p + ggplot2::theme(legend.position = legend_position)
  }
  if(!missing(facet_var)){
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[facet_var]]), strip.position = facet_var_strip_position)
  }
  if(!missing(facet_var) & facet_var_text_bold == TRUE){
    p <- p + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
  }
  if(aesthetic_options == TRUE){
    utils::browseURL("https://ggplot2.tidyverse.org/articles/ggplot2-specs.html")
  }
  if(interactive == TRUE){
    if(missing(fill_var) && missing(colour_var)) {
      p <- plotly::ggplotly(p)
    } else {
      p <- plotly::ggplotly(p)
      p <- plotly::layout(p, boxmode = "group")
    }
    return(p)
  }
  if(interactive == FALSE){
    return(p)
  }
}

# plot_violin -------------------------------------------------------------
#' @title
#'
#' Generate a violin plot.
#'
#' @description Easily generate violin plots using ggplot2 with a simplified
#'   customization interface for common modifications with static (ggplot) and
#'   interactive (plotly) output options. The static output is useful for
#'   producing static reports (e.g. for manuscripts) and is readily customized
#'   further using ggplot2 syntax. The interactive output is helpful for
#'   exploring the data and producing dynamic html reports. See
#'   \href{https://craig.rbind.io/post/2021-05-17-asgr-3-1-data-visualization/}{this
#'    blog post} for an introduction to ggplot2.
#'
#' @importFrom dplyr mutate
#' @importFrom forcats fct_relevel
#' @importFrom forcats fct_recode
#' @importFrom rlang !!!
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_violin
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_fill_grey
#' @importFrom ggplot2 lims
#' @importFrom ggplot2 labs
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
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 coord_cartesian
#' @importFrom plotly ggplotly
#' @importFrom utils browseURL
#'
#' @param data A data frame or tibble containing the dependent measure "y" and
#'   any grouping variables.
#'
#' @param y A numeric variable you want to obtain violin plots for (quoted or
#'   unquoted), e.g. y = "variable" or y = variable.
#'
#' @param x A categorical variable you want to obtain separate violin plots of y
#'   for (quoted or unquoted), e.g. x = "variable" or x = variable.
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
#'   colour, e.g. fill_var = "grouping_variable" or fill_var =
#'   grouping_variable. Produces separate sets of bars for each level of the
#'   fill variable. See \code{\link[ggplot2]{aes}} for details.
#'
#' @param colour_var Use if you want to assign a variable to the violin outline
#'   colour, e.g. colour_var = "grouping_variable" or colour_var =
#'   grouping_variable. Produces separate sets of bars for each level of the
#'   colour variable. See \code{\link[ggplot2]{aes}} for details.
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
#' @param ybreaks This allows you to change the break points to use for tick
#'   marks on the y-axis. \code{\link{seq}} is particularly useful here. See
#'   \code{\link[ggplot2]{scale_y_continuous}} for details. If ybreaks is
#'   specified, then ylim should be also.
#'
#' @param transform_y Would you like to transform the y axis? (TRUE or FALSE)
#'
#' @param y_transformation If transform_y = TRUE, this determines the
#'   transformation to be applied. Common choices include "log10" (the default),
#'   "log2", "sqrt", or "exp". See \code{\link[ggplot2]{scale_continuous}} for
#'   details.
#'
#' @param y_var_labs Allows you to modify the labels displayed with the y-axis
#'   tick marks. See \code{\link[ggplot2]{scale_continuous}} for details.
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
#' @param palette If a variable is assigned to fill_var or colour_var, this
#'   determines which
#'   \href{https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html}{viridis
#'    colour palette} to use. Options include "plasma" or "C" (default), "magma"
#'   or "A", "inferno" or "B", "viridis" or "D", and "cividis" or "E". See
#'   \href{https://craig.rbind.io/post/2021-05-17-asgr-3-1-data-visualization/#colourblind-friendly-palettes}{this
#'    link} for examples. You can override these colour palettes with
#'   fill_var_values or colour_var_values.
#'
#' @param palette_direction Choose "d2l" for dark to light (default) or "l2d"
#'   for light to dark.
#'
#' @param palette_begin Value between 0 and 1 that determines where along the
#'   full range of the chosen colour palette's spectrum to begin sampling
#'   colours. See \code{\link[ggplot2]{scale_fill_viridis_d}} for details.
#'
#' @param palette_end Value between 0 and 1 that determines where along the full
#'   range of the chosen colour palette's spectrum to end sampling colours. See
#'   \code{\link[ggplot2]{scale_fill_viridis_d}} for details.
#'
#' @param alpha This adjusts the transparency/opacity of the graphical
#'   components of the plot, ranging from 0 = 100% transparent to 1 = 100%
#'   opaque.
#'
#' @param greyscale Set to TRUE if you want the plot converted to greyscale.
#'
#' @param line_size Controls the thickness of the violin outlines.
#'
#' @param theme Adjusts the theme using 1 of 6 predefined "complete" theme
#'   templates provided by ggplot2. Currently supported options are: "classic"
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
#'   variable (i.e. a facetted plot), e.g. facet_var = "grouping_variable" or
#'   facet_var = grouping_variable. See \code{\link[ggplot2]{facet_wrap}} for
#'   details.
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
#' \donttest{
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
#'           interactive = TRUE)
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
#'           theme = "bw", interactive = TRUE)
#' }
#'
#'
#' @references
#' Wickham, H. (2016). ggplot2: elegant graphics for data analysis. New York, N.Y.: Springer-Verlag.
#'
#' @seealso \code{\link[ggplot2]{geom_violin}}, \code{\link[plotly]{ggplotly}}
#'
#' @export
plot_violin <- function(data, y, #essential parameters
                        x = NULL,
                        ..., #geom-specific customization see ?geom_violin for details
                        fill_var = NULL, colour_var = NULL, #grouping variable aesthetic mappings
                        xlab = NULL, ylab = NULL, title = NULL,
                        fill_var_title = NULL, colour_var_title = NULL, #titles
                        ylim = c(NA, NA), ybreaks = ggplot2::waiver(), #control the y axis limits and scaling
                        transform_y = FALSE, y_transformation = "log10", y_var_labs = ggplot2::waiver(),
                        x_var_order = NULL, x_var_labs = NULL,
                        fill_var_order = NULL, colour_var_order = NULL, #modify grouping variable level order
                        fill_var_labs = NULL, colour_var_labs = NULL, #modify grouping variable labels
                        fill_var_values = NULL, colour_var_values = NULL, #manual colour specification
                        palette = c("plasma", "C", "magma", "A", "inferno", "B", "viridis", "D", "cividis", "E"), #viridis colour palettes
                        palette_direction = c("d2l", "l2d"), palette_begin = 0, palette_end = 1, #viridis colour palette options
                        alpha = 0.6, greyscale = FALSE, #control transparency, convert to greyscale
                        line_size = 1,
                        theme = c("classic", "bw", "grey", "light", "dark", "minimal"),
                        text_size = 14, font = c("sans", "serif", "mono"),#theme options
                        facet_var = NULL, facet_var_order = NULL, facet_var_labs = NULL, #facet options
                        facet_var_strip_position = c("top", "bottom"), facet_var_text_bold = TRUE, #facet aesthetic customization
                        legend_position = c("right", "left", "top", "bottom"), omit_legend = FALSE, #legend position
                        interactive = FALSE, aesthetic_options = FALSE) {#output format

  theme <- match.arg(theme)
  font <- match.arg(font)
  legend_position <- match.arg(legend_position)
  facet_var_strip_position <- match.arg(facet_var_strip_position)
  palette <- match.arg(palette)
  palette_direction <- match.arg(palette_direction)
  palette_direction <- ifelse(palette_direction == "d2l", 1, -1)

  if(is.error(class(data[[y]]))) {
    y <- deparse(substitute(y))
  } else if(!is.character(y) || length(y) > 1){
    stop('If specified, `y` must be a single symbol or character string',
         '\n representing a column in the input data frame supplied to the `data` argument.')
  }
  if(!missing(x)) {
    if(is.error(class(data[[x]]))) {
      x <- deparse(substitute(x))
    } else if(!is.character(x) || length(x) > 1){
      stop('If specified, `x` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }
  if(!missing(fill_var)) {
    if(is.error(class(data[[fill_var]]))) {
      fill_var <- deparse(substitute(fill_var))
    }else if(!is.character(fill_var) || length(fill_var) > 1){
      stop('If specified, `fill_var` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }
  if(!missing(colour_var)) {
    if(is.error(class(data[[colour_var]]))) {
      colour_var <- deparse(substitute(colour_var))
    }else if(!is.character(colour_var) || length(colour_var) > 1){
      stop('If specified, `colour_var` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }
  if(!missing(facet_var)) {
    if(is.error(class(data[[facet_var]]))) {
      facet_var <- deparse(substitute(facet_var))
    } else if(!is.character(facet_var) || length(facet_var) > 1){
      stop('If specified, `facet_var` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }

  #x-variable recoding
  if(!missing(x)){
    data <- dplyr::mutate(data, {{x}} := as.character(.data[[x]]))
  }
  if(!missing(x_var_order)){
    data <- dplyr::mutate(data, {{x}} := forcats::fct_relevel(.data[[x]], levels = !!!x_var_order))
  }
  if(!missing(x_var_labs)){
    data <- dplyr::mutate(data, {{x}} := forcats::fct_recode(.data[[x]], !!!x_var_labs))
  }

  #fill variable recoding
  if(!missing(fill_var)){
    data <- dplyr::mutate(data, {{fill_var}} := as.character(.data[[fill_var]]))
  }
  if(!missing(fill_var) && !missing(fill_var_order)){
    data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_relevel(.data[[fill_var]], levels = !!!fill_var_order))
  }
  if(!missing(fill_var) && !missing(fill_var_labs)){
    data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_recode(.data[[fill_var]], !!!fill_var_labs))
  }

  #colour variable recoding
  if(!missing(colour_var)){
    data <- dplyr::mutate(data, {{colour_var}} := as.character(.data[[colour_var]]))
  }
  if(!missing(colour_var) && !missing(colour_var_order)){
    data <- dplyr::mutate(data, {{colour_var}} := forcats::fct_relevel(.data[[colour_var]], levels = !!!colour_var_order))
  }
  if(!missing(colour_var) && !missing(colour_var_labs)){
    data <- dplyr::mutate(data, {{colour_var}} := forcats::fct_recode(.data[[colour_var]], !!!colour_var_labs))
  }

  #facet label recoding
  if(!missing(facet_var)){
    data <- dplyr::mutate(data, {{facet_var}} := as.character(.data[[facet_var]]))
  }
  if(!missing(facet_var) && !missing(facet_var_order)){
    data <- dplyr::mutate(data, {{facet_var}} := forcats::fct_relevel(.data[[facet_var]], levels = !!!facet_var_order))
  }
  if(!missing(facet_var) && !missing(facet_var_labs)){
    data <- dplyr::mutate(data, {{facet_var}} := forcats::fct_recode(.data[[facet_var]], !!!facet_var_labs))
  }

  #setup foundational plotting object layer
  if(missing(x)) {
    data <- dplyr::mutate(data, .x = 0)
    p <- ggplot2::ggplot(data, ggplot2::aes_string(y = y, x = ".x", fill = fill_var, colour = colour_var))
  } else {
    p <- ggplot2::ggplot(data, ggplot2::aes_string(y = y, x = x, fill = fill_var, colour = colour_var))
  }

  #add the geom layer
  p <- p + ggplot2::geom_violin(alpha = alpha, size = line_size, ...)

  #modification of the colour or fill values
  if (!missing(fill_var) && missing(colour_var)){
    if(!missing(fill_var_values)){
      p <- p +
        ggplot2::scale_fill_manual(values = fill_var_values)
    } else {
      p <- p +
        ggplot2::scale_fill_viridis_d(begin = palette_begin, end = palette_end,
                                      option = palette, direction = palette_direction)
    }
  } else if(missing(fill_var) && !missing(colour_var)) {
    if(!missing(colour_var_values)){
      p <- p +
        ggplot2::scale_colour_manual(values = colour_var_values)
    } else {
      p <- p +
        ggplot2::scale_colour_viridis_d(begin = palette_begin, end = palette_end,
                                        option = palette, direction = palette_direction)
    }
  } else if(!missing(fill_var) && !missing(colour_var)) {
    if(!missing(fill_var_values) && missing(colour_var_values)){
      p <- p +
        ggplot2::scale_fill_manual(values = fill_var_values) +
        ggplot2::scale_colour_viridis_d(begin = palette_begin, end = palette_end,
                                        option = palette, direction = palette_direction)
    } else if(missing(fill_var_values) && !missing(colour_var_values)) {
      p <- p +
        ggplot2::scale_colour_manual(values = colour_var_values) +
        ggplot2::scale_fill_viridis_d(begin = palette_begin, end = palette_end,
                                      option = palette, direction = palette_direction)
    } else if(!missing(fill_var_values) && !missing(colour_var_values)) {
      p <- p +
        ggplot2::scale_fill_manual(values = fill_var_values) +
        ggplot2::scale_colour_manual(values = colour_var_values)
    } else {
      p <- p +
        ggplot2::scale_colour_viridis_d(begin = palette_begin, end = palette_end,
                                        option = palette, direction = palette_direction) +
        ggplot2::scale_fill_viridis_d(begin = palette_begin, end = palette_end,
                                      option = palette, direction = palette_direction)
    }
  }

  #convert to greyscale
  if(greyscale == TRUE){
    p <- p + ggplot2::scale_fill_grey()
  }

  #themes
  if(theme == "classic"){
    p <- p + ggplot2::theme_classic(base_size = text_size, base_family = font)
  } else if (theme == "bw"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "grey"){
    p <- p + ggplot2::theme_grey(base_size = text_size, base_family = font)
  } else if (theme == "light"){
    p <- p + ggplot2::theme_light(base_size = text_size, base_family = font)
  } else if (theme == "dark"){
    p <- p + ggplot2::theme_dark(base_size = text_size, base_family = font)
  } else if (theme == "minimal"){
    p <- p + ggplot2::theme_minimal(base_size = text_size, base_family = font)
  }

  #modification of y-axis limits & transformations
  if(!missing(ylim)) {
    p <- p + ggplot2::coord_cartesian(ylim = c(ylim[1], ylim[2]))
  }
  if(class(y_var_labs) != "waiver") {
    p <- p + ggplot2::scale_y_continuous(labels = y_var_labs)
  } else if(transform_y == FALSE && class(ybreaks) != "waiver"){
    p <- p + ggplot2::scale_y_continuous(breaks = ybreaks, labels = y_var_labs)
  } else if (transform_y == TRUE){
    p <- p + ggplot2::scale_y_continuous(trans = y_transformation, breaks = ybreaks, labels = y_var_labs)
  }

  #labels
  if(!missing(xlab)){
    p <- p + ggplot2::labs(x = xlab)
  }
  if(missing(x)){
    p <- p + ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                            axis.text.x = ggplot2::element_blank(),
                            axis.ticks.x = ggplot2::element_blank())
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
  if(!missing(title)){
    p <- p + ggplot2::labs(title = title)
  }

  #misc
  if(omit_legend == TRUE){
    p <- p + ggplot2::theme(legend.position = "none")
  }
  if(legend_position != "right"){
    p <- p + ggplot2::theme(legend.position = legend_position)
  }
  if(!missing(facet_var)){
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[facet_var]]), strip.position = facet_var_strip_position)
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

# plot_scatter -------------------------------------------------------
#' @title
#'
#' Generate a scatterplot.
#'
#' @description Easily generate scatterplots using ggplot2 with a simplified
#'   customization interface for common modifications with static (ggplot) and
#'   interactive (plotly) output options. The static output is useful for
#'   producing static reports (e.g. for manuscripts) and is readily customized
#'   further using ggplot2 syntax. The interactive output is helpful for
#'   exploring the data and producing dynamic html reports. See
#'   \href{https://craig.rbind.io/post/2021-05-17-asgr-3-1-data-visualization/}{this
#'    blog post} for an introduction to ggplot2.
#'
#' @importFrom dplyr mutate
#' @importFrom forcats fct_relevel
#' @importFrom forcats fct_recode
#' @importFrom rlang !!!
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_jitter
#' @importFrom ggplot2 stat_smooth
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_fill_viridis_c
#' @importFrom ggplot2 scale_fill_viridis_d
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 scale_colour_viridis_c
#' @importFrom ggplot2 scale_colour_viridis_d
#' @importFrom ggplot2 scale_shape_manual
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 scale_size_continuous
#' @importFrom ggplot2 scale_fill_grey
#' @importFrom ggplot2 lims
#' @importFrom ggplot2 labs
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
#' @importFrom ggplot2 coord_cartesian
#' @importFrom plotly ggplotly
#' @importFrom utils browseURL
#'
#' @param data A data frame or tibble containing the dependent measure "y", the
#'   independent measure "x", and any grouping variables or covariates.
#'
#' @param y A numeric variable you want to plot against x (quoted or
#'   unquoted), e.g. y = "variable" or y = variable.
#'
#' @param x A numeric variable you want to plot against y (quoted or
#'   unquoted), e.g. x = "variable" or x = variable.
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
#'   colour, e.g. fill_var = "grouping_variable" or fill_var =
#'   grouping_variable. Produces separate sets of points for each level of the
#'   fill variable. See \code{\link[ggplot2]{aes}} for details. Note: for
#'   geom_point, fill_var and fill only affect shapes 21-24. To split the data
#'   by a variable based on colour, it is therefore easier to use colour_var for
#'   this particular plot geometry. Also works with continuous variables.
#'
#' @param colour_var Use if you want to assign a variable to the point outline
#'   colour, e.g. colour_var = "grouping_variable" or colour_var =
#'   grouping_variable. Produces separate sets of points for each level of the
#'   colour variable. See \code{\link[ggplot2]{aes}} for details.
#'
#' @param shape_var Use if you want to assign a variable to the point shape,
#'   e.g. shape_var = "grouping_variable" or shape_var = grouping_variable.
#'   Produces separate sets of points for each level of the shape variable. See
#'   \code{\link[ggplot2]{aes}} for details.
#'
#' @param size_var Use if you want to assign a continuous variable to the point
#'   size, e.g. size_var = "covariate" or size_var = covariate. Adjusts point
#'   sizes according to the value of the covariate. See
#'   \code{\link[ggplot2]{aes}} for details.
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
#'   the default is ylim = c(NA, NA).
#'
#' @param ybreaks This allows you to change the break points to use for tick
#'   marks on the y-axis. \code{\link{seq}} is particularly useful here. See
#'   \code{\link[ggplot2]{scale_y_continuous}} for details. If ybreaks is
#'   specified, then ylim should be also.
#'
#' @param transform_y Would you like to transform the y axis? (TRUE or FALSE)
#'
#' @param y_transformation If transform_y = TRUE, this determines the
#'   transformation to be applied. Common choices include "log10" (the default),
#'   "log2", "sqrt", or "exp". See \code{\link[ggplot2]{scale_continuous}} for
#'   details.
#'
#' @param y_var_labs Allows you to modify the labels displayed with the y-axis
#'   tick marks. See \code{\link[ggplot2]{scale_continuous}} for details.
#'
#' @param xlim specify the x-axis limits, e.g. xlim = c(lower_limit,
#'   upper_limit). Use NA for the existing minimum or maximum value of x, e.g.
#'   the default is xlim = c(NA, NA)
#'
#' @param xbreaks This allows you to change the break points to use for tick
#'   marks on the x-axis. \code{\link{seq}} is particularly useful here. See
#'   \code{\link[ggplot2]{scale_x_continuous}} for details. If xbreaks is
#'   specified, then xlim should be also.
#'
#' @param transform_x Would you like to transform the x axis? (TRUE or FALSE)
#'
#' @param x_transformation If transform_x = TRUE, this determines the
#'   transformation to be applied. Common choices include "log10" (the default),
#'   "log2", "sqrt", or "exp". See \code{\link[ggplot2]{scale_continuous}} for
#'   details.
#'
#' @param x_var_labs Allows you to modify the labels displayed with the x-axis
#'   tick marks. See \code{\link[ggplot2]{scale_continuous}} for details.
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
#' @param palette If a variable is assigned to fill_var or colour_var, this
#'   determines which
#'   \href{https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html}{viridis
#'    colour palette} to use. Options include "plasma" or "C" (default), "magma"
#'   or "A", "inferno" or "B", "viridis" or "D", and "cividis" or "E". See
#'   \href{https://craig.rbind.io/post/2021-05-17-asgr-3-1-data-visualization/#colourblind-friendly-palettes}{this
#'    link} for examples. You can override these colour palettes with
#'   fill_var_values or colour_var_values.
#'
#' @param palette_direction Choose "d2l" for dark to light (default) or "l2d"
#'   for light to dark.
#'
#' @param palette_begin Value between 0 and 1 that determines where along the
#'   full range of the chosen colour palette's spectrum to begin sampling
#'   colours. See \code{\link[ggplot2]{scale_fill_viridis_d}} for details.
#'
#' @param palette_end Value between 0 and 1 that determines where along the full
#'   range of the chosen colour palette's spectrum to end sampling colours. See
#'   \code{\link[ggplot2]{scale_fill_viridis_d}} for details.
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
#' @param regression_line_type Adjusts the line type of regression lines, e.g.
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
#'   templates provided by ggplot2. Currently supported options are: "classic"
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
#'   variable (i.e. a facetted plot), e.g. facet_var = "grouping_variable" or
#'   facet_var = grouping_variable. See \code{\link[ggplot2]{facet_wrap}} for
#'   details.
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
#' \donttest{
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
#'                regression_line = TRUE, regression_method = "lm")
#'
#' #change the regression line colour
#' mtcars %>%
#'   plot_scatter(y = mpg, x = hp,
#'                regression_line = TRUE, regression_method = "lm",
#'                regression_line_colour = "green")
#'
#' #add standard error envelope
#' mtcars %>%
#'   plot_scatter(y = mpg, x = hp,
#'                regression_line = TRUE, regression_method = "lm", regression_se = TRUE)
#'
#' #adjust standard error envelope transparency
#' mtcars %>%
#'   plot_scatter(y = mpg, x = hp,
#'                regression_line = TRUE, regression_method = "lm", regression_se = TRUE,
#'                regression_alpha = 0.8) #default is 0.5
#'
#'
#' #split by a grouping variable
#' mtcars %>%
#'   plot_scatter(y = mpg, x = hp, colour_var = cyl,
#'                regression_line = TRUE, regression_method = "lm")
#'
#'
#' #fit a polynomial regression line by specifying a regression_formula = formula()
#' mtcars %>%
#'   plot_scatter(y = mpg, x = hp,
#'                regression_line = TRUE, regression_method = "lm", regression_se = TRUE,
#'                regression_formula = y ~ poly(x, 2))
#'
#' mtcars %>%
#'   plot_scatter(y = mpg, x = hp, shape_var = cyl, colour_var = cyl,
#'                regression_line = TRUE, regression_method = "lm",
#'                regression_formula = y ~ poly(x, 3))
#'
#'
#' #fit a non-linear regression line using locally(-weighted) scatterplot smoothing (loess)
#' mtcars %>%
#'   plot_scatter(y = mpg, x = hp,
#'                regression_line = TRUE, regression_se = TRUE,
#'                regression_method = "loess")
#'
#'
#' #fit a non-linear regression line using locally(-weighted) scatterplot smoothing (loess)
#' #& also adjust the span (default = 0.75).
#' #This controls how much of the data is used for the weighted smoothing.
#'
#' mtcars %>%
#'   plot_scatter(y = mpg, x = hp,
#'                regression_line = TRUE, regression_se = TRUE,
#'                regression_method = "loess", loess_span = 0.3)
#'
#' #fit a non-linear regression line using a generalized additive model (gam), the default
#' mtcars %>%
#'   plot_scatter(y = mpg, x = hp,
#'                regression_line = TRUE, regression_se = TRUE,
#'                regression_method = "gam")
#'
#' #use a dashed regression line instead
#' mtcars %>%
#'   plot_scatter(y = mpg, x = hp,
#'                regression_line = TRUE, regression_se = TRUE, regression_line_type = "dashed")
#'
#' #more complex example with overplotting
#' pdata %>%
#'   plot_scatter(y = y1, x = d, colour_var = g,
#'                regression_line = TRUE)
#'
#' #option 1 for dealing with overplotting: add jittering to offset overlappping points
#'  pdata %>%
#'  plot_scatter(y = y1, x = d, colour_var = g,
#'                jitter = TRUE,
#'                regression_line = TRUE)
#'
#' #option 2: make overlapping values more transparent
#'  pdata %>%
#'   plot_scatter(y = y1, x = d, colour_var = g,
#'                alpha = 0.2,
#'                regression_line = TRUE)
#'
#' #option 3: do both and make it interactive
#' pdata %>%
#'  plot_scatter(y = y1, x = d, colour_var = g,
#'               jitter = TRUE, alpha = 0.2,
#'               regression_line = TRUE, interactive = TRUE)
#'
#' #add a faceting variable
#' pdata %>%
#'  plot_scatter(y = y1, x = d,
#'               colour = "black", shape = 21, fill = "green4",
#'               jitter = TRUE, size = 4, alpha = 0.1,
#'               regression_line = TRUE, regression_se = TRUE,
#'               facet_var = g,
#'               ylab = "outcome",
#'               theme = "bw")
#'
#' #open a web page with details on the aesthetic options for ggplot2
#' mtcars %>%
#'  plot_scatter(y = mpg, x = hp, aesthetic_options = TRUE)
#' }
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
                         ylim = c(NA, NA), ybreaks = ggplot2::waiver(),
                         transform_y = FALSE, y_transformation = "log10", y_var_labs = ggplot2::waiver(),
                         xlim = c(NA, NA), xbreaks = ggplot2::waiver(),
                         transform_x = FALSE, x_transformation = "log10", x_var_labs = ggplot2::waiver(),
                         size_lim = c(NA, NA), transform_size = FALSE, size_transformation = "log10",

                         #aesthetic variable mapping customization options
                         fill_var_order = NULL, colour_var_order = NULL, shape_var_order = NULL,
                         fill_var_labs = NULL, colour_var_labs = NULL, shape_var_labs = NULL,
                         fill_var_values = NULL, colour_var_values = NULL, shape_var_values = NULL,
                         palette = c("plasma", "C", "magma", "A", "inferno", "B", "viridis", "D", "cividis", "E"), #viridis colour palettes
                         palette_direction = c("d2l", "l2d"), palette_begin = 0, palette_end = .8, #viridis colour palette options

                         #regression line options
                         regression_line = FALSE, regression_method = "gam",
                         regression_formula = NULL, regression_se = FALSE, ci_level = 0.95,
                         regression_geom = "smooth", regression_line_size = 1, regression_line_colour = NULL,
                         regression_alpha = 0.5, regression_line_type = 1, regression_line_full_range = FALSE,
                         regression_method_args = NULL, loess_span = 0.75,

                         alpha = 0.6, greyscale = FALSE, #control transparency, convert to greyscale
                         theme = c("classic", "bw", "grey", "light", "dark", "minimal"),
                         text_size = 14, font = c("sans", "serif", "mono"), #theme options
                         facet_var = NULL, facet_var_order = NULL, facet_var_labs = NULL, #facet options
                         facet_var_strip_position = c("top", "bottom"), facet_var_text_bold = TRUE, #facet aesthetic customization
                         legend_position = c("right", "left", "top", "bottom"), omit_legend = FALSE, #legend position
                         interactive = FALSE, aesthetic_options = FALSE) {#output format

  theme <- match.arg(theme)
  font <- match.arg(font)
  legend_position <- match.arg(legend_position)
  facet_var_strip_position <- match.arg(facet_var_strip_position)
  palette <- match.arg(palette)
  palette_direction <- match.arg(palette_direction)
  palette_direction <- ifelse(palette_direction == "d2l", 1, -1)

  if(is.error(class(data[[y]]))) {
    y <- deparse(substitute(y))
  } else if(!is.character(y) || length(y) > 1){
    stop('If specified, `y` must be a single symbol or character string',
         '\n representing a column in the input data frame supplied to the `data` argument.')
  }
  if(is.error(class(data[[x]]))) {
    x <- deparse(substitute(x))
  } else if(!is.character(x) || length(x) > 1){
    stop('If specified, `x` must be a single symbol or character string',
         '\n representing a column in the input data frame supplied to the `data` argument.')
  }
  if(!missing(fill_var)) {
    if(is.error(class(data[[fill_var]]))) {
      fill_var <- deparse(substitute(fill_var))
    }else if(!is.character(fill_var) || length(fill_var) > 1){
      stop('If specified, `fill_var` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }
  if(!missing(colour_var)) {
    if(is.error(class(data[[colour_var]]))) {
      colour_var <- deparse(substitute(colour_var))
    }else if(!is.character(colour_var) || length(colour_var) > 1){
      stop('If specified, `colour_var` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }
  if(!missing(shape_var)) {
    if(is.error(class(data[[shape_var]]))) {
      shape_var <- deparse(substitute(shape_var))
    }else if(!is.character(shape_var) || length(shape_var) > 1){
      stop('If specified, `shape_var` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }
  if(!missing(size_var)) {
    if(is.error(class(data[[size_var]]))) {
      size_var <- deparse(substitute(size_var))
    }else if(!is.character(size_var) || length(size_var) > 1){
      stop('If specified, `size_var` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }
  if(!missing(facet_var)) {
    if(is.error(class(data[[facet_var]]))) {
      facet_var <- deparse(substitute(facet_var))
    } else if(!is.character(facet_var) || length(facet_var) > 1){
      stop('If specified, `facet_var` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }

  #fill variable recoding
  if(!missing(fill_var) && class(.data[[fill_var]]) %ni% c("numeric", "integer")){
    data <- dplyr::mutate(data, {{fill_var}} := as.character(.data[[fill_var]]))
    if(!missing(fill_var) && !missing(fill_var_order)){
      data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_relevel(.data[[fill_var]], levels = !!!fill_var_order))
    }
    if(!missing(fill_var) && !missing(fill_var_labs)){
      data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_recode(.data[[fill_var]], !!!fill_var_labs))
    }
  }

  #colour variable recoding
  if(!missing(colour_var) && class(.data[[colour_var]]) %ni% c("numeric", "integer")){
    data <- dplyr::mutate(data, {{colour_var}} := as.character(.data[[colour_var]]))
    if(!missing(colour_var_order)){
      data <- dplyr::mutate(data, {{colour_var}} := forcats::fct_relevel(.data[[colour_var]], levels = !!!colour_var_order))
    }
    if(!missing(colour_var_labs)){
      data <- dplyr::mutate(data, {{colour_var}} := forcats::fct_recode(.data[[colour_var]], !!!colour_var_labs))
    }
  }

  #shape variable recoding
  if(!missing(shape_var)){
    data <- dplyr::mutate(data, {{shape_var}} := as.character(.data[[shape_var]]))
  }
  if(!missing(shape_var) && !missing(shape_var_order)){
    data <- dplyr::mutate(data, {{shape_var}} := forcats::fct_relevel(.data[[shape_var]], levels = !!!shape_var_order))
  }
  if(!missing(shape_var) && !missing(shape_var_labs)){
    data <- dplyr::mutate(data, {{shape_var}} := forcats::fct_recode(.data[[shape_var]], !!!shape_var_labs))
  }

  #facet label recoding
  if(!missing(facet_var)){
    data <- dplyr::mutate(data, {{facet_var}} := as.character(.data[[facet_var]]))
  }
  if(!missing(facet_var) && !missing(facet_var_order)){
    data <- dplyr::mutate(data, {{facet_var}} := forcats::fct_relevel(.data[[facet_var]], levels = !!!facet_var_order))
  }
  if(!missing(facet_var) && !missing(facet_var_labs)){
    data <- dplyr::mutate(data, {{facet_var}} := forcats::fct_recode(.data[[facet_var]], !!!facet_var_labs))
  }

  #core plotting layer
  if(jitter == FALSE){
    p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y,
                                                   colour = colour_var, fill = fill_var,
                                                   shape = shape_var, size = size_var)) +
      ggplot2::geom_point(alpha = alpha, ...)

  } else if(jitter == TRUE){
    p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y,
                                                   colour = colour_var, fill = fill_var,
                                                   shape = shape_var, size = size_var)) +
      ggplot2::geom_jitter(alpha = alpha, ...)
  }

  if(!missing(fill_var)){
    warning("For ggplot2 scatterplots, fill argument only works for point shapes 21-24.\nSpecify the point shape using the shape argument.\nNote: For scatterplots, mapping a grouping variable to colour using colour_var works for all point shapes.")
  }

  #modification of the colour, fill, or shape values
  if (!missing(fill_var) && missing(colour_var)){
    if(!missing(fill_var_values)){
      p <- p +
        ggplot2::scale_fill_manual(values = fill_var_values)
    } else {
      if(class(.data[[colour_var]]) %ni% c("numeric", "integer")) {
        p <- p +
          ggplot2::scale_fill_viridis_d(begin = palette_begin, end = palette_end,
                                        option = palette, direction = palette_direction)
      } else {
        p <- p +
          ggplot2::scale_fill_viridis_c(begin = palette_begin, end = palette_end,
                                        option = palette, direction = palette_direction)
      }
    }
  } else if(missing(fill_var) && !missing(colour_var)) {
    if(!missing(colour_var_values) && class(.data[[colour_var]]) %ni% c("numeric", "integer")) {
      p <- p +
        ggplot2::scale_colour_manual(values = colour_var_values)
    } else {
      if(class(.data[[colour_var]]) %ni% c("numeric", "integer")) {
        p <- p +
          ggplot2::scale_colour_viridis_d(begin = palette_begin, end = palette_end,
                                          option = palette, direction = palette_direction)
      } else {
        p <- p +
          ggplot2::scale_colour_viridis_c(begin = palette_begin, end = palette_end,
                                          option = palette, direction = palette_direction)
      }
    }
  } else if(!missing(fill_var) && !missing(colour_var)) {
    if(!missing(fill_var_values) && missing(colour_var_values)){
      if(class(.data[[colour_var]]) %ni% c("numeric", "integer")) {
        p <- p +
          ggplot2::scale_fill_manual(values = fill_var_values) +
          ggplot2::scale_colour_viridis_d(begin = palette_begin, end = palette_end,
                                          option = palette, direction = palette_direction)
      } else {
        p <- p +
          ggplot2::scale_fill_manual(values = fill_var_values) +
          ggplot2::scale_colour_viridis_c(begin = palette_begin, end = palette_end,
                                          option = palette, direction = palette_direction)
      }
    } else if(missing(fill_var_values) && !missing(colour_var_values)) {
      if(class(.data[[fill_var]]) %ni% c("numeric", "integer")) {
        p <- p +
          ggplot2::scale_colour_manual(values = colour_var_values) +
          ggplot2::scale_fill_viridis_d(begin = palette_begin, end = palette_end,
                                        option = palette, direction = palette_direction)
      } else {
        p <- p +
          ggplot2::scale_colour_manual(values = colour_var_values) +
          ggplot2::scale_fill_viridis_c(begin = palette_begin, end = palette_end,
                                        option = palette, direction = palette_direction)
      }
    } else if(!missing(fill_var_values) && !missing(colour_var_values)) {
      p <- p +
        ggplot2::scale_fill_manual(values = fill_var_values) +
        ggplot2::scale_colour_manual(values = colour_var_values)
    } else {
      if(class(.data[[fill_var]]) %ni% c("numeric", "integer") &&
         class(.data[[colour_var]]) %ni% c("numeric", "integer")) {
        p <- p +
          ggplot2::scale_fill_viridis_d(begin = palette_begin, end = palette_end,
                                        option = palette, direction = palette_direction) +
          ggplot2::scale_colour_viridis_d(begin = palette_begin, end = palette_end,
                                          option = palette, direction = palette_direction)
      } else if(class(.data[[fill_var]]) %in% c("numeric", "integer") &&
                class(.data[[colour_var]]) %ni% c("numeric", "integer")) {
        p <- p +
          ggplot2::scale_fill_viridis_c(begin = palette_begin, end = palette_end,
                                        option = palette, direction = palette_direction) +
          ggplot2::scale_colour_viridis_d(begin = palette_begin, end = palette_end,
                                          option = palette, direction = palette_direction)
      } else if(class(.data[[fill_var]]) %ni% c("numeric", "integer") &&
                class(.data[[colour_var]]) %in% c("numeric", "integer")) {
        p <- p +
          ggplot2::scale_fill_viridis_d(begin = palette_begin, end = palette_end,
                                        option = palette, direction = palette_direction) +
          ggplot2::scale_colour_viridis_c(begin = palette_begin, end = palette_end,
                                          option = palette, direction = palette_direction)
      } else {
        p <- p +
          ggplot2::scale_fill_viridis_c(begin = palette_begin, end = palette_end,
                                        option = palette, direction = palette_direction) +
          ggplot2::scale_colour_viridis_c(begin = palette_begin, end = palette_end,
                                          option = palette, direction = palette_direction)
      }
    }
  }
  if (!missing(shape_var) && !missing(shape_var_values)){
    p <- p +
      ggplot2::scale_shape_manual(values = shape_var_values)
    if(interactive == FALSE){
      warning("Custom shape value assignments may not appear in shape legend when viewed in the R studio plots panel.\nIf this happens, try expanding the plot with the zoom button, view it in interactive mode with plotly,\n or print it to a separate graphics window")
    }
  }

  #regression line options
  if(!missing(regression_line_colour)){
    if(regression_line == TRUE && missing(regression_formula)){
      if(regression_method != "gam"){
        p <- p +
          ggplot2::stat_smooth(method = regression_method, formula = y ~ x,
                               se = regression_se, alpha = regression_alpha,
                               linetype = regression_line_type, colour = regression_line_colour,
                               size = regression_line_size, fullrange = regression_line_full_range,
                               geom = regression_geom, level = ci_level, span = loess_span,
                               method.args = regression_method_args)
      } else if(regression_method == "gam"){
        p <- p +
          ggplot2::stat_smooth(method = regression_method, formula = y ~ s(x, bs = "cr"),
                               se = regression_se, alpha = regression_alpha,
                               linetype = regression_line_type, colour = regression_line_colour,
                               size = regression_line_size, fullrange = regression_line_full_range,
                               geom = regression_geom, level = CI_level, span = loess_span,
                               method.args = regression_method_args)
      }
    } else if(regression_line == TRUE && !missing(regression_formula)){
      p <- p +
        ggplot2::stat_smooth(method = regression_method, formula = regression_formula,
                             se = regression_se, alpha = regression_alpha,
                             linetype = regression_line_type, colour = regression_line_colour,
                             size = regression_line_size, fullrange = regression_line_full_range,
                             geom = regression_geom, level = ci_level, span = loess_span,
                             method.args = regression_method_args)
    }
  } else if (missing(regression_line_colour)){
    if(regression_line == TRUE && missing(regression_formula)){
      if(regression_method != "gam"){
        p <- p +
          ggplot2::stat_smooth(method = regression_method, formula = y ~ x,
                               se = regression_se, alpha = regression_alpha,
                               linetype = regression_line_type,
                               size = regression_line_size, fullrange = regression_line_full_range,
                               geom = regression_geom, level = ci_level, span = loess_span,
                               method.args = regression_method_args)
      } else if(regression_method == "gam"){
        p <- p +
          ggplot2::stat_smooth(method = regression_method, formula = y ~ s(x, bs = "cr"),
                               se = regression_se, alpha = regression_alpha,
                               linetype = regression_line_type,
                               size = regression_line_size, fullrange = regression_line_full_range,
                               geom = regression_geom, level = ci_level, span = loess_span,
                               method.args = regression_method_args)
      }
    } else if(regression_line == TRUE && !missing(regression_formula)){
      p <- p +
        ggplot2::stat_smooth(method = regression_method, formula = regression_formula,
                             se = regression_se, alpha = regression_alpha,
                             linetype = regression_line_type,
                             size = regression_line_size, fullrange = regression_line_full_range,
                             geom = regression_geom, level = ci_level, span = loess_span,
                             method.args = regression_method_args)
    }
  }

  #convert to greyscale
  if(greyscale == TRUE){
    p <- p + ggplot2::scale_fill_grey()
  }

  #themes
  if(theme == "classic"){
    p <- p + ggplot2::theme_classic(base_size = text_size, base_family = font)
  } else if (theme == "bw"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "grey"){
    p <- p + ggplot2::theme_grey(base_size = text_size, base_family = font)
  } else if (theme == "light"){
    p <- p + ggplot2::theme_light(base_size = text_size, base_family = font)
  } else if (theme == "dark"){
    p <- p + ggplot2::theme_dark(base_size = text_size, base_family = font)
  } else if (theme == "minimal"){
    p <- p + ggplot2::theme_minimal(base_size = text_size, base_family = font)
  }

  #modification of x/y-axis limits & transformations
  if(!missing(xlim) && missing(ylim)) {
    p <- p + ggplot2::coord_cartesian(xlim = c(xlim[1], xlim[2]))
  } else if (missing(xlim) && !missing(ylim)) {
    p <- p + ggplot2::coord_cartesian(ylim = c(ylim[1], ylim[2]))
  } else if (!missing(xlim) && !missing(ylim)) {
    p <- p + ggplot2::coord_cartesian(xlim = c(xlim[1], xlim[2]), ylim = c(ylim[1], ylim[2]))
  }
  #x
  if(class(x_var_labs) != "waiver") {
    p <- p + ggplot2::scale_x_continuous(labels = x_var_labs)
  } else if(transform_x == FALSE && class(xbreaks) != "waiver"){
    p <- p + ggplot2::scale_x_continuous(breaks = xbreaks, labels = x_var_labs)
  } else if (transform_x == TRUE){
    p <- p + ggplot2::scale_x_continuous(trans = x_transformation, breaks = xbreaks, labels = x_var_labs)
  }
  #y
  if(class(y_var_labs) != "waiver") {
    p <- p + ggplot2::scale_y_continuous(labels = y_var_labs)
  } else if(transform_y == FALSE && class(ybreaks) != "waiver"){
    p <- p + ggplot2::scale_y_continuous(breaks = ybreaks, labels = y_var_labs)
  } else if (transform_y == TRUE){
    p <- p + ggplot2::scale_y_continuous(trans = y_transformation, breaks = ybreaks, labels = y_var_labs)
  }

  #modification of (continuous) size variable limits & transformations
  if(!missing(size_var) && !missing(size_lim) && transform_size == FALSE){
    if(is.numeric(size_var)){
      p <- p + ggplot2::scale_size_continuous(limits = c(size_lim[1], size_lim[2]))
    } else {
      message("size variable must be of numeric class to modify limits or apply transformations")
    }
  } else if (!missing(size_var) && missing(size_lim) && transform_size == TRUE){
    if(is.numeric(size_var)){
      p <- p + ggplot2::scale_size_continuous(limits = c(NA, NA), trans = size_transformation)
    } else {
      message("size variable must be of numeric class to modify limits or apply transformations")
    }
  } else if (!missing(size_var) && !missing(size_lim) && transform_size  == TRUE){
    if(is.numeric(size_var)){
      p <- p + ggplot2::scale_size_continuous(limits = c(size_lim[1], size_lim[2]), trans = size_transformation)
    } else {
      message("size variable must be of numeric class to modify limits or apply transformations")
    }
  }

  #labels
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
  if(!missing(title)){
    p <- p + ggplot2::labs(title = title)
  }

  #misc
  if(omit_legend == TRUE){
    p <- p + ggplot2::theme(legend.position = "none")
  }
  if(legend_position != "right"){
    p <- p + ggplot2::theme(legend.position = legend_position)
  }
  if(!missing(facet_var)){
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[facet_var]]), strip.position = facet_var_strip_position)
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

# plot_bar ----------------------------------------------------------------
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
#'   \code{\link{plot_stat_error}} instead.  See
#'   \href{https://craig.rbind.io/post/2021-05-17-asgr-3-1-data-visualization/}{this
#'    blog post} for an introduction to ggplot2.
#'
#' @importFrom dplyr mutate
#' @importFrom forcats fct_relevel
#' @importFrom forcats fct_recode
#' @importFrom forcats fct_reorder
#' @importFrom forcats fct_infreq
#' @importFrom forcats fct_rev
#' @importFrom rlang !!!
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_fill_grey
#' @importFrom ggplot2 lims
#' @importFrom ggplot2 labs
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
#' @importFrom ggplot2 waiver
#' @importFrom ggplot2 coord_cartesian
#' @importFrom plotly ggplotly
#' @importFrom utils browseURL
#'
#' @param data A data frame or tibble containing at least one categorical
#'   variable.
#'
#' @param x A categorical variable you want to obtain separate bar plots for (quoted or
#'   unquoted), e.g. x = "variable" or x = variable. If
#'   you want to plot all bars on top of each other (position = "fill" or
#'   position = "stack") to form a single banded bar leave "x" blank and assign
#'   a variable to either fill_var or colour_var instead. N.B. failing to assign
#'   a variable to x will also remove x-axis ticks and labels.
#'
#' @param y A numeric variable containing the values you would like plotted on
#'   the y-axis (quoted or unquoted), e.g. y = "variable" or y = variable. If y
#'   is not specified, then the stat = "count" option will be used for
#'   \code{\link[ggplot2]{geom_bar}} and the counts of the variable(s) assigned
#'   to x, fill_var, and/or colour_var will be plotted on the y-axis.
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
#'   e.g. fill_var = "grouping_variable" or fill_var = grouping_variable.
#'   Produces separate sets of bars for each level of the fill variable. See
#'   \code{\link[ggplot2]{aes}} for details.
#'
#' @param colour_var Use if you want to assign a variable to the bar outline
#'   colour, e.g. colour_var = "grouping_variable" or colour_var =
#'   grouping_variable. Produces separate sets of bars for each level of the
#'   colour variable. See \code{\link[ggplot2]{aes}} for details.
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
#' @param ybreaks This allows you to change the break points to use for tick
#'   marks on the y-axis. \code{\link{seq}} is particularly useful here. See
#'   \code{\link[ggplot2]{scale_y_continuous}} for details. If ybreaks is
#'   specified, then ylim should be also.
#'
#' @param transform_y Would you like to transform the y axis? (TRUE or FALSE)
#'
#' @param y_transformation If transform_y = TRUE, this determines the
#'   transformation to be applied. Common choices include "log10" (the default),
#'   "log2", "sqrt", or "exp". See \code{\link[ggplot2]{scale_continuous}} for
#'   details.
#'
#' @param y_var_labs Allows you to modify the labels displayed with the y-axis
#'   tick marks. See \code{\link[ggplot2]{scale_continuous}} for details.
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
#' @param palette If a variable is assigned to fill_var or colour_var, this
#'   determines which
#'   \href{https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html}{viridis
#'    colour palette} to use. Options include "plasma" or "C" (default), "magma"
#'   or "A", "inferno" or "B", "viridis" or "D", and "cividis" or "E". See
#'   \href{https://craig.rbind.io/post/2021-05-17-asgr-3-1-data-visualization/#colourblind-friendly-palettes}{this
#'    link} for examples. You can override these colour palettes with
#'   fill_var_values or colour_var_values.
#'
#' @param palette_direction Choose "d2l" for dark to light (default) or "l2d"
#'   for light to dark.
#'
#' @param palette_begin Value between 0 and 1 that determines where along the
#'   full range of the chosen colour palette's spectrum to begin sampling
#'   colours. See \code{\link[ggplot2]{scale_fill_viridis_d}} for details.
#'
#' @param palette_end Value between 0 and 1 that determines where along the full
#'   range of the chosen colour palette's spectrum to end sampling colours. See
#'   \code{\link[ggplot2]{scale_fill_viridis_d}} for details.
#'
#' @param alpha This adjusts the transparency/opacity of the graphical
#'   components of the plot, ranging from 0 = 100% transparent to 1 = 100%
#'   opaque.
#'
#' @param greyscale Set to TRUE if you want the plot converted to greyscale.
#'
#' @param line_size Controls the thickness of the bar outlines.
#'
#' @param coord_flip Flips the x and y axes. See
#'   \code{\link[ggplot2]{coord_flip}} for details.
#'
#' @param theme Adjusts the theme using 1 of 6 predefined "complete" theme
#'   templates provided by ggplot2. Currently supported options are: "classic"
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
#'   variable (i.e. a facetted plot), e.g. facet_var = "grouping_variable" or
#'   facet_var = grouping_variable. See \code{\link[ggplot2]{facet_wrap}} for
#'   details.
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
                     ylim = c(NA, NA), ybreaks = ggplot2::waiver(), #control the y axis limits and scaling
                     transform_y = FALSE, y_transformation = "log10", y_var_labs = ggplot2::waiver(),
                     x_var_order_by_y = NULL, x_var_order = NULL,
                     fill_var_order_by_y = NULL, fill_var_order = NULL,
                     colour_var_order_by_y = NULL, colour_var_order = NULL, #modify grouping variable level order
                     x_var_labs = NULL, fill_var_labs = NULL, colour_var_labs = NULL, #modify grouping variable labels
                     fill_var_values = NULL, colour_var_values = NULL, #manual colour specification
                     palette = c("plasma", "C", "magma", "A", "inferno", "B", "viridis", "D", "cividis", "E"), #viridis colour palettes
                     palette_direction = c("d2l", "l2d"), palette_begin = 0, palette_end = 0.9, #viridis colour palette options
                     alpha = 0.8, greyscale = FALSE, #control transparency, convert to greyscale
                     line_size = 1,
                     coord_flip = FALSE,
                     theme = c("classic", "bw", "grey", "light", "dark", "minimal"),
                     text_size = 14, font = c("sans", "serif", "mono"), #theme options
                     facet_var = NULL, facet_var_order = NULL, facet_var_labs = NULL, #facet options
                     facet_var_strip_position = c("top", "bottom"), facet_var_text_bold = TRUE, #facet aesthetic customization
                     legend_position = c("right", "left", "top", "bottom"), omit_legend = FALSE, #legend position
                     interactive = FALSE, aesthetic_options = FALSE) {#output format

  theme <- match.arg(theme)
  position <- match.arg(position)
  font <- match.arg(font)
  legend_position <- match.arg(legend_position)
  facet_var_strip_position <- match.arg(facet_var_strip_position)
  palette <- match.arg(palette)
  palette_direction <- match.arg(palette_direction)
  palette_direction <- ifelse(palette_direction == "d2l", 1, -1)

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
  if(!missing(x)) {
    if(is.error(class(data[[x]]))) {
      x <- deparse(substitute(x))
    } else if(!is.character(x) || length(x) > 1){
      stop('If specified, `x` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }
  if(!missing(y)) {
    if(is.error(class(data[[y]]))) {
      y <- deparse(substitute(y))
    } else if(!is.character(y) || length(y) > 1){
      stop('If specified, `y` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }
  if(!missing(fill_var)) {
    if(is.error(class(data[[fill_var]]))) {
      fill_var <- deparse(substitute(fill_var))
    }else if(!is.character(fill_var) || length(fill_var) > 1){
      stop('If specified, `fill_var` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }
  if(!missing(colour_var)) {
    if(is.error(class(data[[colour_var]]))) {
      colour_var <- deparse(substitute(colour_var))
    }else if(!is.character(colour_var) || length(colour_var) > 1){
      stop('If specified, `colour_var` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }
  if(!missing(facet_var)) {
    if(is.error(class(data[[facet_var]]))) {
      facet_var <- deparse(substitute(facet_var))
    } else if(!is.character(facet_var) || length(facet_var) > 1){
      stop('If specified, `facet_var` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }

  #x-variable recoding
  if(!missing(x)){
    data <- dplyr::mutate(data, {{x}} := as.character(.data[[x]]))
  } else {
    data <- dplyr::mutate(data, .x = 0)
  }
  if(!missing(x) && !missing(x_var_order_by_y)) {
    if(!missing(y)) {
      if(x_var_order_by_y == "d") {
        data <- dplyr::mutate(data, {{x}} := forcats::fct_reorder(.data[[x]], .data[[y]], .desc = TRUE))
      } else {
        data <- dplyr::mutate(data, {{x}} := forcats::fct_reorder(.data[[x]], .data[[y]], .desc = FALSE))
      }
    } else {
      if(x_var_order_by_y == "d") {
        data <- dplyr::mutate(data, {{x}} := forcats::fct_infreq(.data[[x]]))
      } else {
        data <- dplyr::mutate(data, {{x}} := forcats::fct_rev(forcats::fct_infreq(.data[[x]])))
      }
    }
  }
  if(!missing(x) && !missing(x_var_order)){
    data <- dplyr::mutate(data, {{x}} := forcats::fct_relevel(.data[[x]], levels = !!!x_var_order))
  }
  if(!missing(x) && !missing(x_var_labs)){
    data <- dplyr::mutate(data, {{x}} := forcats::fct_recode(.data[[x]], !!!x_var_labs))
  }

  #fill variable recoding
  if(!missing(fill_var)){
    data <- dplyr::mutate(data, {{fill_var}} := as.character(.data[[fill_var]]))
  }
  if(!missing(fill_var) && !missing(fill_var_order_by_y)) {
    if(!missing(y)) {
      if(fill_var_order_by_y == "d") {
        data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_reorder(.data[[fill_var]], .data[[y]], .desc = TRUE))
      } else {
        data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_reorder(.data[[fill_var]], .data[[y]], .desc = FALSE))
      }
    } else {
      if(fill_var_order_by_y == "d") {
        data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_infreq(.data[[fill_var]]))
      } else {
        data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_rev(forcats::fct_infreq(.data[[fill_var]])))
      }
    }
  }
  if(!missing(fill_var) && !missing(fill_var_order)){
    data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_relevel(.data[[fill_var]], levels = !!!fill_var_order))
  }
  if(!missing(fill_var) && !missing(fill_var_labs)){
    data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_recode(.data[[fill_var]], !!!fill_var_labs))
  }

  #colour variable recoding
  if(!missing(colour_var)){
    data <- dplyr::mutate(data, {{colour_var}} := as.character(.data[[colour_var]]))
  }
  if(!missing(colour_var) && !missing(colour_var_order_by_y)) {
    if(!missing(y)) {
      if(colour_var_order_by_y == "d") {
        data <- dplyr::mutate(data, {{colour_var}} := forcats::fct_reorder(.data[[colour_var]], .data[[y]], .desc = TRUE))
      } else {
        data <- dplyr::mutate(data, {{colour_var}} := forcats::fct_reorder(.data[[colour_var]], .data[[y]], .desc = FALSE))
      }
    } else {
      if(colour_var_order_by_y == "d") {
        data <- dplyr::mutate(data, {{colour_var}} := forcats::fct_infreq(.data[[colour_var]]))
      } else {
        data <- dplyr::mutate(data, {{colour_var}} := forcats::fct_rev(forcats::fct_infreq(.data[[colour_var]])))
      }
    }
  }
  if(!missing(colour_var) && !missing(colour_var_order)){
    data <- dplyr::mutate(data, {{colour_var}} := forcats::fct_relevel(.data[[colour_var]], levels = !!!colour_var_order))
  }
  if(!missing(colour_var) && !missing(colour_var_labs)){
    data <- dplyr::mutate(data, {{colour_var}} := forcats::fct_recode(.data[[colour_var]], !!!colour_var_labs))
  }

  #facet label recoding
  if(!missing(facet_var)){
    data <- dplyr::mutate(data, {{facet_var}} := as.character(.data[[facet_var]]))
  }
  if(!missing(facet_var) && !missing(facet_var_order)){
    data <- dplyr::mutate(data, {{facet_var}} := forcats::fct_relevel(.data[[facet_var]], levels = !!!facet_var_order))
  }
  if(!missing(facet_var) && !missing(facet_var_labs)){
    data <- dplyr::mutate(data, {{facet_var}} := forcats::fct_recode(.data[[facet_var]], !!!facet_var_labs))
  }

  #setup foundational plotting object layer
  if(!missing(y) && !missing(x)) {
    p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y, fill = fill_var, colour = colour_var))
  } else if (!missing(x)) {
    p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, fill = fill_var, colour = colour_var))
  } else if (!missing(y) && missing(x)) {
    p <- ggplot2::ggplot(data, ggplot2::aes_string(y = y, x = ".x", fill = fill_var, colour = colour_var))
  } else {
    p <- ggplot2::ggplot(data, ggplot2::aes_string(x = ".x", fill = fill_var, colour = colour_var))
  }

  #add the geom layer
  if(missing(y)) {
    if(position == "dodge") {
      p <- p +
        ggplot2::geom_bar(alpha = alpha, stat = "count", width = width,
                          position = ggplot2::position_dodge2(padding = dodge_padding), size = line_size, ...)
    } else {
      p <- p +
        ggplot2::geom_bar(alpha = alpha, stat = "count", width = width,  position = position, size = line_size, ...)
    }
  } else {
    if(position == "dodge") {
      p <- p +
        ggplot2::geom_bar(alpha = alpha, stat = "identity", width = width,
                          position = ggplot2::position_dodge2(padding = dodge_padding), size = line_size, ...)
    } else {
      p <- p +
        ggplot2::geom_bar(alpha = alpha, stat = "identity", width = width,  position = position, size = line_size, ...)
    }

  }

  #modification of the colour or fill values
  if (!missing(fill_var) && missing(colour_var)){
    if(!missing(fill_var_values)){
      p <- p +
        ggplot2::scale_fill_manual(values = fill_var_values)
    } else {
      p <- p +
        ggplot2::scale_fill_viridis_d(begin = palette_begin, end = palette_end,
                                      option = palette, direction = palette_direction)
    }
  } else if(missing(fill_var) && !missing(colour_var)) {
    if(!missing(colour_var_values)){
      p <- p +
        ggplot2::scale_colour_manual(values = colour_var_values)
    } else {
      p <- p +
        ggplot2::scale_colour_viridis_d(begin = palette_begin, end = palette_end,
                                        option = palette, direction = palette_direction)
    }
  } else if(!missing(fill_var) && !missing(colour_var)) {
    if(!missing(fill_var_values) && missing(colour_var_values)){
      p <- p +
        ggplot2::scale_fill_manual(values = fill_var_values) +
        ggplot2::scale_colour_viridis_d(begin = palette_begin, end = palette_end,
                                        option = palette, direction = palette_direction)
    } else if(missing(fill_var_values) && !missing(colour_var_values)) {
      p <- p +
        ggplot2::scale_colour_manual(values = colour_var_values) +
        ggplot2::scale_fill_viridis_d(begin = palette_begin, end = palette_end,
                                      option = palette, direction = palette_direction)
    } else if(!missing(fill_var_values) && !missing(colour_var_values)) {
      p <- p +
        ggplot2::scale_fill_manual(values = fill_var_values) +
        ggplot2::scale_colour_manual(values = colour_var_values)
    } else {
      p <- p +
        ggplot2::scale_colour_viridis_d(begin = palette_begin, end = palette_end,
                                        option = palette, direction = palette_direction) +
        ggplot2::scale_fill_viridis_d(begin = palette_begin, end = palette_end,
                                      option = palette, direction = palette_direction)
    }
  }

  #convert to greyscale
  if(greyscale == TRUE){
    p <- p + ggplot2::scale_fill_grey()
  }

  #themes
  if(theme == "classic"){
    p <- p + ggplot2::theme_classic(base_size = text_size, base_family = font)
  } else if (theme == "bw"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "grey"){
    p <- p + ggplot2::theme_grey(base_size = text_size, base_family = font)
  } else if (theme == "light"){
    p <- p + ggplot2::theme_light(base_size = text_size, base_family = font)
  } else if (theme == "dark"){
    p <- p + ggplot2::theme_dark(base_size = text_size, base_family = font)
  } else if (theme == "minimal"){
    p <- p + ggplot2::theme_minimal(base_size = text_size, base_family = font)
  }

  #misc
  if(omit_legend == TRUE){
    p <- p + ggplot2::theme(legend.position = "none")
  }
  if(legend_position != "right"){
    p <- p + ggplot2::theme(legend.position = legend_position)
  }
  if(!missing(facet_var)){
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[facet_var]]), strip.position = facet_var_strip_position)
  }
  if(!missing(facet_var) & facet_var_text_bold == TRUE){
    p <- p + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
  }

  #modification of y-axis limits & transformations
  if(!missing(ylim) && coord_flip == FALSE) {
    p <- p + ggplot2::coord_cartesian(ylim = c(ylim[1], ylim[2]))
  } else if (!missing(ylim) && coord_flip == TRUE) {
    p <- p + ggplot2::coord_flip(ylim = c(ylim[1], ylim[2]))
  } else if (missing(ylim) && coord_flip == TRUE) {
    p <- p + ggplot2::coord_flip()
  }
  if(class(y_var_labs) != "waiver") {
    p <- p + ggplot2::scale_y_continuous(labels = y_var_labs)
  } else if(transform_y == FALSE && class(ybreaks) != "waiver"){
    p <- p + ggplot2::scale_y_continuous(breaks = ybreaks, labels = y_var_labs)
  } else if (transform_y == TRUE){
    p <- p + ggplot2::scale_y_continuous(trans = y_transformation, breaks = ybreaks, labels = y_var_labs)
  }

  #modification of axis labels
  if(missing(x) && coord_flip == FALSE){
    p <- p + ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                            axis.text.x = ggplot2::element_blank(),
                            axis.ticks.x = ggplot2::element_blank())
  } else if (missing(x) && coord_flip == TRUE) {
    p <- p + ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                            axis.text.y = ggplot2::element_blank(),
                            axis.ticks.y = ggplot2::element_blank())
  }
  if(!missing(xlab)){
    p <- p + ggplot2::labs(x = xlab)
  } else if(missing(x)) {
    p <- p + ggplot2::labs(x = NULL)
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
  if(!missing(title)){
    p <- p + ggplot2::labs(title = title)
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


# plot_stat_error ---------------------------------------------------------
#' @title
#'
#' Plot a sample mean or median +/- error bars.
#'
#' @description Easily generate plots of a sample mean or median +/- error bars
#'   using ggplot2 with a simplified customization interface with static
#'   (ggplot) and interactive (plotly) output options. The static output is
#'   useful for producing static reports (e.g. for manuscripts) and is readily
#'   customized further using ggplot2 syntax. The interactive output is helpful
#'   for exploring the data and producing dynamic html reports. See
#'   \href{https://craig.rbind.io/post/2021-05-17-asgr-3-1-data-visualization/}{this
#'    blog post} for an introduction to ggplot2.
#'
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
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 position_dodge
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 scale_fill_grey
#' @importFrom ggplot2 lims
#' @importFrom ggplot2 labs
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
#' @importFrom ggplot2 waiver
#' @importFrom ggplot2 coord_cartesian
#' @importFrom rlang .data
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
#' @param y A numeric variable you want to plot on the y-axis (quoted or
#'   unquoted), e.g. y = "variable" or y = variable.
#'
#' @param x A categorical variable you want to plot on the x-axis (quoted or
#'   unquoted), e.g. x = "variable" or x = variable.
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
#' @param ybreaks This allows you to change the break points to use for tick
#'   marks on the y-axis. \code{\link{seq}} is particularly useful here. See
#'   \code{\link[ggplot2]{scale_y_continuous}} for details. If ybreaks is
#'   specified, then ylim should be also.
#'
#' @param transform_y Would you like to transform the y axis? (TRUE or FALSE)
#'
#' @param y_transformation If transform_y = TRUE, this determines the
#'   transformation to be applied. Common choices include "log10" (the default),
#'   "log2", "sqrt", or "exp". See \code{\link[ggplot2]{scale_continuous}} for
#'   details.
#'
#' @param y_var_labs Allows you to modify the labels displayed with the y-axis
#'   tick marks. See \code{\link[ggplot2]{scale_continuous}} for details.
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
#'   colour, e.g. fill_var = "grouping_variable" or fill_var =
#'   grouping_variable. Produces separate sets of points for each level of the
#'   fill variable. See \code{\link[ggplot2]{aes}} for details. Note: for geom =
#'   "point", fill_var and fill only affect shapes 21-24 (21 is the default). To
#'   split the data by a variable based on colour, it is therefore easier to use
#'   colour_var for this particular plot geometry.
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
#'   colour, e.g. colour_var = "grouping_variable" or colour_var =
#'   grouping_variable. Produces separate sets of points for each level of the
#'   colour variable. See \code{\link[ggplot2]{aes}} for details.
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
#' @param palette If a variable is assigned to fill_var or colour_var, this
#'   determines which
#'   \href{https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html}{viridis
#'    colour palette} to use. Options include "plasma" or "C" (default), "magma"
#'   or "A", "inferno" or "B", "viridis" or "D", and "cividis" or "E". See
#'   \href{https://craig.rbind.io/post/2021-05-17-asgr-3-1-data-visualization/#colourblind-friendly-palettes}{this
#'    link} for examples. You can override these colour palettes with
#'   fill_var_values or colour_var_values.
#'
#' @param palette_direction Choose "d2l" for dark to light (default) or "l2d"
#'   for light to dark.
#'
#' @param palette_begin Value between 0 and 1 that determines where along the
#'   full range of the chosen colour palette's spectrum to begin sampling
#'   colours. See \code{\link[ggplot2]{scale_fill_viridis_d}} for details.
#'
#' @param palette_end Value between 0 and 1 that determines where along the full
#'   range of the chosen colour palette's spectrum to end sampling colours. See
#'   \code{\link[ggplot2]{scale_fill_viridis_d}} for details.
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
#' @param eb_line_type Controls the error bar line type. Default = 1 or "solid".
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
#' @param line_type Controls the connection line type. Default = 1 or "solid".
#'
#' @param line_size Controls the thickness of the connection lines. Default =
#'   0.5.
#'
#' @param theme Adjusts the theme using 1 of 6 predefined "complete" theme
#'   templates provided by ggplot2. Currently supported options are: "classic"
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
#'   variable (i.e. a facetted plot), e.g. facet_var = "grouping_variable" or
#'   facet_var = grouping_variable. See \code{\link[ggplot2]{facet_wrap}} for
#'   details.
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
#' \donttest{
#'
#' pdata %>%
#'   plot_stat_error(y = y1, x = d, colour_var = g, print_stats = TRUE,
#'                   geom = "point", p_size = 3,
#'                   add_lines = TRUE,
#'                   dodge_width = 0,
#'                   alpha = 0.6)
#'
#' pdata %>%
#'  plot_stat_error(y = y1, x = g, coord_flip = TRUE,
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
#'                   stat = "mean", error = "ci", ci_level = 0.8, interactive = TRUE)
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
#' }
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
#' @seealso \code{\link{plot_stat_error}}, \code{\link[ggplot2]{geom_point}}, \code{\link[ggplot2]{geom_line}},
#'   \code{\link[ggplot2]{geom_bar}}, \code{\link[plotly]{ggplotly}},
#'   \code{\link[base]{mean}}, \code{\link[stats]{sd}}, \code{\link[stats]{quantile}},
#'   \code{\link[boot]{boot.ci}}, \code{\link{median_ci}}
#'
#' @export
plot_stat_error <- function(data, y, x = NULL, geom = c("point", "bar"), stat = c("mean", "median"),
                            error = c("ci", "sd", "se", "var", "quartile"),
                            ci_level = 0.95, ci_type = c("perc","bca", "norm", "basic"),
                            replicates = 2000, parallel = FALSE, cores = NULL,
                            xlab = NULL, ylab = NULL, title = NULL, ...,
                            ylim = c(NA, NA), ybreaks = ggplot2::waiver(), #control the y axis limits and scaling
                            transform_y = FALSE, y_transformation = "log10", y_var_labs = ggplot2::waiver(),
                            x_var_order = NULL, x_var_labs = NULL,
                            fill_var = NULL, fill_var_order = NULL, fill_var_values = NULL,
                            fill_var_labs = NULL, fill_var_title = NULL,
                            colour_var = NULL, colour_var_order = NULL, colour_var_values = NULL,
                            colour_var_labs = NULL, colour_var_title = NULL,
                            palette = c("plasma", "C", "magma", "A", "inferno", "B", "viridis", "D", "cividis", "E"), #viridis colour palettes
                            palette_direction = c("d2l", "l2d"), palette_begin = 0, palette_end = 0.8, #viridis colour palette options
                            greyscale = FALSE,
                            b_width = 0.75, p_size = 3, p_shape = 21,
                            dodge_width = 0.9, eb_size = 0.3, eb_width = 0.2, eb_alpha = 1,
                            eb_line_type = 1, eb_colour = NULL,
                            add_lines = F, line_alpha = 0.75, line_group = NULL,
                            line_colour = NULL, line_type = 1, line_size = 0.5,
                            theme = c("classic", "bw", "grey", "light", "dark", "minimal"),
                            text_size = 14, font = c("sans", "serif", "mono"),
                            coord_flip = FALSE, omit_legend = FALSE,
                            legend_position = c("right", "left", "bottom", "top"),
                            facet_var = NULL, facet_var_order = NULL, facet_var_labs = NULL,
                            facet_var_strip_position = c("top", "bottom"),
                            facet_var_text_bold = TRUE,
                            print_stats = F, aesthetic_options = FALSE,
                            output = "p", interactive = FALSE, na.rm = TRUE){

  theme <- match.arg(theme)
  geom <- match.arg(geom)
  stat <- match.arg(stat)
  error <- match.arg(error)
  ci_type <- match.arg(ci_type)
  font <- match.arg(font)
  legend_position <- match.arg(legend_position)
  facet_var_strip_position <- match.arg(facet_var_strip_position)
  palette <- match.arg(palette)
  palette_direction <- match.arg(palette_direction)
  palette_direction <- ifelse(palette_direction == "d2l", 1, -1)

  if(is.error(class(data[[y]]))) {
    y <- deparse(substitute(y))
  } else if(!is.character(y) || length(y) > 1){
    stop('If specified, `y` must be a single symbol or character string',
         '\n representing a column in the input data frame supplied to the `data` argument.')
  }
  if(!missing(x)) {
    if(is.error(class(data[[x]]))) {
      x <- deparse(substitute(x))
    } else if(!is.character(x) || length(x) > 1){
      stop('If specified, `x` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }
  if(!missing(fill_var)) {
    if(is.error(class(data[[fill_var]]))) {
      fill_var <- deparse(substitute(fill_var))
    }else if(!is.character(fill_var) || length(fill_var) > 1){
      stop('If specified, `fill_var` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }
  if(!missing(colour_var)) {
    if(is.error(class(data[[colour_var]]))) {
      colour_var <- deparse(substitute(colour_var))
    }else if(!is.character(colour_var) || length(colour_var) > 1){
      stop('If specified, `colour_var` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }
  if(!missing(facet_var)) {
    if(is.error(class(data[[facet_var]]))) {
      facet_var <- deparse(substitute(facet_var))
    } else if(!is.character(facet_var) || length(facet_var) > 1){
      stop('If specified, `facet_var` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }

  #x-variable recoding
  if(!missing(x)){
    data <- dplyr::mutate(data, {{x}} := as.character(.data[[x]]))
  }
  if(!missing(x_var_order)){
    data <- dplyr::mutate(data, {{x}} := forcats::fct_relevel(.data[[x]], levels = !!!x_var_order))
  }
  if(!missing(x_var_labs)){
    data <- dplyr::mutate(data, {{x}} := forcats::fct_recode(.data[[x]], !!!x_var_labs))
  }

  #fill variable recoding
  if(!missing(fill_var)){
    data <- dplyr::mutate(data, {{fill_var}} := as.character(.data[[fill_var]]))
  }
  if(!missing(fill_var) && !missing(fill_var_order)){
    data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_relevel(.data[[fill_var]], levels = !!!fill_var_order))
  }
  if(!missing(fill_var) && !missing(fill_var_labs)){
    data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_recode(.data[[fill_var]], !!!fill_var_labs))
  }

  #colour variable recoding
  if(!missing(colour_var)){
    data <- dplyr::mutate(data, {{colour_var}} := as.character(.data[[colour_var]]))
  }
  if(!missing(colour_var) && !missing(colour_var_order)){
    data <- dplyr::mutate(data, {{colour_var}} := forcats::fct_relevel(.data[[colour_var]], levels = !!!colour_var_order))
  }
  if(!missing(colour_var) && !missing(colour_var_labs)){
    data <- dplyr::mutate(data, {{colour_var}} := forcats::fct_recode(.data[[colour_var]], !!!colour_var_labs))
  }

  #facet label recoding
  if(!missing(facet_var)){
    data <- dplyr::mutate(data, {{facet_var}} := as.character(.data[[facet_var]]))
  }
  if(!missing(facet_var) && !missing(facet_var_order)){
    data <- dplyr::mutate(data, {{facet_var}} := forcats::fct_relevel(.data[[facet_var]], levels = !!!facet_var_order))
  }
  if(!missing(facet_var) && !missing(facet_var_labs)){
    data <- dplyr::mutate(data, {{facet_var}} := forcats::fct_recode(.data[[facet_var]], !!!facet_var_labs))
  }
  DT <- data.table::as.data.table(data)
  ST <- stat

  if(!is.numeric(DT[[y]])){
    stop("y must be a numeric vector or column of a data frame")
  }

  #grouping options
  if (!missing(x) && missing(fill_var) && missing(colour_var) && missing(facet_var)) {
    G <- x
  } else if (missing(x) && !missing(fill_var) && missing(colour_var) && missing(facet_var)) {
    G <- fill_var
  } else if (missing(x) && missing(fill_var) && !missing(colour_var) && missing(facet_var)) {
    G <- colour_var
  } else if (missing(x) && missing(fill_var) && missing(colour_var) && !missing(facet_var)) {
    G <- facet_var
  } else if (!missing(x) && !missing(fill_var) && missing(colour_var) && missing(facet_var)) {
    G <- c(x, fill_var)
  } else if (!missing(x) && missing(fill_var) && !missing(colour_var) && missing(facet_var)) {
    G <- c(x, colour_var)
  } else if (!missing(x) && missing(fill_var) && missing(colour_var) && !missing(facet_var)) {
    G <- c(x, facet_var)
  } else if (missing(x) && !missing(fill_var) && !missing(colour_var) && missing(facet_var)) {
    G <- c(fill_var, colour_var)
  } else if (missing(x) && !missing(fill_var) && missing(colour_var) && !missing(facet_var)) {
    G <- c(fill_var, facet_var)
  } else if (missing(x) && missing(fill_var) && !missing(colour_var) && !missing(facet_var)) {
    G <- c(colour_var, facet_var)
  } else if (!missing(x) && !missing(fill_var) && !missing(colour_var) && missing(facet_var)) {
    G <- c(fill_var, colour_var)
  } else if (!missing(x) && !missing(fill_var) && missing(colour_var) && !missing(facet_var)) {
    G <- c(x, fill_var, facet_var)
  } else if (!missing(x) && missing(fill_var) && !missing(colour_var) && !missing(facet_var)) {
    G <- c(x, colour_var, facet_var)
  } else if (missing(x) && !missing(fill_var) && !missing(colour_var) && !missing(facet_var)) {
    G <- c(fill_var, colour_var, facet_var)
  } else if (!missing(x) && !missing(fill_var) && !missing(colour_var) && !missing(facet_var)) {
    G <- c(x, colour_var, fill_var, facet_var)
  }

  #produce the descriptive stats & plot
  if(stat == "mean" && error == "se"){
    if(!missing(x) || !missing(colour_var) || !missing(fill_var) || !missing(facet_var)){
      desc <- DT[, .(cases = .N,
                     n = sum(!is.na(get(y))),
                     na = sum(is.na(get(y))),
                     p_na = round(sum(is.na(get(y)))/length(get(y)), 3),
                     mean = round(sum(get(y), na.rm = na.rm)/length(na.omit(get(y))), 3),
                     se = round(se(get(y), na.rm = na.rm), 3)),
                 by = eval(G)]
      desc <- tibble::as_tibble(desc)

    } else {
      desc <- DT[, .(cases = .N,
                     n = sum(!is.na(get(y))),
                     na = sum(is.na(get(y))),
                     p_na = round(sum(is.na(get(y)))/length(get(y)), 3),
                     mean = round(sum(get(y), na.rm = na.rm)/length(na.omit(get(y))), 3),
                     se = round(se(get(y), na.rm = na.rm), 3))]
      desc <- tibble::as_tibble(desc)
    }
    if (print_stats == TRUE){
      print(desc, n = Inf)
    }

    if(missing(x)){
      desc <- dplyr::mutate(desc, .x = 0)
      p <- ggplot2::ggplot(desc, ggplot2::aes_string(y = "mean", x = ".x", fill = fill_var, colour = colour_var))

    } else if(!missing(x)){
      p <- ggplot2::ggplot(desc, ggplot2::aes_string(y = "mean", x = x, fill = fill_var, colour = colour_var))
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
                                      linetype = eb_line_type,
                                      position = ggplot2::position_dodge(dodge_width))
      p
    } else if (!missing(eb_colour) && missing(colour_var)){
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se),
                                      size = eb_size, width = eb_width, alpha = eb_alpha,
                                      linetype = eb_line_type, colour = eb_colour,
                                      position = ggplot2::position_dodge(dodge_width))
      p
    } else if (!missing(eb_colour) && !missing(colour_var)){
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se, group = .data[[colour_var]]),
                                      size = eb_size, width = eb_width, alpha = eb_alpha,
                                      linetype = eb_line_type, colour = eb_colour,
                                      position = ggplot2::position_dodge(dodge_width))
      p
    }

    if(!missing(ylab)){
      p <- p + ggplot2::labs(y = ylab)
    } else if(missing(ylab)){
      p <- p + ggplot2::labs(y = paste0("mean ", y, " \u00B1 SE"))
    }

  } else if(stat == "mean" && error == "sd"){
    if(!missing(x) || !missing(colour_var) || !missing(fill_var) || !missing(facet_var)){
      desc <- DT[, .(cases = .N,
                     n = sum(!is.na(get(y))),
                     na = sum(is.na(get(y))),
                     p_na = round(sum(is.na(get(y)))/length(get(y)), 3),
                     mean = round(sum(get(y), na.rm = na.rm)/length(na.omit(get(y))), 3),
                     sd = round(stats::sd(get(y), na.rm = na.rm), 3)),
                 by = eval(G)]
      desc <- tibble::as_tibble(desc)

    } else {
      desc <- DT[, .(cases = .N,
                     n = sum(!is.na(get(y))),
                     na = sum(is.na(get(y))),
                     p_na = round(sum(is.na(get(y)))/length(get(y)), 3),
                     mean = round(sum(get(y), na.rm = na.rm)/length(na.omit(get(y))), 3),
                     sd = round(stats::sd(get(y), na.rm = na.rm), 3))]
      desc <- tibble::as_tibble(desc)
    }
    if (print_stats == TRUE){
      print(desc, n = Inf)
    }

    if(missing(x)){
      desc <- dplyr::mutate(desc, .x = 0)
      p <- ggplot2::ggplot(desc, ggplot2::aes_string(y = "mean", x = ".x", fill = fill_var, colour = colour_var))

    } else if(!missing(x)){
      p <- ggplot2::ggplot(desc, ggplot2::aes_string(y = "mean", x = x, fill = fill_var, colour = colour_var))
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
                                      linetype = eb_line_type,
                                      position = ggplot2::position_dodge(dodge_width))
      p
    } else if (!missing(eb_colour) && missing(colour_var)) {
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - sd, ymax = mean + sd),
                                      size = eb_size, width = eb_width, alpha = eb_alpha,
                                      linetype = eb_line_type, colour = eb_colour,
                                      position = ggplot2::position_dodge(dodge_width))
      p
    } else if (!missing(eb_colour) && !missing(colour_var)) {
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - sd, ymax = mean + sd, group = .data[[colour_var]]),
                                      size = eb_size, width = eb_width, alpha = eb_alpha,
                                      linetype = eb_line_type, colour = eb_colour,
                                      position = ggplot2::position_dodge(dodge_width))
      p
    }

    if(!missing(ylab)){
      p <- p + ggplot2::labs(y = ylab)
    } else if(missing(ylab)){
      p <- p + ggplot2::labs(y = paste0("mean ", y, " \u00B1 s"))
    }

  } else if(stat == "mean" && error == "var"){
    if(!missing(x) || !missing(colour_var) || !missing(fill_var) || !missing(facet_var)){
      desc <- DT[, .(cases = .N,
                     n = sum(!is.na(get(y))),
                     na = sum(is.na(get(y))),
                     p_na = round(sum(is.na(get(y)))/length(get(y)), 3),
                     mean = round(sum(get(y), na.rm = na.rm)/length(na.omit(get(y))), 3),
                     var = round(stats::var(get(y), na.rm = na.rm), 3)),
                 by = eval(G)]
      desc <- tibble::as_tibble(desc)

    } else {
      desc <- DT[, .(cases = .N,
                     n = sum(!is.na(get(y))),
                     na = sum(is.na(get(y))),
                     p_na = round(sum(is.na(get(y)))/length(get(y)), 3),
                     mean = round(sum(get(y), na.rm = na.rm)/length(na.omit(get(y))), 3),
                     var = round(stats::var(get(y), na.rm = na.rm), 3))]
      desc <- tibble::as_tibble(desc)
    }
    if (print_stats == TRUE){
      print(desc, n = Inf)
    }

    if(missing(x)){
      desc <- dplyr::mutate(desc, .x = 0)
      p <- ggplot2::ggplot(desc, ggplot2::aes_string(y = "mean", x = ".x", fill = fill_var, colour = colour_var))
    } else if(!missing(x)){
      p <- ggplot2::ggplot(desc, ggplot2::aes_string(y = "mean", x = x, fill = fill_var, colour = colour_var))
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
                                      linetype = eb_line_type,
                                      position = ggplot2::position_dodge(dodge_width))
      p
    } else if (!missing(eb_colour) && missing(colour_var)) {
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - var, ymax = mean + var),
                                      size = eb_size, width = eb_width, alpha = eb_alpha,
                                      linetype = eb_line_type, colour = eb_colour,
                                      position = ggplot2::position_dodge(dodge_width))
      p
    } else if (!missing(eb_colour) && !missing(colour_var)) {
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - var, ymax = mean + var, group = .data[[colour_var]]),
                                      size = eb_size, width = eb_width, alpha = eb_alpha,
                                      linetype = eb_line_type, colour = eb_colour,
                                      position = ggplot2::position_dodge(dodge_width))
      p
    }

    if(!missing(ylab)){
      p <- p + ggplot2::labs(y = ylab)
    } else if(missing(ylab)){
      p <- p + ggplot2::labs(y = paste0("mean ", y, " \u00B1 s\u00B2"))
    }

  } else if(stat == "mean" && error == "ci"){
    if(!missing(x) || !missing(colour_var) || !missing(fill_var) || !missing(facet_var)){
      desc <- DT[, .(cases = .N,
                     n = sum(!is.na(get(y))),
                     na = sum(is.na(get(y))),
                     p_na = round(sum(is.na(get(y)))/length(get(y)), 3),
                     lower = round(mean(get(y), na.rm = TRUE) - (abs(stats::qnorm((1-ci_level)/2))*se(get(y))), 3),
                     mean = round(sum(get(y), na.rm = na.rm)/length(na.omit(get(y))), 3),
                     upper = round(mean(get(y), na.rm = TRUE) + (abs(stats::qnorm((1-ci_level)/2))*se(get(y))), 3)),
                 by = eval(G)]
      desc <- tibble::as_tibble(desc)

    } else {
      desc <- DT[, .(cases = .N,
                     n = sum(!is.na(get(y))),
                     na = sum(is.na(get(y))),
                     p_na = round(sum(is.na(get(y)))/length(get(y)), 3),
                     lower = round(mean(get(y), na.rm = TRUE) - (abs(stats::qnorm((1-ci_level)/2))*se(get(y))), 3),
                     mean = round(sum(get(y), na.rm = na.rm)/length(na.omit(get(y))), 3),
                     upper = round(mean(get(y), na.rm = TRUE) + (abs(stats::qnorm((1-ci_level)/2))*se(get(y))), 3))]
      desc <- tibble::as_tibble(desc)
    }
    if (print_stats == TRUE){
      print(desc, n = Inf)
    }

    if(missing(x)){
      desc <- dplyr::mutate(desc, .x = 0)
      p <- ggplot2::ggplot(desc, ggplot2::aes_string(y = "mean", x = ".x", fill = fill_var, colour = colour_var))

    } else if(!missing(x)){
      p <- ggplot2::ggplot(desc, ggplot2::aes_string(y = "mean", x = x, fill = fill_var, colour = colour_var))
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
                                      linetype = eb_line_type)
      p
    } else if (!missing(eb_colour) && missing(colour_var)) {
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
                                      position = ggplot2::position_dodge(dodge_width),
                                      size = eb_size, width = eb_width, alpha = eb_alpha,
                                      linetype = eb_line_type, colour = eb_colour)
      p
    } else if (!missing(eb_colour) && !missing(colour_var)) {
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper, group = .data[[colour_var]]),
                                      position = ggplot2::position_dodge(dodge_width),
                                      size = eb_size, width = eb_width, alpha = eb_alpha,
                                      linetype = eb_line_type, colour = eb_colour)
      p
    }
    if(!missing(ylab)){
      p <- p + ggplot2::labs(y = ylab)
    } else if(missing(ylab)){
      p <- p + ggplot2::labs(y = paste0("mean ", y, " \u00B1 ", scales::percent(ci_level),  " CI"))
    }

  } else if(stat == "median" && error == "quartile"){
    if(!missing(x) || !missing(colour_var) || !missing(fill_var) || !missing(facet_var)){
      desc <- DT[, .(cases = .N,
                     n = sum(!is.na(get(y))),
                     na = sum(is.na(get(y))),
                     p_na = round(sum(is.na(get(y)))/length(get(y)), 3),
                     p25 = round(stats::quantile(get(y), probs = 0.25, na.rm = na.rm), 3),
                     p50 = round(stats::quantile(get(y), probs = 0.50, na.rm = na.rm), 3),
                     p75 = round(stats::quantile(get(y), probs = 0.75, na.rm = na.rm), 3)),
                 by = eval(G)]
      desc <- tibble::as_tibble(desc)

    } else {
      desc <- DT[, .(cases = .N,
                     n = sum(!is.na(get(y))),
                     na = sum(is.na(get(y))),
                     p_na = round(sum(is.na(get(y)))/length(get(y)), 3),
                     p25 = round(stats::quantile(get(y), probs = 0.25, na.rm = na.rm), 3),
                     p50 = round(stats::quantile(get(y), probs = 0.50, na.rm = na.rm), 3),
                     p75 = round(stats::quantile(get(y), probs = 0.75, na.rm = na.rm), 3))]
      desc <- tibble::as_tibble(desc)
    }
    if (print_stats == TRUE){
      print(desc, n = Inf)
    }
    if(missing(x)){
      desc <- dplyr::mutate(desc, .x = 0)
      p <- ggplot2::ggplot(desc, ggplot2::aes_string(y = "p50", x = ".x", fill = fill_var, colour = colour_var))
    } else if(!missing(x)){
      p <- ggplot2::ggplot(desc, ggplot2::aes_string(y = "p50", x = x, fill = fill_var, colour = colour_var))
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
                                      linetype = eb_line_type,
                                      position = ggplot2::position_dodge(dodge_width))
      p
    } else if (!missing(eb_colour) && missing(colour_var)) {
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = p25, ymax = p75),
                                      size = eb_size, width = eb_width, alpha = eb_alpha,
                                      linetype = eb_line_type, colour = eb_colour,
                                      position = ggplot2::position_dodge(dodge_width))
      p
    } else if (!missing(eb_colour) && !missing(colour_var)) {
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = p25, ymax = p75, group = .data[[colour_var]]),
                                      size = eb_size, width = eb_width, alpha = eb_alpha,
                                      linetype = eb_line_type, colour = eb_colour,
                                      position = ggplot2::position_dodge(dodge_width))
      p
    }
    if(!missing(ylab)){
      p <- p + ggplot2::labs(y = ylab)
    } else if(missing(ylab)){
      p <- p + ggplot2::labs(y = paste0("median ", y, " \u00B1 quartile"))
    }
  } else if(stat == "median" && error == "ci"){
    if(!missing(x) || !missing(fill_var) || !missing(colour_var) || !missing(facet_var)){
      desc1 <- DT[, .(cases = .N,
                      n = sum(!is.na(get(y))),
                      na = sum(is.na(get(y))),
                      p_na = round(sum(is.na(get(y)))/length(get(y)), 3)),
                  by = eval(G)]
      desc1 <- tibble::as_tibble(desc1)

      desc2 <- DT[,
                  .(measure = c("lower", "median", "upper"),
                    value = median_ci(get(y), replicates = replicates, ci_type = ci_type,
                                      ci_level = ci_level, parallel = parallel, cores = cores)),
                  by = eval(G)]
      desc2 <- stats::na.omit(desc2)
      desc2 <- data.table::dcast(desc2, formula = ... ~ measure, value.var = "value")
      desc2 <- tibble::as_tibble(desc2)
      suppressMessages(
        desc <- dplyr::left_join(desc1, desc2)
      )
    } else {
      desc1 <- DT[, .(cases = .N,
                      n = sum(!is.na(get(y))),
                      na = sum(is.na(get(y))),
                      p_na = round(sum(is.na(get(y)))/length(get(y)), 3))]
      desc1 <- tibble::as_tibble(desc1)
      desc2 <- DT[,
                  .(measure = c("lower", "median", "upper"),
                    value = median_ci(get(y), replicates = replicates, ci_type = ci_type,
                                      ci_level = ci_level, parallel = parallel, cores = cores))]
      desc2 <- stats::na.omit(desc2)
      desc2 <- data.table::dcast(desc2, formula = ... ~ measure, value.var = "value")
      desc2 <- tibble::as_tibble(desc2)[,-1]
      desc <- dplyr::bind_cols(desc1, desc2)
    }
    if (print_stats == TRUE){
      print(desc, n = Inf)
    }
    if(missing(x)){
      desc <- dplyr::mutate(desc, .x = 0)
      p <- ggplot2::ggplot(desc, ggplot2::aes_string(y = "median", x = ".x", fill = fill_var, colour = colour_var))

    } else if(!missing(x)){
      p <- ggplot2::ggplot(desc, ggplot2::aes_string(y = "median", x = x, fill = fill_var, colour = colour_var))
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
                                      linetype = eb_line_type,
                                      position = ggplot2::position_dodge(dodge_width))
      p
    } else if (!missing(eb_colour) && missing(colour_var)) {
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
                                      size = eb_size, width = eb_width, alpha = eb_alpha,
                                      linetype = eb_line_type, colour = eb_colour,
                                      position = ggplot2::position_dodge(dodge_width))
      p
    } else if (!missing(eb_colour) && !missing(colour_var)) {
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper, group = .data[[colour_var]]),
                                      size = eb_size, width = eb_width, alpha = eb_alpha,
                                      linetype = eb_line_type, colour = eb_colour,
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
      p <- p + ggplot2::labs(y = paste0("median ", y, " \u00B1 ",
                                        scales::percent(ci_level),  " ", ci_tp,
                                        " CI\n(", scales::comma(replicates)," replicates)"))
    }
  }

  #lines
  if(add_lines == T && missing(x)){
    errorCondition("a variable must be assigned to x to connect statistical estimates with lines")
  } else if (add_lines == T && !missing(x)) {
    if (missing(fill_var) && missing(colour_var)){
      if (!missing(line_colour)) {
        p <- p + ggplot2::geom_line(ggplot2::aes(group = 1), position = ggplot2::position_dodge(dodge_width),
                                    colour = line_colour, linetype = line_type, size = line_size,
                                    alpha = line_alpha)
      } else {
        p <- p + ggplot2::geom_line(ggplot2::aes(group = 1), position = ggplot2::position_dodge(dodge_width),
                                    linetype = line_type, size = line_size,
                                    alpha = line_alpha)
      }

    } else if (!missing(fill_var) && missing(colour_var)){
      if (!missing(line_colour)) {
        p <- p + ggplot2::geom_line(ggplot2::aes_string(group = fill_var), position = ggplot2::position_dodge(dodge_width),
                                    colour = line_colour, linetype = line_type, size = line_size,
                                    alpha = line_alpha)
      } else {
        p <- p + ggplot2::geom_line(ggplot2::aes_string(group = fill_var), position = ggplot2::position_dodge(dodge_width),
                                    linetype = line_type, size = line_size,
                                    alpha = line_alpha)
      }
    } else if (missing(fill_var) && !missing(colour_var)){
      if (!missing(line_colour)) {
        p <- p + ggplot2::geom_line(ggplot2::aes_string(group = colour_var), position = ggplot2::position_dodge(dodge_width),
                                    colour = line_colour, linetype = line_type, size = line_size,
                                    alpha = line_alpha)
      } else {
        p <- p + ggplot2::geom_line(ggplot2::aes_string(group = colour_var, colour = colour_var),
                                    position = ggplot2::position_dodge(dodge_width),
                                    linetype = line_type, size = line_size,
                                    alpha = line_alpha)
      }
    } else if (!missing(fill_var) && !missing(colour_var) && !missing(line_group)){
      if (!missing(line_colour)) {
        p <- p + ggplot2::geom_line(ggplot2::aes_string(group = line_group), position = ggplot2::position_dodge(dodge_width),
                                    colour = line_colour, linetype = line_type, size = line_size,
                                    alpha = line_alpha)
      } else {
        p <- p + ggplot2::geom_line(ggplot2::aes_string(group = line_group, colour = line_group),
                                    position = ggplot2::position_dodge(dodge_width),
                                    colour = line_colour, linetype = line_type, size = line_size,
                                    alpha = line_alpha)
      }
    } else if (!missing(fill_var) && !missing(colour_var) && missing(line_group)){
      errorCondition("When variables are assigned to both fill and colour, specify which to use for splitting lines using line_group")
    }
  }


  #modification of the colour or fill values
  if (!missing(fill_var) && missing(colour_var)){
    if(!missing(fill_var_values)){
      p <- p +
        ggplot2::scale_fill_manual(values = fill_var_values)
    } else {
      p <- p +
        ggplot2::scale_fill_viridis_d(begin = palette_begin, end = palette_end,
                                      option = palette, direction = palette_direction)
    }
  } else if(missing(fill_var) && !missing(colour_var)) {
    if(!missing(colour_var_values)){
      p <- p +
        ggplot2::scale_colour_manual(values = colour_var_values)
    } else {
      p <- p +
        ggplot2::scale_colour_viridis_d(begin = palette_begin, end = palette_end,
                                        option = palette, direction = palette_direction)
    }
  } else if(!missing(fill_var) && !missing(colour_var)) {
    if(!missing(fill_var_values) && missing(colour_var_values)){
      p <- p +
        ggplot2::scale_fill_manual(values = fill_var_values) +
        ggplot2::scale_colour_viridis_d(begin = palette_begin, end = palette_end,
                                        option = palette, direction = palette_direction)
    } else if(missing(fill_var_values) && !missing(colour_var_values)) {
      p <- p +
        ggplot2::scale_colour_manual(values = colour_var_values) +
        ggplot2::scale_fill_viridis_d(begin = palette_begin, end = palette_end,
                                      option = palette, direction = palette_direction)
    } else if(!missing(fill_var_values) && !missing(colour_var_values)) {
      p <- p +
        ggplot2::scale_fill_manual(values = fill_var_values) +
        ggplot2::scale_colour_manual(values = colour_var_values)
    } else {
      p <- p +
        ggplot2::scale_colour_viridis_d(begin = palette_begin, end = palette_end,
                                        option = palette, direction = palette_direction) +
        ggplot2::scale_fill_viridis_d(begin = palette_begin, end = palette_end,
                                      option = palette, direction = palette_direction)
    }
  }

  #convert to greyscale
  if(greyscale == TRUE){
    p <- p + ggplot2::scale_fill_grey()
  }

  #themes
  if(theme == "classic"){
    p <- p + ggplot2::theme_classic(base_size = text_size, base_family = font)
  } else if (theme == "bw"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "grey"){
    p <- p + ggplot2::theme_grey(base_size = text_size, base_family = font)
  } else if (theme == "light"){
    p <- p + ggplot2::theme_light(base_size = text_size, base_family = font)
  } else if (theme == "dark"){
    p <- p + ggplot2::theme_dark(base_size = text_size, base_family = font)
  } else if (theme == "minimal"){
    p <- p + ggplot2::theme_minimal(base_size = text_size, base_family = font)
  }

  #labels
  if(missing(x) && coord_flip == FALSE){
    p <- p + ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                            axis.text.x = ggplot2::element_blank(),
                            axis.ticks.x = ggplot2::element_blank())
  } else if (missing(x) && coord_flip == TRUE) {
    p <- p + ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                            axis.text.y = ggplot2::element_blank(),
                            axis.ticks.y = ggplot2::element_blank())
  }
  y_name <- names(dplyr::select(data, {{y}}))
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
    p <- p + ggplot2::labs(title = title)
  }

  #modification of y-axis limits & transformations
  if(!missing(ylim) && coord_flip == FALSE) {
    p <- p + ggplot2::coord_cartesian(ylim = c(ylim[1], ylim[2]))
  } else if (missing(ylim) && coord_flip == TRUE){
    p <- p + ggplot2::coord_flip()
  } else if (!missing(ylim) && coord_flip == TRUE){
    p <- p + ggplot2::coord_flip(ylim = c(ylim[1], ylim[2]))
  }
  if(class(y_var_labs) != "waiver") {
    p <- p + ggplot2::scale_y_continuous(labels = y_var_labs)
  } else if(transform_y == FALSE && class(ybreaks) != "waiver"){
    p <- p + ggplot2::scale_y_continuous(breaks = ybreaks, labels = y_var_labs)
  } else if (transform_y == TRUE){
    p <- p + ggplot2::scale_y_continuous(trans = y_transformation, breaks = ybreaks, labels = y_var_labs)
  }

  #misc
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
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[facet_var]]), strip.position = facet_var_strip_position)
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

# plot_pie ----------------------------------------------------------------
#' @title
#'
#' Generate a pie chart.
#'
#' @description Easily generate pie charts, AKA bar charts with polar
#'   coordinates, using ggplot2 with a simplified customization interface for
#'   common modifications. Pie charts are rarely the most effective way of
#'   visualizing data (especially when >5 groups are being compared), but that
#'   doesn't mean there shouldn't be an easy way to build one with ggplot2 in
#'   case your project stakeholders ask. See
#'   \href{https://craig.rbind.io/post/2021-05-17-asgr-3-1-data-visualization/}{this
#'    blog post} for an introduction to ggplot2.
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
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 coord_polar
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_fill_grey
#' @importFrom ggplot2 labs
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
#' @param fill_var A categorical variable to assign to the slice fill colour
#'   (quoted or unquoted), e.g. fill_var = "grouping_variable" or fill_var =
#'   grouping_variable. Produces separate slices each level of the fill
#'   variable. See \code{\link[ggplot2]{aes}} for details.
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
#' @param palette If a variable is assigned to fill_var, this determines which
#'   \href{https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html}{viridis
#'    colour palette} to use. Options include "plasma" or "C" (default), "magma"
#'   or "A", "inferno" or "B", "viridis" or "D", and "cividis" or "E". See
#'   \href{https://craig.rbind.io/post/2021-05-17-asgr-3-1-data-visualization/#colourblind-friendly-palettes}{this
#'    link} for examples. You can override these colour palettes with
#'   fill_var_values.
#'
#' @param palette_direction Choose "d2l" for dark to light (default) or "l2d"
#'   for light to dark.
#'
#' @param palette_begin Value between 0 and 1 that determines where along the
#'   full range of the chosen colour palette's spectrum to begin sampling
#'   colours. See \code{\link[ggplot2]{scale_fill_viridis_d}} for details.
#'
#' @param palette_end Value between 0 and 1 that determines where along the full
#'   range of the chosen colour palette's spectrum to end sampling colours. See
#'   \code{\link[ggplot2]{scale_fill_viridis_d}} for details.
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
#' @param line_size Controls the slice outline thickness if a colour is
#'   specified (e.g. colour = "black").
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
#'   "grouping_variable" or facet_var = grouping_variable. See
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
                     palette = c("plasma", "C", "magma", "A", "inferno", "B", "viridis", "D", "cividis", "E"), #viridis colour palettes
                     palette_direction = c("d2l", "l2d"), palette_begin = 0, palette_end = 1, #viridis colour palette options
                     fill_var_title = NULL,
                     slice_text = NULL,
                     slice_text_prefix = "",
                     slice_text_suffix = "",
                     slice_text_colour = "black",
                     slice_text_size = 4,
                     slice_text_custom = NULL,
                     line_size = 1,
                     round_n = NULL,
                     lump_n = NULL,
                     lump_lab = NULL,
                     facet_var = NULL, facet_var_order = NULL, facet_var_labs = NULL,
                     facet_var_strip_position = c("top", "bottom"), facet_var_text_bold = TRUE,
                     greyscale = FALSE, #control transparency, convert to greyscale
                     text_size = 14, font = c("sans", "serif", "mono"), #theme options
                     legend_position = c("right", "left", "bottom", "top"), omit_legend = FALSE, #legend position
                     aesthetic_options = FALSE) {#output format

  facet_var_strip_position <- match.arg(facet_var_strip_position)
  legend_position <- match.arg(legend_position)
  font <- match.arg(font)
  palette <- match.arg(palette)
  palette_direction <- match.arg(palette_direction)
  palette_direction <- ifelse(palette_direction == "d2l", 1, -1)

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
  if(!missing(fill_var)) {
    if(is.error(class(data[[fill_var]]))) {
      fill_var <- deparse(substitute(fill_var))
    }else if(!is.character(fill_var) || length(fill_var) > 1){
      stop('If specified, `fill_var` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }
  if(!missing(facet_var)) {
    if(is.error(class(data[[facet_var]]))) {
      facet_var <- deparse(substitute(facet_var))
    } else if(!is.character(facet_var) || length(facet_var) > 1){
      stop('If specified, `facet_var` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }

  #fill variable recoding
  if(!missing(fill_var)){
    data <- dplyr::mutate(data, {{fill_var}} := as.character(.data[[fill_var]]))
  } else {
    stop("A categorical variable in the data frame source must be assigned to fill_var!")
  }

  if(!missing(lump_n)) {
    if(!missing(y)) {
      if(!missing(lump_lab)) {
        data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_lump_n(.data[[fill_var]], w = .data[[y]], n = lump_n, other_level = lump_lab))
      } else {
        data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_lump_n(.data[[fill_var]], w = .data[[y]], n = lump_n, other_level = "other"))
      }
    } else {
      if(!missing(lump_lab)) {
        data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_lump_n(.data[[fill_var]], n = lump_n, other_level = lump_lab))
      } else {
        data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_lump_n(.data[[fill_var]], n = lump_n, other_level = "other"))
      }
    }
  }

  if(data.table::uniqueN(data[[fill_var]]) > 5) {
    warning('Pie charts tend to be difficult to read with more than 5 slices.\n  Consider lumping infrequent categories together with "lump_n" or using plot_bar() to visualize these data.')
  }
  if(!missing(fill_var_order_by_y)) {
    if(!missing(y)) {
      if(fill_var_order_by_y == "d") {
        data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_reorder(.data[[fill_var]], w = .data[[y]], .desc = TRUE))
      } else {
        data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_reorder(.data[[fill_var]], w = .data[[y]], .desc = FALSE))
      }
    } else {
      if(fill_var_order_by_y == "d") {
        data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_infreq(.data[[fill_var]]))
      } else {
        data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_rev(forcats::fct_infreq(.data[[fill_var]])))
      }
    }
  }

  if(!missing(fill_var_order)){
    data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_relevel(.data[[fill_var]], levels = !!!fill_var_order))
  }
  if(!missing(fill_var_labs)){
    data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_recode(.data[[fill_var]], !!!fill_var_labs))
  }

  #facet label recoding
  if(!missing(facet_var)){
    data <- dplyr::mutate(data, {{facet_var}} := as.character(.data[[facet_var]]))
  }
  if(!missing(facet_var) && !missing(facet_var_order)){
    data <- dplyr::mutate(data, {{facet_var}} := forcats::fct_relevel(.data[[facet_var]], levels = !!!facet_var_order))
  }
  if(!missing(facet_var) && !missing(facet_var_labs)){
    data <- dplyr::mutate(data, {{facet_var}} := forcats::fct_recode(.data[[facet_var]], !!!facet_var_labs))
  }

  #produce the descriptive stats & plot
  if(missing(y)) {
    if(missing(facet_var)) {
      plot_data <- dplyr::group_by(data, .data[[fill_var]])
      plot_data <- dplyr::count(plot_data, name = "n_")
      plot_data <- dplyr::ungroup(plot_data)
      plot_data <- dplyr::arrange(plot_data, rev(.data[[fill_var]]))
      plot_data <- dplyr::mutate(plot_data, p_ = n_/sum(n_), st_coords = cumsum(p_) - 0.5*p_)
    } else {
      plot_data <- dplyr::group_by(data, .data[[fill_var]], .data[[facet_var]])
      plot_data <- dplyr::count(plot_data, name = "n_")
      plot_data <- dplyr::ungroup(plot_data)
      plot_data <- dplyr::arrange(plot_data, rev(.data[[fill_var]]))
      plot_data <- dplyr::group_by(plot_data, .data[[facet_var]])
      plot_data <- dplyr::mutate(plot_data, p_ = n_/sum(n_), st_coords = cumsum(p_) - 0.5*p_)
      plot_data <- dplyr::ungroup(plot_data)
    }
  } else {
    if(missing(facet_var)) {
      plot_data <- dplyr::group_by(data, .data[[fill_var]])
      plot_data <- dplyr::summarise(plot_data, ytot = sum(.data[[y]], na.rm = TRUE), .groups = "drop")
      plot_data <- dplyr::ungroup(plot_data)
      plot_data <- dplyr::arrange(plot_data, rev(.data[[fill_var]]))
      plot_data <- dplyr::mutate(plot_data, p_ = ytot/sum(ytot), st_coords = cumsum(p_) - 0.5*p_)
    } else {
      plot_data <- dplyr::group_by(data, .data[[fill_var]], .data[[facet_var]])
      plot_data <- dplyr::summarise(plot_data, ytot = sum(.data[[y]], na.rm = TRUE), .groups = "drop")
      plot_data <- dplyr::arrange(plot_data, rev(.data[[fill_var]]))
      plot_data <- dplyr::group_by(plot_data, .data[[facet_var]])
      plot_data <- dplyr::mutate(plot_data, p_ = ytot/sum(ytot), st_coords = cumsum(p_) - 0.5*p_)
      plot_data <- dplyr::ungroup(plot_data)
    }
  }
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(y = p_, x = "", fill = .data[[fill_var]])) +
    ggplot2::geom_bar(stat = "identity", position = "stack", width = 1, size = line_size, ...) +
    ggplot2::coord_polar("y", start= 0)

  if(!missing(slice_text) && !missing(slice_text_custom)) {
    stop('only one of "slice_text" or "slice_text_custom" should be specified')
  } else if(!missing(slice_text) && missing(slice_text_custom)) {
    if(slice_text == "pct") {
      if(!missing(round_n)) {
        p <- p + ggplot2::geom_text(ggplot2::aes(x=1, y = st_coords,
                                                 label=paste(slice_text_prefix, round(p_*100, round_n), slice_text_suffix)),
                                    colour = slice_text_colour, size = slice_text_size)
      } else {
        p <- p + ggplot2::geom_text(ggplot2::aes(x=1, y = st_coords,
                                                 label=paste(slice_text_prefix, p_*100, slice_text_suffix)),
                                    colour = slice_text_colour, size = slice_text_size)
      }
    } else if (slice_text == "tot") {
      if(missing(y)) {
        if(!missing(round_n)) {
          p <- p + ggplot2::geom_text(ggplot2::aes(x=1, y = st_coords,
                                                   label=paste(slice_text_prefix, round(n_, round_n), slice_text_suffix)),
                                      colour = slice_text_colour, size = slice_text_size)
        } else {
          p <- p + ggplot2::geom_text(ggplot2::aes(x=1, y = st_coords,
                                                   label=paste(slice_text_prefix, n_), slice_text_suffix),
                                      colour = slice_text_colour, size = slice_text_size)
        }
      } else {
        if(!missing(round_n)) {
          p <- p + ggplot2::geom_text(ggplot2::aes(x=1, y = st_coords,
                                                   label=paste(slice_text_prefix, round(ytot, round_n), slice_text_suffix)),
                                      colour = slice_text_colour, size = slice_text_size)
        } else {
          p <- p + ggplot2::geom_text(ggplot2::aes(x=1, y = st_coords,
                                                   label=paste(slice_text_prefix, ytot, slice_text_suffix)),
                                      colour = slice_text_colour, size = slice_text_size)
        }
      }
    } else if (slice_text == "grp") {
      p <- p + ggplot2::geom_text(ggplot2::aes(x=1, y = st_coords, label=unique(.data[[fill_var]])),
                                  colour = slice_text_colour, size = slice_text_size)
    }
  } else if(missing(slice_text) && !missing(slice_text_custom)) {
    if(length(slice_text_custom) != data.table::uniqueN(data[[fill_var]])) {
      stop('"slice_text_custom" must be a character vector with as many values as there are levels of "fill_var"')
    }
    p <- p + ggplot2::geom_text(ggplot2::aes(x=1, y = st_coords, label=slice_text_custom),
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
    p <- p + ggplot2::labs(title = title)
    p <- p + ggplot2::theme(plot.title = ggplot2::element_text(hjust = title_alignment))
  }
  if(!missing(fill_var_title)){
    p <- p + ggplot2::labs(fill = fill_var_title)
  }

  #customize fill colours
  if (!missing(fill_var) && !missing(fill_var_values)){
    p <- p +
      ggplot2::scale_fill_manual(values = fill_var_values)
  } else if (!missing(fill_var) && missing(fill_var_values)) {
    p <- p +
      ggplot2::scale_fill_viridis_d(begin = palette_begin, end = palette_end,
                                    option = palette, direction = palette_direction)
  }

  #facets
  if(!missing(facet_var)){
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[facet_var]]), strip.position = facet_var_strip_position)
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

# plot_raincloud ----------------------------------------------------------
#' @title
#'
#' Generate a rain cloud plot.
#'
#' @description Easily generate hybrid half-violin/half-scatter plots AKA "rain
#'   cloud plots", with or without overlaid box plots, using ggplot2 and
#'   gghalves. Like other plot_* functions, plot_raincloud() provides a
#'   simplified argument-based customization interface for common modifications
#'   and yields plots that can be further modified with ggplot2 syntax. See
#'   \href{https://craig.rbind.io/post/2021-05-17-asgr-3-1-data-visualization/}{this
#'    blog post} for an introduction to ggplot2.
#'
#' @importFrom dplyr mutate
#' @importFrom forcats fct_relevel
#' @importFrom forcats fct_recode
#' @importFrom rlang !!!
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 scale_fill_grey
#' @importFrom ggplot2 scale_fill_viridis_d
#' @importFrom ggplot2 lims
#' @importFrom ggplot2 labs
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
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 coord_cartesian
#' @importFrom ggplot2 waiver
#' @importFrom gghalves geom_half_violin
#' @importFrom gghalves geom_half_point
#' @importFrom gghalves geom_half_boxplot
#' @importFrom ggplot2 remove_missing
#' @importFrom plotly ggplotly
#' @importFrom utils browseURL
#'
#' @param data A data frame or tibble containing the dependent measure "y" and
#'   any grouping variables.
#'
#' @param y A numeric variable you want to obtain rain cloud plots for (quoted
#'   or unquoted), e.g. y = "variable" or y = variable.
#'
#' @param x A categorical variable you want to obtain separate rain cloud plots
#'   of y for (optional; quoted or unquoted), e.g. x = "variable" or x =
#'   variable.
#'
#' @param fill_var Use if you want to assign a variable to the fill colour of
#'   the half-violin and scattered points on the plot, e.g. fill_var =
#'   "grouping_var" or fill_var = grouping_variable. Produces separate rain
#'   cloud plots for each level of the fill variable. See
#'   \code{\link[ggplot2]{aes}} for details. N.B. If you intend to add box plots
#'   (via box_plot = TRUE), the same variable should be assigned to both the
#'   x-axis and fill_var, otherwise the box plots will not show up in the
#'   correct locations.
#'
#' @param violin_colour Outline colour to use for the half-violin plot segment
#'   of the rain cloud plot. Default is "black". You can use
#'   \code{\link{colour_options}} to see many of the available options.
#'
#' @param violin_fill Fill colour to use for the half-violin plot segment of the
#'   rain cloud plot. You can use \code{\link{colour_options}} to see many of
#'   the available options. To assign different colours to groups of a variable
#'   mapped to x or fill_var, use fill_var, palette, and/or fill_var_values
#'   instead.
#'
#' @param violin_side Set to "r" (default) if you want the half-violin plot
#'   segment to be draw on the right side of the rain cloud plot midline (or "l"
#'   for left).
#'
#' @param violin_line_size Adjusts the thickness of the half-violin plot outline.
#'
#' @param violin_alpha This adjusts the transparency/opacity of the half-violin
#'   plot component of the rain cloud plot, with valid values ranging from 0 =
#'   100% transparent to 1 = 100% opaque.
#'
#' @param violin_trim Set this to TRUE if you want to trim the tails of the
#'   half-violin plot component of the rain cloud plot.
#'
#' @param violin_quantiles Accepts a vector of quantile values to draw as lines
#'   on the half-violin plot. For example, to draw a line at the median of y,
#'   you would set this to 0.5.
#'
#' @param point_colour Outline colour to use for the scatter plot segment
#'   of the rain cloud plot. Default is "black". You can use
#'   \code{\link{colour_options}} to see many of the available options.
#'
#' @param point_fill Fill colour to use for the scatter plot segment of the
#'   rain cloud plot. You can use \code{\link{colour_options}} to see many of
#'   the available options. To assign different colours to groups of a variable
#'   mapped to x or fill_var, use fill_var, palette, and/or fill_var_values
#'   instead.
#'
#' @param point_side Set to "l" (default) if you want the box plot to be draw on
#'   the left side of the rain cloud plot midline (or "r" for right).
#'
#' @param point_shape Shape to use for the scatter plot points. Options include
#'   \code{\link[ggplot2]{geom_point}} shapes that have both colour and fill
#'   aesthetics: "circle", "square", "diamond", "triangle up", and "triangle
#'   down".
#'
#' @param point_size Adjusts the size of the points in the scatter plot portion of
#'   the rain cloud plot.
#'
#' @param point_alpha This adjusts the transparency/opacity of the scattered
#'   points, with valid values ranging from 0 = 100% transparent to 1 = 100%
#'   opaque.
#'
#' @param point_line_size Adjusts the scatter plot point outline thickness.
#'
#' @param point_position This typically does not need to be modified in a rain
#'   cloud plot. See the "position adjustment" section of the
#'   \href{https://ggplot2.tidyverse.org/reference/}{ggplot2 reference page} for
#'   options and detailed information.
#'
#' @param box_plot Set this to TRUE to add a box plot of y to the rain cloud
#'   plot.
#'
#' @param box_half Set to TRUE if you only want half of a box plot added
#'   (ignored if box_plot = FALSE).
#'
#' @param box_colour Outline colour to use for the box plots (ignored if
#'   box_plot = FALSE). Default is "black". You can use
#'   \code{\link{colour_options}} to see many of the available options. To
#'   assign different colours to groups of a variable mapped to x or fill_var,
#'   use fill_var, palette, and/or fill_var_values instead.
#'
#' @param box_fill Fill colour to use for the box plots (ignored if box_plot =
#'   FALSE). You can use \code{\link{colour_options}} to see many of the
#'   available options. To assign different colours to groups of a variable
#'   mapped to x or fill_var, use fill_var, palette, and/or fill_var_values
#'   instead.
#'
#' @param box_side Set to "r" (default) if you want the box plot to be draw on
#'   the right side of the rain cloud plot midline (or "l" for left). Ignored if
#'   box_plot = FALSE.
#'
#' @param box_alpha This adjusts the transparency/opacity of the scattered
#'   points, with valid values ranging from 0 = 100% transparent to 1 = 100%
#'   opaque (ignored if box_plot = FALSE).
#'
#' @param box_line_size Adjusts the thickness of box plot lines (ignored if box_plot = FALSE).
#'
#' @param box_line_type Used to specify the type of line to use for box plots
#'   (ignored if box_plot = FALSe). Options include: "solid", "dashed",
#'   "dotted", "dotdash", "longdash", and "twodash".
#'
#' @param box_whisker_coef The length of box plot whiskers as a multiple of the
#'   interquartile range (marked by the box length). Default is the standard
#'   1.5. Ignored if box_plot = FALSE.
#'
#' @param box_width Controls the box width (ignored if box_plot = FALSE).
#'
#' @param box_error_bars Set to TRUE if you want to add error bar lines to the
#'   ends of the box plot whiskers (ignored if box_plot = FALSE).
#'
#' @param box_nudge Controls the distance between the box plot and mid line of
#'   the overall rain cloud plot, where 0 is touching the midline and higher
#'   values displace the box plot futher in the direction specified with the
#'   box_side argument. Ignored if box_plot = FALSE.
#'
#' @param box_outlier_colour Controls the colour of the boxplot outlier
#'   indicator points. If box_outlier_shape is set to a value between 21 and 25
#'   then it controls the outline colour instead of the overall colour. Ignored
#'   if box_plot = FALSE.
#'
#' @param box_outlier_fill If box_outlier_shape is set to a value between 21 and
#'   25, this controls the fill colour of the box plot outlier indicator points.
#'   Ignored if box_plot = FALSE or box_outlier_shape is a value outside of
#'   21-25 since these other shapes do not have a fill aesthetic.
#'
#' @param box_outlier_size Controls the size of the box plot outlier indicator
#'   points. Ignored if box_plot = FALSE.
#'
#' @param box_outlier_shape Controls the shape of the box plot outlier indicator
#'   points. Ignored if box_plot = FALSE. To see examples of the available options
#'   in a web browser, set the aesthetic_options argument to TRUE.
#'
#' @param box_outlier_alpha This adjusts the transparency/opacity of the box
#'   plot outlier indicator points, with valid values ranging from 0 = 100%
#'   transparent to 1 = 100% opaque. Ignored if box_plot = FALSE.
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
#' @param ylim specify the y-axis limits, e.g. ylim = c(lower_limit,
#'   upper_limit). Use NA for the existing minimum or maximum value of y, e.g.
#'   the default is ylim = c(NA, NA)
#'
#' @param ybreaks This allows you to change the break points to use for tick
#'   marks on the y-axis. \code{\link{seq}} is particularly useful here. See
#'   \code{\link[ggplot2]{scale_y_continuous}} for details. If ybreaks is
#'   specified, then ylim should be also.
#'
#' @param transform_y Would you like to transform the y axis? (TRUE or FALSE)
#'
#' @param y_transformation If transform_y = TRUE, this determines the
#'   transformation to be applied. Common choices include "log10" (the default),
#'   "log2", "sqrt", or "exp". See \code{\link[ggplot2]{scale_continuous}} for
#'   details.
#'
#' @param y_var_labs Allows you to modify the labels displayed with the y-axis
#'   tick marks. See \code{\link[ggplot2]{scale_continuous}} for details.
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
#' @param fill_var_order If a variable has been assigned to fill using fill_var,
#'   this allows you to modify the order of the variable groups, e.g. fill_var =
#'   grouping_variable, fill_var_order = c("group_2", "group_1"). See
#'   \code{\link[forcats]{fct_relevel}} for details.
#'
#' @param fill_var_labs If a variable has been assigned to fill using fill_var,
#'   this allows you to modify the labels of the variable groups, e.g. fill_var
#'   = grouping_variable, fill_var_labs = c("group_1_new_label" =
#'   "group_1_old_label", "group_2_new_label" = "group_2_old_label"). See
#'   \code{\link[forcats]{fct_recode}} for details.
#'
#' @param fill_var_values If a variable has been assigned to fill using
#'   fill_var, this allows you to modify the colours assigned to the fill of
#'   each of the variable groups, e.g. fill_var = grouping_variable,
#'   fill_var_values = c("blue", "red"). See
#'   \code{\link[ggplot2]{scale_fill_manual}} for details. For the colour
#'   options available in base R, see \code{\link[elucidate]{colour_options}}.
#'
#' @param palette If a variable is assigned to fill_var, this determines which
#'   \href{https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html}{viridis
#'    colour palette} to use. Options include "plasma" or "C" (default), "magma"
#'   or "A", "inferno" or "B", "viridis" or "D", and "cividis" or "E". See
#'   \href{https://craig.rbind.io/post/2021-05-17-asgr-3-1-data-visualization/#colourblind-friendly-palettes}{this
#'    link} for examples. You can override these colour palettes with
#'   fill_var_values.
#'
#' @param palette_direction Choose "d2l" for dark to light (default) or "l2d"
#'   for light to dark.
#'
#' @param palette_begin Value between 0 and 1 that determines where along the
#'   full range of the chosen colour palette's spectrum to begin sampling
#'   colours. See \code{\link[ggplot2]{scale_fill_viridis_d}} for details.
#'
#' @param palette_end Value between 0 and 1 that determines where along the full
#'   range of the chosen colour palette's spectrum to end sampling colours. See
#'   \code{\link[ggplot2]{scale_fill_viridis_d}} for details.
#'
#' @param coord_flip Flips the x and y axes, which makes this type of plot look
#'   like a rain cloud (half-violin plot "cloud" on top of scattered point "rain
#'   drops"). See \code{\link[ggplot2]{coord_flip}} for details.
#'
#' @param greyscale Set to TRUE if you want the plot converted to greyscale.
#'   Note that this will override colours you may have specified with other
#'   arguments.
#'
#' @param theme Adjusts the theme using 1 of 6 predefined "complete" theme
#'   templates provided by ggplot2. Currently supported options are: "classic"
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
#'   variable (i.e. a facetted plot), e.g. facet_var = "grouping_variable" or
#'   facet_var = grouping_variable. See \code{\link[ggplot2]{facet_wrap}} for
#'   details.
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
#' @param aesthetic_options If set to TRUE, opens a web browser to the tidyverse
#'   online aesthetic options vignette.
#'
#' @return A ggplot rain cloud plot.
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gov.bc.ca}
#'
#' @examples
#'
#' data(mtcars) #load the mtcars data
#'
#' library(magrittr)
#'
#' #basic raincloud plot
#'
#' plot_raincloud(mtcars, y = mpg)
#'
#' \donttest{
#' #set coord_flip = TRUE to flip the x and y axes so it looks like a rain cloud
#' #add a box plot with box_plot = TRUE
#' #add fill colours with violin_fill and point_fill
#'
#' mtcars %>%
#'   plot_raincloud(mpg,
#'                  coord_flip = TRUE, box_plot = TRUE,
#'                  violin_fill = "steelblue", point_fill = "blue2")
#'
#' #split the plot by a grouping variable with the x argument
#' #assign a variable to fill colour with the fill_var argument
#' #it is recommended to assign the same variable to x and fill_colour or box
#' #plots will not show up correctly
#'
#' mtcars %>%
#'   plot_raincloud(mpg, x = cyl, fill_var = cyl,
#'                  coord_flip = TRUE, box_plot = TRUE)
#' }
#'
#' @references
#' Wickham, H. (2016). ggplot2: elegant graphics for data analysis. New York, N.Y.: Springer-Verlag.
#' Allen, M., Poggiali, D., Whitaker, K., Marshall, T. R., & Kievit, R. A. (2019). Raincloud plots: a multi-platform tool for robust data visualization. Wellcome open research, 4.
#'
#' @seealso \code{\link{plot_violin}}, \code{\link{plot_scatter}}, \code{\link{plot_box}},
#' \code{\link[gghalves]{geom_half_violin}}, \code{\link[gghalves]{geom_half_point}}, \code{\link[gghalves]{geom_half_boxplot}}
#'
#' @export
plot_raincloud <- function(data, y,#essential parameters
                           x = NULL,
                           fill_var = NULL, #grouping variable aesthetic mapping
                           #half-violin plot parameters
                           violin_colour = "black", violin_fill = NULL, violin_side = c("r", "l"),
                           violin_line_size = 1,
                           violin_alpha = 0.75, violin_trim = FALSE, violin_quantiles = NULL,
                           #half-point plot parameters
                           point_colour = "black", point_fill = NULL, point_side = c("l", "r"),
                           point_shape = c("circle", "square", "diamond", "triangle up", "triangle down"),
                           point_size = 2, point_alpha = 0.5, point_line_size = 1, point_position = "dodge2",
                           #half-boxplot parameters
                           box_plot = FALSE, box_half = FALSE,
                           box_colour = "black", box_fill = NULL, box_side = c("r", "l"), box_alpha = 0,
                           box_line_size = 1, box_line_type = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                           box_whisker_coef = 1.5, box_width = 0.1, box_error_bars = FALSE, box_nudge = 0.015,
                           box_outlier_colour = "red3", box_outlier_fill = NULL, box_outlier_size = 2,
                           box_outlier_shape = 18, box_outlier_alpha = 0.8,
                           #general parameters
                           xlab = NULL, ylab = NULL, title = NULL,
                           fill_var_title = NULL,
                           ylim = c(NA, NA), ybreaks = ggplot2::waiver(), #control the y axis limits and scaling
                           transform_y = FALSE, y_transformation = "log10", y_var_labs = ggplot2::waiver(),
                           x_var_order = NULL, x_var_labs = NULL,
                           fill_var_order = NULL,  #modify grouping variable level order
                           fill_var_labs = NULL, #modify grouping variable labels
                           fill_var_values = NULL, #manual colour specification
                           palette = c("plasma", "C", "magma", "A", "inferno", "B", "viridis", "D", "cividis", "E"), #viridis colour palettes
                           palette_direction = c("d2l", "l2d"), palette_begin = 0, palette_end = 1, #viridis colour palette options
                           coord_flip = FALSE, #flip the x- and y- axes so that the half-violin plots look like rain clouds
                           greyscale = FALSE, #control transparency, convert to greyscale
                           theme = c("classic", "bw", "grey", "light", "dark", "minimal"),
                           text_size = 14, font = c("sans", "serif", "mono"), #theme options
                           facet_var = NULL, facet_var_order = NULL, facet_var_labs = NULL, #facet options
                           facet_var_strip_position = c("top", "bottom"), facet_var_text_bold = TRUE, #facet aesthetic customization
                           legend_position = c("right", "left", "top", "bottom"), omit_legend = FALSE, #legend position
                           aesthetic_options = FALSE) {

  theme <- match.arg(theme)
  font <- match.arg(font)
  legend_position <- match.arg(legend_position)
  facet_var_strip_position <- match.arg(facet_var_strip_position)
  box_line_type <- match.arg(box_line_type)
  violin_side <- match.arg(violin_side)
  box_side <- match.arg(box_side)
  point_side <- match.arg(point_side)
  point_shape <- match.arg(point_shape)
  palette <- match.arg(palette)
  palette_direction <- match.arg(palette_direction)
  palette_direction <- ifelse(palette_direction == "d2l", 1, -1)

  point_shape <- switch(point_shape,
                        "circle" = 21,
                        "square" = 22,
                        "diamond" = 23,
                        "triangle up" = 24,
                        "triangle down" = 25)


  if(is.error(class(data[[y]]))) {
    y <- deparse(substitute(y))
  } else if(!is.character(y) || length(y) > 1){
    stop('If specified, `y` must be a single symbol or character string',
         '\n representing a column in the input data frame supplied to the `data` argument.')
  }
  if(!missing(x)) {
    if(is.error(class(data[[x]]))) {
      x <- deparse(substitute(x))
    } else if(!is.character(x) || length(x) > 1){
      stop('If specified, `x` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }
  if(!missing(fill_var)) {
    if(is.error(class(data[[fill_var]]))) {
      fill_var <- deparse(substitute(fill_var))
    }else if(!is.character(fill_var) || length(fill_var) > 1){
      stop('If specified, `fill_var` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }
  if(!missing(facet_var)) {
    if(is.error(class(data[[facet_var]]))) {
      facet_var <- deparse(substitute(facet_var))
    } else if(!is.character(facet_var) || length(facet_var) > 1){
      stop('If specified, `facet_var` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }

  #x-variable recoding
  if(!missing(x)){
    data <- dplyr::mutate(data, {{x}} := as.character(.data[[x]]))
  }
  if(!missing(x_var_order)){
    data <- dplyr::mutate(data, {{x}} := forcats::fct_relevel(.data[[x]], levels = !!!x_var_order))
  }
  if(!missing(x_var_labs)){
    data <- dplyr::mutate(data, {{x}} := forcats::fct_recode(.data[[x]], !!!x_var_labs))
  }

  #fill variable recoding
  if(!missing(fill_var)){
    data <- dplyr::mutate(data, {{fill_var}} := as.character(.data[[fill_var]]))
  }
  if(!missing(fill_var) && !missing(fill_var_order)){
    data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_relevel(.data[[fill_var]], levels = !!!fill_var_order))
  }
  if(!missing(fill_var) && !missing(fill_var_labs)){
    data <- dplyr::mutate(data, {{fill_var}} := forcats::fct_recode(.data[[fill_var]], !!!fill_var_labs))
  }

  #facet label recoding
  if(!missing(facet_var)){
    data <- dplyr::mutate(data, {{facet_var}} := as.character(.data[[facet_var]]))
  }
  if(!missing(facet_var) && !missing(facet_var_order)){
    data <- dplyr::mutate(data, {{facet_var}} := forcats::fct_relevel(.data[[facet_var]], levels = !!!facet_var_order))
  }
  if(!missing(facet_var) && !missing(facet_var_labs)){
    data <- dplyr::mutate(data, {{facet_var}} := forcats::fct_recode(.data[[facet_var]], !!!facet_var_labs))
  }

  #setup foundational plotting object layer
  p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y, fill = fill_var))

  #add the geom layers

  #half-violin layer
  if (missing(violin_fill)) {
    p <- p + gghalves::geom_half_violin(side = violin_side, alpha = violin_alpha, size = violin_line_size, trim = violin_trim,
                                        draw_quantiles = violin_quantiles, colour = violin_colour)
  } else {
    p <- p + gghalves::geom_half_violin(side = violin_side, alpha = violin_alpha, size = violin_line_size, trim = violin_trim,
                                        draw_quantiles = violin_quantiles, colour = violin_colour, fill = violin_fill)
  }

  #points layer
  if (missing(point_fill)){
    p <- p + gghalves::geom_half_point(side = point_side, shape = point_shape, size = point_size,
                                       alpha = point_alpha, position = point_position, stroke = point_line_size,
                                       colour = point_colour)
  } else {
    p <- p + gghalves::geom_half_point(side = point_side, shape = point_shape, size = point_size,
                                       alpha = point_alpha, position = point_position, stroke = point_line_size,
                                       fill = point_fill, colour = point_colour)
  }

  #boxplot layer
  if(box_plot == TRUE) {
    if(missing(box_fill) && missing(box_outlier_fill)) {
      p <- p + gghalves::geom_half_boxplot(side = box_side, center = !box_half, nudge = box_nudge,
                                           colour = box_colour,
                                           alpha = box_alpha, width = box_width, size = box_line_size,
                                           linetype = box_line_type, coef = box_whisker_coef,
                                           errorbar.draw = box_error_bars,
                                           outlier.colour = box_outlier_colour, outlier.alpha = box_outlier_alpha,
                                           outlier.size = box_outlier_size, outlier.shape = box_outlier_shape)
    } else if (missing(box_fill) && !missing(box_outlier_fill)) {
      if(box_outlier_shape %in% c(21:25)) {
        p <- p + gghalves::geom_half_boxplot(side = box_side, center = !box_half, nudge = box_nudge,
                                             colour = box_colour,
                                             alpha = box_alpha, width = box_width, size = box_line_size,
                                             linetype = box_line_type, coef = box_whisker_coef,
                                             errorbar.draw = box_error_bars, outlier.fill = box_outlier_fill,
                                             outlier.colour = box_outlier_colour, outlier.alpha = box_outlier_alpha,
                                             outlier.size = box_outlier_size, outlier.shape = box_outlier_shape)
      } else {
        message("Only shapes 21-25 have a fill colour aesthetic option, box_outlier_fill specification ignored.")
        p <- p + gghalves::geom_half_boxplot(side = box_side, center = !box_half, nudge = box_nudge,
                                             colour = box_colour,
                                             alpha = box_alpha, width = box_width, size = box_line_size,
                                             linetype = box_line_type, coef = box_whisker_coef,
                                             errorbar.draw = box_error_bars,
                                             outlier.colour = box_outlier_colour, outlier.alpha = box_outlier_alpha,
                                             outlier.size = box_outlier_size, outlier.shape = box_outlier_shape)
      }
    } else if (!missing(box_fill) && missing(box_outlier_fill)) {
      p <- p + gghalves::geom_half_boxplot(side = box_side, center = !box_half, nudge = box_nudge,
                                           colour = box_colour, box_fill,
                                           alpha = box_alpha, width = box_width, size = box_line_size,
                                           linetype = box_line_type, coef = box_whisker_coef,
                                           errorbar.draw = box_error_bars,
                                           outlier.colour = box_outlier_colour, outlier.alpha = box_outlier_alpha,
                                           outlier.size = box_outlier_size, outlier.shape = box_outlier_shape)
    } else if (!missing(box_fill) && !missing(box_outlier_fill)) {
      if(box_outlier_shape %in% c(21:25)) {
        p <- p + gghalves::geom_half_boxplot(side = box_side, center = !box_half, nudge = box_nudge,
                                             colour = box_colour, fill = box_fill,
                                             alpha = box_alpha, width = box_width, size = box_line_size,
                                             linetype = box_line_type, coef = box_whisker_coef,
                                             errorbar.draw = box_error_bars, outlier.fill = box_outlier_fill,
                                             outlier.colour = box_outlier_colour, outlier.alpha = box_outlier_alpha,
                                             outlier.size = box_outlier_size, outlier.shape = box_outlier_shape)
      } else {
        message("Only shapes 21-25 have a fill colour aesthetic option, box_outlier_fill specification ignored.")
        p <- p + gghalves::geom_half_boxplot(side = box_side, center = !box_half, nudge = box_nudge,
                                             colour = box_colour, fill = box_fill,
                                             alpha = box_alpha, width = box_width, size = box_line_size,
                                             linetype = box_line_type, coef = box_whisker_coef,
                                             errorbar.draw = box_error_bars,
                                             outlier.colour = box_outlier_colour, outlier.alpha = box_outlier_alpha,
                                             outlier.size = box_outlier_size, outlier.shape = box_outlier_shape)
      }
    }

  }

  #modification of the fill values
  if (!missing(fill_var)){
    if(!missing(fill_var_values)){
      p <- p +
        ggplot2::scale_fill_manual(values = fill_var_values)
    } else {
      p <- p +
        ggplot2::scale_fill_viridis_d(begin = palette_begin, end = palette_end,
                                      option = palette, direction = palette_direction)
    }
  }

  #convert to greyscale
  if(greyscale == TRUE){
    p <- p + ggplot2::scale_fill_grey()
  }

  #themes
  if(theme == "classic"){
    p <- p + ggplot2::theme_classic(base_size = text_size, base_family = font)
  } else if (theme == "bw"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "grey"){
    p <- p + ggplot2::theme_grey(base_size = text_size, base_family = font)
  } else if (theme == "light"){
    p <- p + ggplot2::theme_light(base_size = text_size, base_family = font)
  } else if (theme == "dark"){
    p <- p + ggplot2::theme_dark(base_size = text_size, base_family = font)
  } else if (theme == "minimal"){
    p <- p + ggplot2::theme_minimal(base_size = text_size, base_family = font)
  }

  #modification of y-axis limits & transformations
  if(!missing(ylim) && coord_flip == FALSE) {
    p <- p + ggplot2::coord_cartesian(ylim = c(ylim[1], ylim[2]))
  } else if (missing(ylim) && coord_flip == TRUE){
    p <- p + ggplot2::coord_flip()
  } else if (!missing(ylim) && coord_flip == TRUE){
    p <- p + ggplot2::coord_flip(ylim = c(ylim[1], ylim[2]))
  }
  if(class(y_var_labs) != "waiver") {
    p <- p + ggplot2::scale_y_continuous(labels = y_var_labs)
  } else if(transform_y == FALSE && class(ybreaks) != "waiver"){
    p <- p + ggplot2::scale_y_continuous(breaks = ybreaks, labels = y_var_labs)
  } else if (transform_y == TRUE){
    p <- p + ggplot2::scale_y_continuous(trans = y_transformation, breaks = ybreaks, labels = y_var_labs)
  }

  #modification of axis labels
  if(missing(x) && coord_flip == FALSE){
    p <- p + ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                            axis.text.x = ggplot2::element_blank(),
                            axis.ticks.x = ggplot2::element_blank())
  } else if (missing(x) && coord_flip == TRUE) {
    p <- p + ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                            axis.text.y = ggplot2::element_blank(),
                            axis.ticks.y = ggplot2::element_blank())
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
  if(!missing(title)){
    p <- p + ggplot2::labs(title = title)
  }

  #misc
  if(omit_legend == TRUE){
    p <- p + ggplot2::theme(legend.position = "none")
  }
  if(legend_position != "right"){
    p <- p + ggplot2::theme(legend.position = legend_position)
  }
  if(!missing(facet_var)){
    p <- p + ggplot2::facet_wrap(vars(.data[[facet_var]]), strip.position = facet_var_strip_position)
  }
  if(!missing(facet_var) & facet_var_text_bold == TRUE){
    p <- p + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
  }
  if(aesthetic_options == TRUE){
    utils::browseURL("https://ggplot2.tidyverse.org/articles/ggplot2-specs.html")
  }
  return(p)
}


# plot_line ---------------------------------------------------------------
#' @title
#'
#' Generate a line graph.
#'
#' @description Easily generate line graphs using ggplot2 with a simplified
#'   customization interface for common modifications with static (ggplot) and
#'   interactive (plotly) output options. Unlike
#'   \code{\link[ggplot2]{geom_line}}, plot_line() will automatically check if
#'   there are multiple values of the y-axis variable for each level of the
#'   x-axis variable and/or other grouping variables (e.g. used for facetting)
#'   and will aggregate values for you using a summary statistic specified via
#'   the "stat" argument (default is the mean). This effectively produces a
#'   single line per group level combination and should make your line graphs
#'   easier to read. If such aggregation is necessary, a message explaining what
#'   is being done and the number of rows affected will be printed to the
#'   console. If your main goal is to plot sample group means or medians and
#'   error bars, see \code{\link{plot_stat_error}} instead. The static output is
#'   useful for producing static reports (e.g. for manuscripts) and is readily
#'   customized further using ggplot2 syntax. The interactive output is helpful
#'   for exploring the data and producing dynamic html reports. Line graphs are
#'   commonly used to show changes over time e.g. in time-series analysis. See
#'   \href{https://craig.rbind.io/post/2021-05-17-asgr-3-1-data-visualization/}{this
#'    blog post} for an introduction to ggplot2.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr across
#' @importFrom dplyr bind_rows
#' @importFrom data.table as.data.table
#' @importFrom forcats fct_relevel
#' @importFrom forcats fct_recode
#' @importFrom forcats fct_reorder
#' @importFrom forcats fct_infreq
#' @importFrom forcats fct_rev
#' @importFrom rlang !!!
#' @importFrom rlang .data
#' @importFrom tibble as_tibble
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 waiver
#' @importFrom ggplot2 scale_linetype_manual
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_x_date
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 scale_x_date
#' @importFrom ggplot2 scale_colour_grey
#' @importFrom ggplot2 lims
#' @importFrom ggplot2 labs
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
#' @param y A numeric variable containing the values you would like plotted on
#'   the y-axis (quoted or unquoted), e.g. y = "variable" or y = variable.
#'
#' @param x Typically a numeric or date/POSIX.ct variable to use for the x-axis
#'   (quoted or unquoted), e.g. x = "variable" or x = variable. If you assign a
#'   variable of a different class to x, it will be converted to a factor and
#'   arranged in order of factor levels (left to right), unless it is already a
#'   factor. The ordering of such variables can be modified with x_var_order*
#'   arguments.
#'
#' @param ... Other graphical parameters (not associated with variables) to be
#'   passed to \code{\link[ggplot2]{geom_line}} to be applied to all lines can
#'   be specified as well, e.g. "colour", "linejoin" or "lineend". To see some
#'   of the available options in a web browser, set the aesthetic_options
#'   argument to TRUE. For colour options, see \code{\link{colour_options}}.
#'
#' @param colour_var Use if you want to assign a categorical variable to line
#'   colour, e.g. colour_var = "grouping_variable" or colour_var =
#'   grouping_variable. Produces separate lines for each level of the colour
#'   variable. See \code{\link[ggplot2]{aes}} for details.
#'
#' @param line_type_var Use if you want to assign a categorical variable to the
#'   line type, e.g. line_type_var = "grouping_variable" or line_type_var =
#'   grouping_variable. Produces separate lines for each level of the fill
#'   variable. See \code{\link[ggplot2]{aes}} for details.
#'
#' @param stat If multiple values of the y-variable are detected for at least
#'   one grouping variable level combination based on variables assigned to any
#'   of the "x", "colour_var", "line_type_var", or "facet_var" arguments, the
#'   specified summary "stat" is used to aggregate the data such that a single
#'   line per group/x level combination is plotted. Options include "mean" (the
#'   default), "quantile", "sum", and "count". This argument supports partial
#'   matching, so "q" would be read as "quantile" for example. If "quantile" is
#'   chosen, then the probability value to use to extract a quantile can be
#'   specified with the "qprob" argument.
#'
#' @param qprob Probability value to pass to \code{\link[stats]{quantile}} if
#'   stat = "quantile". Default is 0.5 to get the median.
#'
#' @param xlab Specify/overwrite the x-axis label using a character string, e.g.
#'   "x-axis label"
#'
#' @param ylab Specify/overwrite the y-axis label using a character string, e.g.
#'   "y-axis label". Note that in cases wehre a summary statistic was used to
#'   aggregate some of the y-variable values (see "stat" argument description),
#'   the y-axis label will automatically be updated to specify which summary
#'   statistic was used by default.
#'
#' @param title Add a main title to the plot using a character string, e.g.
#'   "bar plots of y for each group of x"
#'
#' @param colour_var_title If a variable has been assigned to colour using
#'   colour_var, this allows you to modify the variable label in the plot
#'   legend.
#'
#' @param line_type_var_title If a variable has been assigned to line type using
#'   line_type_var, this allows you to modify the variable label in the plot
#'   legend.
#'
#' @param ylim specify the y-axis limits, e.g. ylim = c(lower_limit,
#'   upper_limit). Use NA for the existing minimum or maximum value of y, e.g.
#'   the default is ylim = c(NA, NA).
#'
#' @param ybreaks This allows you to change the break points to use for tick
#'   marks on the y-axis. \code{\link{seq}} is particularly useful here. See
#'   \code{\link[ggplot2]{scale_continuous}} for details. If ybreaks is
#'   specified, then ylim should be also.
#'
#' @param transform_y Would you like to transform the y axis (TRUE or FALSE)?
#'
#' @param y_transformation If transform_y = TRUE, this determines the
#'   transformation to be applied. Common choices include "log10" (the default),
#'   "log2", "sqrt", or "exp". See \code{\link[ggplot2]{scale_y_continuous}} for
#'   details.
#'
#' @param y_var_labs Allows you to modify the labels displayed with the y-axis
#'   tick marks. See \code{\link[ggplot2]{scale_continuous}} for details.
#'
#' @param xlim If x is a numeric or date variable, this allows you to specify
#'   the x-axis limits, e.g. xlim = c(lower_limit, upper_limit). Use NA for the
#'   existing minimum or maximum value of y, e.g. the default is ylim = c(NA,
#'   NA). See \code{\link[ggplot2]{scale_x_continuous}} if x is a numeric
#'   variable and \code{\link[ggplot2]{scale_x_date}} if x is a date variable
#'   for details.
#'
#' @param xbreaks If x is a numeric variable, this allows you to change the
#'   break points to use for tick marks via a numeric vector. \code{\link{seq}}
#'   is particularly useful here. See \code{\link[ggplot2]{scale_x_continuous}}
#'   for details. If x is a date variable, you can instead specify the break
#'   interval to use with a string, e.g. "2 years" to use a 2-year break point
#'   interval. See the "date_breaks" argument documentation under
#'   \code{\link[ggplot2]{scale_x_date}} for details. If xbreaks is
#'   specified for a numeric variable, then xlim should be also.
#'
#' @param transform_x Would you like to transform the x-axis (TRUE or FALSE)?
#'   Only works for numeric variables.
#'
#' @param x_transformation If transform_x = TRUE, this determines the
#'   transformation to be applied. Common choices include "log10" (the default),
#'   "log2", "sqrt", or "exp". See \code{\link[ggplot2]{scale_continuous}} for
#'   details. Only works for numeric variables.
#'
#' @param x_var_labs Allows you to modify the labels displayed with the x-axis
#'   tick marks. See \code{\link[ggplot2]{scale_x_continuous}} if x is a numeric
#'   variable, \code{\link[ggplot2]{scale_x_date}} if x is a date variable, or
#'   \code{\link[forcats]{fct_recode}} if x is a character variable/factor for
#'   details.
#'
#' @param x_var_order_by_y If a non-numeric/non-date variable has been assigned
#'   to x, this allows you to sort the points used to draw lines in order of
#'   increasing/ascending ("i" or "a") or decreasing ("d") value of y.
#'
#' @param x_var_order If a non-numeric/non-date variable has been assigned to x,
#'   this allows you to manually modify the order of the variable groups, e.g. x
#'   = grouping_variable, x_var_order = c("group_2", "group_1"). See
#'   \code{\link[forcats]{fct_relevel}} for details.
#'
#' @param colour_var_order_by_y If a variable has been assigned to colour_var,
#'   this allows you to sort the lines in order of increasing/ascending ("i" or
#'   "a") or decreasing ("d") value of y.
#'
#' @param colour_var_order If a variable has been assigned to colour using
#'   colour_var, this allows you to modify the order of the variable
#'   groups, e.g. colour_var = grouping_variable, colour_var_order =
#'   c("group_2", "group_1"). See \code{\link[forcats]{fct_relevel}} for
#'   details.
#'
#' @param line_type_var_order_by_y If a variable has been assigned to
#'   line_type_var, this allows you to sort the lines in order of
#'   increasing/ascending ("i" or "a") or decreasing ("d") value of y.
#'
#' @param line_type_var_order If a variable has been assigned to line type using
#'   line_type_var, this allows you to modify the order of the variable groups,
#'   e.g. line_type_var = grouping_variable, line_type_var_order = c("group_2",
#'   "group_1"). See \code{\link[forcats]{fct_relevel}} for details.
#'
#' @param colour_var_labs If a variable has been assigned to colour using
#'   colour_var, this allows you to modify the labels of the variable groups,
#'   e.g. colour_var = grouping_variable, colour_var_labs =
#'   c("group_1_new_label" = "group_1_old_label", "group_2_new_label" =
#'   "group_2_old_label"). See \code{\link[forcats]{fct_recode}} for details.
#'
#' @param line_type_var_labs If a variable has been assigned to line type using
#'   line_type_var, this allows you to modify the labels of the variable groups,
#'   e.g. line_type_var = grouping_variable, line_type_var_labs =
#'   c("group_1_new_label" = "group_1_old_label", "group_2_new_label" =
#'   "group_2_old_label"). See \code{\link[forcats]{fct_recode}} for details.
#'
#' @param colour_var_values If a variable has been assigned to colour using
#'   colour_var, this allows you to modify the colours assigned to the outline
#'   of each of the variable groups, e.g. colour_var = grouping_variable,
#'   colour_var_values = c("blue", "red"). See
#'   \code{\link[ggplot2]{scale_fill_manual}} for details. For the colour
#'   options available in base R, see \code{\link[elucidate]{colour_options}}.
#'
#' @param line_type_var_values If a variable has been assigned to line type
#'   using line_type_var, this allows you to modify the line types assigned to
#'   each of the variable groups, e.g. line_type_var = grouping_variable,
#'   fill_var_values = c("solid", "dashed"). See
#'   \code{\link[ggplot2]{scale_linetype_manual}} for details. Options are the
#'   same as those listed under the "line_type" argument.
#'
#' @param palette If a variable is assigned to colour_var, this determines which
#'   \href{https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html}{viridis
#'    colour palette} to use. Options include "plasma" or "C" (default), "magma"
#'   or "A", "inferno" or "B", "viridis" or "D", and "cividis" or "E". See
#'   \href{https://craig.rbind.io/post/2021-05-17-asgr-3-1-data-visualization/#colourblind-friendly-palettes}{this
#'    link} for examples. You can override these colour palettes with
#'   colour_var_values.
#'
#' @param palette_direction Choose "d2l" for dark to light (default) or "l2d"
#'   for light to dark.
#'
#' @param palette_begin Value between 0 and 1 that determines where along the
#'   full range of the chosen colour palette's spectrum to begin sampling
#'   colours. See \code{\link[ggplot2]{scale_colour_viridis_d}} for details.
#'
#' @param palette_end Value between 0 and 1 that determines where along the full
#'   range of the chosen colour palette's spectrum to end sampling colours. See
#'   \code{\link[ggplot2]{scale_colour_viridis_d}} for details.
#'
#' @param alpha This adjusts the transparency/opacity of the lines on the plot,
#'   ranging from 0 = 100% transparent to 1 = 100% opaque.
#'
#' @param greyscale Set to TRUE if you want the plot converted to greyscale.
#'
#' @param line_size Controls the thickness of the lines.
#'
#' @param line_type Use this to modify the type of lines used by
#'   \code{\link[ggplot2]{geom_line}} if line_type_var is unspecified. Options
#'   are: "solid", "dashed", "dotted", "dotdash", "longdash", and "twodash".
#'
#' @param points Would you like to add points to the plot with a
#'   \code{\link[ggplot2]{geom_point}} layer (TRUE or FALSE)? Default is FALSE.
#'
#' @param point_colour If points = TRUE and no variable has been assigned to
#'   colour_var, this determines the colour to use for points.
#'
#' @param point_fill If points = TRUE and point_shape is one of the options that
#'   have a fill aesthetic (see below), this controls the fill colour of the
#'   points.
#'
#' @param point_alpha If points = TRUE, this controls the transparency of the
#'   points.
#'
#' @param point_shape Point shape to use if points = TRUE. Only shapes 21-25
#'   have both fill and colour aesthetic parameters; the others only use colour.
#'   To see the options you can view the ggplot2 aesthetic options web page by
#'   setting the aesthetic_options argument to TRUE.
#'
#' @param point_size If points = TRUE, this controls the size of the points.
#'
#' @param theme Adjusts the theme using 1 of 6 predefined "complete" theme
#'   templates provided by ggplot2. Currently supported options are: "classic"
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
#'   variable (i.e. a facetted plot), e.g. facet_var = "grouping_variable" or
#'   facet_var = grouping_variable. See \code{\link[ggplot2]{facet_wrap}} for
#'   details.
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
#' @param verbose Set this to FALSE to prevent a message from being printed to
#'   the console if some of the data need to be aggregated to display a single
#'   line per group level combination.
#'
#' @return A ggplot object or plotly object depending on whether static or
#'   interactive output was requested.
#'
#' @author Craig P. Hutton, \email{Craig.Hutton@@gov.bc.ca}
#'
#' @examples
#'
#' #basic line graph split by a grouping variable that has been assigned to line
#' #colour
#'
#' plot_line(pdata, y = y1, x = d, colour_var = "g")
#'
#' #add points with "points = TRUE"
#' #disable the message that data needed to be aggregated to show a single line
#' #for each level of the x-variable "d" and colour-variable "g" by setting
#' #"verbose = FALSE"
#'
#' plot_line(pdata, y = y1, x = d, colour_var = g, points = TRUE, verbose = FALSE)
#'
#' @references
#' Wickham, H. (2016). ggplot2: elegant graphics for data analysis. New York, N.Y.: Springer-Verlag.
#'
#' @seealso \code{\link[ggplot2]{geom_bar}}, \code{\link[plotly]{ggplotly}},
#'   \code{\link{plot_stat_error}}
#'
#' @export
plot_line <- function(data, y, x, ...,
                      colour_var = NULL, line_type_var = NULL, #grouping variable aesthetic mappings
                      stat = c("mean", "quantile", "sum", "count"),
                      qprob = 0.5, #probability to use if stat = "quantile"
                      xlab = NULL, ylab = NULL, title = NULL,
                      colour_var_title = NULL, line_type_var_title = NULL, #titles
                      ylim = c(NA, NA), ybreaks = ggplot2::waiver(),
                      transform_y = FALSE, y_transformation = "log10", y_var_labs = ggplot2::waiver(), #control the y axis limits and scaling
                      xlim = c(NA, NA), xbreaks = ggplot2::waiver(),
                      transform_x = FALSE, x_transformation = "log10", x_var_labs = ggplot2::waiver(), #control the x axis limits and scaling
                      x_var_order_by_y = NULL, x_var_order = NULL,
                      colour_var_order_by_y = NULL, colour_var_order = NULL, #modify grouping variable level order
                      line_type_var_order_by_y = NULL, line_type_var_order = NULL,
                      colour_var_labs = NULL, line_type_var_labs = NULL, #modify grouping variable labels
                      colour_var_values = NULL, line_type_var_values = NULL, #manual colour specification
                      palette = c("plasma", "C", "magma", "A", "inferno", "B", "viridis", "D", "cividis", "E"), #viridis colour palettes
                      palette_direction = c("d2l", "l2d"), palette_begin = 0, palette_end = 0.8, #viridis colour palette options
                      alpha = 1, greyscale = FALSE, #control transparency, convert to greyscale
                      line_size = 1.1, line_type = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                      points = FALSE,
                      point_colour = "black", point_fill = "black", point_alpha = 1,
                      point_shape = "circle", point_size = 3,
                      theme = c("classic", "bw", "grey", "light", "dark", "minimal"),
                      text_size = 14, font = c("sans", "serif", "mono"), #theme options
                      facet_var = NULL, facet_var_order = NULL, facet_var_labs = NULL, #facet options
                      facet_var_strip_position = c("top", "bottom"), facet_var_text_bold = TRUE, #facet aesthetic customization
                      legend_position = c("right", "left", "top", "bottom"), omit_legend = FALSE, #legend position
                      interactive = FALSE, aesthetic_options = FALSE, verbose = FALSE) {#output format

  theme <- match.arg(theme)
  font <- match.arg(font)
  legend_position <- match.arg(legend_position)
  facet_var_strip_position <- match.arg(facet_var_strip_position)
  palette <- match.arg(palette)
  palette_direction <- match.arg(palette_direction)
  palette_direction <- ifelse(palette_direction == "d2l", 1, -1)
  line_type <- match.arg(line_type)
  stat <- match.arg(stat)

  if(!is.numeric(qprob) || length(qprob) > 1 || qprob > 1 || qprob < 0){
    stop('Quantile probability argument "qprob" must be a single number between 0 and 1.')
  }
  if(is.error(class(data[[y]]))) {
    y <- deparse(substitute(y))
  } else if(!is.character(y) || length(y) > 1){
    stop('If specified, `y` must be a single symbol or character string',
         '\n representing a column in the input data frame supplied to the `data` argument.')
  }
  if(is.error(class(data[[x]]))) {
    x <- deparse(substitute(x))
  } else if(!is.character(x) || length(x) > 1){
    stop('If specified, `x` must be a single symbol or character string',
         '\n representing a column in the input data frame supplied to the `data` argument.')
  }
  if(!missing(colour_var)) {
    if(is.error(class(data[[colour_var]]))) {
      colour_var <- deparse(substitute(colour_var))
    }else if(!is.character(colour_var) || length(colour_var) > 1){
      stop('If specified, `colour_var` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }
  if(!missing(line_type_var)) {
    if(is.error(class(data[[line_type_var]]))) {
      line_type_var <- deparse(substitute(line_type_var))
    }else if(!is.character(line_type_var) || length(line_type_var) > 1){
      stop('If specified, `line_type_var` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }
  if(!missing(facet_var)) {
    if(is.error(class(data[[facet_var]]))) {
      facet_var <- deparse(substitute(facet_var))
    } else if(!is.character(facet_var) || length(facet_var) > 1){
      stop('If specified, `facet_var` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }


  .classes <- class(data)
  x_var_class <- class(data[[x]])

  if("data.frame" %ni% .classes) {
    stop("Input data must be a data.table, tibble, or data.frame.")
  }
  if(missing(x) || missing(y)) {
    stop('x and y must be specified.')
  }
  if(!is.numeric(data[[y]]) && !is.integer(data[[y]])){
    stop("y must be a numeric column of a data frame")
  }

  #aggregate data if needed
  if("data.table" %ni% .classes) {
    data <- data.table::as.data.table(data)
  } else {
    data <- data.table::as.data.table(as.data.frame(data))
    #this is conversion and reversal is necessary to prevent subsequent
    #modification of the original data source in the global environment when the
    #input is already a data.table due to the use of the := operator below.
  }

  #grouping options
  if (missing(colour_var) && missing(line_type_var) && missing(facet_var)) {
    G <- x
  } else if (!missing(line_type_var) && missing(colour_var) && missing(facet_var)) {
    G <- c(x, line_type_var)
  } else if (missing(line_type_var) && !missing(colour_var) && missing(facet_var)) {
    G <- c(x, colour_var)
  } else if (missing(line_type_var) && missing(colour_var) && !missing(facet_var)) {
    G <- c(x, facet_var)
  } else if (!missing(line_type_var) && !missing(colour_var) && missing(facet_var)) {
    G <- c(line_type_var, colour_var)
  } else if (!missing(line_type_var) && missing(colour_var) && !missing(facet_var)) {
    G <- c(x, line_type_var, facet_var)
  } else if (missing(line_type_var) && !missing(colour_var) && !missing(facet_var)) {
    G <- c(x, colour_var, facet_var)
  } else if (!missing(x) && !missing(line_type_var) && !missing(colour_var) && !missing(facet_var)) {
    G <- c(x, colour_var, line_type_var, facet_var)
  }
  G <- unique(G) #eliminate duplicates if the same variable is mapped to more than one aesthetic parameter
  data[, n_copies := .N, by = eval(G)]

  data <- tibble::as_tibble(data)

  df_unique <- dplyr::filter(data, n_copies == 1)
  df_dupes <- dplyr::filter(data, n_copies > 1)

  orig_rows <- nrow(data)
  unique_count <- nrow(df_unique)
  dupe_count <- nrow(df_dupes)


  if(dupe_count > 0) {
    if(verbose == TRUE) {
      message(paste0(dupe_count, ' of ', orig_rows,
                     ' rows in the input data contain multiple values of the y-axis variable\nfor one or more levels of the grouping variables assigned to arguments:\n "x", "colour_var", "line_type_var", and/or "facet_var".\nAggregating values with the chosen summary statistic = "', stat, '"'))
    }
    df_dupes <- dplyr::select(df_dupes, {{x}}, {{colour_var}}, {{line_type_var}}, {{facet_var}}, {{y}})
    df_dupes <- dplyr::group_by(df_dupes, dplyr::across(-.data[[y]]))

    if(stat == "mean") {
      df_dupes <- dplyr::summarise(df_dupes, {{y}} := as.double(mean(.data[[y]], na.rm = TRUE)), .groups = "drop")
    } else if (stat == "quantile") {
      df_dupes <- dplyr::summarise(df_dupes,
                                   {{y}} := as.double(quantile(.data[[y]], probs = qprob, na.rm = TRUE)),
                                   .groups = "drop")
    } else if (stat == "sum") {
      df_dupes <- dplyr::summarise(df_dupes, {{y}} := as.double(sum(.data[[y]], na.rm = TRUE)), .groups = "drop")
    } else if (stat == "count") {
      message('When argument "stat" is set to "count" the y variable will be converted to a value of 1 for non-duplicated rows.')
      df_dupes <- dplyr::ungroup(dplyr::select(df_dupes, {{y}} := n_copies))
      if (unique_count > 0) {
        df_unique <- dplyr::select(df_unique, {{x}}, {{colour_var}}, {{line_type_var}}, {{facet_var}}, {{y}} := n_copies)
      }
    }
    if (unique_count > 0) {
      df_unique <- dplyr::select(df_unique, {{y}})
      data <- dplyr::bind_rows(df_unique, df_dupes)
    } else {
      data <- df_dupes
    }
  } else {
    if (stat == "count") {
      message('When argument "stat" is set to "count" the y variable will be converted to a value of 1 for non-duplicated rows.')
      df_unique <- dplyr::select(df_unique, {{x}}, {{colour_var}}, {{line_type_var}}, {{facet_var}}, {{y}} := n_copies)
    } else {
      df_unique <- dplyr::select(df_unique, {{x}}, {{colour_var}}, {{line_type_var}}, {{facet_var}}, {{y}})
    }
    data <- df_unique
  }

  #x-variable recoding
  if(x_var_class %in% c("character", "factor")) {
    data <- dplyr::mutate(data, {{x}} := as.factor(.data[[x]]))

    if(!missing(x_var_order_by_y)) {
      if(x_var_order_by_y == "d") {
        data <- dplyr::mutate(data, {{x}} := forcats::fct_reorder(.data[[x]], .data[[y]], .desc = TRUE))
      } else {
        data <- dplyr::mutate(data, {{x}} := forcats::fct_reorder(.data[[x]], .data[[y]], .desc = FALSE))
      }
    }

    if(!missing(x_var_order)){
      data <- dplyr::mutate(data, {{x}} := forcats::fct_relevel(.data[[x]], levels = !!!x_var_order))
    }
    if(class(x_var_labs) != "waiver"){
      data <- dplyr::mutate(data, {{x}} := forcats::fct_recode(.data[[x]], !!!x_var_labs))
    }
    #geom_line expects the x-axis to be numeric
    xlabels <- as.character(levels(data[[x]]))
    data <- dplyr::mutate(data, {{x}} := as.numeric(.data[[x]]))
  }

  #colour variable recoding
  if(!missing(colour_var)){
    data <- dplyr::mutate(data, {{colour_var}} := as.character(.data[[colour_var]]))
  }
  if(!missing(colour_var) && !missing(colour_var_order_by_y)) {
    if(colour_var_order_by_y == "d") {
      data <- dplyr::mutate(data, {{colour_var}} := forcats::fct_reorder(.data[[colour_var]], .data[[y]], .desc = TRUE))
    } else {
      data <- dplyr::mutate(data, {{colour_var}} := forcats::fct_reorder(.data[[colour_var]], .data[[y]], .desc = FALSE))
    }
  }

  if(!missing(colour_var) && !missing(colour_var_order)){
    data <- dplyr::mutate(data, {{colour_var}} := forcats::fct_relevel(.data[[colour_var]], levels = !!!colour_var_order))
  }
  if(!missing(colour_var) && !missing(colour_var_labs)){
    data <- dplyr::mutate(data, {{colour_var}} := forcats::fct_recode(.data[[colour_var]], !!!colour_var_labs))
  }

  #linetype variable recoding
  if(!missing(line_type_var)){
    data <- dplyr::mutate(data, {{line_type_var}} := as.character(.data[[line_type_var]]))
  }
  if(!missing(line_type_var) && !missing(line_type_var_order_by_y)) {
    if(line_type_var_order_by_y == "d") {
      data <- dplyr::mutate(data, {{line_type_var}} := forcats::fct_reorder(.data[[line_type_var]], .data[[y]], .desc = TRUE))
    } else {
      data <- dplyr::mutate(data, {{line_type_var}} := forcats::fct_reorder(.data[[line_type_var]], .data[[y]], .desc = FALSE))
    }
  }
  if(!missing(line_type_var) && !missing(line_type_var_order)){
    data <- dplyr::mutate(data, {{line_type_var}} := forcats::fct_relevel(.data[[line_type_var]], levels = !!!line_type_var_order))
  }
  if(!missing(line_type_var) && !missing(line_type_var_labs)){
    data <- dplyr::mutate(data, {{line_type_var}} := forcats::fct_recode(.data[[line_type_var]], !!!line_type_var_labs))
  }

  #facet label recoding
  if(!missing(facet_var)){
    data <- dplyr::mutate(data, {{facet_var}} := as.character(.data[[facet_var]]))
  }
  if(!missing(facet_var) && !missing(facet_var_order)){
    data <- dplyr::mutate(data, {{facet_var}} := forcats::fct_relevel(.data[[facet_var]], levels = !!!facet_var_order))
  }
  if(!missing(facet_var) && !missing(facet_var_labs)){
    data <- dplyr::mutate(data, {{facet_var}} := forcats::fct_recode(.data[[facet_var]], !!!facet_var_labs))
  }

  #setup foundational plotting object layer
  p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y, linetype = line_type_var, colour = colour_var))

  #add the geom_line layer
  if(!missing(colour_var)) {
    if(!missing(line_type_var)) {
      p <- p + ggplot2::geom_line(alpha = alpha, size = line_size, ...)
    } else {
      p <- p + ggplot2::geom_line(alpha = alpha, size = line_size, linetype = line_type, ...)
    }
  } else {
    if(!missing(line_type_var)) {
      p <- p + ggplot2::geom_line(alpha = alpha, size = line_size, ...)
    } else {
      p <- p + ggplot2::geom_line(alpha = alpha, size = line_size, linetype = line_type, ...)
    }
  }

  #add points optionally
  if(points == TRUE) {
    if(!missing(colour_var)) {
      p <- p + ggplot2::geom_point(fill = point_fill, size = point_size,
                                   shape = point_shape, alpha = alpha, stroke = line_size)
    } else {
      p <- p + ggplot2::geom_point(colour = point_colour, fill = point_fill, size = point_size,
                                   shape = point_shape, alpha = alpha, stroke = line_size)

    }
  }

  #modification of the colour_var values
  if(!missing(colour_var)) {
    if(!missing(colour_var_values)){
      p <- p +
        ggplot2::scale_colour_manual(values = colour_var_values)
    } else {
      p <- p +
        ggplot2::scale_colour_viridis_d(begin = palette_begin, end = palette_end,
                                        option = palette, direction = palette_direction)
    }
  }

  #modification of line_type_var values
  if(!missing(line_type_var) && !missing(line_type_var_values)) {
    p <- p + ggplot2::scale_linetype_manual(values = line_type_var_values)
  }

  #convert to greyscale
  if(greyscale == TRUE){
    p <- p + ggplot2::scale_colour_grey()
  }

  #themes
  if(theme == "classic"){
    p <- p + ggplot2::theme_classic(base_size = text_size, base_family = font)
  } else if (theme == "bw"){
    p <- p + ggplot2::theme_bw(base_size = text_size, base_family = font)
  } else if (theme == "grey"){
    p <- p + ggplot2::theme_grey(base_size = text_size, base_family = font)
  } else if (theme == "light"){
    p <- p + ggplot2::theme_light(base_size = text_size, base_family = font)
  } else if (theme == "dark"){
    p <- p + ggplot2::theme_dark(base_size = text_size, base_family = font)
  } else if (theme == "minimal"){
    p <- p + ggplot2::theme_minimal(base_size = text_size, base_family = font)
  }

  #modification of x/y-axis limits & transformations
  if(!missing(xlim) && missing(ylim)) {
    p <- p + ggplot2::coord_cartesian(xlim = c(xlim[1], xlim[2]))
  } else if (missing(xlim) && !missing(ylim)) {
    p <- p + ggplot2::coord_cartesian(ylim = c(ylim[1], ylim[2]))
  } else if (!missing(xlim) && !missing(ylim)) {
    p <- p + ggplot2::coord_cartesian(xlim = c(xlim[1], xlim[2]), ylim = c(ylim[1], ylim[2]))
  }
  #y
  if(class(y_var_labs) != "waiver") {
    p <- p + ggplot2::scale_y_continuous(labels = y_var_labs)
  } else if(transform_y == FALSE && class(ybreaks) != "waiver"){
    p <- p + ggplot2::scale_y_continuous(breaks = ybreaks, labels = y_var_labs)
  } else if (transform_y == TRUE){
    p <- p + ggplot2::scale_y_continuous(trans = y_transformation, breaks = ybreaks, labels = y_var_labs)
  }
  #x
  if(x_var_class %in% c("numeric", "integer")) {
    if(class(x_var_labs) != "waiver") {
      p <- p + ggplot2::scale_x_continuous(labels = x_var_labs)
    }
    if(transform_x == FALSE){
      p <- p + ggplot2::scale_x_continuous(breaks = xbreaks, labels = x_var_labs)
    } else if (transform_x == TRUE){
      p <- p + ggplot2::scale_x_continuous(trans = x_transformation, breaks = xbreaks, labels = x_var_labs)
    }
  } else if (x_var_class == "Date") {
    if(class(xbreaks) == "waiver" && class(x_var_labs) != "waiver") {
      p <- p + ggplot2::scale_x_date(labels = x_var_labs)
    } else if (class(xbreaks) != "waiver" && class(x_var_labs) == "waiver") {
      p <- p + ggplot2::scale_x_date(date_breaks = xbreaks)
    } else if (class(xbreaks) != "waiver" && class(xbreaks) != "waiver") {
      p <- p + ggplot2::scale_x_date(date_breaks = xbreaks, labels = x_var_labs)
    }
  }

  #modification of axis labels
  if(!missing(ylab)){
    p <- p + ggplot2::labs(y = ylab)
  } else if (dupe_count > 0 && stat == "mean") {
    p <- p + ggplot2::labs(y = paste("mean", y))
  } else if (dupe_count > 0 && stat == "quantile" && qprob == 0.5) {
    p <- p + ggplot2::labs(y = paste("median", y))
  } else if (dupe_count > 0 && stat == "quantile" && qprob == 0) {
    p <- p + ggplot2::labs(y = paste("minimum", y))
  } else if (dupe_count > 0 && stat == "quantile" && qprob == 1) {
    p <- p + ggplot2::labs(y = paste("maximum", y))
  } else if (dupe_count > 0 && stat == "quantile" && qprob %ni% c(0, 0.5, 1)) {
    p <- p + ggplot2::labs(y = paste0(round(qprob*100), "th percentile of ", y))
  } else if (dupe_count > 0 && stat == "sum") {
    p <- p + ggplot2::labs(y = paste("total", y))
  } else if (dupe_count > 0 && stat == "count") {
    p <- p + ggplot2::labs(y = paste("count of unique", y, "values"))
  } else {
    p <- p + ggplot2::labs(y = y)
  }

  if(!missing(xlab)){
    p <- p + ggplot2::labs(x = xlab)
  }

  if(!missing(colour_var_title)){
    p <- p + ggplot2::labs(color = colour_var_title)
  }
  if(!missing(line_type_var_title)){
    p <- p + ggplot2::labs(linetype = line_type_var_title)
  }
  if(!missing(title)){
    p <- p + ggplot2::labs(title = title)
  }

  #misc
  if(omit_legend == TRUE){
    p <- p + ggplot2::theme(legend.position = "none")
  }
  if(legend_position != "right"){
    p <- p + ggplot2::theme(legend.position = legend_position)
  }
  if(!missing(facet_var)){
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[facet_var]]), strip.position = facet_var_strip_position)
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

# plot_var ----------------------------------------------------------------
#' @title
#'
#' Plot variables/vectors using a class-appropriate plot_* function.
#'
#' @description Easily generate a ggplot2 graph using a class-appropriate
#'   geometry for the chosen primary (required) and secondary (optional)
#'   variable(s) from the same data frame source using other elucidate `plot_*`
#'   functions with a restricted set of customization options and some modified
#'   defaults. See "Arguments" section for details and
#'   \href{https://craig.rbind.io/post/2021-05-17-asgr-3-1-data-visualization/}{this
#'    blog post} for an introduction to ggplot2. To obtain plots of all
#'   variables/columns of a data frame, use \code{\link{plot_var_all}} or
#'   \code{\link{plot_var_pairs}} instead.
#'
#' @importFrom ggplot2 waiver
#' @importFrom ggplot2 geom_violin
#' @importFrom plotly ggplotly
#' @importFrom plotly layout
#'
#' @param data Either a data frame containing variables to be plotted or a
#'   vector to be plotted (unlike most other `plot_*` functions, which always
#'   require a data frame). Note that if a vector is supplied here, arguments
#'   `var1`, `var2`, and/or `group_var` cannot also be used.
#'
#' @param var1 The name of the primary variable you want plot(s) for (quoted or
#'   unquoted), e.g. var1 = "variable" or var1 = variable. If a data frame is
#'   supplied to the `data` argument, then `var1` must also be specified. `var1`
#'   will be assigned to the x-axis if `var2` is not specified. If `var2` is
#'   also specified, `var1` will be assigned to the y-axis if it is a numeric,
#'   integer, or date variable or to the x-axis if it is a factor, character, or
#'   logical variable.
#'
#' @param var2 The name of a secondary variable to plot against the primary
#'   variable (quoted or unquoted), e.g. var2 = "variable" or var2 = variable.
#'   `var2` is usually assigned to the x-axis. However, if `var1` is a
#'   categorical (factor, character, or logical) variable and `var2` is a
#'   numeric, integer, or date variable, `var2` will be assigned to the y-axis
#'   and `var1` will be assigned to the x-axis. If `var1` and `var2` are both
#'   categorical variables, `var1` will be assigned to the x-axis and `var2`
#'   will be assigned to `facet_var`.
#'
#' @param group_var Use if you want to assign a grouping variable to fill
#'   (colour) and/or (outline) colour e.g. group_var = "grouping_variable" or
#'   group_var = grouping_variable. Whether the grouping variable is mapped to
#'   fill, colour, or both will depend upon which `plot_*` function is used (See
#'   "Value" section). For density plots, both fill and colour are used for
#'   consistency across the main density plots and added normal density curve
#'   lines (if dnorm = TRUE). For bar graphs and box-and-whisker plots, the
#'   variable will be assigned to fill. For scatter plots, the variable will be
#'   assigned to colour. See \code{\link[ggplot2]{aes}} for details.
#'
#' @param var1_lab Accepts a character string to use to change the axis label
#'   for the variable assigned to `var1`.
#'
#' @param var2_lab Accepts a character string to use to change the axis label
#'   for the variable assigned to `var2`. Ignored if `var1` and `var2` are both
#'   categorical variables (since `var2` will be used for faceting in such
#'   cases).
#'
#' @param title A character string to add as a title at the top of the graph.
#'
#' @param fill Fill colour to use for density plots, bar graphs, and box plots.
#'   Ignored if a variable that has been assigned to `group_var` is mapped on to
#'   `fill_var` (see `group_var` argument information above). Default is
#'   "blue2". Use \code{\link{colour_options}} to see colour option examples.
#'
#' @param colour Outline colour to use for density plots, bar graphs, box plots,
#'   and scatter plots. Ignored if a variable that has been assigned to
#'   `group_var` is mapped on to `colour_var` (see `group_var` argument
#'   information above). Default is "black". Use \code{\link{colour_options}} to
#'   see colour option examples.
#'
#' @param palette If a variable is assigned to group_var, this determines which
#'   \href{https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html}{viridis
#'    colour palette} to use. Options include "plasma" or "C" (default), "magma"
#'   or "A", "inferno" or "B", "viridis" or "D", and "cividis" or "E". See
#'   \href{https://craig.rbind.io/post/2021-05-17-asgr-3-1-data-visualization/#colourblind-friendly-palettes}{this
#'    link} for examples.
#'
#' @param palette_direction Choose "d2l" for dark to light (default) or "l2d"
#'   for light to dark.
#'
#' @param palette_begin Value between 0 and 1 that determines where along the
#'   full range of the chosen colour palette's spectrum to begin sampling
#'   colours. See \code{\link[ggplot2]{scale_fill_viridis_d}} for details.
#'
#' @param palette_end Value between 0 and 1 that determines where along the full
#'   range of the chosen colour palette's spectrum to end sampling colours. See
#'   \code{\link[ggplot2]{scale_fill_viridis_d}} for details.
#'
#' @param alpha This adjusts the transparency/opacity of the main geometric
#'   objects in the generated plot, with acceptable values ranging from 0 = 100%
#'   transparent to 1 = 100% opaque.
#'
#' @param greyscale Set to TRUE if you want the plot converted to grey scale.
#'
#' @param line_size Controls the thickness of plotted lines.
#'
#' @param theme Adjusts the theme using 1 of 6 predefined "complete" theme
#'   templates provided by ggplot2. Currently supported options are: "classic"
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
#' @param legend_position This allows you to modify the legend position if a
#'   variable is assigned to `group_var`. Options include "right" (the default),
#'   "left", "top", & "bottom".
#'
#' @param omit_legend Set to TRUE if you want to remove/omit the legend(s).
#'   Ignored if `group_var` is unspecified.
#'
#' @param dnorm When TRUE (default), this adds a dashed line representing a
#'   normal/Gaussian density curve to density plots, which are rendered for
#'   plots of single numeric variables. Disabled if `var1` is a date vector,
#'   `var1_log10` = TRUE, or `basic` = TRUE.
#'
#' @param violin When TRUE (default), this adds violin plot outlines to box
#'   plots, which are rendered in cases where a mixture of numeric and
#'   categorical variables are assigned to `var1` and `var2`. Disabled if
#'   `basic` = TRUE.
#'
#' @param var1_log10 If TRUE, applies a base-10 logarithmic transformation to a
#'   numeric variable that has been assigned to `var1`. Ignored if `var1` is a
#'   categorical variable.
#'
#' @param var2_log10 If TRUE, applies a base-10 logarithmic transformation to a
#'   numeric variable that has been assigned to `var2`. Ignored if `var2` is a
#'   categorical variable.
#'
#' @param point_size Controls the size of points used in scatter plots, which
#'   are rendered in cases where `var1` and `var2` are both numeric, integer, or
#'   date variables.
#'
#' @param point_shape Point shape to use in scatter plots, which
#'   are rendered in cases where `var1` and `var2` are both numeric, integer, or
#'   date variables.
#'
#' @param regression_line If TRUE (the default), adds a regression line to scatter
#'   plots, which are rendered in cases where `var1` and `var2` are both
#'   numeric, integer, or date variables. Disabled if `basic` = TRUE.
#'
#' @param regression_method If `regression_line` = TRUE, this determines the
#'   type of regression line to use. Currently available options are "gam",
#'   "loess", and "lm". "gam" is the default, which fits a generalized additive
#'   model using a smoothing term for x. This method has a longer run time, but
#'   typically provides a better fit to the data than other options and uses an
#'   optimization algorithm to determine the optimal wiggliness of the line. If
#'   the relationship between y and x is linear, the output will be equivalent
#'   to fitting a linear model. "loess" may be preferable to "gam" for small
#'   sample sizes. See \code{\link[ggplot2]{stat_smooth}} and
#'   \code{\link[mgcv]{gam}} for details.
#'
#' @param regression_se If TRUE (the default), adds a 95% confidence envelope for the
#'   regression line. Ignored if `regression_line` = FALSE.
#'
#' @param bar_position In bar plots, which are rendered for one or more
#'   categorical variables, this determines how bars are arranged relative to
#'   one another when a grouping variable is assigned to `group_var`. The
#'   default, "dodge", uses \code{\link[ggplot2]{position_dodge}} to arrange
#'   bars side-by-side; "stack" places the bars on top of each other; "fill"
#'   also stacks bars but additionally converts y-axis from counts to
#'   proportions.
#'
#' @param bar_width In bar plots, which are rendered for one or more categorical
#'   variables, this adjusts the width of the bars (default = 0.9).
#'
#' @param basic This is a shortcut argument that allows you to simultaneously
#'   disable the `dnorm`, `violin`, and `regression_line` arguments to produce a
#'   basic version of a density, box, or scatter plot (depending on
#'   `var1`/`var2` variable class(es)) without any of those additional layers.
#'   Dropping these extra layers may noticeably reduce rendering time and memory
#'   utilization, especially for larger sample sizes and/or when `interactive` =
#'   TRUE.
#'
#' @param interactive Determines whether a static ggplot object or an
#'   interactive html plotly object is returned. See
#'   \code{\link[plotly]{ggplotly}} for details. Note that in cases where a box
#'   plot is generated (for a mix of numeric and categorical variables) and a
#'   variable is also assigned to `group_var`, activating interactive/plotly
#'   mode will cause a spurious warning message about 'layout' objects not
#'   having a 'boxmode' attribute to be printed to the console. This is a
#'   \href{https://github.com/ropensci/plotly/issues/994}{documented bug} with
#'   plotly that can be safely ignored, although unfortunately the message
#'   cannot currently be suppressed.
#'
#' @param verbose If TRUE, this causes a message to be printed to the console
#'   informing you of the classes detected for variables assigned to any of
#'   `var1`, `var2`, and/or `group_var` as well as which arguments those
#'   variables are passed to in the underlying geom-specific elucidate `plot_*`
#'   function that is used to render the plot.
#'
#' @return A ggplot or plotly graph depending on whether static or interactive
#'   output was requested. The type of graph (i.e. `ggplot2::geom*` layers) that
#'   is rendered will depend upon the classes of the chosen variables, as
#'   follows:
#'
#'   - One numeric (classes numeric/integer/date) variable will be graphed with
#'   \code{\link{plot_density}}.
#'
#'   - One or two categorical (classes factor/character/logical) variable(s)
#'   will be graphed with \code{\link{plot_bar}}.
#'
#'   - Two numeric variables will be graphed with \code{\link{plot_scatter}}.
#'
#'   - A mixture of numeric and categorical variables will be graphed with
#'   \code{\link{plot_box}}.
#'
#' @author Craig P. Hutton, \email{Craig.Hutton@@gov.bc.ca}
#'
#' @examples
#'
#' data(mtcars) #load the mtcars data
#'
#' #convert variables "cyl" and "am" to a factors
#' mtcars$cyl <- as.factor(mtcars$cyl)
#' mtcars$am <- as.factor(mtcars$am)
#'
#' # density plot of a single numeric variable on the x-axis
#' # with normal density curve added as dashed line
#' #
#' # normal density curves can be disabled via `dnorm` = FALSE or `basic` = TRUE
#'
#' plot_var(data = mtcars, var1 = mpg)
#'
#' # density plot with a primary numeric variable on the x-axis
#' # split by a categorical grouping variable assigned to the `fill_var`
#' # argument of plot_density() & normal density curves distabled
#'
#' plot_var(mtcars, mpg, group_var = cyl, dnorm = FALSE)
#'
#' plot_var(mtcars, cyl) #bar plot of a single categorical variable on x-axis
#'
#' # bar plot with a primary categorical variable on the x-axis and a secondary
#' # categorical variable used for faceting.
#'
#' plot_var(mtcars, var1 = cyl, var2 = am)
#'
#' # box plot with added violin plots for a mix of numeric and categorical variables
#' # and verbose mode enabled to print variable assignment information to the console
#' #
#' # the violin plots can be disabled via `violin` = FALSE or `basic` = TRUE
#'
#' plot_var(mtcars, mpg, cyl, verbose = TRUE)
#'
#' # scatter plot with added generalized additive model regression line and
#' # 95% confidence envelope for two numeric variables
#' #
#' # N.B. the regression line and CI can be disabled via `regression_line` = FALSE or
#' # `basic`= TRUE
#'
#' plot_var(mtcars, mpg, hp)
#'
#' @references
#' Wickham, H. (2016). ggplot2: elegant graphics for data analysis. New York, N.Y.: Springer-Verlag.
#'
#' @seealso \code{\link{plot_density}}, \code{\link{plot_bar}},
#'   \code{\link{plot_scatter}}, \code{\link{plot_box}},
#'   \code{\link{plot_var_all}}, \code{\link{plot_var_pairs}}
#'
#' @export
plot_var <- function(data,
                     var1 = NULL, var2 = NULL, group_var = NULL,
                     var1_lab = ggplot2::waiver(), var2_lab = ggplot2::waiver(), title = ggplot2::waiver(),
                     fill = "blue2", colour = "black",
                     palette = c("plasma", "C", "magma", "A", "inferno", "B", "viridis", "D", "cividis", "E"),
                     palette_direction = c("d2l", "l2d"), palette_begin = 0, palette_end = 0.8,
                     alpha = 0.75, greyscale = FALSE, line_size = 1,
                     theme = c("classic", "bw", "grey", "light", "dark", "minimal"),
                     text_size = 14, font = c("sans", "serif", "mono"),
                     legend_position = c("right", "left", "top", "bottom"), omit_legend = FALSE,
                     dnorm = TRUE, violin = TRUE, var1_log10 = FALSE, var2_log10 = FALSE,
                     point_size = 2, point_shape = c("circle", "square", "diamond", "triangle up", "triangle down"),
                     regression_line = TRUE, regression_method = c("gam", "loess", "lm"), regression_se = TRUE,
                     bar_position = c("dodge", "fill", "stack"), bar_width = 0.9,
                     basic = FALSE, interactive = FALSE, verbose = FALSE) {

  theme <- match.arg(theme)
  palette <- match.arg(palette)
  palette_direction <- match.arg(palette_direction)
  font = match.arg(font)
  legend_position = match.arg(legend_position)
  bar_position = match.arg(bar_position)
  point_shape <- match.arg(point_shape)
  regression_method <- match.arg(regression_method)

  if(basic == TRUE) {
    dnorm <- FALSE
    violin <- FALSE
    regression_line <- FALSE
  }

  .data_nrow <- nrow(data)

  if(!missing(var1)) {
    if(is.error(class(data[[var1]]))) {
      var1 <- deparse(substitute(var1))
    } else if(!is.character(var1) || length(var1) > 1){
      stop('If specified, `var1` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
    .var1_class <- class(data[[var1]])
  }
  if(!missing(var2)) {
    if(is.error(class(data[[var2]]))) {
      var2 <- deparse(substitute(var2))
    } else if(!is.character(var2) || length(var2) > 1){
      stop('If specified, `var2` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
    .var2_class <- class(data[[var2]])
  }
  if(!missing(group_var)) {
    if(is.error(class(data[[group_var]]))) {
      group_var <- deparse(substitute(group_var))
    }else if(!is.character(group_var) || length(group_var) > 1){
      stop('If specified, `group_var` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }
  if(!missing(var1)) {
    #input data class checks
    if(.var1_class %ni% c("numeric", "integer", "logical", "factor", "character", "Date")) {
      stop('"var1" must be a numeric, logical, categorical, or date variable in the input data frame provided to the "data" argument.')
    }
    if(!missing(var2) && .var2_class %ni% c("numeric", "integer", "logical", "factor", "character", "Date")) {
      stop('If "var2" is specified then it must be a numeric, logical, categorical, or date variable in the input data frame provided to the "data" argument.')
    }
    if("data.frame" %ni% class(data)) {
      stop('If "var1" is specified, input data must be a data.table, tibble, or data.frame.')
    }
    #conditional plot_* evaluation
    if(missing(var2) || var2 == var1) { #one variable in a data frame
      if(.var1_class %in% c("numeric", "integer", "Date")) {
        if(missing(group_var)) {
          if(verbose == TRUE) {
            message(paste0('`var1` class = "', .var1_class, '".',
                           '\nBuilding graph with `plot_density()`.',
                           '\nAssigning `var1` to `x`.',
                           '\nUse `plot_density()` instead to access additional customization options.',
                           '\nSee help("plot_density") for details.'))
          }
          p <- plot_density(data, x = var1, xlab = var1_lab, title = title,
                            fill = fill, colour = colour,
                            palette = palette, palette_direction = palette_direction,
                            palette_begin = palette_begin, palette_end = palette_end,
                            alpha = alpha, greyscale = greyscale, theme = theme,
                            text_size = text_size, font = font, interactive = interactive,
                            dnorm = dnorm, dnorm_colour = colour, dnorm_alpha = 0.8,
                            line_size = line_size, transform_x = var1_log10)
        } else {
          if(verbose == TRUE) {
            message(paste0('`var1` class = "', .var1_class, '".',
                           '\nBuilding graph with `plot_density()`.',
                           '\nAssigning `var1` to `x` and `group_var` to both `fill_var` and `colour_var`.',
                           '\nUse `plot_density()` instead to access additional customization options.',
                           '\nSee help("plot_density") for details.'))
          }
          p <- plot_density(data, x = var1, xlab = var1_lab, title = title,
                            fill_var = group_var, colour_var = group_var,
                            palette = palette, palette_direction = palette_direction,
                            palette_begin = palette_begin, palette_end = palette_end,
                            alpha = alpha, greyscale = greyscale, theme = theme,
                            text_size = text_size, font = font, interactive = interactive,
                            dnorm = dnorm, dnorm_colour = colour, dnorm_alpha = 0.8,
                            line_size = line_size, transform_x = var1_log10,
                            legend_position = legend_position, omit_legend = omit_legend)

        }
      } else if(.var1_class %in% c("factor", "character", "logical")) {
        if(missing(group_var)) {
          if(verbose == TRUE) {
            message(paste0('`var1` class = "', .var1_class, '".',
                           '\nBuilding graph with `plot_bar()`.',
                           '\nAssigning `var1` to `x`.',
                           '\nUse `plot_bar()` instead to access additional customization options.',
                           '\nSee help("plot_bar") for details.'))
          }
          p <- plot_bar(data, x = var1, xlab = var1_lab, title = title,
                        fill = fill, colour = colour,
                        palette = palette, palette_direction = palette_direction,
                        palette_begin = palette_begin, palette_end = palette_end,
                        alpha = alpha, greyscale = greyscale, theme = theme,
                        text_size = text_size, font = font, interactive = interactive,
                        position = bar_position, width = bar_width, line_size = line_size)
        } else {
          if(verbose == TRUE) {
            message(paste0('`var1` class = "', .var1_class, '".',
                           '\nBuilding graph with `plot_bar()`.',
                           '\nAssigning `var1` to `x` and `group_var` to `fill_var`.',
                           '\nUse `plot_bar()` instead to access additional customization options.',
                           '\nSee help("plot_bar") for details.'))
          }
          p <- plot_bar(data, x = var1, xlab = var1_lab, title = title,
                        fill_var = group_var, colour = colour,
                        palette = palette, palette_direction = palette_direction,
                        palette_begin = palette_begin, palette_end = palette_end,
                        alpha = alpha, greyscale = greyscale, theme = theme,
                        text_size = text_size, font = font, interactive = interactive,
                        position = bar_position, width = bar_width, line_size = line_size,
                        legend_position = legend_position, omit_legend = omit_legend)

        }
      }
    } else if(!missing(var2) && var2 != var1) { #2 variables in a data frame
      if(.var1_class %in% c("numeric", "integer", "Date") && .var2_class %in% c("numeric", "integer", "Date")) {
        if(missing(group_var)) {
          if(verbose == TRUE) {
            message(paste0('`var1` class = "', .var1_class, '" and `var2` class = "', .var2_class, '".',
                           '\nBuilding graph with `plot_scatter()`.',
                           '\nAssigning `var1` to `y` and `var2` to `x`.',
                           '\nUse `plot_scatter()` instead to access additional customization options.',
                           '\nSee help("plot_scatter") for details.'))
          }
          p <- plot_scatter(data, y = var1, x = var2,
                            xlab = var1_lab, ylab = var2_lab, title = title,
                            colour = colour,
                            palette = palette, palette_direction = palette_direction,
                            palette_begin = palette_begin, palette_end = palette_end,
                            alpha = alpha, greyscale = greyscale, theme = theme,
                            text_size = text_size, font = font, interactive = interactive,
                            jitter = TRUE, size = point_size, shape = point_shape,
                            regression_line = regression_line, regression_se = regression_se,
                            regression_method = regression_method,
                            transform_y = var1_log10, transform_x = var2_log10)
        } else {
          if(verbose == TRUE) {
            message(paste0('`var1` class = "', .var1_class, '" and `var2` class = "', .var2_class, '".',
                           '\nBuilding graph with `plot_scatter()`.',
                           '\nAssigning `var1` to `y`, `var2` to `x`, and `group_var` to `colour_var`.',
                           '\nUse `plot_scatter()` instead to access additional customization options.',
                           '\nSee help("plot_scatter") for details.'))
          }
          p <- plot_scatter(data, y = var1, x = var2,
                            xlab = var1_lab, ylab = var2_lab, title = title,
                            colour_var = group_var,
                            palette = palette, palette_direction = palette_direction,
                            palette_begin = palette_begin, palette_end = palette_end,
                            alpha = alpha, greyscale = greyscale, theme = theme,
                            text_size = text_size, font = font, interactive = interactive,
                            jitter = TRUE, size = point_size, shape = point_shape,
                            regression_line = regression_line, regression_se = regression_se,
                            regression_method = regression_method,
                            transform_y = var1_log10, transform_x = var2_log10,
                            legend_position = legend_position, omit_legend = omit_legend)
        }
      } else if(.var1_class %in% c("numeric", "integer", "Date") && .var2_class %in% c("factor", "character", "logical")) {
        if(missing(group_var)) {
          if(verbose == TRUE) {
            message(paste0('`var1` class = "', .var1_class, '" and `var2` class = "', .var2_class, '".',
                           '\nBuilding graph with `plot_box()`.',
                           '\nAssigning `var1` to `y` and `var2` to `x`.',
                           '\nUse `plot_box()` instead to access additional customization options.',
                           '\nSee help("plot_box") for details.'))
          }
          p <- plot_box(data, y = var1, x = var2,
                        ylab = var1_lab, xlab = var2_lab, title = title,
                        fill = fill, colour = colour,
                        palette = palette, palette_direction = palette_direction,
                        palette_begin = palette_begin, palette_end = palette_end,
                        alpha = alpha, greyscale = greyscale, theme = theme,
                        text_size = text_size, font = font,
                        line_size = line_size, transform_y = var1_log10)
          if(violin == TRUE) {
            p <- p + ggplot2::geom_violin(colour = colour, fill = NA, size = line_size*0.9)
          }
          if(interactive == TRUE){
            if(missing(group_var)) {
              p <- plotly::ggplotly(p)
            } else {
              p <- plotly::ggplotly(p)
              p <- plotly::layout(p, boxmode = "group")
            }
            return(p)
          }
        } else {
          if(verbose == TRUE) {
            message(paste0('`var1` class = "', .var1_class, '" and `var2` class = "', .var2_class, '".',
                           '\nBuilding graph with `plot_box()`.',
                           '\nAssigning `var1` to `y`, `var2` to `x`, and `group_var` to `fill_var`.',
                           '\nUse `plot_box()` instead to access additional customization options.',
                           '\nSee help("plot_box") for details.'))
          }
          p <- plot_box(data, y = var1, x = var2,
                        ylab = var1_lab, xlab = var2_lab, title = title,
                        fill_var = group_var, colour = colour,
                        palette = palette, palette_direction = palette_direction,
                        palette_begin = palette_begin, palette_end = palette_end,
                        alpha = alpha, greyscale = greyscale, theme = theme,
                        text_size = text_size, font = font,
                        line_size = line_size, transform_y = var1_log10,
                        position = ggplot2::position_dodge(width = 0.9),
                        legend_position = legend_position, omit_legend = omit_legend)
          if(violin == TRUE) {
            p <- p + ggplot2::geom_violin(colour = colour, fill = NA,
                                          size = line_size*0.9, position = ggplot2::position_dodge(width = 0.9))
          }
          if(interactive == TRUE){
            if(missing(group_var)) {
              p <- plotly::ggplotly(p)
            } else {
              p <- plotly::ggplotly(p)
              p <- plotly::layout(p, boxmode = "group")
            }
            return(p)
          }
        }
      } else if(.var1_class %in% c("factor", "character", "logical") && .var2_class %in% c("numeric", "integer", "Date")) {
        if(missing(group_var)) {
          if(verbose == TRUE) {
            message(paste0('`var1` class = "', .var1_class, '" and `var2` class = "', .var2_class, '".',
                           '\nBuilding graph with `plot_box()`.',
                           '\nAssigning `var1` to `x` and `var2` to `y`.',
                           '\nUse `plot_box()` instead to access additional customization options.',
                           '\nSee help("plot_box") for details.'))
          }
          p <- plot_box(data, y = var2, x = var1,
                        ylab = var2_lab, xlab = var1_lab, title = title,
                        fill = fill, colour = colour,
                        palette = palette, palette_direction = palette_direction,
                        palette_begin = palette_begin, palette_end = palette_end,
                        alpha = alpha, greyscale = greyscale, theme = theme,
                        text_size = text_size, font = font,
                        line_size = line_size, transform_y = var2_log10)
          if(violin == TRUE) {
            p <- p + ggplot2::geom_violin(colour = colour, fill = NA, size = line_size*0.9)
          }
          if(interactive == TRUE){
            if(missing(group_var)) {
              p <- plotly::ggplotly(p)
            } else {
              p <- plotly::ggplotly(p)
              p <- plotly::layout(p, boxmode = "group")
            }
            return(p)
          }
        } else {
          if(verbose == TRUE) {
            message(paste0('`var1` class = "', .var1_class, '" and `var2` class = "', .var2_class, '".',
                           '\nBuilding graph with `plot_box()`.',
                           '\nAssigning `var1` to `x`, `var2` to `y`, and `group_var` to `fill_var`.',
                           '\nUse `plot_box()` instead to access additional customization options.',
                           '\nSee help("plot_box") for details.'))
          }
          p <- plot_box(data, y = var2, x = var1,
                        ylab = var2_lab, xlab = var1_lab, title = title,
                        fill_var = group_var, colour = colour,
                        palette = palette, palette_direction = palette_direction,
                        palette_begin = palette_begin, palette_end = palette_end,
                        alpha = alpha, greyscale = greyscale, theme = theme,
                        text_size = text_size, font = font,
                        line_size = line_size, transform_y = var2_log10,
                        position = ggplot2::position_dodge(width = 0.9),
                        legend_position = legend_position, omit_legend = omit_legend)
          if(violin == TRUE) {
            p <- p + ggplot2::geom_violin(colour = colour, fill = NA,
                                          size = line_size*0.9, position = ggplot2::position_dodge(width = 0.9))
          }
          if(interactive == TRUE){
            if(missing(group_var)) {
              p <- plotly::ggplotly(p)
            } else {
              p <- plotly::ggplotly(p)
              p <- plotly::layout(p, boxmode = "group")
            }
            return(p)
          }
        }
      } else if(.var1_class %in% c("factor", "character", "logical") && .var2_class %in% c("factor", "character", "logical")) {
        if(missing(group_var)) {
          if(verbose == TRUE) {
            message(paste0('`var1` class = "', .var1_class, '" and `var2` class = "', .var2_class, '".',
                           '\nBuilding graph with `plot_bar()`.',
                           '\nAssigning `var1` to `x` and `var2` to `facet_var`.',
                           '\nUse `plot_bar()` instead to access additional customization options.',
                           '\nSee help("plot_bar") for details.'))
          }
          p <- plot_bar(data, x = var1, xlab = var1_lab, facet_var = var2, title = title,
                        fill = fill, colour = colour, line_size = line_size,
                        palette = palette, palette_direction = palette_direction,
                        palette_begin = palette_begin, palette_end = palette_end,
                        alpha = alpha, greyscale = greyscale, theme = theme,
                        text_size = text_size, font = font, interactive = interactive,
                        width =  bar_width)
        } else {
          if(verbose == TRUE) {
            message(paste0('`var1` class = "', .var1_class, '" and `var2` class = "', .var2_class, '".',
                           '\nBuilding graph with `plot_bar()`.',
                           '\nAssigning `var1` to `x` and `var2` to `facet_var`.',
                           '\nUse `plot_bar()` instead to access additional customization options.',
                           '\nSee help("plot_bar") for details.'))
          }
          p <- plot_bar(data, x = var1, xlab = var1_lab, facet_var = var2, title = title,
                        colour = colour, fill_var = group_var, line_size = line_size,
                        palette = palette, palette_direction = palette_direction,
                        palette_begin = palette_begin, palette_end = palette_end,
                        alpha = alpha, greyscale = greyscale, theme = theme,
                        text_size = text_size, font = font, interactive = interactive,
                        width = bar_width, position = bar_position,
                        legend_position = legend_position, omit_legend = omit_legend)
        }
      }
    } else {
      stop('"var1" or "var2" variable class not supported.\nValid input classes include: "numeric", "integer", "logical", "factor", "character", and "Date".')
    }
    return(p)

  } else {
    #input data class check
    .v_class <- class(data)

    .v_length <- length(data)

    if(.v_class %ni% c("numeric", "integer", "logical", "factor", "character", "Date")) {
      stop('If "var1" is not specified, data must be a vector of class: "numeric", "integer", "logical", "factor", "character", or "Date".')
    }

    vec_df <- data.frame("var1" = data)

    if(class(var1_lab) == "waiver") {
      .var1_lab <- deparse(substitute(data))
    }

    if(.v_class %in% c("numeric", "integer", "Date")) {
      if (verbose == TRUE) {
        message(paste0('`data` class = "', .v_class, '".',
                       '\nConverting input from a vector to data frame column.',
                       '\nBuilding graph with `plot_density()` and assigning input vector to `x`.',
                       '\nUse `plot_density()` instead to access additional customization options.',
                       '\nSee help("plot_density") for details.'))
      }
      p <- plot_density(vec_df, x = "var1", xlab = .var1_lab, title = title,
                        fill = fill, colour = colour,
                        palette = palette, palette_direction = palette_direction,
                        palette_begin = palette_begin, palette_end = palette_end,
                        alpha = alpha, greyscale = greyscale, theme = theme,
                        text_size = text_size, font = font, interactive = interactive,
                        dnorm = dnorm, dnorm_colour = colour, dnorm_alpha = 0.8,
                        line_size = line_size, transform_x = var1_log10)
    } else if (.v_class %in% c("factor", "character", "logical")) {
      if (verbose == TRUE) {
        message(paste0('`data` class = "', .v_class, '".',
                       '\nConverting input from a vector to data frame column.',
                       '\nBuilding graph with `plot_bar()` and assigning input vector to `x`.',
                       '\nUse `plot_bar()` instead to access additional customization options.',
                       '\nSee help("plot_bar") for details.'))
      }
      p <- plot_bar(vec_df, x = "var1", xlab = .var1_lab, title = title,
                    fill = fill, colour = colour,
                    palette = palette, palette_direction = palette_direction,
                    palette_begin = palette_begin, palette_end = palette_end,
                    alpha = alpha, greyscale = greyscale, theme = theme,
                    text_size = text_size, font = font, interactive = interactive,
                    position = bar_position, width = bar_width, line_size = line_size)

    }
    return(p)
  }
}

# plot_var_all ------------------------------------------------------------
#' @title
#'
#' Plot all variables/vectors in a data frame using a class-appropriate `plot_*`
#' function.
#'
#' @description Easily generate ggplot2 graphs for all (or a named vector of)
#'   variables in a data frame using a class-appropriate geometry via other
#'   elucidate `plot_*` functions with a restricted set of customization options
#'   and some modified defaults. The `var2` argument also allows you to plot all
#'   variables against a specific named secondary variable. The collection of
#'   generated graphs will be combined into a single lattice-style figure with
#'   either the patchwork package or trelliscopejs package. See "Arguments"
#'   section for details and
#'   \href{https://craig.rbind.io/post/2021-05-17-asgr-3-1-data-visualization/}{this
#'    blog post} for an introduction to ggplot2. To obtain a plot of a single
#'   variable or vector, use \code{\link{plot_var}} instead. To obtain pairwise plots of
#'   all bivariate combinations of variables, use \code{\link{plot_var_pairs}}
#'   instead.
#'
#' @importFrom ggplot2 waiver
#' @importFrom dplyr mutate
#' @importFrom rlang exec
#' @importFrom trelliscopejs map_plot
#' @importFrom trelliscopejs trelliscope
#' @importFrom patchwork wrap_plots
#' @importFrom patchwork plot_annotation
#'
#' @param data A data frame containing variables to be plotted.
#'
#' @param var2 The (quoted or unquoted) name of a secondary variable to plot
#'   against all other variables in the input data (or a subset of them if the
#'   `cols` argument is used), where the latter set of "primary" variables will
#'   be automatically assigned to the `var1` argument of \code{\link{plot_var}}.
#'   `var2` is usually assigned to the x-axis. However, if the primary variable
#'   (i.e. `var1`) is a categorical (factor, character, or logical) variable and
#'   `var2` is a numeric, integer, or date variable, `var2` will be assigned to
#'   the y-axis and `var1` will be assigned to the x-axis. If `var1` and `var2`
#'   are both categorical variables, `var1` will be assigned to the x-axis and
#'   `var2` will be assigned to `facet_var`.
#'
#' @param group_var Use if you want to assign a grouping variable to fill
#'   (colour) and/or (outline) colour e.g. group_var = "grouping_variable" or
#'   group_var = grouping_variable. Whether the grouping variable is mapped to
#'   fill, colour, or both will depend upon which `plot_*` function is used (See
#'   "Value" section). For density plots, both fill and colour are used for
#'   consistency across the main density plots and added normal density curve
#'   lines (if dnorm = TRUE). For bar graphs and box-and-whisker plots, the
#'   variable will be assigned to fill. For scatter plots, the variable will be
#'   assigned to colour. See \code{\link[ggplot2]{aes}} for details.
#'
#' @param cols A character (or integer) vector of column names (or indices)
#'   which allows you to plot only a subset of the columns in the input data
#'   frame, where each of these primary variable columns will be automatically
#'   assigned to the `var1` argument of \code{\link{plot_var}}. Note that a
#'   variable which has been assigned to `var2` or `group_var` does not also
#'   need to be listed here.
#'
#' @param var2_lab Accepts a character string to use to change the axis label
#'   for the variable assigned to `var2`. Ignored if `var2` and the primary
#'   variable are both categorical variables (since `var2` will be used for
#'   faceting in such cases).
#'
#' @param title A character string to add as a title at the top of the combined
#'   multiple-panel patchwork graph or trelliscopejs display.
#'
#' @param fill Fill colour to use for density plots, bar graphs, and box plots.
#'   Ignored if a variable that has been assigned to `group_var` is mapped on to
#'   `fill_var` (see `group_var` argument information above). Default is
#'   "blue2". Use \code{\link{colour_options}} to see colour option examples.
#'
#' @param colour Outline colour to use for density plots, bar graphs, box plots,
#'   and scatter plots. Ignored if a variable that has been assigned to
#'   `group_var` is mapped on to `colour_var` (see `group_var` argument
#'   information above). Default is "black". Use \code{\link{colour_options}} to
#'   see colour option examples.
#'
#' @param palette If a variable is assigned to group_var, this determines which
#'   \href{https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html}{viridis
#'    colour palette} to use. Options include "plasma" or "C" (default), "magma"
#'   or "A", "inferno" or "B", "viridis" or "D", and "cividis" or "E". See
#'   \href{https://craig.rbind.io/post/2021-05-17-asgr-3-1-data-visualization/#colourblind-friendly-palettes}{this
#'    link} for examples.
#'
#' @param palette_direction Choose "d2l" for dark to light (default) or "l2d"
#'   for light to dark.
#'
#' @param palette_begin Value between 0 and 1 that determines where along the
#'   full range of the chosen colour palette's spectrum to begin sampling
#'   colours. See \code{\link[ggplot2]{scale_fill_viridis_d}} for details.
#'
#' @param palette_end Value between 0 and 1 that determines where along the full
#'   range of the chosen colour palette's spectrum to end sampling colours. See
#'   \code{\link[ggplot2]{scale_fill_viridis_d}} for details.
#'
#' @param alpha This adjusts the transparency/opacity of the main geometric
#'   objects in the generated plot, with acceptable values ranging from 0 = 100%
#'   transparent to 1 = 100% opaque.
#'
#' @param greyscale Set to TRUE if you want the plot converted to grey scale.
#'
#' @param line_size Controls the thickness of plotted lines.
#'
#' @param theme Adjusts the theme using 1 of 6 predefined "complete" theme
#'   templates provided by ggplot2. Currently supported options are: "classic"
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
#' @param legend_position This allows you to modify the legend position if a
#'   variable is assigned to `group_var`. Options include "right" (the default),
#'   "left", "top", & "bottom".
#'
#' @param omit_legend Set to TRUE if you want to remove/omit the legend(s).
#'   Ignored if `group_var` is unspecified.
#'
#' @param dnorm When TRUE (default), this adds a dashed line representing a
#'   normal/Gaussian density curve to density plots, which are rendered for
#'   plots of single numeric variables. Disabled if `var1` is a date vector,
#'   `var1_log10` = TRUE, or `basic` = TRUE.
#'
#' @param violin When TRUE (default), this adds violin plot outlines to box
#'   plots, which are rendered in cases where a mixture of numeric and
#'   categorical variables are assigned to `var1` and `var2`. Disabled if
#'   `basic` = TRUE.
#'
#' @param var1_log10 If TRUE, applies a base-10 logarithmic transformation to a
#'   numeric variable that has been assigned to `var1`. Ignored if `var1` is a
#'   categorical variable.
#'
#' @param var2_log10 If TRUE, applies a base-10 logarithmic transformation to a
#'   numeric variable that has been assigned to `var2`. Ignored if `var2` is a
#'   categorical variable.
#'
#' @param point_size Controls the size of points used in scatter plots, which
#'   are rendered in cases where `var1` and `var2` are both numeric, integer, or
#'   date variables.
#'
#' @param point_shape Point shape to use in scatter plots, which
#'   are rendered in cases where `var1` and `var2` are both numeric, integer, or
#'   date variables.
#'
#' @param regression_line If TRUE (the default), adds a regression line to scatter
#'   plots, which are rendered in cases where `var1` and `var2` are both
#'   numeric, integer, or date variables. Disabled if `basic` = TRUE.
#'
#' @param regression_method If `regression_line` = TRUE, this determines the
#'   type of regression line to use. Currently available options are "gam",
#'   "loess", and "lm". "gam" is the default, which fits a generalized additive
#'   model using a smoothing term for x. This method has a longer run time, but
#'   typically provides a better fit to the data than other options and uses an
#'   optimization algorithm to determine the optimal wiggliness of the line. If
#'   the relationship between y and x is linear, the output will be equivalent
#'   to fitting a linear model. "loess" may be preferable to "gam" for small
#'   sample sizes. See \code{\link[ggplot2]{stat_smooth}} and
#'   \code{\link[mgcv]{gam}} for details.
#'
#' @param regression_se If TRUE (the default), adds a 95% confidence envelope for the
#'   regression line. Ignored if `regression_line` = FALSE.
#'
#' @param bar_position In bar plots, which are rendered for one or more
#'   categorical variables, this determines how bars are arranged relative to
#'   one another when a grouping variable is assigned to `group_var`. The
#'   default, "dodge", uses \code{\link[ggplot2]{position_dodge}} to arrange
#'   bars side-by-side; "stack" places the bars on top of each other; "fill"
#'   also stacks bars but additionally converts y-axis from counts to
#'   proportions.
#'
#' @param bar_width In bar plots, which are rendered for one or more categorical
#'   variables, this adjusts the width of the bars (default = 0.9).
#'
#' @param basic This is a shortcut argument that allows you to simultaneously
#'   disable the `dnorm`, `violin`, and `regression_line` arguments to produce a
#'   basic version of a density, box, or scatter plot (depending on
#'   `var1`/`var2` variable class(es)) without any of those additional layers.
#'   Dropping these extra layers may noticeably reduce rendering time and memory
#'   utilization, especially for larger sample sizes and/or when `interactive` =
#'   TRUE.
#'
#' @param interactive Determines whether a static ggplot object or an
#'   interactive html plotly object is returned. Interactive/plotly mode for
#'   multiple plots should only be used in conjunction with `trelliscope` =
#'   TRUE. See \code{\link[plotly]{ggplotly}} for details. Note that in cases
#'   where a box plot is generated (for a mix of numeric and categorical
#'   variables) and a variable is also assigned to `group_var`, activating
#'   interactive/plotly mode will cause a spurious warning message about
#'   'layout' objects not having a 'boxmode' attribute to be printed to the
#'   console. This is a
#'   \href{https://github.com/ropensci/plotly/issues/994}{documented bug} with
#'   plotly that can be safely ignored, although unfortunately the message
#'   cannot currently be suppressed.
#'
#' @param trelliscope If changed to TRUE, plots will be combined into an
#'   interactive trelliscope display rather than a static patchwork graph grid.
#'   See \code{\link[trelliscopejs]{trelliscope}} for more information.
#'
#' @param nrow This controls the number of rows to use when arranging plots in
#'   the combined patchwork or trelliscopejs display.
#'
#' @param ncol This controls the number of columns to use when arranging plots
#'   in the combined patchwork or trelliscopejs display.
#'
#' @param guides Controls the pooling of `group_var` legends/guides across plot
#'   panels if a categorical variable has been assigned to `group_var` and
#'   `trelliscope` = FALSE. See \code{\link[patchwork]{wrap_plots}} for details.
#'
#' @return A static "patchwork" or dynamic "trelliscope" multi-panel graphical
#'   display of ggplot2 or plotly graphs depending upon the values of the
#'   `trelliscope` and `interactive` arguments. The type of graph (i.e.
#'   `ggplot2::geom*` layers) that is rendered in each panel will depend upon
#'   the classes of the chosen variables, as follows:
#'
#'   - One numeric (classes numeric/integer/date) variable will be graphed with
#'   \code{\link{plot_density}}.
#'
#'   - One or two categorical (classes factor/character/logical) variable(s)
#'   will be graphed with \code{\link{plot_bar}}.
#'
#'   - Two numeric variables will be graphed with \code{\link{plot_scatter}}.
#'
#'   - A mixture of numeric and categorical variables will be graphed with
#'   \code{\link{plot_box}}.
#'
#' @author Craig P. Hutton, \email{Craig.Hutton@@gov.bc.ca}
#'
#' @examples
#'
#' data(mtcars) #load the mtcars data
#'
#' #convert variables "cyl" to a factors
#' mtcars$cyl <- as.factor(mtcars$cyl)
#'
#' #plot variables "hp", "wt", and "cyl" from the mtcars data frame
#' plot_var_all(mtcars, cols = c("hp", "wt", "cyl"))
#'
#' #plot each of the same variables against column "mpg"
#' plot_var_all(mtcars, var2 = mpg, cols = c("hp", "wt", "cyl"))
#'
#' #plot "hp" and "wt" against mpg, group by "cyl"
#' plot_var_all(mtcars, var2 = mpg, group_var = cyl, cols = c("hp", "wt"),
#'              basic = TRUE, #distable regression lines/CIs
#'              ncol = 1, nrow = 2) #change the layout
#'
#' @references
#' Wickham, H. (2016). ggplot2: elegant graphics for data analysis. New York, N.Y.: Springer-Verlag.
#'
#' @seealso \code{\link{plot_density}}, \code{\link{plot_bar}},
#'   \code{\link{plot_scatter}}, \code{\link{plot_box}}, \code{\link{plot_var}},
#'   \code{\link{plot_var_pairs}}, \code{\link[patchwork]{wrap_plots}},
#'   \code{\link[trelliscopejs]{trelliscope}}
#'
#' @export
plot_var_all <- function(data, var2 = NULL, group_var = NULL, cols = NULL,
                         var2_lab = ggplot2::waiver(), title = ggplot2::waiver(),
                         fill = "blue2", colour = "black",
                         palette = c("plasma", "C", "magma", "A", "inferno", "B", "viridis", "D", "cividis", "E"),
                         palette_direction = c("d2l", "l2d"), palette_begin = 0, palette_end = 0.8,
                         alpha = 0.75, greyscale = FALSE, line_size = 1,
                         theme = c("classic", "bw", "grey", "light", "dark", "minimal"),
                         text_size = 14, font = c("sans", "serif", "mono"),
                         legend_position = c("right", "left", "top", "bottom"), omit_legend = FALSE,
                         dnorm = TRUE, violin = TRUE, var1_log10 = FALSE, var2_log10 = FALSE,
                         point_size = 2, point_shape = c("circle", "square", "diamond", "triangle up", "triangle down"),
                         regression_line = TRUE, regression_method = c("gam", "loess", "lm"), regression_se = TRUE,
                         bar_position = c("dodge", "fill", "stack"), bar_width = 0.9,
                         basic = FALSE, interactive = FALSE, trelliscope = FALSE, nrow = NULL, ncol = NULL, guides = "collect") {

  theme <- match.arg(theme)
  palette <- match.arg(palette)
  palette_direction <- match.arg(palette_direction)
  font = match.arg(font)
  legend_position = match.arg(legend_position)
  bar_position = match.arg(bar_position)
  point_shape <- match.arg(point_shape)
  regression_method <- match.arg(regression_method)


  .df_name <- deparse(substitute(data))
  if(!missing(cols)) {
    .col_names <- names(data[, cols])
  } else {
    .col_names <- names(data)
  }
  if(!missing(var2)) {
    if(is.error(class(data[[var2]]))) {
      var2 <- deparse(substitute(var2))
    } else if(!is.character(var2) || length(var2) > 1){
      stop('If specified, `var2` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
    .var2_class <- class(data[[var2]])
  }
  if(!missing(group_var)) {
    if(is.error(class(data[[group_var]]))) {
      group_var <- deparse(substitute(group_var))
    } else if(!is.character(group_var) || length(group_var) > 1){
      stop('If specified, `group_var` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }
  if(trelliscope == FALSE) {
    if(missing(var2) && missing(group_var)) {
      .pl <- lapply(1:length(.col_names), function(i) rlang::exec("plot_var", data,
                                                                  var1 = .col_names[i],
                                                                  fill = fill, colour = colour,
                                                                  palette = palette, palette_direction = palette_direction,
                                                                  palette_begin = palette_begin, palette_end = palette_end,
                                                                  alpha = alpha, greyscale = greyscale, theme = theme,
                                                                  text_size = text_size, font = font, line_size = line_size,
                                                                  legend_position = legend_position, omit_legend = omit_legend,
                                                                  dnorm = dnorm, violin = violin,
                                                                  var1_log10 = var1_log10, var2_log10 = var2_log10,
                                                                  point_size = point_size, point_shape = point_shape,
                                                                  regression_line = regression_line,
                                                                  regression_se = regression_se,
                                                                  regression_method = regression_method,
                                                                  bar_position = bar_position, bar_width = bar_width,
                                                                  interactive = interactive, verbose = FALSE, basic = basic)
      )
    } else if(!missing(var2) && missing(group_var)) {
      .pl <- lapply(1:length(.col_names), function(i) rlang::exec("plot_var", data,
                                                                  var1 = .col_names[i], var2 = var2, var2_lab = var2_lab,
                                                                  fill = fill, colour = colour,
                                                                  palette = palette, palette_direction = palette_direction,
                                                                  palette_begin = palette_begin, palette_end = palette_end,
                                                                  alpha = alpha, greyscale = greyscale, theme = theme,
                                                                  text_size = text_size, font = font, line_size = line_size,
                                                                  legend_position = legend_position, omit_legend = omit_legend,
                                                                  dnorm = dnorm, violin = violin,
                                                                  var1_log10 = var1_log10, var2_log10 = var2_log10,
                                                                  point_size = point_size, point_shape = point_shape,
                                                                  regression_line = regression_line,
                                                                  regression_se = regression_se,
                                                                  regression_method = regression_method,
                                                                  bar_position = bar_position, bar_width = bar_width,
                                                                  interactive = interactive, verbose = FALSE, basic = basic)
      )
    } else if(missing(var2) && !missing(group_var)) {
      .pl <- lapply(1:length(.col_names), function(i) rlang::exec("plot_var", data,
                                                                  var1 = .col_names[i],
                                                                  group_var = group_var,
                                                                  fill = fill, colour = colour,
                                                                  palette = palette, palette_direction = palette_direction,
                                                                  palette_begin = palette_begin, palette_end = palette_end,
                                                                  alpha = alpha, greyscale = greyscale, theme = theme,
                                                                  text_size = text_size, font = font, line_size = line_size,
                                                                  legend_position = legend_position, omit_legend = omit_legend,
                                                                  dnorm = dnorm, violin = violin,
                                                                  var1_log10 = var1_log10, var2_log10 = var2_log10,
                                                                  point_size = point_size, point_shape = point_shape,
                                                                  regression_line = regression_line,
                                                                  regression_se = regression_se,
                                                                  regression_method = regression_method,
                                                                  bar_position = bar_position, bar_width = bar_width,
                                                                  interactive = interactive, verbose = FALSE, basic = basic)
      )
    } else {
      .pl <- lapply(1:length(.col_names), function(i) rlang::exec("plot_var", data,
                                                                  var1 = .col_names[i], var2 = var2, var2_lab = var2_lab,
                                                                  group_var = group_var,
                                                                  fill = fill, colour = colour,
                                                                  palette = palette, palette_direction = palette_direction,
                                                                  palette_begin = palette_begin, palette_end = palette_end,
                                                                  alpha = alpha, greyscale = greyscale, theme = theme,
                                                                  text_size = text_size, font = font, line_size = line_size,
                                                                  legend_position = legend_position, omit_legend = omit_legend,
                                                                  dnorm = dnorm, violin = violin,
                                                                  var1_log10 = var1_log10, var2_log10 = var2_log10,
                                                                  point_size = point_size, point_shape = point_shape,
                                                                  regression_line = regression_line,
                                                                  regression_se = regression_se,
                                                                  regression_method = regression_method,
                                                                  bar_position = bar_position, bar_width = bar_width,
                                                                  interactive = interactive, verbose = FALSE, basic = basic)
      )
    }

    p <- patchwork::wrap_plots(.pl, nrow = nrow, ncol = ncol, guides = guides, byrow = TRUE)

    if(class(title) != "waiver") {
      p <- patchwork::plot_annotation(title = title)
    }

    return(p)

  } else {
    if(missing(var2) && missing(group_var)) {
      .p_df <- dplyr::mutate(
        data.frame("var1" = .col_names),
        data_plot = trelliscopejs::map_plot(1:length(.col_names),
                                            function(i) rlang::exec("plot_var", data,
                                                                    var1 = .col_names[i],
                                                                    fill = fill, colour = colour,
                                                                    palette = palette, palette_direction = palette_direction,
                                                                    palette_begin = palette_begin, palette_end = palette_end,
                                                                    alpha = alpha, greyscale = greyscale, theme = theme,
                                                                    text_size = text_size, font = font, line_size = line_size,
                                                                    legend_position = legend_position, omit_legend = omit_legend,
                                                                    dnorm = dnorm, violin = violin,
                                                                    var1_log10 = var1_log10, var2_log10 = var2_log10,
                                                                    point_size = point_size, point_shape = point_shape,
                                                                    regression_line = regression_line,
                                                                    regression_se = regression_se,
                                                                    regression_method = regression_method,
                                                                    bar_position = bar_position, bar_width = bar_width,
                                                                    interactive = interactive, verbose = FALSE, basic = basic)
        )
      )
    } else if(!missing(var2) && missing(group_var)) {
      .p_df <- dplyr::mutate(
        data.frame("var1" = .col_names),
        data_plot = trelliscopejs::map_plot(1:length(.col_names),
                                            function(i) rlang::exec("plot_var", data,
                                                                    var1 = .col_names[i], var2 = var2, var2_lab = var2_lab,
                                                                    fill = fill, colour = colour,
                                                                    palette = palette, palette_direction = palette_direction,
                                                                    palette_begin = palette_begin, palette_end = palette_end,
                                                                    alpha = alpha, greyscale = greyscale, theme = theme,
                                                                    text_size = text_size, font = font, line_size = line_size,
                                                                    legend_position = legend_position, omit_legend = omit_legend,
                                                                    dnorm = dnorm, violin = violin,
                                                                    var1_log10 = var1_log10, var2_log10 = var2_log10,
                                                                    point_size = point_size, point_shape = point_shape,
                                                                    regression_line = regression_line,
                                                                    regression_se = regression_se,
                                                                    regression_method = regression_method,
                                                                    bar_position = bar_position, bar_width = bar_width,
                                                                    interactive = interactive, verbose = FALSE, basic = basic)
        )
      )
    } else if(missing(var2) && !missing(group_var)) {
      .p_df <- dplyr::mutate(
        data.frame("var1" = .col_names),
        data_plot = trelliscopejs::map_plot(1:length(.col_names),
                                            function(i) rlang::exec("plot_var", data,
                                                                    var1 = .col_names[i],
                                                                    group_var = group_var, fill = fill, colour = colour,
                                                                    palette = palette, palette_direction = palette_direction,
                                                                    palette_begin = palette_begin, palette_end = palette_end,
                                                                    alpha = alpha, greyscale = greyscale, theme = theme,
                                                                    text_size = text_size, font = font, line_size = line_size,
                                                                    legend_position = legend_position, omit_legend = omit_legend,
                                                                    dnorm = dnorm, violin = violin,
                                                                    var1_log10 = var1_log10, var2_log10 = var2_log10,
                                                                    point_size = point_size, point_shape = point_shape,
                                                                    regression_line = regression_line,
                                                                    regression_se = regression_se,
                                                                    regression_method = regression_method,
                                                                    bar_position = bar_position, bar_width = bar_width,
                                                                    interactive = interactive, verbose = FALSE, basic = basic)
        )
      )
    } else {
      .p_df <- dplyr::mutate(
        data.frame("var1" = .col_names),
        data_plot = trelliscopejs::map_plot(1:length(.col_names),
                                            function(i) rlang::exec("plot_var", data,
                                                                    var1 = .col_names[i], var2 = var2, var2_lab = var2_lab,
                                                                    group_var = group_var, fill = fill, colour = colour,
                                                                    palette = palette, palette_direction = palette_direction,
                                                                    palette_begin = palette_begin, palette_end = palette_end,
                                                                    alpha = alpha, greyscale = greyscale, theme = theme,
                                                                    text_size = text_size, font = font, line_size = line_size,
                                                                    legend_position = legend_position, omit_legend = omit_legend,
                                                                    dnorm = dnorm, violin = violin,
                                                                    var1_log10 = var1_log10, var2_log10 = var2_log10,
                                                                    point_size = point_size, point_shape = point_shape,
                                                                    regression_line = regression_line,
                                                                    regression_se = regression_se,
                                                                    regression_method = regression_method,
                                                                    bar_position = bar_position, bar_width = bar_width,
                                                                    interactive = interactive, verbose = FALSE, basic = basic)
        )
      )
    }
    if(missing(nrow) && missing(ncol)) {
      trelliscopejs::trelliscope(.p_df,
                                 name = paste(.df_name, "variable plots"))
    } else if(!missing(nrow) && missing(ncol)) {
      trelliscopejs::trelliscope(.p_df,
                                 name = paste(.df_name, "variable plots"),
                                 nrow = nrow)
    } else if(missing(nrow) && !missing(ncol)) {
      trelliscopejs::trelliscope(.p_df,
                                 name = paste(.df_name, "variable plots"),
                                 nrow = nrow)
    } else {
      trelliscopejs::trelliscope(.p_df,
                                 name = paste(.df_name, "variable plots"),
                                 nrow = nrow, ncol = ncol)
    }
  }
}

# plot_var_pairs ----------------------------------------------------------
#' @title
#'
#' Plot all variables/vectors in a data frame using a class-appropriate `plot_*`
#' function.
#'
#' @description Easily generate a matrix of ggplot2 graphs for all pairwise
#'   combinations of all (or a subset of) variables in a data frame using a
#'   class-appropriate geometry via other elucidate `plot_*` functions with a
#'   restricted set of customization options and some modified defaults. The
#'   collection of generated graphs will be combined into a single lattice-style
#'   matrix figure with either the patchwork package or trelliscopejs package.
#'   See "Arguments" section for details and
#'   \href{https://craig.rbind.io/post/2021-05-17-asgr-3-1-data-visualization/}{this
#'    blog post} for an introduction to ggplot2. To obtain a plot of a single
#'   variable or vector, use \code{\link{plot_var}} instead. To obtain
#'   univariate plots of all variables, or plots of all variables against a
#'   specific secondary variable, use \code{\link{plot_var_all}} instead.
#'
#' @importFrom ggplot2 waiver
#' @importFrom dplyr mutate
#' @importFrom rlang exec
#' @importFrom trelliscopejs map_plot
#' @importFrom trelliscopejs trelliscope
#' @importFrom patchwork wrap_plots
#' @importFrom patchwork plot_annotation
#'
#' @param data A data frame containing variables to be plotted against each
#'   other in pairwise/bivariate combinations.
#'
#' @param group_var Use if you want to assign a grouping variable to fill
#'   (colour) and/or (outline) colour e.g. group_var = "grouping_variable" or
#'   group_var = grouping_variable. Whether the grouping variable is mapped to
#'   fill, colour, or both will depend upon which `plot_*` function is used (See
#'   "Value" section). For density plots, both fill and colour are used for
#'   consistency across the main density plots and added normal density curve
#'   lines (if dnorm = TRUE). For bar graphs and box-and-whisker plots, the
#'   variable will be assigned to fill. For scatter plots, the variable will be
#'   assigned to colour. See \code{\link[ggplot2]{aes}} for details.
#'
#' @param cols A character (or integer) vector of column names (or indices)
#'   which allows you to generate pair plots only a subset of the columns in the
#'   input data frame, where each variable combinmation will be assigned once
#'   each of the `var1` and `var2` arguments of  \code{\link{plot_var}}. Note
#'   that a variable which has been assigned to `group_var` does not also need
#'   to be listed here.
#'
#' @param title A character string to add as a title at the top of the combined
#'   multiple-panel patchwork graph or trelliscopejs display.
#'
#' @param fill Fill colour to use for density plots, bar graphs, and box plots.
#'   Ignored if a variable that has been assigned to `group_var` is mapped on to
#'   `fill_var` (see `group_var` argument information above). Default is
#'   "blue2". Use \code{\link{colour_options}} to see colour option examples.
#'
#' @param colour Outline colour to use for density plots, bar graphs, box plots,
#'   and scatter plots. Ignored if a variable that has been assigned to
#'   `group_var` is mapped on to `colour_var` (see `group_var` argument
#'   information above). Default is "black". Use \code{\link{colour_options}} to
#'   see colour option examples.
#'
#' @param palette If a variable is assigned to group_var, this determines which
#'   \href{https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html}{viridis
#'    colour palette} to use. Options include "plasma" or "C" (default), "magma"
#'   or "A", "inferno" or "B", "viridis" or "D", and "cividis" or "E". See
#'   \href{https://craig.rbind.io/post/2021-05-17-asgr-3-1-data-visualization/#colourblind-friendly-palettes}{this
#'    link} for examples.
#'
#' @param palette_direction Choose "d2l" for dark to light (default) or "l2d"
#'   for light to dark.
#'
#' @param palette_begin Value between 0 and 1 that determines where along the
#'   full range of the chosen colour palette's spectrum to begin sampling
#'   colours. See \code{\link[ggplot2]{scale_fill_viridis_d}} for details.
#'
#' @param palette_end Value between 0 and 1 that determines where along the full
#'   range of the chosen colour palette's spectrum to end sampling colours. See
#'   \code{\link[ggplot2]{scale_fill_viridis_d}} for details.
#'
#' @param alpha This adjusts the transparency/opacity of the main geometric
#'   objects in the generated plot, with acceptable values ranging from 0 = 100%
#'   transparent to 1 = 100% opaque.
#'
#' @param greyscale Set to TRUE if you want the plot converted to grey scale.
#'
#' @param line_size Controls the thickness of plotted lines.
#'
#' @param theme Adjusts the theme using 1 of 6 predefined "complete" theme
#'   templates provided by ggplot2. Currently supported options are: "classic"
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
#' @param legend_position This allows you to modify the legend position if a
#'   variable is assigned to `group_var`. Options include "right" (the default),
#'   "left", "top", & "bottom".
#'
#' @param omit_legend Set to TRUE if you want to remove/omit the legend(s).
#'   Ignored if `group_var` is unspecified.
#'
#' @param dnorm When TRUE (default), this adds a dashed line representing a
#'   normal/Gaussian density curve to density plots, which are rendered for
#'   plots of single numeric variables. Disabled if `var1` is a date vector,
#'   `var1_log10` = TRUE, or `basic` = TRUE.
#'
#' @param violin When TRUE (default), this adds violin plot outlines to box
#'   plots, which are rendered in cases where a mixture of numeric and
#'   categorical variables are assigned to `var1` and `var2`. Disabled if
#'   `basic` = TRUE.
#'
#' @param var1_log10 If TRUE, applies a base-10 logarithmic transformation to a
#'   numeric variable that has been assigned to `var1`. Ignored if `var1` is a
#'   categorical variable.
#'
#' @param var2_log10 If TRUE, applies a base-10 logarithmic transformation to a
#'   numeric variable that has been assigned to `var2`. Ignored if `var2` is a
#'   categorical variable.
#'
#' @param point_size Controls the size of points used in scatter plots, which
#'   are rendered in cases where `var1` and `var2` are both numeric, integer, or
#'   date variables.
#'
#' @param point_shape Point shape to use in scatter plots, which
#'   are rendered in cases where `var1` and `var2` are both numeric, integer, or
#'   date variables.
#'
#' @param regression_line If TRUE (the default), adds a regression line to scatter
#'   plots, which are rendered in cases where `var1` and `var2` are both
#'   numeric, integer, or date variables. Disabled if `basic` = TRUE.
#'
#' @param regression_method If `regression_line` = TRUE, this determines the
#'   type of regression line to use. Currently available options are "gam",
#'   "loess", and "lm". "gam" is the default, which fits a generalized additive
#'   model using a smoothing term for x. This method has a longer run time, but
#'   typically provides a better fit to the data than other options and uses an
#'   optimization algorithm to determine the optimal wiggliness of the line. If
#'   the relationship between y and x is linear, the output will be equivalent
#'   to fitting a linear model. "loess" may be preferable to "gam" for small
#'   sample sizes. See \code{\link[ggplot2]{stat_smooth}} and
#'   \code{\link[mgcv]{gam}} for details.
#'
#' @param regression_se If TRUE (the default), adds a 95% confidence envelope for the
#'   regression line. Ignored if `regression_line` = FALSE.
#'
#' @param bar_position In bar plots, which are rendered for one or more
#'   categorical variables, this determines how bars are arranged relative to
#'   one another when a grouping variable is assigned to `group_var`. The
#'   default, "dodge", uses \code{\link[ggplot2]{position_dodge}} to arrange
#'   bars side-by-side; "stack" places the bars on top of each other; "fill"
#'   also stacks bars but additionally converts y-axis from counts to
#'   proportions.
#'
#' @param bar_width In bar plots, which are rendered for one or more categorical
#'   variables, this adjusts the width of the bars (default = 0.9).
#'
#' @param basic This is a shortcut argument that allows you to simultaneously
#'   disable the `dnorm`, `violin`, and `regression_line` arguments to produce a
#'   basic version of a density, box, or scatter plot (depending on
#'   `var1`/`var2` variable class(es)) without any of those additional layers.
#'   Dropping these extra layers may noticeably reduce rendering time and memory
#'   utilization, especially for larger sample sizes and/or when `interactive` =
#'   TRUE.
#'
#' @param interactive Determines whether a static ggplot object or an
#'   interactive html plotly object is returned. Interactive/plotly mode for
#'   multiple plots should only be used in conjunction with `trelliscope` =
#'   TRUE. See \code{\link[plotly]{ggplotly}} for details. Note that in cases
#'   where a box plot is generated (for a mix of numeric and categorical
#'   variables) and a variable is also assigned to `group_var`, activating
#'   interactive/plotly mode will cause a spurious warning message about
#'   'layout' objects not having a 'boxmode' attribute to be printed to the
#'   console. This is a
#'   \href{https://github.com/ropensci/plotly/issues/994}{documented bug} with
#'   plotly that can be safely ignored, although unfortunately the message
#'   cannot currently be suppressed.
#'
#' @param trelliscope If changed to TRUE, plots will be combined into an
#'   interactive trelliscope display rather than a static patchwork graph grid.
#'   See \code{\link[trelliscopejs]{trelliscope}} for more information.
#'
#' @param nrow This controls the number of rows to use when arranging plots in
#'   the combined patchwork or trelliscopejs display. Modifying the arrangement
#'   of the plot matrix this way is not recommended when `trelliscope` = FALSE.
#'
#' @param ncol This controls the number of columns to use when arranging plots
#'   in the combined patchwork or trelliscopejs display. Modifying the arrangement
#'   of the plot matrix this way is not recommended when `trelliscope` = FALSE.
#'
#' @param guides Controls the pooling of `group_var` legends/guides across plot
#'   panels if a categorical variable has been assigned to `group_var` and
#'   `trelliscope` = FALSE. See \code{\link[patchwork]{wrap_plots}} for details.
#'
#' @return A static "patchwork" or dynamic "trelliscope" multi-panel graphical
#'   display matrix of ggplot2 or plotly graphs depending upon the values of the
#'   `trelliscope` and `interactive` arguments. The type of graph (i.e.
#'   `ggplot2::geom*` layers) that is rendered in each panel will depend upon
#'   the classes of the chosen variables, as follows:
#'
#'   - One numeric (classes numeric/integer/date) variable (e.g. on the diagonal
#'   of the plot matrix) will be graphed with \code{\link{plot_density}}.
#'
#'   - One (e.g. on the diagonal) or two (appearing off-diagonal) categorical
#'   (classes factor/character/logical) variable(s) will be graphed with
#'   \code{\link{plot_bar}}.
#'
#'   - Two numeric variables will be graphed with \code{\link{plot_scatter}}.
#'
#'   - A mixture of numeric and categorical variables will be graphed with
#'   \code{\link{plot_box}}, where the numeric variable will always be assigned
#'   to the y-axis.
#'
#' @author Craig P. Hutton, \email{Craig.Hutton@@gov.bc.ca}
#'
#' @examples
#'
#' data(mtcars) #load the mtcars data
#'
#' # convert variables "cyl" to a factors
#' mtcars$cyl <- as.factor(mtcars$cyl)
#'
#' # plot all pairwise combinations of variables "mpg", "hp", and "cyl"
#' plot_var_pairs(mtcars, cols = c("mpg", "hp", "cyl"))
#'
#' # render basic versions of the same plots without normal density curves,
#' # violin plots, or regression lines added.
#' plot_var_pairs(mtcars, cols = c("mpg", "hp", "cyl"), basic = TRUE)
#'
#' @references
#' Wickham, H. (2016). ggplot2: elegant graphics for data analysis. New York, N.Y.: Springer-Verlag.
#'
#' @seealso \code{\link{plot_density}}, \code{\link{plot_bar}},
#'   \code{\link{plot_scatter}}, \code{\link{plot_box}}, \code{\link{plot_var}},
#'   \code{\link{plot_var_all}}, \code{\link[patchwork]{wrap_plots}},
#'   \code{\link[trelliscopejs]{trelliscope}}
#'
#' @export
plot_var_pairs <- function(data, group_var = NULL, cols = NULL, title = ggplot2::waiver(),
                           fill = "blue2", colour = "black",
                           palette = c("plasma", "C", "magma", "A", "inferno", "B", "viridis", "D", "cividis", "E"),
                           palette_direction = c("d2l", "l2d"), palette_begin = 0, palette_end = 0.8,
                           alpha = 0.75, greyscale = FALSE, line_size = 1,
                           theme = "classic", text_size = 14, font = c("sans", "serif", "mono"),
                           legend_position = c("right", "left", "top", "bottom"), omit_legend = FALSE,
                           dnorm = TRUE, violin = TRUE, var1_log10 = FALSE, var2_log10 = FALSE,
                           point_size = 2, point_shape = c("circle", "square", "diamond", "triangle up", "triangle down"),
                           regression_line = TRUE, regression_method = c("gam", "loess", "lm"), regression_se = TRUE,
                           bar_position = c("dodge", "fill", "stack"), bar_width = 0.9, basic = FALSE,
                           interactive = FALSE, trelliscope = FALSE, nrow = NULL, ncol = NULL, guides = "collect") {

  theme <- match.arg(theme)
  palette <- match.arg(palette)
  palette_direction <- match.arg(palette_direction)
  font = match.arg(font)
  legend_position = match.arg(legend_position)
  bar_position = match.arg(bar_position)
  point_shape <- match.arg(point_shape)
  regression_method <- match.arg(regression_method)

  if(!missing(group_var)) {
    if(is.error(class(data[[group_var]]))) {
      group_var <- deparse(substitute(group_var))
    } else if(!is.character(group_var) || length(group_var) > 1){
      stop('If specified, `group_var` must be a single symbol or character string',
           '\n representing a column in the input data frame supplied to the `data` argument.')
    }
  }
  if(!missing(cols)) {
    .col_names <- names(data[, cols])
  } else {
    .col_names <- names(data)
  }

  .ncols <- length(.col_names)
  .comp_df <- expand.grid("v1" = .col_names, "v2" = .col_names, stringsAsFactors = FALSE)
  .v1 <- .comp_df[["v1"]]
  .v2 <- .comp_df[["v2"]]

  if(trelliscope == FALSE) {
    if(missing(group_var)) {
      .pl <- lapply(1:length(.v1),
                    function(i) rlang::exec("plot_var", data,
                                            var1 = .v1[i], var2 = .v2[i],
                                            fill = fill, colour = colour,
                                            palette = palette, palette_direction = palette_direction,
                                            palette_begin = palette_begin, palette_end = palette_end,
                                            alpha = alpha, greyscale = greyscale, theme = theme,
                                            text_size = text_size, font = font, line_size = line_size,
                                            legend_position = legend_position, omit_legend = omit_legend,
                                            dnorm = dnorm, violin = violin,
                                            var1_log10 = var1_log10, var2_log10 = var2_log10,
                                            point_size = point_size, point_shape = point_shape,
                                            regression_line = regression_line,
                                            regression_se = regression_se,
                                            regression_method = regression_method,
                                            bar_position = bar_position, bar_width = bar_width,
                                            interactive = interactive, verbose = FALSE, basic = basic)
      )
    } else {
      .pl <- lapply(1:length(.v1),
                    function(i) rlang::exec("plot_var", data,
                                            var1 = .v1[i], var2 = .v2[i],
                                            group_var = group_var,
                                            fill = fill, colour = colour,
                                            palette = palette, palette_direction = palette_direction,
                                            palette_begin = palette_begin, palette_end = palette_end,
                                            alpha = alpha, greyscale = greyscale, theme = theme,
                                            text_size = text_size, font = font, line_size = line_size,
                                            legend_position = legend_position, omit_legend = omit_legend,
                                            dnorm = dnorm, violin = violin,
                                            var1_log10 = var1_log10, var2_log10 = var2_log10,
                                            point_size = point_size, point_shape = point_shape,
                                            regression_line = regression_line,
                                            regression_se = regression_se,
                                            regression_method = regression_method,
                                            bar_position = bar_position, bar_width = bar_width,
                                            interactive = interactive, verbose = FALSE, basic = basic)
      )
    }
    if(missing(nrow) && missing(ncol)) {
      p <- patchwork::wrap_plots(.pl, ncol = .ncols, nrow = .ncols, guides = guides, byrow = TRUE)
      if(class(title) != "waiver") {
        p <- p + patchwork::plot_annotation(title = title)
      }
    } else if(!missing(nrow) && missing(ncol)) {
      p <- patchwork::wrap_plots(.pl, ncol = nrow, nrow = .ncols, guides = guides, byrow = TRUE)
      if(class(title) != "waiver") {
        p <- p + patchwork::plot_annotation(title = title)
      }
    } else if(missing(nrow) && !missing(ncol)) {
      p <- patchwork::wrap_plots(.pl, ncol = .ncols, nrow = ncol, guides = guides, byrow = TRUE)
      if(class(title) != "waiver") {
        p <- p + patchwork::plot_annotation(title = title)
      }
    } else {
      p <- patchwork::wrap_plots(.pl, ncol = ncol, nrow = ncol, guides = guides, byrow = TRUE)
      if(class(title) != "waiver") {
        p <- p + patchwork::plot_annotation(title = title)
      }
    }
    return(p)
  } else {
    if(missing(group_var)) {
      .p_df <- dplyr::mutate(.comp_df,
                             data_plot = trelliscopejs::map_plot(1:length(.v1),
                                                                 function(i) rlang::exec("plot_var", data,
                                                                                         var1 = .v1[i], var2 = .v2[i],
                                                                                         fill = fill, colour = colour,
                                                                                         palette = palette, palette_direction = palette_direction,
                                                                                         palette_begin = palette_begin, palette_end = palette_end,
                                                                                         alpha = alpha, greyscale = greyscale, theme = theme,
                                                                                         text_size = text_size, font = font, line_size = line_size,
                                                                                         legend_position = legend_position, omit_legend = omit_legend,
                                                                                         dnorm = dnorm, violin = violin,
                                                                                         var1_log10 = var1_log10, var2_log10 = var2_log10,
                                                                                         point_size = point_size, point_shape = point_shape,
                                                                                         regression_line = regression_line,
                                                                                         regression_se = regression_se,
                                                                                         regression_method = regression_method,
                                                                                         bar_position = bar_position, bar_width = bar_width,
                                                                                         interactive = interactive, verbose = FALSE, basic = basic)
                             )
      )
    } else {
      .p_df <- dplyr::mutate(.comp_df,
                             data_plot = trelliscopejs::map_plot(1:length(.v1),
                                                                 function(i) rlang::exec("plot_var", data,
                                                                                         var1 = .v1[i], var2 = .v2[i],
                                                                                         group_var = group_var,
                                                                                         fill = fill, colour = colour,
                                                                                         palette = palette, palette_direction = palette_direction,
                                                                                         palette_begin = palette_begin, palette_end = palette_end,
                                                                                         alpha = alpha, greyscale = greyscale, theme = theme,
                                                                                         text_size = text_size, font = font, line_size = line_size,
                                                                                         legend_position = legend_position, omit_legend = omit_legend,
                                                                                         dnorm = dnorm, violin = violin,
                                                                                         var1_log10 = var1_log10, var2_log10 = var2_log10,
                                                                                         point_size = point_size, point_shape = point_shape,
                                                                                         regression_line = regression_line,
                                                                                         regression_se = regression_se,
                                                                                         regression_method = regression_method,
                                                                                         bar_position = bar_position, bar_width = bar_width,
                                                                                         interactive = interactive, verbose = FALSE, basic = basic)
                             )
      )
    }
    if(missing(nrow) && missing(ncol)) {
      if(class(title) == "waiver") {
        trelliscopejs::trelliscope(.p_df,
                                   name = "bivariate comparison plots")
      } else {
        trelliscopejs::trelliscope(.p_df,
                                   name = title)
      }
    } else if(!missing(nrow) && missing(ncol)) {
      if(class(title) == "waiver") {
        trelliscopejs::trelliscope(.p_df,
                                   name = "bivariate comparison plots",
                                   nrow = nrow)
      } else {
        trelliscopejs::trelliscope(.p_df,
                                   name = title,
                                   nrow = nrow)
      }
    } else if(missing(nrow) && !missing(ncol)) {
      if(class(title) == "waiver") {
        trelliscopejs::trelliscope(.p_df,
                                   name = "bivariate comparison plots",
                                   ncol = ncol)
      } else {
        trelliscopejs::trelliscope(.p_df,
                                   name = title,
                                   ncol = ncol)
      }
    } else {
      if(class(title) == "waiver") {
        trelliscopejs::trelliscope(.p_df,
                                   name = "bivariate comparison plots",
                                   nrow = nrow, ncol = ncol)
      } else {
        trelliscopejs::trelliscope(.p_df,
                                   name = title,
                                   nrow = nrow, ncol = ncol)
      }
    }
  }
}

# plot_c ------------------------------------------------------------------
#' @title
#'
#' Combine plots into a multi-panel display with the `patchwork` or
#' `trelliscopejs` packages.
#'
#' @description Easily combine an arbitrary number of plots into a multi-panel
#'   display using either \code{\link[patchwork]{wrap_plots}} for a single
#'   static display or \code{\link[trelliscopejs]{trelliscope}} for an
#'   interactive display. The static (`patchwork`) output option (trelliscope =
#'   FALSE) is useful if you intend to produce a multi-panel graph for printing
#'   or inclusion in a static document. The interactive (`trelliscopejs`) output
#'   option (trelliscope = TRUE) is useful when you want to combine too many
#'   plots to render them all legibly on the screen or if you want to combine
#'   `plotly` graphs such as those generated by the "interactive" mode of other
#'   elucidate `plot_*` functions.
#'
#' @importFrom patchwork wrap_plots
#' @importFrom trelliscopejs trelliscope
#' @importFrom tibble tibble
#
#' @param ... Any number of plots to combine into a multi-panel display. Also
#'   accepts a (single) list of plots. Set trelliscope = TRUE if any of the
#'   plots are plotly objects.
#'
#' @param ncol The number of columns to use when arranging plot panels.
#'
#' @param nrow The number of rows to use when arranging plot panels.
#'
#' @param guides IF trelliscope = FALSE, this determines how
#'   \code{\link[patchwork]{wrap_plots}} handles guides/legends across graphical
#'   panels. If set to "collect" (default), redundant guides will be collected
#'   such that only a single copy of each unique guide is shown. See
#'   \code{\link[patchwork]{wrap_plots}} for details.
#'
#' @param trelliscope Set this to TRUE if you want to combine plots into an
#'   interactive trelliscope display with
#'   \code{\link[trelliscopejs]{trelliscope}}, otherwise plots will be combined
#'   into a static display using \code{\link[patchwork]{wrap_plots}}.
#'
#' @return A static "patchwork" or dynamic "trelliscope" multi-panel graphical
#'   display depending upon the value of the "trelliscope" argument.
#'
#' @author Craig P. Hutton, \email{Craig.Hutton@@gov.bc.ca}
#'
#' @examples
#'
#' #create a few basic plots with elucidate::plot_* functions
#' p1 <- plot_density(pdata, y1)
#' p2 <- plot_bar(pdata, g)
#' p3 <- plot_histogram(pdata, y2)
#'
#' #combine them into a static multi-panel display with plot_c()
#' plot_c(p1, p2, p3)
#'
#' \donttest{
#' #combine them into a dynamic multi-panel display
#' #by setting the trelliscope argument to TRUE
#' plot_c(p1, p2, p3, trelliscope = TRUE)
#' }
#'
#'
#' @seealso \code{\link[patchwork]{wrap_plots}}, \code{\link[trelliscopejs]{trelliscope}}
#'
#' @export
plot_c <- function(..., nrow = NULL, ncol = NULL, guides = c("collect", "auto", "keep"), trelliscope = FALSE) {
  guides <- match.arg(guides)
  .classes <- lapply(list(...), class)
  if("list" %in% .classes && length(.classes) == 1) {
    if(trelliscope == FALSE) {
      p <- patchwork::wrap_plots(..., nrow = nrow, ncol = ncol, guides = guides)
      return(p)
    } else {
      .p_names <- names(...)
      if(is.null(.p_names)) {
        .p_names <- paste0("plot_", seq_along(...))
      }

      .p_df <- tibble::tibble("plot" = .p_names, "panel" = unlist(list(...), recursive = FALSE))

      if(missing(nrow) && missing(ncol)) {
        trelliscopejs::trelliscope(.p_df, panel_col = "panel", name = "trelliscope display")
      } else if(!missing(nrow) && missing(ncol)) {
        trelliscopejs::trelliscope(.p_df, panel_col = "panel", name = "trelliscope display",
                                   nrow = nrow)
      } else if(missing(nrow) && !missing(ncol)) {
        trelliscopejs::trelliscope(.p_df, panel_col = "panel", name = "trelliscope display",
                                   nrow = nrow)
      } else {
        trelliscopejs::trelliscope(.p_df, panel_col = "panel", name = "trelliscope display",
                                   nrow = nrow, ncol = ncol)
      }
    }
  } else {
    if(trelliscope == FALSE) {
      p <- patchwork::wrap_plots(list(...), nrow = nrow, ncol = ncol, guides = guides)
      return(p)
    } else {
      .p_names <- gsub(" ", "", unlist(strsplit(deparse(substitute(list(...))), "[(,)]")))[-1]

      .p_df <- tibble::tibble("plot" = .p_names, "panel" = list(...))

      if(missing(nrow) && missing(ncol)) {
        trelliscopejs::trelliscope(.p_df, panel_col = "panel", name = "trelliscope display")
      } else if(!missing(nrow) && missing(ncol)) {
        trelliscopejs::trelliscope(.p_df, panel_col = "panel", name = "trelliscope display",
                                   nrow = nrow)
      } else if(missing(nrow) && !missing(ncol)) {
        trelliscopejs::trelliscope(.p_df, panel_col = "panel", name = "trelliscope display",
                                   nrow = nrow)
      } else {
        trelliscopejs::trelliscope(.p_df, panel_col = "panel", name = "trelliscope display",
                                   nrow = nrow, ncol = ncol)
      }
    }
  }
}

