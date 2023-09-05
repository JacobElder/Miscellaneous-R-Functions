#' Complete list of palettes
#'
#' Use \code{\link{vg_palette}} to construct palettes of desired length.
#'
#' @export
vg_palettes <- list(
  VG1 = c("#1BA39C", "#11635F", "#BF8200", "#717777", "#0072CE", "#0032A0"),
  VG2 = c("#1BA39C", "#11635F", "#BF8200", "#660026", "#717777", "#0072CE"),
  VG3 = c("#1BA39C", "#11635F", "#8DD1CE", "#BF8200", "#FFAD00", "#660026", "#B27F92", "#B3B4B4", "#717777", "#80B8EF", "#0072CE", "#0032A0"),
  VG4 = c("#57BE5B", "#8FD392", "#FFD780", "#FB808F", "#F84056")

)

#' A Wes Anderson palette generator
#'
#' These are a handful of color palettes from Wes Anderson movies.
#'
#' @param n Number of colors desired.
#' @param name Name of desired palette. Choices are:
#'   \code{VG1}, \code{VG2},  \code{VG3},
#'   \code{VG4}
#' @param type Either "continuous" or "discrete". Use continuous if you want
#'   to automatically interpolate between colours.
#'   @importFrom graphics rgb rect par image text
#' @return A vector of colours.
#' @export
#' @keywords colors
#' # If you need more colours than normally found in a palette, you
#' # can use a continuous palette to interpolate between existing
#' # colours
#' pal <- wes_palette(21, name = "Zissou1", type = "continuous")
#' image(volcano, col = pal)
vg_palette <- function(name, n, type = c("discrete", "continuous")) {
  type <- match.arg(type)
  
  pal <- vg_palettes[[name]]
  if (type == "continuous" && name == "VG1") {
    pal <- vg_palettes[["VG1"]]
  }
  
  if (is.null(pal))
    stop("Palette not found.")
  
  if (missing(n)) {
    n <- length(pal)
  }
  
  if (type == "discrete" && n > length(pal)) {
    stop("Number of requested colors greater than what palette can offer")
  }
  
  out <- switch(type,
                continuous = grDevices::colorRampPalette(pal)(n),
                discrete = pal[1:n]
  )
  structure(out, class = "palette", name = name)
}

#' @export
#' @importFrom graphics rect par image text
#' @importFrom grDevices rgb
print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))
  
  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")
  
  rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 1, family = "serif")
}

#' heatmap
#'
#' A heatmap example
"heatmap"


## THEME


vg_theme_colors <- c(
  text    = '#040505',
  panel   = '#DE002F',
  border  = '#C20029',
  lighter = "#DE002F",
  light   = '#F6001E',
  medium  = '#C20029',
  dark    = '#660026',
  point  = '#660026'
)



# THEME: rename function and theme() arguments according to your theme design, feel free to edit this how you would like

theme_VG <- function(vg_font = TRUE, ...){
  
  # CUSTOM FONT: add a custom font from google fonts
  font_family = "sans"
  
  # CUSTOM THEME:
  ggplot2::theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    text = element_text(color = vg_theme_colors["text"], family = font_family),
    title = element_text(size=20),
    panel.background = element_rect(fill = vg_theme_colors["panel"]),
    panel.border = element_rect(fill = NA, color = vg_theme_colors["border"],linewidth=1.2),
    axis.title = element_text(size=17),
    axis.text = element_text(size=13,color = vg_theme_colors["text"]),
    axis.ticks = element_line(color = vg_theme_colors["border"],linewidth=1),
    legend.background = element_rect(fill = vg_theme_colors["panel"], color = NA),
    strip.background = element_rect(fill = vg_theme_colors["lighter"], colour = vg_theme_colors["border"]),
    strip.text = element_text(colour = vg_theme_colors["text"]),
    ...
  )
}

# COLOR SCALES: Make pretty color scales
#'
#' library(ggplot2)
#'
scale_fill_vg <- function(...) {
  ggplot2::scale_fill_gradient(low = '#DE002F', high = '#660026')
}

#' @rdname scale_barbie
#' @export
scale_color_vg <- function(...) {
  ggplot2::scale_color_gradient(low = '#DE002F', high = '#660026')
}

