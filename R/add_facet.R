#' Add a facet label to a ggplot object which only contains 1 panel
#'
#' @param p the ggplot2 object to be added
#' @param label the text to be added as a facet label
#' @param fill the strip background color
#' @param color the color of the text
#' @param size the size of the text, default size is 14
#' @param face the font face of the text
#' @param r the radius of the rounded corners, a \code{unit} object,
#' default is unit(0.1, 'snpc').
#'
#' @return a ggplot2 object with a facet label
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' p <- ggplot(mtcars, aes(disp, drat)) + geom_point()
#' ggfacet::add_facet(p, label = "test\n (mg/L)\n p = 0.01")
add_facet <- function(p, label,
                      fill = "#7fc97f",
                      color = "black",
                      size = 14,
                      face = "bold",
                      r = 0.25) {
  lb <- paste0("'", eval(label), "'")
  lb <- paste0("~", lb)
  p <- p + facet_grid(eval(parse(text = lb))) +
    theme(strip.background = element_rect(fill = fill, color = NA),
          strip.text = element_text(color = color,
                                    size = size,
                                    angle = 0,
                                    face = face))
  p <- p + theme(strip.background = ggfun::element_roundrect(r = r))
  return(p)
}
