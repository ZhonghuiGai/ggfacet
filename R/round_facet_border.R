#' Round rectangle border of facet. A swap function of ggfun
#'
#' @param r the radius of the rounded corners, a \code{unit} object,
#' default is unit(0.1, 'snpc').
#'
#' @return
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' p <- ggplot(mpg, aes(displ, cty)) + geom_point() + facet_grid(cols = vars(cyl))
#' round_facet_border(p, r = 0.25)
round_facet_border <- function(p, r = 0.2){
  p1 <- p + theme(strip.background = element_roundrect(r = r))
  return(p1)
}

element_roundrect <- function(fill = NULL, colour = NULL, size = NULL,
                              linetype = NULL, color = NULL, r=grid::unit(0.1, "snpc"), inherit.blank = FALSE) {
  if (!is.null(color))  colour <- color
  if (!grid::is.unit(r)) r <- grid::unit(r, 'snpc')
  structure(
    list(fill = fill,
         colour = colour,
         size = size,
         linetype = linetype,
         r = r,
         inherit.blank = inherit.blank),
    class = c("element_roundrect", "element_rect", "element")
  )
}

#' @importFrom ggplot2 element_grob
#' @method element_grob element_roundrect
#' @export
element_grob.element_roundrect <- function(element,
                                           x = 0.5, y = 0.5, width = 1, height = 1,
                                           fill = NULL, colour = NULL, size = NULL, linetype = NULL,
                                           ...) {
  # step 1 make a function for step 2
  len0_null <- function (x){if (length(x) == 0) NULL else x}
  # step 2 calculate the new and old par
  gp <- grid::gpar(lwd = len0_null(size * .pt),
                   col = colour, fill = fill, lty = linetype)
  element_gp <- grid::gpar(lwd = len0_null(element$size * .pt), col = element$colour,
                           fill = element$fill, lty = element$linetype)
  # step 3 make a function for step 3
  modify_list <- function (old, new){for (i in names(new)) old[[i]] <- new[[i]]; old}
  # step 4 a new function
  grid::roundrectGrob(x, y, width, height, r = element$r, gp = modify_list(element_gp, gp), ...)
}





