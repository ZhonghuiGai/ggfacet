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
#' round_facet_strip(p, r = 0.25)
round_facet_border <- function(p, r = 0.2){
  p1 <- p + theme(strip.background = ggfun::element_roundrect(r = r))
  return(p1)
}

