#' This function is adapted from ggfun package of facet_set function
#'
#' @param label the text to ba edited
#'
#' @return a ggplot2 pacakge with modified facet label
#' @export
#'
#' @author ZhonghuiGai
#' @examples
#' install.packages("ggfun")
#' p <- ggplot(mtcars, aes(disp, drat)) + geom_point() + facet_wrap(~am)
#' p + edit_facet_label(label = c(`0` = "Zero\n p = 0.01\n(mg/L)"))
edit_facet_label <- function(label){
  structure(list(label = label, side  = "top"), class = "facet_set")
}




