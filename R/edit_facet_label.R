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


extract_strip_label <- function(facet, plot, labeller=NULL){
  layout <- facet$compute_layout(list(plot$data),
                                 c(plot$facet$params,
                                   list(.possible_columns=names(plot$data)),
                                   plot_env = plot$plot_env
                                 )
  )
  label_df <- layout[names(c(plot$facet$params$facet,
                             plot$facet$params$cols,
                             plot$facet$params$rows))]
  if (is.null(labeller)){
    labels <- lapply(plot$facet$params$labeller(label_df), cbind)
  }else{
    labels <- lapply(labeller(label_df), cbind)
  }
  labels <- do.call("cbind", labels)
  labels <- unique(as.vector(labels))
  names(labels) <- labels
  return(labels)
}



