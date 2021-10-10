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
  structure(list(label = label, side  = "top"), class = "edit_facet")
}


extract_strip_label <- function(facet, plot, labeller=NULL){
  layout <- facet$compute_layout(list(plot$data),
                                 c(plot$facet$params,
                                   list(.possible_columns=names(plot$data)),
                                   plot_env = plot$plot_env))
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


#' @importFrom ggplot2 ggplot_add
#' @method ggplot_add edit_facet
#' @export
ggplot_add.edit_facet <- function(object, plot, object_name){
  if (object$side == 'right' && is.null(object$angle)) {
    object$angle <- -90
  }
  plot <- build_new_plot(object=object, plot=plot)
  return(plot)
}

build_new_plot <- function(object, plot){
  flag.params <- TRUE
  if (!inherits(plot$facet, "FacetNull")){
    if (inherits(object$label, "labeller") || !is.null(names(object$label))){
      facet.fun <- eval(parse(text=class(plot$facet)[1]))
      facet.obj <- ggplot2::ggproto(NULL,
                                    facet.fun,
                                    shrink = plot$facet$shrink,
                                    params = plot$facet$params
      )
      if (!is.null(plot$facet$strip)){
        facet.obj$strip <- plot$facet$strip
      }
      strip.labels <- extract_strip_label(facet=facet.fun, plot=plot)
      if (inherits(object$label, "labeller")){
        tmp.label <- extract_strip_label(facet=facet.fun, plot=plot, labeller=object$label)
        names(tmp.label) <- names(strip.labels)
        object$label <- tmp.label[!is.na(tmp.label)]
      }
      newnm <- intersect(names(object$label), names(strip.labels))
      if (length(newnm) > 0){
        strip.labels[match(newnm, names(strip.labels))] <- object$label[match(newnm, names(object$label))]
      }
      facet.obj$params$labeller <- ggplot2::as_labeller(strip.labels)
      flag.params <- FALSE
    }else{
      plot <- ggplotify::as.ggplot(plot)
    }
  }
  if (flag.params){
    lb <- paste0("'", eval(object$label[1]), "'")
    if (object$side == 'top') {
      params <- list(paste0('~', lb))
    } else {
      params <- list(paste0(lb, '~.'))
    }
  }else{
    params <- NULL
  }
  if (!is.null(params)){
    facet.layer <- do.call("facet_grid", params)
    th <- theme(strip.background = element_rect(fill='grey85', colour = NA),
                strip.text = element_text(colour = 'grey10',
                                          size = rel(0.8),
                                          angle = object$angle,
                                          margin = margin(4.4, 4.4, 4.4, 4.4)))
    plot <- plot + facet.layer + th
  }else{
    plot <- plot + facet.obj
  }
  return (plot)
}


