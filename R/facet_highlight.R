#' @title Highlight the facet using different color and background powered by ggtext package
#'
#' forked from https://stackoverflow.com/questions/60332202/conditionally-fill-ggtext-text-boxes-in-facet-wrap
#'
#' @param ... other paras
#' @param hi.labels which facet to be highlight
#' @param hi.fill the color of the background
#' @param hi.col  the color of the text
#' @param hi.box.col the color of the box
#' @param hi.labels2 which facet to be highlight 2
#' @param hi.fill2 the color of the background 2
#' @param hi.col2 the color of the text 2
#' @param hi.box.col2 the color of the box 2
#'
#' @return
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mpg, aes(cty, hwy)) +
#' geom_point() +
#'   facet_wrap(~class) +
#'   ggtheme::theme_pub() +
#'   theme(
#'     strip.background = element_blank(),
#'     strip.text = element_textbox_highlight(
#'       size = 12, face = "bold",
#'       # unnamed set (all facet windows except named sets below)
#'       color = "white", fill = "#5D729D", box.color = "#4A618C",
#'       halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
#'       padding = margin(5, 0, 3, 0), margin = margin(3, 3, 3, 3),
#'       # this is new relative to element_textbox():
#'       # first named set
#'       hi.labels = c("minivan", "suv"),
#'       hi.fill = "#F89096", hi.box.col = "#A6424A", hi.col = "black",
#'       # add second named set
#'       hi.labels2 = c("compact", "pickup"),
#'       hi.fill2 = "green", hi.box.col2 = "#A6424A", hi.col2 = "black"
#'    )
#'   )
#'
#'   ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
#' geom_point() + facet_grid(~Species) +
#'   theme(
#'     strip.background = element_blank(),
#'     strip.text = element_textbox_highlight(
#'       size = 12, face = "bold",
#'       # unnamed set (all facet windows except named sets below)
#'       color = "white", fill = "#5D729D", box.color = "#4A618C",
#'       halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
#'       padding = margin(5, 0, 3, 0), margin = margin(3, 3, 3, 3),
#'       # this is new relative to element_textbox():
#'       # first named set
#'      hi.labels = c("versicolor"),
#'       hi.fill = "#F89096", hi.box.col = "#A6424A", hi.col = "black",
#'       # add second named set
#'       hi.labels2 = c("virginica"),
#'       hi.fill2 = "green", hi.box.col2 = "#A6424A", hi.col2 = "black"
#'     )
#'   )
element_textbox_highlight <- function(...,
                                      hi.labels = NULL, hi.fill = NULL,
                                      hi.col = NULL, hi.box.col = NULL,
                                      hi.labels2 = NULL, hi.fill2 = NULL,
                                      hi.col2 = NULL, hi.box.col2 = NULL) {
  structure(
    c(ggtext::element_textbox(...),
      list(hi.labels = hi.labels, hi.fill = hi.fill, hi.col = hi.col, hi.box.col = hi.box.col,
           hi.labels2 = hi.labels2, hi.fill2 = hi.fill2, hi.col2 = hi.col2, hi.box.col2 = hi.box.col2)
    ),
    class = c("element_textbox_highlight", "element_textbox", "element_text", "element")
  )
}

#' @method element_grob element_textbox_highlight
#' @export

element_grob.element_textbox_highlight <- function(element, label = "", ...) {
  if (label %in% element$hi.labels) {
    element$fill <- element$hi.fill %||% element$fill
    element$colour <- element$hi.col %||% element$colour
    element$box.colour <- element$hi.box.col %||% element$box.colour
  }
  if (label %in% element$hi.labels2) {
    element$fill <- element$hi.fill2 %||% element$fill
    element$colour <- element$hi.col2 %||% element$colour
    element$box.colour <- element$hi.box.col2 %||% element$box.colour
  }
  NextMethod()
}

`%||%` <- function (a, b) {
  if (!is.null(a)) a else b
}
