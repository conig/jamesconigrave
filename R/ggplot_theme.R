#' theme_pub
#'

#' A theme for ggplot2
#' @export

theme_pub <- function(){

  ggplot2::theme_bw() +
  ggplot2::theme(
    text = ggplot2::element_text(family = "serif"),
    strip.text.y = ggplot2::element_text(angle = 0),
    strip.background.y = ggplot2::element_blank()
  )

}



