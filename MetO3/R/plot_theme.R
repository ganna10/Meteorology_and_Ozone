#' Common Theme element
#' 
#' The common theme items for my plots
#' @return List of items included in my theme
#' @export

plot_theme <- function () {
  list(theme_tufte(),
       theme(axis.line = element_line(colour = "black")),
       theme(strip.text = element_text(face = "bold")),
       theme(strip.text.y = element_text(angle = 0)),
       theme(plot.title = element_text(face = "bold")),
       theme(axis.title = element_text(face = "bold")),
       theme(axis.line.y = element_line(colour = "black")),
       theme(axis.line.x = element_line(colour = "black"))
  )
}