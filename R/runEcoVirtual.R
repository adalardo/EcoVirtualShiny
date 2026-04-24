#' @import shiny
#' @import EcoVirtual
NULL
#' @export
runEcoVirtual <- function() {
  www_path <- system.file("www", package = "EcoVirtualShiny")
  if (www_path != "") {
    shiny::addResourcePath(
      prefix = 'www', 
      directoryPath = www_path
    )
  }
    shiny::shinyApp(
    ui = ecoVirtualUI(), 
    server = ecoVirtualServer
  )
}
