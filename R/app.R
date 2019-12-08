#' Search interface for queries based on different topic models
#'
#' @export
titlesearch <- function(){
  shiny::runApp(system.file("search", package="topicmodelr"), launch.browser = T)
}
