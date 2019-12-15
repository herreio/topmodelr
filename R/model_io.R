#' Read models strored as RDS from given paths
#'
#' @export
read_models <- function(paths) {
  fnames <- make.names(name_from_path(paths))
  lapply(setNames(paths, fnames), readRDS)
}

#' Get file name from full path 
#'
#' @export
name_from_path <- function(path) {
  gsub("\\.[a-zA-Z0-9]+$", "", gsub("^.*\\/", "", path))
}

#' Get file names given path with pattern
#'
#' @export
get_paths <- function(path=".", pattern=".RDS") {
  list.files(path, pattern=pattern, full.names=T)
}
