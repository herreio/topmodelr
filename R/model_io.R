#' Read models strored as RDS from given paths
#'
#' @export
read_models <- function(paths) {
  fnames <- make.names(name_from_path(paths))
  lapply(stats::setNames(paths, fnames), readRDS)
}

#' Get model from list of models created by read_models
#'
#' @export
get_models <- function(models, pattern) {
  res <- Filter(Negate(is.null), sapply(seq_along(models), function(x) {
    if(length(grep(pattern,names(models[x])))==1) models[[x]]}))
  if (length(res) == 1) res[[1]]
  else res
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
