get_paths <- function(path=".", pattern=".RDS") {
  list.files(path, pattern=pattern, full.names=T)
}

get_dp_lda_rds <- function() {
  system.file("topmax/lda.RDS", package="topicmodelr")
}

get_dp_btm_rds <- function() {
  system.file("topmax/btm.RDS", package="topicmodelr")
}

#' @export
get_btm_paths <- function(pattern="btm_") {
  dp_btm_rds <- get_dp_btm_rds()
  get_paths(dp_btm_rds, pattern=pattern)
}

#' @export
get_lda_paths <- function(pattern="lda_") {
  dp_lda_rds <- get_dp_lda_rds()
  get_paths(dp_lda_rds, pattern=pattern)
}
