#' @export
corpus_freq <- function(dtm) {
  sort(slam::col_sums(dtm), decreasing = T)
}

#' @export
corpus_del_max_freq <- function(dtm, x = 100) {
  n <- as.numeric(corpus_freq(dtm)[x])
  dtm <- dtm[,which(slam::col_sums(dtm) < n)]
  dtm[slam::row_sums(dtm) > 0, ]    # return none empty docs
}

#' @export
corpus_del_min_freq <- function(dtm, n = 2) {
  terms.freq <- slam::col_sums(dtm)
  dtm <- dtm[,which(slam::col_sums(dtm) > n)]
  dtm[slam::row_sums(dtm) > 0, ]    # return none empty docs
}
