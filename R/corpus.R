# ------------------- #
# -- CORPUS FUNCS -- #
# ----------------- #

#' @export
corpus_doc_tokens <- function(doc_term) {
  slam::row_sums(doc_term)
}

#' @export
corpus_tokens_doc <- function(doc_term) {
  slam::col_sums(doc_term)
}

#' @export
corpus_freq <- function(doc_term) {
  sort(corpus_tokens_doc(doc_term), decreasing = T)
}

#' @export
corpus_search <- function(doc_term, query) {
  doc_term <- doc_term[, grepl(query, doc_term$dimnames$Terms)]
  doc_term[slam::row_sums(doc_term) > 0, ]    # return none empty docs
}

#' @export
corpus_doc_search <- function(doc_term, query) {
  doc_term <- doc_term[grepl(query, doc_term$dimnames$Doc),]
  doc_term <- doc_term[slam::row_sums(doc_term) > 0, ]    # return none empty docs
  doc_term[,which(slam::col_sums(doc_term) > 0)] # return non empty vocab
}

#' @export
corpus_del_max_freq <- function(doc_term, x = 100) {
  n <- as.numeric(corpus_freq(doc_term)[x])
  doc_term <- doc_term[,which(slam::col_sums(doc_term) < n)]
  doc_term[slam::row_sums(doc_term) > 0, ]    # return none empty docs
}

#' @export
corpus_del_min_freq <- function(doc_term, n = 2) {
  doc_term <- doc_term[,which(slam::col_sums(doc_term) > n)]
  doc_term[slam::row_sums(doc_term) > 0, ]    # return none empty docs
}
