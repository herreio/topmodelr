# ---------------------- #
# -- LDA MODEL FUNCS -- #
# -------------------- #

#' @export
lda_doc_topic <- function(fitted, doc=1) {
  as.integer(which.max(topicmodels::posterior(fitted)$topics[doc,]))
}

lda_docs_topics <- function(fitted) {
  topicmodels::posterior(fitted)$topics
}

#' @export
lda_doc_topics <- function(fitted, doc=1, n = 5) {
    prob <- lda_docs_topics(fitted)[doc,]
    order(prob, decreasing=T)[1:n]
}

#' @export
lda_topics_words <- function(fitted, n=10) {
  topicmodels::terms(fitted, n)
}

#' @export
lda_topic_words <- function(fitted, topic=1, n=10) {
  lda_topics_words(fitted, n)[, topic]
}

#' @export
lda_topics_labels <- function(fitted, topics=c(1,2,3), n=5) {
  apply(lda_topic_words(fitted, topics, n), 2, function(x) {
    paste(x, collapse=', ')
  })
}

#' @export
lda_topic_docs <- function(fitted, topic=1, n=10) {
  docs <- which(topicmodels::topics(fitted) == topic)
  prob <- topicmodels::posterior(fitted)$topics[docs,topic]
  docs[order(prob, decreasing=T)[1:n]]
}

# ---------------------- #
# -- BTM MODEL FUNCS -- #
# -------------------- #

#' @importFrom stats terms
#' @import BTM
#' @export
btm_topics_words <- function(fitted, n=10) {
  tt <- terms(fitted, top_n=n)
  sapply(tt, function(x) as.character(unlist(x[1])))
}

#' @export
btm_topic_words <- function(fitted, topic=1, n=10) {
  btm_topics_words(fitted, n)[,topic]
}

#' @export
btm_topic_labels <- function(fitted, topics=c(1,2,3), n=5) {
  words <- btm_topic_words(fitted, topics, n)
  res <- sapply(as.data.frame(words), function(x) {
    paste(x, collapse=', ')
  })
  names(res) <- sapply(topics, function(x) paste("Topic", x))
  res
}
