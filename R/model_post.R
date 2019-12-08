# ---------------------- #
# -- LDA MODEL FUNCS -- #
# -------------------- #

doc_topic <- function(fitted, doc=1) {
  as.integer(which.max(posterior(fitted)$topics[doc,]))
}

doc_topics <- function(fitted, doc=1, n = 5) {
    prob <- posterior(fitted)$topics[doc,]
    order(prob, decreasing=T)[1:n]
}

topic_words <- function(fitted, topic=1, n=10) {
  terms(fitted, n)[,topic]
}

topic_labels <- function(fitted, topics=c(1,2,3), n = 5) {
  apply(terms(fitted, n)[,topics], 2, function(x) paste(x, collapse=', '))
}

topic_docs <- function(fitted, topic=1, n=10) {
  docs <- which(topics(fitted) == topic)
  prob <- posterior(fitted)$topics[docs,topic]
  docs[order(prob, decreasing=T)[1:n]]
}
