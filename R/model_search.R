#' @include pyimp.R
NULL

# ------------------------- #
# -- TOPIC SEARCH FUNCS -- #
# ----------------------- #

infer_query <- function(lda, query) {
  query <- preprocess(query)
  dtm <- tm::DocumentTermMatrix(tm::VCorpus(tm::VectorSource(query)))  
  topicmodels::posterior(lda,dtm,lda@control)$topics
}

lda_search <- function(lda, query, n=10, t=2) {
  query <- preprocess(query)
  dtm <- tm::DocumentTermMatrix(tm::VCorpus(tm::VectorSource(query)))
  control <- lda@control
  post <- topicmodels::posterior(lda,dtm,control)$topics
  post <- order(post, decreasing=TRUE)
  topicmodels::terms(lda,n)[,post[1:t]]
}

btm_search <- function(btm, query, n=10, t=2) {
  query <- preprocess(query)
  token <- unlist(strsplit(query, "\\s"))
  docs <- rep(1,times=length(token))
  post <- predict(btm, data.frame(docs,token))
  post <- order(post, decreasing=T)
  ttprob <- terms(btm, top_n=n)[post[1:t]]
  tt <- sapply(ttprob, function(x) as.character(unlist(x[1])) )
  colnames(tt) <- sapply(post[1:t], function(x) paste("Topic",x))
  tt
}

# ----------------------- #
# -- TOPIC NEIGHBOURS -- #
# --------------------- #

build_tree <- function(lda, tree=50) {
  d <- dim(lda@gamma)
  vctr <- d[1]  # num docs
  vdim <- d[2]  # num topics
  a <- new(RcppAnnoy::AnnoyEuclidean, vdim)
  for (i in seq(1,vctr,1)) {
    a$addItem(i-1, lda@gamma[i, ])
  }
  a$build(tree)
  a
}

corpus_nn <- function(a, corpus, title=1, neighbours=10, lines=F) {
  res <- a$getNNsByItem(title-1, neighbours+10)
  if (lines) {
    corpus[res+1]   # readLines
  } else {
    unique(unlist(sapply(res, function(x) {
      paste(corpus[[x+1]]$content)
    })))[1:neighbours]
  }
}

query_nn <- function(lda, a, corpus, query, neighbours=10, lines=F) {
  prob <- infer_query(lda, query)
  res <- a$getNNsByVector(prob, 3*neighbours)
  if(lines) {
    corpus[res+1]
  } else {
    unique(unlist(sapply(res, function(x) {
      paste(corpus[[x+1]]$content)
    })))[1:neighbours]
  }
}
