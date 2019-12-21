# ------------------------- #
# -- TOPIC SEARCH FUNCS -- #
# ----------------------- #

#' @export
lda_infer <- function(lda, query) {
  dtm <- tm::DocumentTermMatrix(tm::VCorpus(tm::VectorSource(query)))  
  topicmodels::posterior(lda,dtm,lda@control)$topics
}

#' @export
lda_search <- function(lda, query, n=10, t=2) {
  post <- lda_infer(lda, query)
  post <- order(post, decreasing=TRUE)
  topicmodels::terms(lda,n)[,post[1:t]]
}

#' @importFrom stats predict
#' @import BTM
#' @export
btm_infer <- function(btm, query) {
  token <- unlist(strsplit(query, "\\s"))
  docs <- rep(1,times=length(token))
  predict(btm, newdata=data.frame(docs,token))
}

#' @export
btm_search <- function(btm, query, n=10, t=2, tt=NULL) {
  post <- btm_infer(btm, query)
  post <- order(post, decreasing=T)
  if(is.null(tt)) tt <- btm_topics_words(btm, n)
  tt <- tt[,post[1:t]]
  if (t > 1) {
    colnames(tt) <- sapply(post[1:t], function(x) paste("Topic", x))
  }
  tt
}

# ----------------------- #
# -- TOPIC NEIGHBOURS -- #
# --------------------- #

#' @export
build_tree <- function(lda, tree=50) {
  d <- dim(lda@gamma)
  vctr <- d[1]  # num docs
  vdim <- d[2]  # num topics
  a <- methods::new(RcppAnnoy::AnnoyAngular, vdim)
  for (i in seq(1,vctr,1)) {
    a$addItem(i-1, lda@gamma[i, ])
  }
  a$build(tree)
  a
}

#' @export
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

#' @export
query_nn <- function(lda, a, corpus, query, neighbours=10, lines=F) {
  prob <- lda_infer(lda, query)
  res <- a$getNNsByVector(prob, 3*neighbours)
  if(lines) {
    corpus[res+1]
  } else {
    unique(unlist(sapply(res, function(x) {
      paste(corpus[[x+1]]$content)
    })))[1:neighbours]
  }
}
