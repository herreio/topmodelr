# ------------------------- #
# -- BTM TOPIC MODELING -- #
# ----------------------- #

#' @export
fit_bi_model <- function(docid_term, k) {
  cat(paste("fit Biterm model with", k, "topics\n"))
  w <- as.numeric(utlr::agg_elements(docid_term, "Doc")[1,2])
  t1 <- Sys.time()
  set.seed(42)
  cat(paste("start time:", t1, "\n"))
  fitted.biterm <- BTM::BTM(docid_term, k = k, iter = 1000, window = w)
  t2 <- Sys.time()
  cat("done fitting Biterm model!\n")
  elapsed <- difftime(t2, t1, units="mins")
  cat(paste("time elapsed:", round(elapsed, 2), "min\n\n"))
  return(fitted.biterm)
}

#' @export
fit_and_save_bi_models <- function(docid_term, topics=seq(25,200,25), fileid="", model_dir=".") {
  for (k in topics) {
    bimod <- fit_bi_model(docid_term, k)
    saveRDS(bimod, file.path(
    model_dir,
    paste0("btm_",fileid,"_t",k,"_",Sys.Date(),".RDS")))
  }
}

#' @export
prepare_bi_corpus <- function(corpus, corpus_id) {
  if(!require(tm)) {
    stop("please install tm package!")
  }
  res <- sapply(corpus, function(x) {
    stringr::str_split(x$content, pattern=" ")})
  res_ids <- docids(corpus_id, res)
  res_ids <- sapply(seq_along(res_ids), function(x) {repdoc(res_ids[[x]], length(unlist(res[[x]])))} )
  dplyr::tibble("Doc"=as.character(unlist(res_ids)),"Term"=as.character(unlist(res)))
}

repdoc <- function(docid, doclen) {
  rep(docid, doclen, doclen)
}

repids <- function(corpid,corplen) {
  rep(corpid, corplen, corplen)
}

docids <- function(corpus_id, docs) {
  prefix <- repids(corpus_id, length(docs))
  if (is.null(names(docs))) {
      paste0(prefix, ":" , seq_along(docs))      
  } else {
      docid <- name_from_path(names(docs))
      paste0(prefix, ":" , docid)
  }
}
