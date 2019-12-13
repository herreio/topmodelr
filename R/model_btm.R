#' @export
fit_bi_model <- function(docid_term, k) {
    cat(paste("fit Biterm model with", k, "topics\n"))
    t1 <- Sys.time()
    set.seed(42)
    cat(paste("start time:", t1, "\n"))
    fitted.biterm <- BTM::BTM(docid_term, k = k, iter = 1000)
    t2 <- Sys.time()
    cat("done fitting Biterm model!\n")
    elapsed <- difftime(t2, t1, units="mins")
    cat(paste("time elapsed:", round(elapsed, 2), "min\n\n"))
    return(fitted.biterm)
}

#' @export
fit_and_save_bi_models <- function(docid_term, topics=seq(25,200,25), fileid="", model_dir="") {
    for (k in topics) {
        bimod <- fit_bi_model(docid_term, k)
        saveRDS(bimod, file.path(
          model_dir,
          paste0("btm_",fileid,"_t",k,"_",Sys.Date(),".RDS")
        ))
    }
}

#' @export
prepare_bi_corpus <- function(corpus, corpus_id) {
  res <- stringr::str_split(
    as.vector(sapply(corpus, function(x) {x$content})), 
    pattern=" "
  )
  res_ids <- docids(corpus_id, res)
  res_ids <- sapply(seq_along(res_ids), function(x) repdoc(res_ids[[x]], length(res[[x]])) )
  dplyr::tibble("Doc"=unlist(res_ids),"Term"=unlist(res))
}

repdoc <- function(docid,doclen) {
  rep(docid, doclen, doclen)
}

repids <- function(corpid,corplen) {
  rep(corpid, corplen, corplen)
}

docids <- function(corpid, docs) {
  prefix <- repids(corpid, length(docs))
  paste0(prefix, ":" , seq_along(prefix))
}
