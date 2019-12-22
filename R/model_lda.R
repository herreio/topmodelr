# ------------------------- #
# -- LDA TOPIC MODELING -- #
# ----------------------- #

#' @export
prepare_dt_corpus <- function(corpus) {
  tm::DocumentTermMatrix(corpus)
}

#' @export
filter_dt_corpus <- function(doc_term, tmin=2, dmin=2) {
  doc_term <- doc_term[,which(slam::col_sums(doc_term) > tmin-1)]
  doc_term <- doc_term[slam::row_sums(doc_term) > dmin-1, ]
  doc_term[,which(slam::col_sums(doc_term) > 0)]
}

#' @export
fit_model <- function(doc_term, k) {
  cat(paste("fit LDA model with", k, "topics\n"))
  t1 <- Sys.time()
  cat(paste("start time:", t1, "\n"))
  fitted_model <- topicmodels::LDA(doc_term, k, method = "Gibbs", control=list(iter=1000, seed=42))
  t2 <- Sys.time()
  cat("done fitting LDA model!\n")
  elapsed <- difftime(t2, t1, units="mins")
  cat(paste("time elapsed:", round(elapsed,2), "min\n\n"))
  fitted_model
}

#' @export
model_json <- function(fitted_model, doc_term) {
  print("set variables...")
  phi <- as.matrix(topicmodels::posterior(fitted_model)$terms)
  theta <- as.matrix(topicmodels::posterior(fitted_model)$topics)
  vocab <- colnames(phi)
  term_freq <- slam::col_sums(doc_term)
  print("create visualization...")
  LDAvis::createJSON(
    phi = phi,
    theta = theta,
    vocab = vocab,
    doc.length = as.vector(table(doc_term$i)),
    term.frequency = term_freq
  )
}

#' @export
model_vis <- function(fitted_model, doc_term, vis_dir="./doc/ldavis", browser=F) {
  json_lda <- model_json(fitted_model, doc_term)
  print("start server...")
  LDAvis::serVis(json_lda, out.dir = vis_dir, open.browser = browser, encoding="UTF-8")
  print("success!")
}

#' @export
fit_model_vis <- function(doc_term, k, vis_dir="./doc", browser=FALSE){
    fitted_model <- fit_model(doc_term, k)
    model_vis(fitted_model, doc_term, vis_dir, browser)
}

#' @export
fit_and_save_models <- function(doc_term, topics=seq(25,200,25), fileid="", model_dir=".") {
    for(k in topics) {
        topmod <- fit_model(doc_term, k)
        saveRDS(topmod, file.path(
          model_dir,
          paste0("lda_",fileid,"_t",k,"_",Sys.Date(),".RDS")
        ))
    }
}

#' @export
vis_from_rds <- function(rds_path, doc_term, browser=FALSE) {
  fitted_model <- readRDS(rds_path)
  idx <- name_from_path(rds_path)
  out <- paste0("./doc/", idx)
  cat(paste("vis model: ", idx, "\n"))
  t1 <- Sys.time()
  cat(paste("start time:", t1, "\n"))
  model_vis(fitted_model, doc_term, vis_dir=out, browser=browser)
  t2 <- Sys.time()
  cat("done visualizing model!\n")
  elapsed <- difftime(t2, t1, units="mins")
  cat(paste("time elapsed:", round(elapsed,2), "min\n\n"))
}
