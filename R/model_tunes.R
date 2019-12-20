# -------------------------- #
# -- TUNING OF LDA MODEL -- #
# ------------------------ #

#' @export
model_tunes <- function(doc_term, topics=c(5,10,15,25,50,100)) {
  cat(paste("tune LDA model parameters\n"))
  t1 <- Sys.time()
  cat(paste("start time:", t1, "\n"))
  tunes <- ldatuning::FindTopicsNumber(doc_term,
    topics = topics,
    metrics = c("Griffiths2004"),
    method = "Gibbs",
    control = list(seed = 42, iter = 1000),
    verbose = TRUE
  )
  t2 <- Sys.time()
  cat("done tuning LDA model!\n")
  elapsed <- difftime(t2, t1, units="mins")
  cat(paste("time elapsed:", round(elapsed, 2), "min\n\n"))
  tunes
}

#' @export
tune_and_save_plot <- function(doc_term, topics=c(5,10,15,25,50,100), fname="tunes.png") {
  tunes <- model_tunes(doc_term, topics)
  grDevices::png(filename=fname)
  ldatuning::FindTopicsNumber_plot(tunes)
  close <- grDevices::dev.off()
  tunes
}
