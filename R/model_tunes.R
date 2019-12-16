# -------------------------- #
# -- TUNING OF LDA MODEL -- #
# ------------------------ #

#' @export
model_tunes <- function(dtm, topics=c(5,10,15,25,50,100)) {
  cat(paste("tune LDA model parameters\n"))
  t1 <- Sys.time()
  cat(paste("start time:", t1, "\n"))
  tunes <- ldatuning::FindTopicsNumber(dtm,
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
tune_and_save_plot <- function(dtm, fname="tunes.png") {
  tunes <- model_tunes(dtm)
  png(filename=fname)
  ldatuning::FindTopicsNumber_plot(tunes)
  dev.off()
  tunes
}
