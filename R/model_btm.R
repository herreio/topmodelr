# ---------------------------- #
# -- BITERM TOPIC MODELING -- #
# -------------------------- #

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
fit_and_save_bi_models <- function(docid_term, topics=seq(25,200,25), fileid="", model_dir="inst/topmax/lda.RDS") {
    for (k in topics) {
        bimod <- fit_bi_model(docid_term, k)
        saveRDS(bimod, file.path(
          model_dir,
          paste0("btm_",fileid,"_t",k,"_",Sys.Date(),".RDS")
        ))
    }
}
