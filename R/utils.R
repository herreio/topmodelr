# ---------------- #
# -- MODEL I/O -- #
# -------------- #

read_models <- function(paths) {
  fnames <- make.names(name_from_path(paths))
  lapply(setNames(paths, fnames), readRDS)
}
