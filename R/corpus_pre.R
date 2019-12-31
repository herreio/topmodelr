# ------------------------ #
# -- CORPUS PREPROCESS -- #
# ---------------------- #

#' @export
corpus_split <- function(corp, n=0.1) {
  test_n <- floor(0.1 * length(corp))
  test_index <- sample(seq_along(corp), test_n)
  train_index <- setdiff(seq_along(corp), test_index)
  list("test" = corp[test_index], "train" = corp[train_index])
}

#' @export
corpus_create <- function(char_vec) {
    tm::VCorpus(tm::VectorSource(char_vec))
}

#' @export
corpus_prep <- function(corp) {
    corp <- tm::tm_map(corp, tm::content_transformer(trim))
    corp <- tm::tm_map(corp, tm::content_transformer(trim2))
    corp <- tm::tm_map(corp, tm::content_transformer(lnbr))
    corp <- tm::tm_map(corp, tm::content_transformer(tags))
    corp <- tm::tm_map(corp, tm::content_transformer(nums))
    corp <- tm::tm_map(corp, tm::content_transformer(cntrl))
    corp <- tm::tm_map(corp, tm::content_transformer(ascii))
    corp <- tm::tm_map(corp, tm::content_transformer(pnct))
    corp <- tm::tm_map(corp, tm::content_transformer(tolower))
    corp <- tm::tm_map(corp, function(x) { tm::removeWords(x, corpus_stop()) })
    corp <- tm::tm_map(corp, tm::content_transformer(trim))
    tm::tm_map(corp, tm::content_transformer(trim2))
}

#' @export
corpus_stop <- function() {
    stopwords::stopwords("english", source="smart")
}

# returns string w/o multiple whitespaces
trim <- function (x) gsub("\\s+", " ", x)

# returns string w/o leading or trailing whitespace
trim2 <- function (x) gsub("^\\s+|\\s+$", "", x)

# returns string w/o line breaks
lnbr <- function(x) gsub("[\r\n]", " ", x)

# returns string w/o html tags
tags <- function (x) gsub("<.*?>", "", x)

# remove digits and words containing digits
nums <- function (x) gsub("[^ ]*[[:digit:]][^ ]*", "", x, perl=T)

# remove words containing non-ascii characters
ascii <- function(x) gsub("[^ ]*[^\\x00-\\x7F][^ ]*", "", x, perl=T)

# remove punction marks
pnct <- function (x) gsub("[[:punct:]]"," ", x)

# remove control characters
cntrl <- function (x) gsub("[[:cntrl:]]"," ", x)
