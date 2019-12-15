# ------------------------ #
# -- CORPUS PREPROCESS -- #
# ---------------------- #

#' @export
corpus_stop <- function() {
    stopwords::stopwords("english", source="smart")
}

#' @export
corpus_prep <- function(corp) {
    corp <- tm::tm_map(corp, tm::content_transformer(trim))
    corp <- tm::tm_map(corp, tm::content_transformer(trim2))
    corp <- tm::tm_map(corp, tm::content_transformer(lnbr))
    corp <- tm::tm_map(corp, tm::content_transformer(tags))
    corp <- tm::tm_map(corp, tm::content_transformer(nums))
    corp <- tm::tm_map(corp, tm::content_transformer(pnct))
    corp <- tm::tm_map(corp, tm::content_transformer(cntrl))
    corp <- tm::tm_map(corp, tm::content_transformer(tolower))
    corp <- tm::tm_map(corpus, function(x) { removeWords(x, corpus_stop()) })
    corp <- tm::tm_map(corp, tm::content_transformer(trim))
    corp <- tm::tm_map(corp, tm::content_transformer(trim2))
}

# returns string w/o leading or trailing whitespace
trim2 <- function (x) gsub("^\\s+|\\s+$", "", x)

# returns string w/o multiple whitespaces
trim <- function (x) gsub("\\s+", " ", x)

# returns string w/o line breaks
lnbr <- function(x) gsub("[\r\n]", " ", x)

# returns string w/o html tags
tags <- function (x) gsub("<.*?>", "", x)

# remove punction marks
pnct <- function (x) gsub("[[:punct:]]"," ", x)

# remove control characters
cntrl <- function (x) gsub("[[:cntrl:]]"," ", x)

# returns string w/o multiple whitespaces
nums <- function (x) gsub("[[:digit:]]", "", x)
