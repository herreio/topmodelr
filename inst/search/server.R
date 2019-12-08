lda_model <- readRDS(get_lda_paths("lda_pers_lang_t200"))

tts <- function(query, w=5, t=3) {
  res <- lda_search(lda_model, query, n=w, t=t+1)
  topic_no <- colnames(res)[1:t]
  paste0(sapply(topic_no, function(x) {c(x,res[,x],"\n")}),"\n")
}

server <- function(input, output) {
  values <- reactiveValues(search = "natural language processing", words=5, topics=1)
  observe({
    if(input$button > 0) {
        values$search <- isolate(input$search)
        values$words <- isolate(input$words)
        values$topics <- isolate(input$topics)
    }
})
  output$value <- renderText({ tts(values$search,w=values$words,t=values$topics ) })
}
