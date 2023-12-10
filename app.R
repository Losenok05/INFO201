library(shiny)
library(tm)
library(wordcloud2)


grouped_df <- read.csv("distinct_grouped_df.csv")


corpus <- Corpus(VectorSource(as.character(grouped_df$title)))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, stripWhitespace)


dtm <- DocumentTermMatrix(corpus)
dtm_df <- as.data.frame(as.matrix(dtm))
colnames(dtm_df) <- colnames(dtm)


word_freq <- colSums(dtm_df)


ui <- fluidPage(
  titlePanel("Word Cloud Generator"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("column", "Choose Column:", choices = c("tags", "title")),
      selectInput("metric", "Choose Metric:", choices = c("views", "engagement_rate", "likes.x", "dislikes")),
      sliderInput("top_words", "Top Words:", min = 1, max = 100, value = 10)
    ),
    mainPanel(
      wordcloud2Output("wordcloud")
    )
  )
)

# Define the server
server <- function(input, output) {
  output$wordcloud <- renderWordcloud2({
    column_name <- input$column
    metric <- input$metric
    top_words <- input$top_words
    

    columns <- c(column_name, metric)
    df_selected <- grouped_df[!sapply(grouped_df[columns], is.na), columns]
    

    text_data <- paste(df_selected[[metric]], df_selected[[column_name]])
    corpus_column <- Corpus(VectorSource(text_data))
    corpus_column <- tm_map(corpus_column, content_transformer(tolower))
    corpus_column <- tm_map(corpus_column, removePunctuation)
    corpus_column <- tm_map(corpus_column, removeNumbers)
    corpus_column <- tm_map(corpus_column, removeWords, stopwords("en"))
    corpus_column <- tm_map(corpus_column, stripWhitespace)
    

    dtm_column <- DocumentTermMatrix(corpus_column)
    dtm_df_column <- as.data.frame(as.matrix(dtm_column))
    

    word_freq_column <- colSums(dtm_df_column)
    top_words_data <- head(sort(word_freq_column, decreasing = TRUE), top_words)
    
    wordcloud2(data = data.frame(word = names(top_words_data), freq = top_words_data),
               color = "white",
               backgroundColor = "black",
               fontFamily = "Arial",
    )
  })
}

# Run the Shiny app
shinyApp(ui, server)