library(dplyr)
library(stringr)
library(shiny)
library(ggplot2)
library(shinythemes)
library(tm)
library(wordcloud2)

df <- read.csv("toSend.csv")
categories_source <- read.csv("Categories.csv")


# Category vs Views
df_views <- summarize(group_by(df, category_id), mean_views = mean(views))
df_views <- merge(df_views, categories_source, by = "category_id", all.x = TRUE, all.y = TRUE) 
df_views <- na.omit(df_views)

# Category vs. average Comments
df_comments <- summarize(group_by(df, category_id), mean_comments = mean(comment_total))
df_comments <- merge(df_comments, categories_source, by = "category_id", all.x = TRUE, all.y = TRUE) 
df_comments <- na.omit(df_comments)

# Category vs likes ratio
df_likes <- summarize(group_by(df, category_id), mean_ER = mean(engagement_rate, na.rm = TRUE))
df_likes <- merge(df_likes, categories_source, by = "category_id", all.x = TRUE, all.y = TRUE) 
df_likes <- na.omit(df_likes)

# Word cloud
grouped_df <- read.csv("distinct_grouped_df.csv")
weights <- c(1, 10, 20, 20)
interaction_matrix <- as.matrix(grouped_df[c("likes.x", "comment_total", "total_likes", "total_replies")])
grouped_df$engagement_rate <- log((rowSums(interaction_matrix * weights)^2) / grouped_df$views)


# Shiny portion

# UI
ui <- fluidPage(
  titlePanel("YouTube Trending: Luck or skill?"),
  
  navbarPage(theme = shinytheme("simplex"), "Navigation",
             # UI - Introductory Page
             tabPanel("Introduction",
                      div(
                        imageOutput("YouTubeLogo", width = 300, height = 300),
                        align = "center"
                      ),
                      p(
                        "Welcome to our YouTube Trending analysis! As an integral part of our daily lives, YouTube.com is a vital platform visited by millions worldwide. It stands as the second most accessed website on the internet, serving various purposes from classrooms to entertainment.",
                        "The platform's impact on our youth is profound, with \"TikToker/YouTuber/Vlogger\" ranking as the 4th most desirable job for children, just above Actors and Police Officers. This raises the question: how do people become successful YouTubers?"
                      ),
                      p(
                        "Upon opening YouTube.com, the 'trending' videos capture immediate attention, featuring a diverse mix of music videos, movie trailers, vlogs, and more. These videos often accumulate millions of views, representing a significant conversion from viewer to subscriber.",
                        "While not every video turns a low-traffic channel into a viral sensation, the potential for substantial earnings exists, especially if the video is monetized. Now, what types of videos make it to the trending page? What does the YouTube algorithm prioritize? Can you improve your chances of going viral, or is it all just luck?"
                      ),
                      p(
                        "Explore the data with us to uncover the secrets behind YouTube's trending videos. Whether you're a creator seeking insights or a curious viewer, join us on this journey to understand the dynamics that determine a video's success on YouTube."
                      )
             )
             ,
             tabPanel("BarCharts", 
                      titlePanel("How does video category affect viewer engagement?"),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("engagementType", "Engagement Type: ", choices = c("Views", "Comments", "Likes")),
                          h4(tags$b("About this data:")),
                          p("When it comes to posting YouTube videos, audience engagement is an important metric to see how well your video is doing. By comparing this
                   metric against the categories that the videos are uploaded under, we can make inferences about what categories will receive which type
                   of engagement."),
                          p("Videos uploaded under the \"Music\" category seem to receive very good engagement under all metrics. We also see the categories that do particularly bad with specific engagement
                   types. Videos uploaded under the \"Shows\" and \"Nonprofits & Activism\" receive very little engagement in the comments section."),
                          p("Note: Not all categories had videos reach the trending page, meaning there is some correlation between the type of category a video is labeled as and
                   whether or not it reaches the trending page.")
                        ),
                        mainPanel(
                          titlePanel(h3("Engagement Statistics")), 
                          plotOutput("plotEngagement")
                        )
                      )
             ),
             tabPanel("Word Cloud",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("wordcloud_column", "Choose Column:", choices = c("tags", "title")),
                          selectInput("wordcloud_metric", "Choose Metric:", choices = c("views", "engagement_rate", "likes.x", "dislikes")),
                          sliderInput("wordcloud_top_words", "Top Words:", min = 50, max = 100, value = 75),
                          p("This Word Cloud visually represents the most frequently occurring words in videos, providing valuable insights into the content that resonates the most with the audience. It can be useful for content creators, marketers, and viewers alike."),
                          p("For Content Creators: Identify popular topics and themes that contribute to higher engagement. Optimize video titles and tags based on the most commonly used words in successful videos."),
                          p("For Marketers: Gain a better understanding of audience preferences and trends. Use insights from the Word Cloud to inform content strategies and tailor marketing efforts."),
                          p("For Viewers: Explore the common themes in trending videos. Discover what captures the attention of a broader audience and understand the factors contributing to a video's popularity.")
                        ),
                        mainPanel(
                          fluidRow(
                            column(12, 
                                   wordcloud2Output("wordcloud_output")),
                            column(12, 
                                   h4("Analysis of Word Cloud:"),
                                   p("The most frequently appeared words in videos with the highest user activity include 'official', 'video', 'trailer', 'audio', and 'live'. This pattern could be attributed to recent presentations of new iPhones and music releases."),
                                   p("Notably, the word 'makeup' is surprisingly popular, indicating a significant interest in makeup-related content."),
                                   p("The Word Clouds for tags and titles exhibit similar patterns, suggesting that the amount of dislikes is proportional to the amount of likes for the videos."),
                                   p("Explore the Word Clouds to uncover more insights into the trending topics and themes on YouTube.")
                            )
                          )
                        )
                      )
             )
  )
)

# Server
server <- function(input, output) {
  #---------------------------------------------------------------------------------------------
  # Intro Page
  output$YouTubeLogo <- renderImage({
    list(src = "www/YouTubeLogo.png", width = 300, align = "center")
  }, deleteFile = FALSE)
  
  #---------------------------------------------------------------------------------------------
  
  #---------------------------------------------------------------------------------------------
  # Bar Chart Page
  # Views
  output$plotEngagement <- renderPlot({
    if (input$engagementType == "Views") { 
      ggplot(df_views, aes(x = category_name, y = mean_views, fill = category_id)) + 
        geom_bar(stat = "identity") + 
        labs(x = "Category", y = "Average Views", title = "Average View Count by Category") + 
        theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1)) + 
        scale_fill_gradient(low = "lightcoral", high = "darkred")
    } else if (input$engagementType == "Comments") {
      plotComments <- ggplot(df_comments, aes(x = category_name, y = mean_comments, fill = category_id)) +
        geom_bar(stat = "identity") +
        labs(x = "Category", y = "Average # of Comments", title = "Average # of Comments by Category") +
        theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1)) + 
        scale_fill_gradient(low = "lightcoral", high = "darkred")
      print(plotComments)
    } else if (input$engagementType == "Likes") {
      plotLikes <- ggplot(df_likes, aes(x = category_name, y = mean_ER, fill = category_id)) + 
        geom_bar(stat = "identity") + 
        labs(x = "Category", y = "Likes to Views Ratio", title = "Like to Views Ratio by Category") + 
        theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1)) + 
        scale_fill_gradient(low = "lightcoral", high = "darkred")
      print(plotLikes)
    }
  })
  
  #---------------------------------------------------------------------------------------------
  
  # Word Cloud
  output$wordcloud_output <- renderWordcloud2({
    column_name <- input$wordcloud_column
    metric <- input$wordcloud_metric
    top_words <- input$wordcloud_top_words
    
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
               color = "darkred",
               fontFamily = "Arial",
               
    )
  })
  
}

# Run the Shiny app
shinyApp(ui, server)


