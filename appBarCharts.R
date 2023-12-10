library(dplyr)
library(stringr)
library(shiny)
library(ggplot2)
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

#Category vs likes ratio

df_likes <- summarize(group_by(df, category_id), mean_ER = mean(engagement_rate, na.rm = TRUE))
df_likes <- merge(df_likes, categories_source, by = "category_id", all.x = TRUE, all.y = TRUE) 
df_likes<- na.omit(df_likes)




#shiny portion

#ui
ui <- fluidPage(
  titlePanel("How does video category effect viewer engagement?"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("engagementType", "Engagement Type: ", choices = c("Views", "Comments", "Likes")),
      h4(tags$b("About this data:")),
      p("When it comes to posting YouTube videos, audience engagement is an important metric to see how well your video is doing. By comparing this
        metric against the categories that the videos are uploaded under, we can make inferences what categories will receive which type
        of engagement."),
      p("Videos uploaded under the \"Music\" category seem to receive very good engagement under all metrics. We also see the categories that do particularly bad with specific engagement
        types. Videos uploaded under the \"Shows\" and \"Nonprofits & Activism\" receive very little engagement in the comments section."),
      p("Note: Not all categories had videos reach the trending page, meaning there is some correlation between the type of category a video is labeled as and
        whether or not it reaches the trending page.")
      
    ),
    mainPanel(
      titlePanel(
      h3("Engagement Statistics")), 
      plotOutput("plotEngagement")
  )
  )
)
#, "Likes", "Comment)
#server
server <- function(input, output) {
  
  #views
  output$plotEngagement <- renderPlot({
    if(input$engagementType == "Views"){ 
    ggplot(df_views, aes(x = category_name, y = mean_views, fill = category_id)) + 
    geom_bar(stat = "identity") + 
    labs(x = "Category", y = "Average Views", title = "Average View Count by Category") + 
    theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1)) + 
    scale_fill_gradient(low = "lightcoral", high = "darkred")
      
    } else if (input$engagementType == "Comments"){
      print(
        plotComments <- ggplot(df_comments, aes(x = category_name, y = mean_comments, fill = category_id)) +
        geom_bar(stat = "identity") +
        labs(x = "Category", y = "Average # of Comments", title = "Average # of Comments by Category") +
        theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1)) + 
        scale_fill_gradient(low = "lightcoral", high = "darkred")
        
        )
      
    } else if (input$engagementType == "Likes"){
      print(
        plotLikes <- ggplot(df_likes, aes(x = category_name, y = mean_ER, fill = category_id)) + 
        geom_bar(stat = "identity") + 
        labs(x = "Category", y = "Likes to Views Ratio", title = "Like to Views Ratio by Category") + 
        theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1)) + 
        scale_fill_gradient(low = "lightcoral", high = "darkred")
        
      )
    }

  })
}

# Run the Shiny app
shinyApp(ui, server)

