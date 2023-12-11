library(dplyr)
library(stringr)
library(shiny)
library(ggplot2)
library(shinythemes)

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
  titlePanel("YouTube Trending: Luck or skill?"),
  
  navbarPage(theme = shinytheme("simplex"), "Navigation",
    tabPanel("Introduction",
             div(imageOutput("YouTubeLogo", width = 300, height = 300), align = "center"),
             div(style = "margin-top: -150px"),
             p("With our continued reliance on the internet in our daily lives, YouTube.com has become a seemingly essential part of our society. 
               YouTube is visited by millions of users on a day to day basis, and is the second most accessed website 
               on the internet. YouTube is used in classrooms by teachers, by 
               individuals for entertainment, and much more. This large reliance on the platform has had a significant effect on our youth. 
               \"TikToker/YouTuber/Vlogger\" has become the 4th most desirable job for children, coming in just above Actors and Police Officers. So this begs the question, how do
               people become YouTubers?", style = "margin-top: 10px;"),
             p("When opening the frontpage of YouTube.com, you are almost immediately greeted by the \"trending\" videos at that current moment. A mix of
               music videos, movie trailers, and vlogs, and more. These videos oftentimes rack up millions of views. While not always converting a low 
               traffic channel to a viral sensation, there is still a conversion from viewer to subscriber, and if the video is monetized, could lead to 
               thousands of tens of thousands of dollars for the video uploader. Now what type of videos are able to reach such page? What types of videos 
               does the YouTube algorithm push to to the trending page? How can you improve your chances of making your video go trending? Or is it all just 
               luck? Explore the data and find out.")
             
             
             ),
    tabPanel("BarCharts", 
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
      
  )
  
)



#server
server <- function(input, output) {
  #---------------------------------------------------------------------------------------------
  #Intro Page
output$YouTubeLogo <- renderImage({
  list(src = "www/YouTubeLogo.png", width = 300, align = "center")}, deleteFile = FALSE)
  

  #---------------------------------------------------------------------------------------------
  
  
#---------------------------------------------------------------------------------------------
#Bar Chart Page
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
#---------------------------------------------------------------------------------------------



# Run the Shiny app
shinyApp(ui, server)

