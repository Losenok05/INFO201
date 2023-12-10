library(dplyr)
library(stringr)
library(shiny)
library(ggplot2)
df <- read.csv("toSend.csv")
categories_source <- read.csv("Categories.csv")

# Category vs Average Engagement Rate per category
df_categories <- summarize(group_by(df, category_id), mean_ER = mean(engagement_rate, na.rm = TRUE))
df_categories <- merge(df_categories, categories_source, by = "category_id", all.x = TRUE, all.y = TRUE) 
df_engagementrate<- na.omit(df_categories)

plotER <- ggplot(df_engagementrate, aes(x = category_name, y = mean_ER)) + 
              geom_bar(stat = "identity") + 
              labs(x = "Category", y = "Likes to Views Ratio", title = "Like to Views Ratio by Category")
print(plotER)

# Category vs Views
df_views <- summarize(group_by(df, category_id), mean_views = mean(views))
df_views <- merge(df_views, categories_source, by = "category_id", all.x = TRUE, all.y = TRUE) 
df_views <- na.omit(df_views)

plotViews <- ggplot(df_views, aes(x = category_name, y = mean_views)) + 
  geom_bar(stat = "identity") + 
  labs(x = "Category", y = "Average Views", title = "Average View Count by Category")

print(plotViews)


# Category vs. average Comments


