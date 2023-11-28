library(dplyr)

#US_youtube_trending_data <- read.csv("US_youtube_trending_data.csv")
#Global_Youtube_stats <- read.csv("Global YouTube Statistics.csv")
#Global_Youtube_stats <- na.omit(Global_Youtube_stats)

#USvideos <- read.csv("USvideos.csv")
#GBvideos <- read.csv("GBvideos.csv")

#english_youtube_1 <- rbind(USvideos, GBvideos)


USvideos2 <- read.csv("USvideos0.csv")
GBvideos2 <- read.csv("GBvideos0.csv")
GBcomments <- read.csv("GBcomments.csv")
UScomments <- read.csv("UScomments.csv")

all_comments <- rbind(GBcomments, UScomments)


english_youtube_2 <- rbind(USvideos2, GBvideos2)
english_youtube_2 <- subset(english_youtube_2, select = -thumbnail_link)

df <- merge(x = english_youtube_1, y = all_comments, by = "video_id")
df <- na.omit(df)
df_unique <- df[!duplicated(df), ]

#comments <- read.csv("comments.csv")
#video_stats <- read.csv("videos-stats.csv")


#stats_and_comments <- merge(x = video_stats, y = comments, by.x = "Video.ID", by.y = "Video.ID")
#stats_and_comments <- na.omit(stats_and_comments)
