library(dplyr)

# Reading YouTube video and comment datasets for US and GB
USvideos2 <- read.csv("USvideos0.csv")
GBvideos2 <- read.csv("GBvideos0.csv")
GBcomments <- read.csv("GBcomments.csv")
UScomments <- read.csv("UScomments.csv")

# Combining comments datasets from GB and US
all_comments <- rbind(GBcomments, UScomments)

# Combining video datasets from US and GB
english_youtube_2 <- rbind(USvideos2, GBvideos2)

# Removing 'thumbnail_link' column from the videos dataset
english_youtube_2 <- subset(english_youtube_2, select = -thumbnail_link)

# Merging videos and comments datasets using 'video_id' as the key
# and removing NA values and duplicate rows
df <- merge(x = english_youtube_2, y = all_comments, by = "video_id")
# The data set turned out to be clean, but just in case:
df <- na.omit(df)
df <- df[!duplicated(df), ] # takes about 5 minutes to complete
# additional columns: engagement rate and high engagemnt
df$engagement_rate <- df$likes.x / df$views
likes_threshold <- quantile(df$likes.x, 0.75)
df$high_engagement <- ifelse(df$likes.x > likes_threshold, "High", "Low") # runs for 1 minute

#summarisation df
tag_list <- unlist(strsplit(as.character(df$tags), "\\|"))#one minute
tag_table <- table(tag_list)
tag_summary <- data.frame(tag = names(tag_table), frequency = as.integer(tag_table))
