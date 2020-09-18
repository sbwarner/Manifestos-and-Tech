########################
# Party Platforms, Technology, and Tech-Induced Job Loss
#
# Part 2 - Counting keywords, drawing first coding sample
#
# Last updated by Seth Warner, 9-18-20
########################

# 0. Download dataframe and stringr package
load("~/Penn State/Tech-induced job loss/manifesto_data.RData")
library(stringr)

#####

# 1. Load and count keywords

# a. load keywords in regular expression terms

keywords <- "comput*|automate|automation|robot*|digital|artificial intelligence|data|programmer|
algorithm|high tech|internet|information technology|voice recog*|software|machine learning"

# b. count numbers of keywords per manifesto and sentence
df$keywords <- str_count(df$text, regex(keywords,ignore.case = T))

#####

# 2. Take SRS of sentences and of sentences w/ 1 or more keywords

set.seed(1986) #go mets

# random
temp <- sample(row(df), 75)
random_sample <- df[temp, c("id","text")]

# 1+ keywords
temp <- sample(row(df[df$keywords>=1,]), 75)
keyword_sample <- df[df$keywords>=1,][temp, c("id","text")]

# bind together
sample <- rbind(keyword_sample,random_sample)

# shuffle for coding
shuffle <- sample(nrow(sample))
sample <- sample[shuffle,]

# export
write.csv(sample,file="~/Penn State/Tech-induced job loss/coding_sample1.csv")
