########################
# Party Platforms, Technology, and Tech-Induced Job Loss
#
# Part 2 - Counting keywords, drawing first coding sample
#
# Last updated by Seth Warner, 11-4-20
########################


#####
# 0. Download dataframe
load("~/Penn State/Tech-induced job loss/manifesto_data.RData")

#####
# 1. Take simple random sample of sentences

# Set random seed
set.seed(1986)

# Sample at random
temp <- sample_n(df,20)
random_sample <- temp[,c("id","text")]

#####
# 2. Take sample of sentences w/ keywords, stratified by sentiment score

# Simple random sample
temp <- sample_n(df[df$keyword_count >= 1,], 20)
keyword_sample <- temp[,c("id","text")]

# Sample of sentences w/ 10% highest sentiment scores (to ensure tech-pos sentences)
temp <- sample_n(df[(df$sentiment > 0.62 & df$keyword_count>=1),], 10)
pos_sample <- temp[,c("id","text")]

# Sample of sentence w/ 10% lowest sentiment scores (to ensure tech-neg sentences)
temp <- sample_n(df[(df$sentiment < -0.20 & df$keyword_count>=1),],10)
neg_sample <- temp[,c("id","text")]


#####
# 3. Combine, shuffle, and export

# Combine sentences
sample <- rbind(random_sample,neg_sample,pos_sample,keyword_sample)

# Shuffle so coders don't see same types in a row
order <- sample(1:60, 60, rep = F)
sample <- sample[order,]

# Export to csv
write.csv(sample, file = "~/Penn State/Tech-induced job loss/initial_sample.csv", row.names = F)