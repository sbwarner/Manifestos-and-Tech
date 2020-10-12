###########################
# Party Platforms, Technology, and Tech-Induced Job Loss
#
# Part 3 - Active learning and iterative sampling
#
# Last updated by Seth Warner, 10-12-20
########################

library(mltest)
library(dplyr)

load("~/Penn State/Tech-induced job loss/manifesto_data.RData")

#####
# 0. Prepare test data
# (SKIP to next section if working with real data)

# set.seed(1969) # go mets

# draw random samples
# temp <- sample(row(df), replace = F, 30)  # simple random sample
# random_sample <- df[temp, c("id","text")]

# temp <- sample(row(df[df$keyword_count>=1,]), replace = F, 30) # sample of sents with 1+ keyword
# keyword_sample <- df[df$keyword_count>=1,][temp, c("id","text")]

# assign fake codes
# random_sample$handcode <- sample(c("nontech","techneg","techneutral","techpos"), 30, replace = T, prob = c(.97,.01,.01,.01))
# keyword_sample$handcode <- sample(c("nontech","techneg","techneutral","techpos"), 30, replace = T, prob = c(.2,.2,.3,.3))

# merge
# sample <- rbind(random_sample, keyword_sample)
# sample <- sample[!duplicated(sample$id),]
# rm(keyword_sample,random_sample)


#####
# 1. Combine hand-coded sentences with text features
training <- merge(df, sample[,c(1,3)], # 1 - ID, 3 - handcoded, skip 2 to avoid dup text columns
                  by = "id") 
training$handcode <- factor(training$handcode) # set as factor for random forest


#####
# 2. Create random forest model and run diagnostics
set.seed(1986)
rf <- randomForest(handcode ~ ., data = training[,c(3,10:28)])

rf # OOB error rate of 45%
ml_test(rf$predicted,training$handcode) # Balanced accuracy between 43 and 57% for each category


#####
# 3. Create predicted values for all sentences
preds <- predict(rf, df, type = "prob") # "prob" returns pr(nontech), pr(techneg), etc. in matrix form
preds <- as.data.frame(preds)
df <- cbind(df,preds) # combine matrix with df


#####
# 4. Calculate preliminary results
results <- aggregate(df[,c(28:31)], by = list(df$manifesto), FUN = sum) # "sum" adds probs to estimate predicted number of sentences
# of each type by manifesto

results$tech_sentences <- results$techneg + results$techneutral + results$techpos # total estimated tech-relevant sentences
results$tech_salience <- results$tech_sentences / (results$tech_sentences + results$nontech) # % sentences that are tech-related
results$tech_valence <- results$techpos / (results$techneg + results$techpos) # % techpos/neg sentences that are positive

# 10 most tech-salient manifestos
print(results[order(-results$techsalience),][1:10,])
# 10 least tech-salient
print(results[order(results$techsalience),][1:10,])

# 10 most tech-positive
print(results[order(-results$techvalence),][1:10,])
# 10 most tech-negative
print(results[order(results$techvalence),][1:10,])


#####
# 5. Measure estimate uncertainty with Shannon Entropy

# Create Shannon Entropy function
shannon <- function(x){
  -1 * sum(x * log(x))
}

# Replace 0s in data to avoid NAs [log(0) = -Inf]
df$techneg[df$techneg==0] <- 0.0001
df$techneutral[df$techneutral==0] <- 0.0001
df$techpos[df$techpos==0] <- 0.0001
df$nontech[df$nontech==0] <- 0.0001

# Calculate entropy for each sentence
for (i in 1:nrow(df)){
  temp <- df[i,28:31]
  entropy[i] <- shannon(temp)
}

df$entropy <- entropy


#####
# 6. Select next round of hand-coding
set.seed(2004) # Go UCONN

# Select some based on high uncertainty
percentiles <- quantile(df$entropy, probs = seq(0,1,0.01), na.rm = T)
entropy_sample <- sample_n(df[(complete.cases(df$entropy) & 
                                 df$entropy > percentiles[100]),], 
                           52, na.rm = T)

# Select some at random from manifs that have not yet been coded
training$coded <- 1
manifcounts <- aggregate(training$coded, by = list(training$manifesto), FUN = sum)
names(manifcounts) <- c("manifesto","coded1")
df <- merge(df, manifcounts, by = "manifesto", all.x = T)
df$coded1[is.na(df$coded1)] <- 0

coverage_sample <- sample_n(df[df$coded1<1,], 8)