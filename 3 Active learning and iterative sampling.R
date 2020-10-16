###########################
# Party Platforms, Technology, and Tech-Induced Job Loss
#
# Part 3 - Active learning and iterative sampling
#
# Last updated by Seth Warner, 10-12-20
########################

library(randomForest)
library(mltest)
library(dplyr)
library(tidyr)
library(reshape2)

load("~/Penn State/Tech-induced job loss/manifesto_data.RData")

#####
# 0. Prepare test data
# (SKIP to next section if working with real data)

set.seed(1969) # go mets

# draw random samples
temp <- sample(row(df), replace = F, 30)  # simple random sample
random_sample <- df[temp, c("id","text")]

temp <- sample(row(df[df$keyword_count>=1,]), replace = F, 30) # sample of sents with 1+ keyword
keyword_sample <- df[df$keyword_count>=1,][temp, c("id","text")]

# assign fake codes
random_sample$handcode <- sample(c("nontech","techneg","techneutral","techpos"), 30, replace = T, prob = c(.97,.01,.01,.01))
keyword_sample$handcode <- sample(c("nontech","techneg","techneutral","techpos"), 30, replace = T, prob = c(.2,.2,.3,.3))

# merge
sample <- rbind(random_sample, keyword_sample)
sample <- sample[!duplicated(sample$id),]
rm(keyword_sample,random_sample)


#####
# 1. Combine hand-coded sentences with text features
training <- merge(df, sample[,c("id","handcode")],
                  by = "id") 
training$handcode <- factor(training$handcode) # set as factor for random forest


#####
# 2. Create random forest model and run diagnostics

# Create different combos of specifications for RF
trees <- c(10,50,100,250,500,1000)
mtry <- c(5,8,11,14,17,19)
nodesize <- c(1,2,3,4,5,6)
specs <- expand.grid(trees, mtry, nodesize)
names(specs) <- c("trees","mtry","nodesize")

# Prepare elements to contain diags for each specification
oob_err <- rep(0,nrow(specs))
bal_acc <- matrix(nrow = nrow(specs), ncol = 4)

other_features <- c("date","length","sentiment","keyword_count")

# Loop with random forests
for (i in 1:nrow(specs)){
  
set.seed(1986) # Each RF begins in same place
rf <- randomForest(handcode ~ ., data = training[,c("handcode",other_features, keyword_names, keyword_names_0)], 
                   ntree = specs$trees[i], mtry = specs$mtry[i], nodesize = specs$nodesize[i])
diags <- ml_test(rf$predicted,training$handcode)

oob_err[i] <- mean(rf$err.rate[,1])
bal_acc[i,1:4] <- diags$balanced.accuracy
}

# Combine specifications with diagnostic metrics
diagnostics <- cbind(specs,oob_err,bal_acc)
names(diagnostics)[5:8] <- c("nontech_BA", "techneg_BA", "techneutral_BA", "techpos_BA")
diagnostics$mean_BA <- (diagnostics$nontech_BA + diagnostics$techneg_BA + diagnostics$techneutral_BA + diagnostics$techpos_BA) / 4

# Create combined metric: mean Balanced Accuracy X OOB error (* -1 so higher values are better)
diagnostics$BA_X_ooberr <- diagnostics$mean_BA * -1*diagnostics$oob_err

# Simple OLS identifies specifications associated with best performance
summary(lm(BA_X_ooberr ~ factor(trees) + factor(mtry) + factor(nodesize), data = diagnostics))

# Run RF with best specifications
set.seed(2010)
rf <- randomForest(handcode ~ ., data = training[,c("handcode",other_features, keyword_names, keyword_names_0)], 
      ntree = 1000, mtry = 5, nodesize = 5)

# Record OOB error and balanced accuracy
rf
ml_test(rf$predicted, training$handcode)


#####
# 3. Create predicted values for all sentences
preds <- predict(rf, df, type = "prob") # "prob" returns pr(nontech), pr(techneg), etc. in matrix form
preds <- as.data.frame(preds)

#for later rounds of coding, create separate varnames
#names(preds) <- c("nontech2","techneg2","techneutral2","techpos2") 

df <- cbind(df,preds) # combine matrix with df


#####
# 4. Calculate preliminary results

category_names <- c("nontech","techneg","techneutral","techpos")
results <- aggregate(df[,category_names], by = list(df$manifesto), FUN = sum) # "sum" adds probs to estimate predicted number of sentences per type in each manif

results$tech_sentences <- results$techneg + results$techneutral + results$techpos # total estimated tech-relevant sentences
results$tech_salience <- results$tech_sentences / (results$tech_sentences + results$nontech) # % sentences that are tech-related
results$tech_valence <- (results$techpos + -1*results$techneg) / results$tech_sentences # Score ranging from -1 (all neg) to 1 (all pos)

# 10 most tech-salient manifestos
print(results[order(-results$tech_salience),][1:10,])
# 10 least tech-salient
print(results[order(results$tech_salience),][1:10,])

# 10 most tech-positive
print(results[order(-results$tech_valence),][1:10,])
# 10 most tech-negative
print(results[order(results$tech_valence),][1:10,])


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

entropy <- rep(0,nrow(df))

# Calculate entropy for each sentence
for (i in 1:nrow(df)){
  temp <- df[i,category_names]
  entropy[i] <- shannon(temp)
}

df$entropy <- entropy


#####
# 6. Select next round of hand-coding

# Select some based on high uncertainty
sorted <- df[order(-df$entropy),]
entropy_sample <- sorted[1:52,]
entropy_sample <- entropy_sample[,c("id","text")]

# Select some at random from manifs that have not yet been coded
training$code_round <- 1 #to vary with each round

df <- merge(df, training[,c("id","code_round")], by = "id", all.x = T) 
df$handcoded <- ifelse(complete.cases(df$code_round),1,0) # indicate whether sentence has ever been coded

manifcounts <- aggregate(df$handcoded, by = list(df$manifesto), FUN = sum) # count handcodes per manif
names(manifcounts) <- c("manifesto","manif_sents_handcoded")
manifcounts$manif_sents_handcoded[manifcounts$manif_sents_handcoded==0] <- 0.001 # assign tiny value to manifs w/ 0 codes...
df <- merge(df, manifcounts, by = "manifesto", all.x = T) 

set.seed(2010)
coverage_sample <- sample_n(df, 8, prob = (1/(df$manif_sents_handcoded))) #...so we can inverse them and take a weighted prob sample from undercoded manifs
coverage_sample <- coverage_sample[,c("id","text")]

# Combine into one sample
new_sample <- rbind(entropy_sample, coverage_sample)
write.csv(new_sample,file="~/Penn State/Tech-induced job loss/coding_sampleXXX.csv")