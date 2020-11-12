###########################
# Party Platforms, Technology, and Tech-Induced Job Loss
#
# Part 3 - Active learning and iterative sampling
#
# Last updated by Seth Warner, 11-11-20
########################

library(doParallel)
library(randomForest)
library(mltest)
library(dplyr)
library(tidyr)
library(reshape2)
library(readxl)



#####
# 0. Load and merge data
load("~/Penn State/Tech-induced job loss/manifesto_data.RData")
coded_sample <- read_xlsx("~/Penn State/Tech-induced job loss/coded_sample.xlsx")

training <- merge(df, coded_sample, by = "id")
training$handcode <- factor(training$handcode)



#####
# 1. Determine best random forest specifications

# Try all combos of following values for number of trees, vars-per-split, and nodesize
trees <- c(10,50,100,250,500,1000)
mtry <- c(5,8,11,14,17,19)
nodesize <- c(1,2,3,4,5,6)
specs <- expand.grid(trees, mtry, nodesize)
names(specs) <- c("trees","mtry","nodesize")

# Create empty vectors to collect out-of-bag error and balanced accuracy for each forest
oob_err <- rep(0,nrow(specs))
bal_acc <- matrix(nrow = nrow(specs), ncol = 4)

# Name ind variables for RF to use (some text features already in data environment)
other_features <- c("date","length","sentiment","keyword_count")

# Prepare parallelizer to use all available CPU cores
numCores <- detectCores()
registerDoParallel(numCores)

# Random forest collects the OOB Error * Balanced Accuracy for each specification
set.seed(1992)
BA_X_ooberr <- 
  foreach(i=1:nrow(specs), .combine=rbind, .packages=c('randomForest','mltest')) %dopar%{
rf <- randomForest(handcode ~ ., data = training[,c("handcode",other_features, party_data, keyword_names, keyword_names_0)], 
                     ntree = specs$trees[i], mtry = specs$mtry[i], nodesize = specs$nodesize[i],
                   na.action = na.roughfix, mfixrep = 5, missfill = 2)
diags <- ml_test(rf$predicted,training$handcode)
  
  oob_err[i] <- mean(rf$err.rate[,"OOB"])
  bal_acc[i,1:4] <- diags$balanced.accuracy
  -1 * oob_err[i] * mean(bal_acc[i,1:4]) # multiply OOB error by -1 so higher values are better
}

# Select specifications with best accuracy / lowest error
diagnostics <- cbind(specs, BA_X_ooberr)
diagnostics <- diagnostics[order(-diagnostics$BA_X_ooberr),]
specs <- diagnostics[1,c("trees","mtry","nodesize")]



#####
# 2. Study improvement in OOB / stability in resulting metrics as more sentences are coded

# Sort by date_coded, first to last
training$date_coded <- as.Date(training$date_coded)
training <- training[order(training$date_coded),]

# Prepare to run 20 RFs on first 50 sentences, then 100, ... until total coded reached
upto <- rep(c(50,59), 20) # manually specify intervals of 50 until total
upto <- upto[order(upto)]

# Prepare separate seed for each RF run
set.seed(75)
seeds <- sample(1:length(upto),length(upto))

# Run 20 RFs for each set of 50 sentences, cumulative
# CAUTION: TAKES 10 OR MORE MINUTES TO RUN
results <- foreach(i=1:length(upto), .combine=rbind, .packages=c('randomForest','mltest')) %dopar%{
  set.seed(seeds[i])
  rows <- sample(1:nrow(training),upto[i])
  rf <- randomForest(handcode ~ ., data = training[rows,c("handcode",other_features, keyword_names, keyword_names_0)], 
                     ntree = specs$trees, mtry = specs$mtry, nodesize = specs$nodesize,
                     na.action = na.roughfix, mfixrep = 5, missfill = 2)
  oob <- mean(rf$err.rate[,"OOB"])
  preds <- predict(rf, df, type = "prob")
  preds <- as.data.frame(preds)
  names(preds) <- c("nontech","techneg","techneutral","techpos")
  tech_sents <- sum(preds$techneg + preds$techneutral + preds$techpos)
  salience <- tech_sents / (tech_sents + sum(preds$nontech))
  valence <- (sum(preds$techpos) + -1*sum(preds$techneg)) / tech_sents
  
  rm(preds)
  c(oob, tech_sents, salience, valence)
}


# Put errors and result stats into dataframe
results <- as.data.frame(cbind(upto,results))
names(results) <- c("upto","oob","tech_sentences","salience","valence")
mean_error$upto <- factor(median_error$upto)

# Plot OOB error and results distributions by number of sentences used
boxplot(oob~upto, data = mean_error)
boxplot(tech_sentences~upto, data = mean_error)
boxplot(salience~upto, data = mean_error)
boxplot(valence~upto, data = mean_error)



#### 3. Create predicted values for each sentence

# Run RF with best specifications
set.seed(2010)
rf <- randomForest(handcode ~ ., data = training[,c("handcode",other_features, keyword_names, keyword_names_0)], 
      ntree = specs$trees, mtry = specs$mtry, nodesize = specs$nodesize,
      na.action = na.roughfix, mfixrep = 5, missfill = 2)

# Record OOB error and balanced accuracy
rf
ml_test(rf$predicted, training$handcode)

# Predict out-of-sample values
preds <- predict(rf, df, type = "prob") # "prob" returns pr(nontech), pr(techneg), etc. in matrix form
preds <- as.data.frame(preds)

# Combine predictions with df
df <- cbind(df,preds)


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

# Prepare empty vector for calculation
entropy <- rep(0,nrow(df))

# Calculate entropy for each sentence
for (i in 1:nrow(df)){
  temp <- df[i,category_names]
  entropy[i] <- shannon(temp)
}

# Merge Shannon entropy into dataframe
df$entropy <- entropy


#####
# 6. Select sample for coding based on uncertainty (high entropy)

# Select specified number based on highest entropy
sorted <- df[order(-df$entropy),]
entropy_sample <- sorted[1:52,] # set number manually
entropy_sample <- entropy_sample[,c("id","text")]


#####
# 7. Select sample for coding based on manifesto undercoverage

# Merge df with training sample to ID sentences that have been coded
df <- merge(df, training[,c("id","date_coded")], by = "id", all.x = T) 
df$handcoded <- ifelse(complete.cases(df$date_coded),1,0) # indicate whether sentence has ever been coded, assign tiny value if not...

# Count the number handcoded for each manifesto
manifcounts <- aggregate(df$handcoded, by = list(df$manifesto), FUN = sum) # count handcodes per manif
names(manifcounts) <- c("manifesto","sents_handcoded")

# Order manifestos (ascending) by number coded, use random draw to break ties
manifcounts$rand <- rnorm(nrow(manifcounts)) # set random draw
manifcounts <- manifcounts[order(manifcounts[,"sents_handcoded"], -manifcounts[,"rand"] ),] # order by sentences coded, then random draw

# Select sentences at random from 4 least-coded manifestos
set.seed(2010)
coverage_sample_1 <- sample_n(df[df$manifesto==manifcounts$manifesto[1]], 2) # set number manually
coverage_sample_2 <- sample_n(df[df$manifesto==manifcounts$manifesto[2]], 2)
coverage_sample_3 <- sample_n(df[df$manifesto==manifcounts$manifesto[3]], 2)
coverage_sample_4 <- sample_n(df[df$manifesto==manifcounts$manifesto[4]], 2)

# Combine sentences
coverage_sample <- rbind(coverage_sample_1,coverage_sample_2,coverage_sample_3,coverage_sample_4)
coverage_sample <- coverage_sample[,c("id","text")]


#####
# 8. Combine uncertainty and undercoverage-based samples into one... export
new_sample <- rbind(entropy_sample, coverage_sample)
new_sample <- new_sample[-duplicated(new_sample$id),]
write.csv(new_sample,file="~/Penn State/Tech-induced job loss/coding_sampleXXX.csv")