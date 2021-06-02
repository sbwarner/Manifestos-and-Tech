###########################
# Party Platforms, Technology, and Tech-Induced Job Loss
#
# Part 3 - Active learning and iterative sampling
#
# Last updated: Toby, 5-27-2021
########################


rm(list=ls())



library(plyr)
library(doParallel)
library(randomForest)
library(mltest)
library(dplyr)
library(tidyr)
library(reshape2)
library(readxl)
library(ggplot2)


#####
setwd("C:/Users/sethb/OneDrive/Penn State/Tech-induced job loss")
 setwd("/Users/th5/Downloads/Manifestos-and-Tech-master")


# 0. Load and merge data
load("manifesto_data.RData")
coded_sample <- read_xlsx("test_coded_sample.xlsx")

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

#specs <- specs[sample(1:nrow(specs), size=4),]



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
training$order <- 1:nrow(training)

# Prepare to run RFs
upto <- c(seq(from=50, to=nrow(training), by=3), nrow(training))
upto <- unique(upto)
n_upto <- length(upto)

# Prepare separate seed for each RF run
set.seed(75)


get_summ <- function(x, ...)
{
  tech_sents <- sum(x$techneg + x$techneutral + x$techpos)
  salience <- tech_sents / nrow(x)
  valence <- (sum(x$techpos) - sum(x$techneg)) / tech_sents
  
  return(data.frame(Valence=valence, Salience=salience))
}

# Run 20 RFs for each set of 50 sentences, cumulative
# CAUTION: TAKES 10 OR MORE MINUTES TO RUN
results <- foreach(i=1:n_upto, .packages=c('randomForest','mltest')) %dopar%{
  rows <- which(training$order <= upto[i])
  rf <- randomForest(handcode ~ ., data = training[rows, c("handcode",other_features, keyword_names, keyword_names_0)], 
                     ntree = specs$trees, mtry = specs$mtry, nodesize = specs$nodesize,
                     na.action = na.roughfix, mfixrep = 5, missfill = 2)
  oob <- mean(rf$err.rate[,"OOB"])
  preds <- predict(rf, df, type = "prob")
  preds <- as.data.frame(preds)
  names(preds) <- c("nontech","techneg","techneutral","techpos")

  ## Summarize
  preds$manifesto <- df$manifesto
  summ_manifesto <- ddply(.data=preds, .variables="manifesto", .fun=function(x) get_summ(x=x))
  summ_manifesto$batch <- upto[i]
  
  return(list(oob=oob, summ_manifesto=summ_manifesto))
}


## Plot estimates by manifesto
res_manifesto <- ldply(.data=results, .fun=function(x) x$summ_manifesto)
res_manifesto <- melt(res_manifesto, id.vars=c("manifesto", "batch"))
translate <- data.frame(batch_lag=upto[-1], batch=upto[-length(upto)])
res_manifesto_lag <- merge(res_manifesto, translate, by="batch", all.x=TRUE)
res_manifesto_lag$batch <- NULL
res_manifesto_lag$batch <- res_manifesto_lag$batch_lag
res_manifesto_lag$batch_lag <- NULL
res_manifesto_lag <- na.omit(res_manifesto_lag)
colnames(res_manifesto_lag)[which(colnames(res_manifesto_lag) == "value")] <- "value_Lag"
res_manifesto <- merge(res_manifesto, res_manifesto_lag, by=c("batch", "manifesto", "variable"))
res_manifesto$Diff <- res_manifesto$value - res_manifesto_lag$value_Lag
res_manifesto <- na.omit(res_manifesto)

g <- ggplot(data=res_manifesto, aes(x=batch, y=Diff, group=manifesto))
g <- g + geom_line(size=0.2) + facet_wrap(~ variable, ncol=2, scales="free_y")
g <- g + theme_minimal()
g <- g + xlab("Number of coded observations") + ylab("Change to previous batch")



## Errors
res_errors <- ldply(.data=results, .fun=function(x) x$oob)
res_errors <- data.frame(oob=res_errors$V1, batch=upto)

g <- ggplot(data=res_errors, aes(x=batch, y=oob))
g <- g + geom_line(size=1)
g



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

category_names <- c("nontech","techneg","techneutral","techpos")
colnames(preds) <- category_names



# Combine predictions with df
df <- cbind(df, preds)


#####
# 4. Calculate preliminary results

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

# Calculate entropy for each sentence
entropy <- apply(df[,category_names], 1, shannon)


# Merge Shannon entropy into dataframe
df$entropy <- entropy


#####
# 6. Select sample for coding based on uncertainty (high entropy)

# Select specified number based on highest entropy
sorted <- df[order(-df$entropy),]
entropy_sample <- sorted[1:52,] # set number manually
entropy_sample <- entropy_sample[, c("id","text")]


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
coverage_sample <- c()
for(i in 1:8)
{
  tmp <- sample_n(df[df$manifesto==manifcounts$manifesto[i], ], 1)
  coverage_sample <- rbind(coverage_sample, tmp)
}


# Combine sentences
coverage_sample <- coverage_sample[,c("id","text")]


#####
# 8. Combine uncertainty and undercoverage-based samples into one... export
new_sample <- rbind(entropy_sample, coverage_sample)
new_sample <- new_sample[-duplicated(new_sample$id),]
new_sample <- new_sample[sample(1:nrow(new_sample)),]
write.csv(new_sample,file="~coding_sampleXXX.csv")