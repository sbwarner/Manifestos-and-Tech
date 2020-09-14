########################
# Party Platforms, Technology, and Tech-Induced Job Loss
#
# Part 1 - Collecting, cleaning, and sampling manifesto data
#
# Last updated by Seth Warner, 9-14-20
########################

# 0. Load packages
install.packages("https://cran.r-project.org/src/contrib/Archive/manifestoR/manifestoR_1.3.0.tar.gz", repos = NULL)
library(manifestoR)
library(tokenizers)
library(stringr)

#####

# 1. Collecting manifesto data

# a. Load CMP API (can be obtained for free online)
mp_setapikey("C:/Users/sethb/OneDrive/Documents/Penn State/manifesto_apikey.txt")
mp_check_for_corpus_update(apikey = "C:/Users/sethb/OneDrive/Documents/Penn State/manifesto_apikey.txt",
                           only_stable = TRUE)

# b. Collect full corpus of manifestos
corpus <- mp_corpus(date > 194500)

# c. Subset to English-language

english <- rep(FALSE,length(corpus)) 
# creates vector to hold language designations

for (i in 1:length(corpus)){
  english[i] <- grepl("english",corpus[[i]]$meta$language) 
  #checks if "English" is listed as manif language
}

table(english) # 420 manifestos are coded as being in English
english_corpus <- corpus[english==T] # create new list, english_corpus
rm(corpus) # remove others

#####

# 2. Clean the data and count keywords

# a. Concatenate CMP quasi-sentences to create one long text for each manifesto

for (i in 1:length(english_corpus)){
  text <- paste(english_corpus[[i]]$content$text, collapse = ' ')
  english_corpus[[i]]$content <- NA
  english_corpus[[i]]$content <- text
}

# b. Break apart into full sentences, one row for each

for (i in 1:length(english_corpus)){
  english_corpus[[i]]$content <- unlist(tokenize_sentences(english_corpus[[i]]$content))
}


# c. load keywords in regular expression terms

keywords <- "comput*|automat*|robot*|digital|artificial intelligence|data|programmer|
algorithm|high tech|internet|information technology|voice recog*|software|machine learning"

# d. count numbers of keywords per manifesto and sentence

keywords_sentence <- list(rep(0,length(english_corpus)))
#empty list for sentences
keywords_manif <- rep(0,length(english_corpus))
# empty vector for manifestos

for (i in 1:length(english_corpus)){
  
  keywords_sentence[[i]] <- rep(0, length(english_corpus[[i]]$content))
  #creates as many rows in list[1, 2, ... N] as there are setnences in that manifesto
  
  keywords_sentence[[i]] <- str_count(english_corpus[[i]]$content, regex(keywords, ignore.case = T))
  #Fills rows with number of keywords in correponding sentence
  #e.g., keywords_setnece[[1]][5] would return the number of keywords in the 5th sentence of 1st manifesto
  
  keywords_manif[i] <- sum(keywords_sentence[[i]])
  #finds sum of all keywords in each manifesto
}


#####

# 3. Draw samples

# a. Draw random sample of sentences
set.seed(1986) #Go Mets

manifs_draw1 <- sample(1:length(english_corpus),75,replace = T)
# simple random sample of manifestos 

sents_draw1 <- rep(NA,75) # vector to capture sentence number
sample_random <- rep(NA,75) # vector to capture sentence text

for (i in 1:length(manifs_draw1)){
  sents_draw1[i] <- sample(1:length(english_corpus[[manifs_draw1[i]]]$content),1)
  # selects and stores random sentence from manifesto
  
  sample_random[i] <- english_corpus[[manifs_draw1[i]]]$content[sents_draw1[i]]
  # stores sentence text 
}


# b. Draw sample of sentences with keywords
set.seed(1969) # Go Mets!

has_keyword <- ifelse(keywords_manif>=1,1,0) 
# vector for whether manifesto contains a keyword

manifs_draw2 <- sample(1:length(english_corpus),75,replace = T,prob = has_keyword) 
# simple random sample of manifestos with keywords 

sents_draw2 <- rep(NA,75) # vector to capture sentence number
sample_keywords <- rep(NA,75) # vector to capture sentence text

for(i in 1:length(manifs_draw2)){
  sents_draw2[i] <- sample(1:length(english_corpus[[manifs_draw2[i]]]$content),1,
                                 prob=keywords_sentence[[manifs_draw2[i]]])
  # selects sentence from manifesto
  # does so by Proportional Probability Sampling
  # i.e., sentence with 2 keywords has twice the prob of being sampled as sentence with 1.
  
  sample_keywords[i] <- english_corpus[[manifs_draw2[i]]]$content[sents_draw2[i]]
  # captures text of sentence
}

#####

# 4. Combine samples and export as spreadsheet
random <- cbind(sample_random,manifs_draw1,sents_draw1)
keywords_sample <- cbind(sample_keywords,manifs_draw2,sents_draw2)
sample <- rbind(random,keywords_sample)
colnames(sample) <- c("sentence","manifesto","sentence")

write.csv(sample,file="~/Penn State/Tech-induced job loss/coding_sample1.csv")
