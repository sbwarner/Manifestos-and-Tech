########################
# Party Platforms, Technology, and Tech-Induced Job Loss
#
# Part 1 - Preparing manifesto dataframe
#
# Last updated by Seth Warner, 9-18-20
########################

# 0. Load packages
install.packages("https://cran.r-project.org/src/contrib/Archive/manifestoR/manifestoR_1.3.0.tar.gz", repos = NULL)
install.packages("sentimentr")
library(manifestoR)
library(tokenizers)
library(stringr)
library(readxl)
library(sentimentr)

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

# 2. Clean the data

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

# c. Count words in sentences, keep sents of 5 words or more

df$length <- sapply(strsplit(df$text, " "), length)
df <- df[df$length>=5,]

#####

# 3. Create dataframe with sentences, IDs, manifesto info


# a. Create dataframe with sentence text

# vector for text
df <- english_corpus[[1]]$content 

# fill with sentences of 2nd to 420th manifs
for (i in 2:420){
  temp <- english_corpus[[i]]$content 
  df <- c(df,temp) 
}

# Assign variable name, change type to chr
df <- as.data.frame(df)
names(df) <- "text"
df$text <- as.character(df$text)


# b. Create sentence IDs

# create vector 
id <- paste(1,1:length(english_corpus[[1]]$content), sep="_")

# fill vector
for (i in 2:420){
  temp <- paste(i,1:length(english_corpus[[i]]$content),sep="_") #fill with IDs
  id <- c(id,temp)
}

# add to dataframe
df$id <- id


# c. Add party code and date

# create vectors
date <- rep(english_corpus[[1]]$meta$date, length(english_corpus[[1]]$content))
party <- rep(english_corpus[[1]]$meta$party, length(english_corpus[[1]]$content))

# fill vectors
for (i in 2:420){
  temp <- rep(english_corpus[[i]]$meta$date, length(english_corpus[[i]]$content))
  date <- c(date,temp)
  temp <- rep(english_corpus[[i]]$meta$party, length(english_corpus[[i]]$content))
  party <- c(party,temp)
}

# add to dataframe
df$party <- party
df$date <- date


#####

# 4. Merge with CMP codebook to get country and party names

# import codebook
manif.codebook <- read_excel("Penn State/Tech-induced job loss/documents_MPDataset_MPDS2020a.xlsx")

# merge by party ID and date, dropping manifs not in our dataset
df <- merge(df, manif.codebook, by = c("party","date"), all.y = F)

# get rid of created dups
dups <- duplicated(df$id)
df <- df[dups==F,]

# create year-party-country names
df$manifesto <- paste(df$partyname,df$countryname,df$date,sep = "-")


#####

# 5. Add initial keywords and sentiment as text features

# initial keywords (remove asterisks for colnames)
keywords <- c("comput*","automate","automation","robot*","digital","artificial intelligence","data","programmer",
"algorithm","high tech","internet","information technology","voice recog*","software","machine learning")

keyword_names <- c("comput","automate","automation","robot","digital","artificialintelligence","data","programmer",
              "algorithm","hightech","internet","informationtechnology","voicerecog","software","machinelearning")

# assign a column to each
df[,10:24] <- 0
names(df)[10:24] <- keyword_names

# count number of instances in each sentence
for (i in 1:length(keywords)){
  df[,keywords[i]] <- str_count(df$text, regex(keywords[i],ignore.case = T))
}

# count total number of keywords per sentence
df$keyword_count <- rowSums(df[,10:24])

# calculate sentiment by sentence
sentiment <- sentiment(get_sentences(df$text)) # sentimentR reqs get_sentences, which breaks them up differently than tokenizer
temp <- aggregate(sentiment$sentiment, by = list(sentiment$element_id), FUN = mean) # change back to vector of df length
df$sentiment <- temp$x

#####

# 6. Save dataframe
save(df, file="~/Penn State/Tech-induced job loss/manifesto_data.RData")
