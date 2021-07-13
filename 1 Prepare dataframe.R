########################
# Party Platforms, Technology, and Tech-Induced Job Loss
#
# Part 1 - Preparing manifesto dataframe
#
# Last updated by Seth, 6-10-21
########################


#####

# 1. Collecting manifesto data

# a. Load CMP API (can be obtained for free online)
mp_setapikey("manifesto_apikey.txt")
mp_check_for_corpus_update(apikey = "manifesto_apikey.txt",
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

table(english) # 428 manifestos are coded as being in English
english_corpus <- corpus[english==T] # create new list, english_corpus
rm(corpus) # remove others

#####

# 3. Create dataframe of manifesto sentences

temp <- vector("list", length(english_corpus))
for(i in 1:length(english_corpus))
{
  temp[[i]]$text <- unlist(tokenize_sentences(english_corpus[[i]]$content$text))
  temp[[i]]$id <- paste(i,1:length(temp[[i]]$text),sep="_")
  temp[[i]]$date <- rep(english_corpus[[i]]$meta$date, length(temp[[i]]$text))
  temp[[i]]$party <- rep(english_corpus[[i]]$meta$party, length(temp[[i]]$text))
  temp[[i]]$length <- sapply(strsplit(temp[[i]]$text, " "), length)
}
df <- do.call("rbind", temp)
df <- as.data.frame(df)
df <- data.frame(matrix(unlist(df), ncol = ncol(df), byrow = F))

names(df) <- c("text","id","date","party","length")

df$length <- as.integer(df$length)
df <- df[df$length>=5,]


#####

# 3. Merge with CMP codebook to get country and party names

# import codebook
manif.codebook <- read_excel("data/manifestos_reference_sheet.xlsx")

# merge by party ID and date, dropping manifs not in our dataset
df <- merge(df, manif.codebook, by = c("party","date"), all.y = F)

# get rid of created dups
dups <- duplicated(df$id)
df <- df[dups==F,]

# create year-party-country names
df$manifesto <- paste(df$partyname,df$countryname,df$date,sep = "-")


#####

# 4. Add initial keywords, party data, and sentiment as text features

###
# a. Idenitfy keywords (remove asterisks for colnames)
keywords <- c("computer|computatational|computerize","automate","automation","robot*","digital","artificial intelligence","data","programmer",
"algorithm","high tech","internet","information technology","voice recog*","software","machine learning")

keyword_names <- c("comput_1","automate_1","automation_1","robot_1","digital_1","artificialintelligence_1","data_1","programmer_1",
              "algorithm_1","hightech_1","internet_1","informationtechnology_1","voicerecog_1","software_1","machinelearning_1")

keyword_names_0 <- c("comput_0","automate_0","automation_0","robot_0","digital_0","artificialintelligence_0","data_0","programmer_0",
                   "algorithm_0","hightech_0","internet_0","informationtechnology_0","voicerecog_0","software_0","machinelearning_0")

# create column for each keyword
df <- cbind(df, setNames( lapply(keyword_names, function(x) x=0), keyword_names) )
df <- cbind(df, setNames( lapply(keyword_names_0, function(x) x=0), keyword_names_0) )


# count number of instances in each sentence
for (i in 1:length(keywords)){
  df[,keyword_names[i]] <- ifelse(str_count(df$text, regex(keywords[i],ignore.case = T))>=1, 1, 0)
  df[,keyword_names_0[i]] <- ifelse(str_count(df$text, regex(keywords[i],ignore.case = T))>=1, 0, 1)
}

# count total number of keywords per sentence
df$keyword_count <- rowSums(df[,keyword_names])


###
# b. merge in V-Dem party data

# import data and crosswalk
vdem <- read.csv("V-Dem-CPD-Party-V1.csv")
crosswalk <- read.csv("vdem_crosswalk.csv")

# use party_state to match to V-DEM party codes
df$party_state <- paste(df$partyname,df$countryname,sep = ", ")
df <- merge(df,crosswalk, by = "party_state", all.x = T)

# create party-year codes to align data temporally
df$year <- as.integer(substr(df$date, start = 1, stop = 4))
df$party_year <- paste(df$v2paid, df$year, sep = "-")
vdem$party_year <- paste(vdem$v2paid, vdem$year, sep = "-")

# specify variables of interest
party_data <- c("v2paanteli","v2papeople","v2paopresp","v2paplur","v2paminor","v2paviol",
                "v2paimmig","v2palgbt","v2paculsup","v2parelig","v2pagender","v2pawomlab",
                "v2pariglef","v2pawelf","v2paclient")

# merge V-DEM variables into dataframe
df <- merge(df,vdem[,c(party_data,"party_year")], by = "party_year", all.x = T)


###
# c. calculate sentiment by sentence

sentiment <- sentiment(get_sentences(df$text)) # sentimentR reqs get_sentences, which breaks them up differently than tokenizer
temp <- aggregate(sentiment$sentiment, by = list(sentiment$element_id), FUN = mean) # change back to vector of df length
df$sentiment <- temp$x

#####

# 5a. Collect garbage
rm(list=setdiff(ls(), c("df","keyword_names","keyword_names_0","keywords","party_data")))

# 5b. Save dataset and environment
save(df, file="data/manifesto_data.RData")
save.image("outputs/1_output.RData")
