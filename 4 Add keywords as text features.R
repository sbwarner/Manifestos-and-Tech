########################
# Party Platforms, Technology, and Tech-Induced Job Loss
#
# Part 4 - Add new keywords as text features
#
# Last updated by Seth Warner, 11-11-20
########################

# After each round of coding, analysts may wish to add new keywords as text features to improve
# predictive accuracy. Keywords can be added into the search_terms vector below.
# Then, the remainder of the script can be exectued.


# 0. Load package and data
library(stringr)
load("~/Penn State/Tech-induced job loss/manifesto_data.RData")


# 1. Identify keywords
search_terms <- c("health","budget","child*","war","defense","sanctions","inequality","wealth","party","elect*",
                  "platform","delegate","parliament","president","prime minister","transport*","infrastructure","deficit",
                  "inflation","unemployment","voting")


# 2. Create column names, removing wild cards and spaces
terms_1 <- paste(search_terms, "_1", sep = "")
terms_0 <- paste(search_terms, "_0", sep = "")

terms_1 <- str_replace(terms_1,"\\*","")
terms_0 <- str_replace(terms_0,"\\*","")

terms_1 <- str_replace(terms_1," ","_")
terms_0 <- str_replace(terms_0," ","_")


# 3. Create matrix to hold keyword counts
term_lookup <- as.data.frame(matrix(0, nrow = length(df$text), ncol = length(c(terms_1,terms_0))))
names(term_lookup) <- c(terms_1,terms_0)


# 4. Count keywords in text
for (i in 1:length(search_terms)){
  term_lookup[,terms_1[i]] <- ifelse(str_count(df$text, regex(search_terms[i],ignore.case = T))>=1, 1, 0)
  term_lookup[,terms_0[i]] <- ifelse(str_count(df$text, regex(search_terms[i],ignore.case = T))>=1, 0, 1)
}


# 5. Merge datasets and vector with column names
df <- cbind(df,term_lookup)
keyword_names <- c(keyword_names, terms_1)
keyword_names_0 <- c(keyword_names_0, terms_0)

# 6. Remove temp items from environment, save dataframe
rm(search_terms,i,term_lookup,terms_0,terms_1)
save.image("~/Penn State/Tech-induced job loss/manifesto_data.RData")
