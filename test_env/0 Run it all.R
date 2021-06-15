

# Load packages req'd for manifestoR
#install.packages("https://cran.r-project.org/src/contrib/Archive/manifestoR/manifestoR_1.3.0.tar.gz", repos = NULL)
library("NLP")
library("tm")
library("httr")
library("jsonlite")
library("functional")
library("zoo")
library("base64enc")
library("htmlwidgets")
library("DT")
library("htmltools")
library(manifestoR)

# Load text analysis packages
library(tokenizers)
library(stringr)
library(sentimentr)

# Load parallelizer and RF packages
library(doParallel)
library(randomForest)
library(mltest)

# Load other packages
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)
library(readxl)
library(ggplot2)


# set working directory
setwd("C:/Users/sethb/OneDrive/Penn State/Tech-induced job loss/test_env")
#setwd("/Users/th5/Downloads/Manifestos-and-Tech-master/test_env")


# run files with specifications
run_script_1 <- T
ifelse(run_script_1, source("1 Prepare dataframe.R"), print("SKIPPED Script 1 - prepare dataframe"))

run_script_2 <- T
ifelse(run_script_2, source("2 Draw first sample for hand-coding.R"), print("SKIPPED Script 2 - draw initial sample for handcoding"))

handcode_file <- "test_coded_sample.xlsx"
new_sample_name <- "handcode_sample_1.csv"
source("3 Active learning and iterative sampling.R")
