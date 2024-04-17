# Homework 11: Batch Processing
# Nicole Phelan

# Question 1

# I created a new project in R called BIOL6100_Homework11 and moved the barracudar and raw data folder into it.

library(log4r)
library(TeachingDemos)
library(tidyverse)
library(pracma)
library(ggmosaic)

source("barracudar/DataTableTemplate.R")
source("barracudar/AddFolder.R")
source("barracudar/BuildFunction.R")
source("barracudar/MetaDataTemplate.R")
source("barracudar/CreatePaddedLabel.R")
source("barracudar/InitiateSeed.R")
source("barracudar/SetUpLog.R")
source("barracudar/SourceBatch.R")

source("barracudar/QBox.R")
source("barracudar/QCon1.R")
source("barracudar/QCon2.R")
source("barracudar/QHist.R")
source("barracudar/QLogis.R")
source("barracudar/QScat.R")
source("barracudar/QBub.R")
source("barracudar/QContour.R")

add_folder() # creates the OriginalData folder

# Question 2

# Iterate through each folder 

getwd()
setwd("/Users/nicole/Desktop/BIOL6100_Homework11/OriginalData")
filelist <- list.files("/Users/nicole/Desktop/BIOL6100_Homework11/OriginalData")
filelist

# paste() or paste0() concatenates strings
paste0("here is ","the filepath ", filelist[1])

# use a for loop for the number of files we're concerned with and pull out files
filenames <- c()
for (i in 1:9) {
  setwd(paste0("/Users/nicole/Desktop/BIOL6100_Homework11/OriginalData", "/", filelist[i]))
  filenames[i] <- list.files(pattern="countdata")
}

filenames

# Question 3

# Cleaning the Data 

# Inside a for loop I want to 
# 1. go to the wd of each countdata file
# 2. read the file
# 3. remove the rows where the scientific name is unknown
# 4. change my wd to the CleanedData folder
# 5. create a new csv file of the cleaned data

years <- c(2015,2016,2017,2018,2019,"2020A","2020B",2021,2022)
# the data has two count data files for 2020
for (i in 1:9){
  setwd(paste0("/Users/nicole/Desktop/BIOL6100_Homework11/OriginalData", "/", filelist[i]))
  a=read.csv(file = filenames[i], na.strings = c("","NA"))
  b=a[complete.cases(a["scientificName"]), ]
  setwd("/Users/nicole/Desktop/BIOL6100_Homework11/CleanedData")
  write.csv(b,paste0("CleanedData_",years[i],".csv"))
}

# Extracting the Year from the File Names

# 1. Create a vector of the cleaned data file names
# 2. Within a for loop, extract the year from each file name

getwd()
filelist1 <- list.files("/Users/nicole/Desktop/BIOL6100_Homework11/CleanedData")
filelist1
for (i in 1:9){
  a = substring(filelist1[i],13,16)
  cat("The year for file ", i, " is ", a,"\n")
}

# Calculate Abundance For Each Year

# Inside a for loop, I want to
# 1. read each cleaned file
# 2. add up all of the values for cluster size
# 3. print out the abundance for each  year

filelist1 <- file.remove("CleanedData_2020B.csv")
filelist1
# removing the second sampling period for 2020 because it occurred at a different time
for (i in 1:8){
  a=read.csv(file = filelist1[i])
  c = substring(filelist1[i],13,16)
  b= sum(a$clusterSize)
  cat("The species abundance for year ", c, " is ", b,"\n")
}

# Calculate Species Richness For Each Year

# Inside a for loop I want to
# 1. read each cleaned file
# 2. determine the length of the unique scientific name vector
# 3 print out the species richness for each year

for (i in 1:8){
  a=read.csv(file = filelist1[i])
  c = substring(filelist1[i],13,16)
  d=length(unique(a$scientificName))
  cat("The species richness for year ", c, " is ", d, " species","\n")
}

# Question 4
Years <- 2015:2022
SpeciesAbundance <- vector("numeric",8L)
for (i in 1:8){
  a=read.csv(file = filelist1[i])
  b= sum(a$clusterSize)
  SpeciesAbundance[i] <- b
}
SpeciesRichness <-  vector("numeric",8L)
for (i in 1:8){
  a=read.csv(file = filelist1[i])
  d=length(unique(a$scientificName))
  SpeciesRichness[i] <- d
}
Summary_Frame <- data.frame("FileNames"=filelist1,Years,SpeciesAbundance,SpeciesRichness)
