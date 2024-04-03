# Accessing useful packages
library(ggplot2)
library(MASS) 
library(tidyverse)

##################################################################
# Function: Data_Cleaning
# Converting the data to a log function and data cleaning 0s
# Input: file_name = .csv file that you want to read
#        variable = variable of interest
# Output: x = cleaned data set
# ---------------------------------------------------------------
Data_Cleaning <- function(file_name,variable){
  z <- read.table(file=file_name,header=TRUE,sep=",")
  x <-z%>%filter({{variable}}>0)%>%
    mutate(myvar=log({{variable}}))
  return(x)
}
a <- Data_Cleaning(file_name="Burnham_field_data_bombus_seasonal_variation_Dataset.csv",pathogen_load)

##################################################################
# Function: Data_Frames
# Converting to a Data Frame
# Input: a = data set, site = site code, variable = pathogen load
# Output: z = list of data frames for each site
# ---------------------------------------------------------------

Data_Frame <- function(a,site,variable){
  b <- data.frame(site,variable)
  S1 <- c(b[site=="CIND",]$variable)
  S2 <- c(b[site=="BOST",]$variable)
  S3 <- c(b[site=="MUDGE",]$variable)
  S4 <- c(b[site=="FLAN",]$variable)
  S5 <- c(b[site=="COL",]$variable)
  z <- list(CIND=S1, BOST=S2, MUDGE=S3, FLAN=S4, COL=S5)
  return(z)
}
b <- Data_Frame(a=a,site=a$site_code,variable=a$pathogen_load)
##################################################################
# Function: Fake_Params
# Generating the mean, sd, and sample size for the fake data
# Input: data = cleaned data frame
#        Sites = names of sites
# Output: mean, sd, and n (sample size)
# ---------------------------------------------------------------


Fake_Params <- function(my_list){
  stats_frame <- rep(NA, 3*length(my_list))
    Mean <- c(mean(my_list$CIND), mean(my_list$BOST), 
              mean(my_list$MUDGE),mean(my_list$FLAN),mean(my_list$COL))
    SD <- c(sd(my_list$CIND), sd(my_list$BOST), 
            sd(my_list$MUDGE),sd(my_list$FLAN),sd(my_list$COL))
    n <- c(length(my_list$CIND), length(my_list$BOST), 
           length(my_list$MUDGE),length(my_list$FLAN),length(my_list$COL))
    stats_frame <- data.frame(Mean,SD,n)
}
c=Fake_Params(my_list=b)

