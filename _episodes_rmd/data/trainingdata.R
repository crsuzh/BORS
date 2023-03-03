library(readxl)
library(tidyverse)
library(writexl)

# need to find the NA symbols used beforehand. Without specifying what NAs are
# columns with dates will not be recognized as such
data.raw <- read_excel('_episodes_rmd/data/trainingdata.xls', 
                     na=c("keine","unbek.",""))
# get the columns which are supposed to contain dates
names(data.raw)
nms.dat <- names(data.raw)
nms.dat
datecols <- c("Start.of.symptoms","Inclusion.date")
mycoltypes <- rep("guess",length(nms.dat))
mycoltypes
mycoltypes[match(datecols, nms.dat)]<-"date"
str(data.raw[,mycoltypes=="date"])
# not all of them are read in with the date format -> force the format to find errors

data.raw.types<-read_excel('_episodes_rmd/data/trainingdata.xls', 
                           na=c("keine","unbek.",""),
                           col_types=mycoltypes)
str(data.raw.types[,mycoltypes=="date"])
# format worked but have to fix the errors

# First row in excel sheet is the header, compare forced and unforced
# -> R puts an NA for nonsensical dates
# look at rows 250, 415 and 480?
range(data.raw$Start.of.symptoms,na.rm=T)
range(data.raw$Inclusion.date,na.rm=T)

