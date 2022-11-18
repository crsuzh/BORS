library(readxl)
library(tidyverse)
library(writexl)

############## Data corrections in R
data.raw<-read_excel('data/DrugSpecies.xlsx')
names(data.raw)
dim(data.raw)
str(data.raw) # -> realize how many columns are numeric/character
head(data.raw)
tail(data.raw)
# -> realize that the first row needs to be skipped and that the last row
# does not contain data
data.raw <- read_excel('data/DrugSpecies.xlsx', skip=1)
data.clean <- data.raw
data.clean <- data.raw[-dim(data.raw)[1],]
str(data.clean) # -> more numeric columns now! D6,D8,D12 D15 and D19 still char
# Look at the last two columns 
unique(data.raw$`Target value`)
unique(data.raw$`Retrieval date`) # -> there is a problem!
# import again, giving the NA information my solve some problems
data.raw<-read_excel('data/DrugSpecies.xlsx', skip=1, na=c("","NA","not possible"))
data.clean <- data.raw
data.clean <- data.raw[-dim(data.raw)[1],]
str(data.clean) # Target value OK now

# The names of most columns are OK now, remove two remaining spaces in column names
names(data.clean)
names(data.clean)[names(data.clean) == 'Target value'] <- 'Target_value'
names(data.clean)[names(data.clean) == 'Retrieval date'] <- 'Retrieval_date'

# Look at the date variable
tmp <- data.clean$Retrieval_date
unique(tmp)
which(tmp == "1/10/2051") # -> potential error, discuss with owner of data
which(tmp == "27.10.2016") # -> potential error, discuss with owner of data
tmp[tmp == "1/10/2015"] <- 20151001
tmp[tmp == "1/10/2051"] <- 20151001 
tmp[tmp == "27.01.2016"] <- 20160127
tmp[tmp == "27.10.2016"] <- 20160127
unique(tmp)
data.clean$Retrieval_date_corrected<-as.numeric(tmp) # keep the original!

# the remaining problem in columns D6,D8,D12 D15 and D19 
str(data.clean) 
# make sure to find all columns with the problem
charcols<-which(sapply(data.clean, is.character)) 
charcols
charcols<-charcols[-c(1:3,9)]
unique(data.clean$D6) # the problem is "o" instead of 0 (need to check others too)
# replace o with 0, inform data owner
data.clean[data.clean=="o"] <- "0"
# need to make the columns numeric
data.clean[,charcols] <- lapply(data.clean[,charcols], as.numeric)
which(sapply(data.clean, is.character)) 
sum(data.clean == "o",na.rm=T) #check

# alternative
data.clean <- data.clean %>%
  mutate(D6 = ifelse(D6=="o", "0", D6)) %>%
  mutate(D8 = ifelse(D8=="o", "0", D8)) %>%
  mutate(D12 = ifelse(D12=="o", "0", D12)) %>%
  mutate(D15 = ifelse(D15=="o", "0", D15)) %>%
  mutate(D19 = ifelse(D19=="o", "0", D19))
sum(data.clean == "o",na.rm=T) #check
data.clean <- data.clean %>% mutate_at(charcols, as.numeric) 

# alternative
data.clean <- data.clean %>%
  mutate(across(.cols = where(is.character),
                .fns = ~if_else(. == "o","0",.)))
sum(data.clean == "o",na.rm=T) # check
data.clean <- data.clean %>% mutate_at(charcols, as.numeric) 

# Finally look at the columns that indeed need to be characters
# the code book sheet of the Excel sheet helps to know what to expect
unique(data.clean$Experiment) 
data.clean$Experiment[data.clean$Experiment %in% c("Referene","Refence")] <- "Reference"
data.clean$Experiment[data.clean$Experiment %in% c("Testing","Tesz", "Tesr")] <- "Test"
unique(data.clean$Experiment)
#
unique(data.clean$Species)
which(data.clean$Species == "Ecoli") # inform data owner
which(data.clean$Species == "E.feacalis") # inform data owner
which(data.clean$Species %in% c("S.aureis","S.areeus")) # inform data owner
data.clean$Species[data.clean$Species == "Ecoli"] <- "E.coli"
data.clean$Species[data.clean$Species == "E.feacalis"] <- "E.faecalis"
data.clean$Species[data.clean$Species %in% c("S.aureis","S.areeus")] <- "S.aureus"
unique(data.clean$Species)

############## Codebook

data.code<-read_excel('data/DrugSpecies.xlsx', sheet= "Code book", col_names = FALSE)
dim(data.code)
data.code
colnames(data.clean)
# the D6 to D40 columns are not explained at all
data.code.new <- data.frame(matrix(NA, nrow = (dim(data.clean)[2]-1), ncol = 4))
colnames(data.code.new)<-c("Name", "Description", "Range", "Categories")
# First row is OK to take, does not have the categories (too many9
data.code.new[1,] <- data.code[1,1:4]
# Second row is OK up to column 3, cat3gories have to be assembled
data.code.new[2,1:3] <- data.code[2,1:3]
data.code.new[2,4] <- paste(data.code[2,4],data.code[2,5],data.code[2,6], sep =", ")
# more elegant to use: with(data.code[2,],  paste(...4, ...5, ...6, sep=", "))
# Third row 
data.code.new[3,1] <- data.code[3,1]
data.code.new[3,3] <- data.code[3,3]
data.code.new[3,2] <- paste(data.code[3,2],data.code[3,6], sep =": ")
data.code.new[3,4] <- paste(data.code[3,4],data.code[3,5], sep =", ")
# Dxx columns
data.code.new[4:(nrow = dim(data.clean)[2]-3),1] <- colnames(data.clean)[4:(nrow = dim(data.clean)[2]-3)]
data.code.new[4:(nrow = dim(data.clean)[2]-3),2] <- "Number of experiments that resulted in the indicated disk diameter [mm]"
data.code.new[4:(nrow = dim(data.clean)[2]-3),3] <- paste("From ", apply(data.clean[,4:(nrow = dim(data.clean)[2]-3)],2,range)[1,], 
                                                        "to ", apply(data.clean[,4:(nrow = dim(data.clean)[2]-3)],2,range)[2,])
# Target value
data.code.new[dim(data.clean)[2]-2,1] <- colnames(data.clean)[dim(data.clean)[2]-2]
data.code.new[dim(data.clean)[2]-2,2] <- data.code[4,2]
data.code.new[dim(data.clean)[2]-2,3] <- paste("From ", range(data.clean[,dim(data.clean)[2]-2], na.rm = TRUE)[1], 
                                             "to ", range(data.clean[,dim(data.clean)[2]-2], na.rm = TRUE)[2])
# Retrieval data
data.code.new[dim(data.clean)[2]-1,1] <- colnames(data.clean)[dim(data.clean)[2]-1]
data.code.new[dim(data.clean)[2]-1,2] <- data.code[5,2]
data.code.new[dim(data.clean)[2]-1,3] <- "Contains errors in the values: next column contains corrections"
# Corrected retrieval date
data.code.new[dim(data.clean)[2],1] <- colnames(data.clean)[dim(data.clean)[2]]
data.code.new[dim(data.clean)[2],2] <- data.code[5,2]
data.code.new[dim(data.clean)[2],3] <- "Two possible dates and NA"
data.code.new[dim(data.clean)[2],4] <- paste(unique(data.clean$Retrieval_date_corrected),collapse = ", ")

##############################
# Write into cleaned data file
##############################

cleandata <- list("Cleaned data" = data.clean, "Code book" = data.code.new)
write_xlsx(cleandata, "data/DrugSpeciesClean.xlsx")


############## Variable types
dat <- data.clean
dat$Drug <- as.factor(dat$Drug)
dat$Species <- as.factor(dat$Species)
dat$Experiment <- as.factor(dat$Experiment)
dat$Target_value <- as.numeric(dat$Target_value)


############## Counts into observations
# the Dx column contain the number of times a diameter of size x was observed
# for the same drug species combination one would repeat the value x as many times
# this is often called long format, i.e. several rows per id (here id = drug species comb)
mycounts <- as.matrix(dat[, 4:38])
rownames(mycounts) <- with(dat,  paste(Drug, Species, sep=", "))
mydiameters <- function(countpercomb) {rep(6:40,countpercomb)}
alldiameters<-apply(mycounts,1,mydiameters)
#hist(alldiameters[1])
hist(unlist(alldiameters[1]))




