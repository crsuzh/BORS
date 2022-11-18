library(readxl)
dat <- read_excel("data/DrugSpeciesClean.xlsx", na="")

######## Variable types
dat$Drug <- as.factor(dat$Drug)
dat$Species <- as.factor(dat$Species)
dat$Experiment <- as.factor(dat$Experiment)
dat$Target_value <- as.numeric(dat$Target_value)


######## Counts into observations
# the Dx column contain the number of times a diameter of size x was observed
# for the same drug species combination one would repeat the value x as many times
# this is often called long format, i.e. several rows per id (here id = drug species comb)
mycounts <- as.matrix(dat[, 4:38])
rownames(mycounts) <- with(dat,  paste(Drug, Species, sep=", "))
mydiameters <- function(countpercomb) {rep(6:40,countpercomb)}
alldiameters<-apply(mycounts,1,mydiameters)


######## Calculate mean and standard deviation and compare with the target value (if it exists)
# For my combinations
mycomb <- cbind(names(alldiameters)[1], 
                round(mean(unlist(alldiameters[1])),2), 
                round(sd(unlist(alldiameters[1])),2), 
                dat$Target_value[1])
colnames(mycomb) <- c("Combination","Mean","SD","Target")

#      Combination          Mean    SD     Target
#[1,] "Ampicillin, E.coli" "20.21" "1.62" "19"  

write.csv(mycomb, "mycombinfo.csv", row.names = FALSE)
