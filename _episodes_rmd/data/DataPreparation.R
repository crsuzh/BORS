library(tidyverse)
library(readxl)
data <- read_xls("trainingdata.xls", 
                 range = cell_cols(c("D", "E", "F", "G", "H", "I", "J", "K")))
str(data)
data <- data %>% mutate(
  "1: Zähler" = as.numeric(gsub(",", ".", data$`1: Zähler`)),
  "1: Nenner" = as.numeric(gsub(",", ".", data$`1: Nenner`)),
  "2: Zähler" = as.numeric(gsub(",", ".", data$`2: Zähler`)),
  "2: Nenner" = as.numeric(gsub(",", ".", data$`2: Nenner`)),
  "3: Zähler" = as.numeric(gsub(",", ".", data$`3: Zähler`)),
  "3: Nenner" = as.numeric(gsub(",", ".", data$`3: Nenner`)),
)

mean(data$`0: Zähler`, na.rm = T)
mean(data$`0: Nenner`, na.rm = T)

n <- nrow(data)
# fake data matrix
dataFake <- matrix(NA, nrow = n, ncol = ncol(data))
for (i in 1:ncol(data)) {
  set.seed(2023)
  t <- data[, i]
  sd <- sd(t[[1]], na.rm = T)
  dataFake[,i] <- round(t[[1]] + rnorm(n, mean = 0, sd = sd), digits = 2)
}
colnames(dataFake) <- colnames(data)
dataFake <- as.data.frame(dataFake)

apply(data, 2, mean, na.rm = T)
apply(dataFake, 2, mean, na.rm = T)

# sample locations of value that using comma as decimal mark

# How many NA's in each column
summary(data) # minal number of NA is 21
# location of non-NA in each column
r <- nrow(data) - 21
loc <- matrix(NA, nrow = r, ncol = ncol(data))
c <- 1
for (i in colnames(data)) {
  t <- !is.na(data[, i])
  na <- rep(NA, length.out = (r - sum(!is.na(data[, i]))))
  loc[,c] <-c(which(t, data[, i]), na)
  c <- c + 1
}
colnames(loc) <- colnames(data)

set.seed(2023)
# locations for 1: Zähler
(loc1z <- sample(loc[1:269,3], size = 5))
# locations for 1: Nenner
(loc1n <- sample(loc[1:269,4], size = 6))
# locations for 2: Zähler
(loc2z <- sample(loc[1:85,5], size = 1))
# locations for 2: Nenner
(loc2n <- sample(loc[1:85,6], size = 1))
# locations for 3: Zähler
(loc3z <- sample(loc[1:161,7], size = 3))
# locations for 3: Nenner
(loc3n <- sample(loc[1:161,8], size = 3))
# Manually modify "." into "," using above locations

writexl::write_xlsx(dataFake, 
                    path = "/Users/lishoushou/Documents/Job/Hiwi/BORS/trainingdata/fakedData.xlsx")
