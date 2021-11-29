####################################
# Kushagra Singh Bisen - M1 CPS2 #
#################################

######################################
# Question 1 - Italian Wines (50 Points) #
######################################
library(readr)
wine <- read.csv("/home/whiskygrandee/Documents/M1 CPS2/From Statistics to Data Mining/Final-Exam/wine.csv",
                 header = TRUE, sep = ",", quote = "\"'", dec = ".")
wine
summary(wine)

f <- function(x){
 if(x == 0){
   value <- 0
 }
if(x == 1){
  value <- 0.3314607
}
if(x == 2){
  value <- 0.7303371
}
if(x == 3){
  value <- 1
}
}

require(stats)
mean_final <- mean()

variance <- 0

for (i in 1:wine_max)
{
  variance <- variance + (expec - i)^2 * proba[i]
}

print(variance)
st_deviation <- sqrt(variance)
print(st_deviation)

pnorm(i, mean = mean_final, sd = st_deviation)
pnorm






f1 <- function(y) {
  
}








########################################
# Question 2 - Minke Whales in Iceland (50 Points) #
##########################################
library(readr)
whales <- read.csv("/home/whiskygrandee/Documents/M1 CPS2/From Statistics to Data Mining/Final-Exam/whales.csv",
                     header = TRUE, sep = ",", quote = "\"'", dec = ".")
whales
summary(whales)

# As we can see from the summary, various values here have NA=<some_value> in them.
# This means that they are variables with missing values, which are weight, age, stomach.volume,
# stomach.weight 

whales_new <- na.omit(whales)
# Checking again for the NA values. 
summary(whales_new)

# I think we can not do statistical analysis with data is the ID varialble, I think it is not that important,
# as it is not related to any other variables, and was maybe taken arbitrary as a reading thing.

whales_final <- whales_new[,2:13 ]
whales_final #Final Dataset we will be working with.

install.packages("lubridate")
install.packages("ggplot2")

library(lubridate)
date_random <- whales_final[,1]
date_random

date_whale <- date(date_random)
week_whale <- week(date_random)
month_whale <- month(date_random)

library(ggplot2)
qplot(month_whale, week_whale)
whales_final

# By looking at these two plots, whaling is mainly allowed in the months of 4,6,7,8,9 with 
# they catching the most fish on the months 7 and 8 with 4 and 3 points each.

keep <- c("month_whale", "date", "lon", "lat", "length", "weight", "age", "stomach.volume", "stomach.weight", "year")

column <- whales_final("date","lon","lat","length","weight","age", "stomach.volume", "stomach.weight", "year")
whales_updated <- data.frame(column)
whales_updated

whales.log <- log(whales_updated)

whales_final
