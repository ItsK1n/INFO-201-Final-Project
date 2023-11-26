library(dplyr)
library(stringr)

death_risk_df <- read.csv("number-of-deaths-by-risk-factor.csv")
food_prices <- read.csv("share-healthy-diet-unaffordable.csv")

df <- left_join(food_prices,death_risk_df, by = c("Entity", "Year"))

# Data Cleaning : Only have data from 2017-2019
#print(df$Code.y)
#for(i in 1:nrow(df)){
 # if(is.null(df$Code.y[i])){
  #  print("No Code.y")
   # df<- df[-i, ]
  #}
#}
#rownames(df) <- NULL 

#df <- filter(df, is.null(df$Code.y) != TRUE)

#new_df <- subset(df, is.null(df$Code.y)) == FALSE))
