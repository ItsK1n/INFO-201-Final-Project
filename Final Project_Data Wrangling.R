library(dplyr)
library(stringr)
library(ggplot2)

# Deleted
# share-healthy-diet-unaffordable.csv
death_risk_df <- read.csv("number-of-deaths-by-risk-factor.csv")
food_prices_df <- read.csv("WLD_RTFP_country_2023-11-20 2.csv")

# New numeric column parsing date and taking out Year
food_prices_df$Year <- as.numeric(format(as.Date(food_prices_df$date, format="%Y-%m-%d"),"%Y"))
# Summarizing food_prices_df taking avg_inflation and median_inflation per year for each country
food_prices_df <- summarise(group_by(food_prices_df, country, date, Year), avg_inflation = mean(Inflation, na.rm = TRUE),
                            med_inflation = median(Inflation, na.rm = TRUE))
# New categorical column telling if avg_inflation rate is "positive", "negative", "no change"
food_prices_df$inflation_trend <- summarise(food_prices_df, )

df <- left_join(death_risk_df, food_prices_df, by = c("Entity" = "country", "Year" = "Year"))
df <- filter(df, Year >= 2007)


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
