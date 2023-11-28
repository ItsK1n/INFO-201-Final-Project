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
# food_prices_df$inflation_trend <- filter(food_prices_df, (food_prices_df$avg_inflation > 1.0) == TRUE)
food_prices_df <- food_prices_df %>%
  mutate(inflation_trends = case_when(
    avg_inflation > 1 ~ "Positive",
    avg_inflation < 1 ~ "Negative",
    TRUE ~ "No Change"
  ))
df <- left_join(death_risk_df, food_prices_df, by = c("Entity" = "country", "Year" = "Year"))
df <- filter(df, Year >= 2007)

#write.csv(df, "combined_df.csv", row.names=FALSE)
