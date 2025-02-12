library(tidyverse)

options(scipen = 99)

df <- read.csv('../zillow_data/stratified_properties_2016.csv')


df <- df[df$yearbuilt_bin != "Early 19th century",]
df <- df[df$propertylandusetypeid != 264,]
# Count occurrences of each category
category_counts <- table(df$regionidzip)

# Filter out categories with counts less than 10
valid_categories <- names(category_counts[category_counts >= 5])

df <- df[df$regionidzip %in% valid_categories,]

table(df$regionidzip)

data.table::fwrite(df, "../zillow_data/stratified_properties_2016v2.csv", row.names = FALSE)




columns_to_factor <- c("airconditioningtypeid", "decktypeid", "fips", "hashottuborspa",
                       "heatingorsystemtypeid","poolcnt", "pooltypeid10", "pooltypeid2",
                       "propertylandusetypeid", "regionidzip", "fireplaceflag", "taxdelinquencyflag",
                       "fireplacecnt", "garagecarcnt", "unitcnt", "yearbuilt_bin")

df[columns_to_factor] <- lapply(df[columns_to_factor], as.factor)
summarytools::stview(summarytools::descr(df, transpose = TRUE))
