library(pastecs)
library(tidyverse)

options(scipen = 99)

df <- read.csv('../zillow_data/properties_2016.csv')

# REMOVE UNIMPORTANT COLUMNS

df <- df %>% 
  select(-c("pooltypeid7", "propertycountylandusecode", "taxdelinquencyyear", "finishedsquarefeet6", "finishedsquarefeet13", "regionidcity",
            "architecturalstyletypeid", "typeconstructiontypeid", "storytypeid", "buildingclasstypeid", "propertyzoningdesc", "finishedsquarefeet50",
            "finishedfloor1squarefeet", "finishedsquarefeet12", "garagetotalsqft", "threequarterbathnbr", "fullbathcnt", "censustractandblock",
            "rawcensustractandblock", "finishedsquarefeet15", "assessmentyear"))

df[is.na(df$structuretaxvaluedollarcnt),]$structuretaxvaluedollarcnt <- 0
df[is.na(df$landtaxvaluedollarcnt),]$landtaxvaluedollarcnt <- 0
df <- df[!is.na(df$latitude),]

summary_func <- function(df){
  summary_df <- stat.desc(df) 
  reshaped_summary <- as.data.frame(t(summary_df))
  reshaped_summary <- tibble::rownames_to_column(reshaped_summary, 'feature')
  #reshaped_summary$perc_na <- round((reshaped_summary$nbr.na/nrow(df))*100,2)
  
  return(reshaped_summary)
}

reshaped_summary <- summary_func(df)

reshaped_summary$perc_missing <- 

# PHASE 1 CLEANING

# correct empty, NA columns
# individually investigate each column

# hashottuborspa
df[df$hashottuborspa == "",]$hashottuborspa <- "false"

# poolcnt
df[is.na(df$poolcnt),]$poolcnt <- 0

# poolsizesum: houses with pool cnt = 0 has 0 sqfoot poolsize
# put it in bin range
df[df$poolcnt == 0 & is.na(df$poolsizesum),]$poolsizesum <- 0
df[is.na(df$poolsizesum),]$poolsizesum <- median(df[df$poolcnt == 1,]$poolsizesum, na.rm = TRUE)


# pooltypeid10: 1 = spa, 0 = hot tub
df[df$hashottuborspa == "true" & is.na(df$pooltypeid10),]$pooltypeid10 <- "hottub"
df[is.na(df$pooltypeid10),]$pooltypeid10 <- "none"
df[df$pooltypeid10 == "1",]$pooltypeid10 <- "spa"

# pooltypeid2: 1 = pool_withtub, 0 = pool_withouttub
df[df$poolcnt == 1 & is.na(df$pooltypeid2),]$pooltypeid2 <- "pool_wotub"
df[is.na(df$pooltypeid2),]$pooltypeid2 <- "no_pool"
df[df$pooltypeid2 == "1",]$pooltypeid2 <- "pool_wtub"

# propertyzoningdesc
#df[df$propertyzoningdesc == "",]$propertyzoningdesc <- NA

# False negatives /  Type 2 erros present in both columns
# fireplaceflag & fireplacecnt
df[df$fireplaceflag == "",]$fireplaceflag <- "false"
df[df$fireplaceflag == 'true' & is.na(df$fireplacecnt),]$fireplacecnt <- 1
df[is.na(df$fireplacecnt),]$fireplacecnt <- 0
df[df$fireplacecnt >= 1,]$fireplaceflag <- "true"

# taxdelinquencyflag
df[df$taxdelinquencyflag == "",]$taxdelinquencyflag <- "false"
df[df$taxdelinquencyflag == "Y",]$taxdelinquencyflag <- "true"

# decktypeid
df$decktypeid <- ifelse(is.na(df$decktypeid), "false", "true")

# yardbuildingsqft17   
names(df)[names(df) == "yardbuildingsqft17"] <- "patio_yrd"
df[is.na(df$patio_yrd),]$patio_yrd <- 0

# yardbuildingsqft26    
names(df)[names(df) == "yardbuildingsqft26"] <- "storageshed_yrd"
df[is.na(df$storageshed_yrd),]$storageshed_yrd <- 0

# garagecarcnt & garagetotalsqft
# some issue with garagetotatlsqft
# 0 foot has multiple garages
df[is.na(df$garagecarcnt),]$garagecarcnt <- 0

# basementsqft
df[is.na(df$basementsqft),]$basementsqft <- 0

# airconditioningtypeid
# from the data dictionary replace NA with 5
df[is.na(df$airconditioningtypeid),]$airconditioningtypeid <- 5

# heatingorsystemtypeid
df[is.na(df$heatingorsystemtypeid),]$heatingorsystemtypeid <- 13

# unitcnt
df[is.na(df$unitcnt),]$unitcnt <- 1

df$taxvaluedollarcnt <- df$structuretaxvaluedollarcnt + df$landtaxvaluedollarcnt

df$buildingqualityFlag <- ifelse(is.na(df$buildingqualitytypeid), FALSE, TRUE)

# only 60 rows with no empty columns
# so have to carefully remove data
nrow(df[complete.cases(df),])

# PHASE 2 CLEANING


# numberofstories > buildingqualitytypeid > lotsizesquarefeet > yearbuilt > calculatedfinishedsquarefeet 
# tax_perc > regionidzip > bathroomcnt > bedroomcnt

# roomcnt showing 0 for majority of houses eventhought bedroom cnt conflict with it
# bathroomcnt is used instead of calculatedbathnbr

shortdf <- df %>% select(-c("regionidneighborhood","calculatedbathnbr", "roomcnt", "buildingqualitytypeid", 
                            "regionidcounty", "numberofstories"))

# buildingqualitytypeid removing NAs makes us lose a lot of information in other columns
# 
#shortdf <- shortdf[!is.na(df$buildingqualitytypeid),]
shortdf <- shortdf[!is.na(shortdf$lotsizesquarefeet),]
shortdf <- shortdf[!is.na(shortdf$yearbuilt),]
shortdf <- shortdf[!is.na(shortdf$calculatedfinishedsquarefeet),]

shortdf <- shortdf[!is.na(shortdf$taxamount),]
shortdf <- shortdf[!is.na(shortdf$regionidzip),]
shortdf <- shortdf[!is.na(shortdf$bathroomcnt),]
shortdf <- shortdf[!is.na(shortdf$bedroomcnt),]

#nrow reduced df
nrow(shortdf)

# columns that lost information after removing NAs in buildingqualitytypeid
# basementsqft, decktypeid, fireplacecnt, garagecarcnt, patio_yrd, storageshed_yrd
# fireplaceflag

#reduce number of categories
shortdf$airconditioningtypeid <- ifelse(shortdf$airconditioningtypeid == 5, "false", "true")
property_type <- c(31, 246, 247, 248, 261, 263, 264, 265, 266, 269)
shortdf <- shortdf[shortdf$propertylandusetypeid %in% property_type,]

`%ni%` <- Negate(`%in%`)
shortdf[shortdf$heatingorsystemtypeid %ni% c(2,7,13),]$heatingorsystemtypeid <- 14

# fireplacecnt
# 0,1,2+
shortdf[shortdf$fireplacecnt >= 2,]$fireplacecnt <- "2+"

# garagecarcnt
# 0,1,2,3,4+
shortdf[shortdf$garagecarcnt >= 4,]$garagecarcnt <- "4+"

# unitcnt
shortdf[shortdf$unitcnt >= 5,]$unitcnt <- "5+"


# yearbuilt
bins <- cut(shortdf$yearbuilt, 
            breaks = c(1800, 1850, 1900, 1945, 1990, 2015), 
            labels = c("Early 19th century", "Industrial revolution", 
                       "Pre-modern era", "Post-war modern", "Contemporary"))
shortdf$yearbuilt_bin <- bins

shortdf$latitude <- shortdf$latitude/1000000
shortdf$longitude <- shortdf$longitude/1000000

shortdf$airconditioningtypeid <- ifelse(shortdf$airconditioningtypeid == "true", 1, 0)
shortdf$decktypeid <- ifelse(shortdf$decktypeid == "true", 1, 0)
shortdf$hashottuborspa <- ifelse(shortdf$hashottuborspa == "true", 1, 0)
shortdf$fireplaceflag <- ifelse(shortdf$fireplaceflag == "true", 1, 0)
shortdf$taxdelinquencyflag <- ifelse(shortdf$taxdelinquencyflag == "true", 1, 0)

columns_to_factor <- c("airconditioningtypeid", "decktypeid", "fips", "hashottuborspa",
                       "heatingorsystemtypeid","poolcnt", "pooltypeid10", "pooltypeid2",
                       "propertylandusetypeid", "regionidzip", "fireplaceflag", "taxdelinquencyflag",
                       "fireplacecnt", "garagecarcnt", "unitcnt", "yearbuilt_bin")

shortdf[columns_to_factor] <- lapply(shortdf[columns_to_factor], as.factor)

shortdf$tax_perc <- round((shortdf$taxamount/shortdf$taxvaluedollarcnt)*100,2)

shortdf <- shortdf %>% select(-c("yearbuilt", "buildingqualityFlag", "parcelid", "taxamount"))

str(shortdf)

summarytools::stview(summarytools::dfSummary(shortdf))

data.table::fwrite(shortdf, "../zillow_data/reduced_properties_2016.csv", row.names = FALSE)

