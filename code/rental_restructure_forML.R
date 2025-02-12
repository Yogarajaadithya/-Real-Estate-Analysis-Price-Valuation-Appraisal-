#library(pastecs)
library(tidyverse)
library(ggplot2)
library(leaflet)
library(leaflet.extras)

options(scipen = 99)

df <- read.csv('../zillow_data/reduced_properties_2016.csv')

columns_to_factor <- c("airconditioningtypeid", "decktypeid", "fips", "hashottuborspa",
                       "heatingorsystemtypeid","poolcnt", "pooltypeid10", "pooltypeid2",
                       "propertylandusetypeid", "regionidzip", "fireplaceflag", "taxdelinquencyflag",
                       "fireplacecnt", "garagecarcnt", "unitcnt", "yearbuilt_bin")

df[columns_to_factor] <- lapply(df[columns_to_factor], as.factor)

ggplot(df[df$taxvaluedollarcnt < 500000,], aes(x = taxvaluedollarcnt)) +
  geom_histogram(binwidth = 5000, fill = "blue", color = "black", alpha = 0.6) +
  scale_x_continuous(labels = scales::label_number(scale = 1e-3, suffix = "k")) +
  labs(title = "Histogram of House Prices",
       x = "House Price",
       y = "Count") +
  theme_minimal()

# summary for df
# summarytools::stview(summarytools::dfSummary(df[df$taxvaluedollarcnt <= 50000,]))

# take only houses above 50,000
df <- df[df$taxvaluedollarcnt >= 50000,]

# 2.68m rows and 30 columns
# take a stratified sample of this
stratified_sample_frac <- df %>%
  group_by(airconditioningtypeid, decktypeid, fips, fireplacecnt, garagecarcnt, hashottuborspa, 
           heatingorsystemtypeid, poolcnt, pooltypeid10, pooltypeid2, propertylandusetypeid, 
           unitcnt, fireplaceflag, yearbuilt_bin) %>%
  sample_frac(0.1)

duplicates <- stratified_sample_frac %>%
  group_by(across(everything())) %>%
  filter(n() > 1)

stratified_sample_frac <- stratified_sample_frac[!duplicated(stratified_sample_frac), ]

#summarytools::stview(summarytools::dfSummary(stratified_sample_frac))

# tax perc
data.table::fwrite(stratified_sample_frac, "../zillow_data/stratified_properties_2016.csv", row.names = FALSE)


# Plotting

# Define the coordinates for each location along with the garage status
plotdf <- df %>%
  select(latitude, longitude, yearbuilt_bin)

plotdf <- plotdf %>%
  group_by(yearbuilt_bin) %>%
  sample_frac(0.005) %>%  # Adjust 0.5 to the desired fraction (e.g., 0.5 for 50% sample)
  ungroup()

pal <- colorFactor(palette = c('red', 'orange', 'green', 'blue', 'violet'), domain = plotdf$yearbuilt_bin)

# Create the leaflet map with both terrain and normal map layers, and add points with conditional color
leaflet(data = plotdf) %>%
  addTiles() %>%  # Default OpenStreetMap (normal map)
  addProviderTiles("Esri.WorldImagery", options = providerTileOptions(opacity = 0.7)) %>%  # Terrain theme with some transparency
  addCircleMarkers(
    ~longitude, ~latitude,  
    radius = 0.5,  
    #color = ifelse(plotdf$airconditioningtypeid == 1, "cyan", "red"),
    color = ~pal(yearbuilt_bin),  
    fill = TRUE,  # Fill the circle
    fillOpacity = 0.1  
    #clusterOptions = markerClusterOptions()
  ) %>%
  addLegend(
    position = "topright", 
    pal = pal, 
    values = ~yearbuilt_bin,      
    title = "yearbuilt_bin",        
    #colors = c("cyan", "red"),  
    #labels = c("Yes", "No"), 
    opacity = 1             
  )



# library(ggplot2)
# library(tigris)
# library(sf)
# 
# # Fetch California counties shapefile
# counties <- counties(state = "CA", year = 2020, cb = TRUE) # Get simplified boundaries
# 
# # Filter for Los Angeles, Ventura, and Orange County
# target_counties <- counties %>%
#   filter(NAME %in% c("Los Angeles", "Ventura", "Orange"))
# 
# # Plot using ggplot2 and geom_sf
# ggplot() +
#   geom_sf(data = target_counties, fill = "lightblue", color = "black") +
#   theme_minimal() +
#   ggtitle("Map of Los Angeles, Ventura, and Orange County") +
#   theme(plot.title = element_text(hjust = 0.5))