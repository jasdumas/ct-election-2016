# Connecticut Presidential Election Results 2016
library(readr)
library(dplyr)
library(leaflet)
library(rvest)
library(dumas)
library(ggplot2)
library(plotly)
library(rgdal)
library(tidyr)

all_results <- read_csv("https://raw.githubusercontent.com/mkearney/presidential_election_county_results_2016/master/pres16results.csv")

# what is in here?
head(all_results)

# what are fips? (https://www.census.gov/2010census/partners/pdf/FIPS_StateCounty_Code.pdf)
# https://en.wikipedia.org/wiki/FIPS_140-2
levels(factor(all_results$fips))

# Connecticut FIPS start with 09 - so lets grab those out are explore
ct_results <- all_results[grep("^09", all_results$fips), ]

# add the county names (8 in CT)
ct_results$county <- NA
# these are the full fips numbers associated with each county
fairfield <- "09001" 
hartford <- "09003"
litchfield <- "09005"
middlesex <- "09007"
newhaven <- "09009"
newlondon <- "09011"
tolland <- "09013"
windham <- "09015"

ct_results$county[which(ct_results$fips %in% fairfield)] <- "Fairfield"
ct_results$county[which(ct_results$fips %in% hartford)] <- "Hartford"
ct_results$county[which(ct_results$fips %in% litchfield)] <- "Litchfield"
ct_results$county[which(ct_results$fips %in% middlesex)] <- "Middlesex"
ct_results$county[which(ct_results$fips %in% newhaven)] <- "New Haven"
ct_results$county[which(ct_results$fips %in% newlondon)] <- "New London"
ct_results$county[which(ct_results$fips %in% tolland)] <- "Tolland"
ct_results$county[which(ct_results$fips %in% windham)] <- "Windham"

View(ct_results)

# exploratory data analysis
ggplot(ct_results, aes(x = cand_name, y = votes, fill = county)) +
  geom_bar(stat = "identity")

## -------------------------------- ##
# CT level county shapefiles for 2015
tmp2 = tempdir()
#url2 = "http://magic.lib.uconn.edu/magic_2/vector/37800/countyct_37800_0000_2010_s100_census_1_shp.zip"
url2 = "http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_county_500k.zip"
file <- basename(url2)
download.file(url2, file)
unzip(file, exdir = tmp2)
ct_shp <- readOGR(dsn = tmp2,
                  layer = "cb_2015_us_county_500k", encoding = "UTF-8")
dim(ct_shp)

# fix the FIPS number in the shape file for merging
ct_shp@data$fips <- paste0(ct_shp@data$GEOID)

# create a empty df
ct_econ <- data.frame(matrix(ncol=10, nrow = 0))

# add some economic data from https://datausa.io/ for each county by web-scrapping
county_urls <- c("https://datausa.io/profile/geo/fairfield-county-ct/", 
                 "https://datausa.io/profile/geo/hartford-county-ct/", 
                 "https://datausa.io/profile/geo/litchfield-county-ct/", 
                 "https://datausa.io/profile/geo/middlesex-county-ct/", 
                 "https://datausa.io/profile/geo/new-haven-county-ct/", 
                 "https://datausa.io/profile/geo/new-london-county-ct/", 
                 "https://datausa.io/profile/geo/tolland-county-ct/", 
                 "https://datausa.io/profile/geo/windham-county-ct/")
#  for loop for each of the counties
for (i in county_urls){
  table <- read_html(i) %>% 
    html_nodes(".stat-text+ .stat-value .stat-span") %>% 
    html_text() %>% 
    data.frame()
  # transpose into rows!
  table_keep <- t(data.frame(table[c(1:9, 17), ]))
  # append to a master data frame
  ct_econ <- rbind(ct_econ, table_keep)
}

# add column names / mentally choose which values to keep after looking on the website
colnames(ct_econ) <- c("med_house_income14", 
                       "avg_male_income", 
                       "avg_female_income", 
                       "highest_income_race", 
                       "wage_gini", 
                       "largest_demo_poverty", 
                       "largest_race_poverty", 
                       "med_native_age", 
                       "med_foreign_age", 
                       "common_major") 
ct_econ$county <- c("Fairfield", "Hartford", "Litchfield", "Middlesex", 
                    "New Haven", "New London", "Tolland", "Windham")

# merge this with the ct_results with the economic data
ct_join <-  dplyr::full_join(ct_results, ct_econ)

# full join the data set 
ct_join <- dplyr::full_join(ct_shp@data, ct_join)

# remove rows with NA's - i.e. remove everything except the choosen state
ct_clean = na.omit(ct_join)

## let's try a specific candidate
#HRC <- ct_join[ct_join$cand_name == "Hillary Clinton", ]

# merge this with the entire shapefile object
ct_shp2 <- ct_shp
ct_shp2 <- sp::merge(x = ct_shp2, y = ct_clean, 
                     by = "fips", all.x = F, 
                     duplicateGeoms=TRUE)
plot(ct_shp2)
## ------------------------------ ##
## let's get mapping with leaflet ##

# create seperate color patterns for each candidate for layers
HRC <- ct_results[which(ct_results$cand_name == "Hillary Clinton"),] 
DT <- ct_results[which(ct_results$cand_name == "Donald Trump"),]
GJ <- ct_results[which(ct_results$cand_name == "Gary Johnson"),]
JT <- ct_results[which(ct_results$cand_name == "Jill Stein"),]

pal1 <- colorBin(palette = "Blues", domain = HRC$votes, bins = 8)
pal2 <- colorBin(palette = "Reds", domain = DT$votes, bins = 8)
pal3 <- colorBin(palette = "YlOrRd", domain = GJ$votes, bins = 8)
pal4 <- colorBin(palette = "Greens", domain = JT$votes, bins = 8)

# pop values statewide regardless of candidate
state_popup <- paste0("<strong>County: </strong>", 
                      ct_shp2$county, 
                      "<br><strong>Total Amount of 2016 Voters: </strong>", 
                      ct_shp2$total,
                      "<br><strong>Median Household Income: </strong>", 
                      ct_shp2$med_house_income14, 
                      "<br><strong>Average Female Income: </strong>",
                      ct_shp2$avg_female_income, 
                      "<br><strong>Wage Equality Index: </strong>", 
                      ct_shp2$wage_gini, 
                      "<br><strong>Largest Demographic in Poverty: </strong>", 
                      ct_shp2$largest_demo_poverty)

# plot the map(s)
hrc_map <- leaflet(data = ct_shp2) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal1(HRC$votes), 
              fillOpacity = 0.7, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = state_popup) %>%
  addLegend("bottomright", 
            pal = pal1, 
            values = ~HRC$votes,
            title = "Total Votes for Hillary Clinton: ",
            opacity = 1)
print(hrc_map)

dt_map <- leaflet(data = ct_shp2) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal2(DT$votes), 
              fillOpacity = 0.7, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = state_popup) %>%
  addLegend("bottomright", 
            pal = pal2, 
            values = ~DT$votes,
            title = "Total Votes for Donald Trump: ",
            opacity = 1) 
print(dt_map)


gj_map <- leaflet(data = ct_shp2) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal3(GJ$votes), 
              fillOpacity = 0.7, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = state_popup) %>%
  addLegend("bottomleft", 
            pal = pal3, 
            values = ~GJ$votes,
            title = "Total Votes for Gary Johnson: ",
            opacity = 1) 
print(gj_map)


jt_map <- leaflet(data = ct_shp2) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal4(JT$votes), 
              fillOpacity = 0.7, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = state_popup) %>%
  addLegend("bottomleft", 
            pal = pal4, 
            values = ~JT$votes,
            title = "Total Votes for Jill Stein: ",
            opacity = 1) 
print(jt_map)





