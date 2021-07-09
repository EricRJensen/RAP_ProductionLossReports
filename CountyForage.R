library(tidyverse)
library(sf)
library(mapview)
library(knitr)
require(scales)
library(reshape2)
library(rmarkdown)
library(plotly)


# ------------------------------- Read in CSV and shapefiles -------------------------------------

#Read-in and filter data
# Read in production csv and filter for county of interest
ctys_df <- read_csv('C:/Users/eric/Desktop/CountyForage/data/csv/Agricultural_Statistics_County_Exported_20210625.csv') %>%
  mutate(COUNTYID = paste(STATEFP,COUNTYFP, sep = '')) #FIPS code

# Read in SF objects for states and counties
ctys_sf <- st_read('C:/Users/eric/Desktop/CountyForage/data/shp/tl_2020_us_county/tl_2020_us_county.shp') %>%
  mutate(COUNTYID = paste(STATEFP,COUNTYFP, sep = '')) #FIPS code
stas_sf <- st_read('C:/Users/eric/Desktop/CountyForage/data/shp/tl_2020_us_state/tl_2020_us_state.shp')


# ----------------- Generate vector of  FIPS codes for RAP counties -----------------------------

# Loop over countys dataframe to produce vector of FIPS codes strings
ctys_id_v <- c()
ctys_id_df <- ctys_df %>%
  select(c('STATEFP', 'COUNTYFP')) %>%
  unique()
for(i in 1:10){
  sta_fp <- ctys_id_df[i,1] %>% unlist()
  cty_fp <- ctys_id_df[i,2] %>% unlist()
  cty_id <- paste(sta_fp, cty_fp, sep = '')
  ctys_id_v <- c(ctys_id_v, cty_id)
}
remove(sta_fp, cty_fp, cty_id, i, ctys_id_df)

# Get current year to pass to the rmarkdown script as a parameter 
current_year = max(ctys_df$year)


# ---------------------- Loop to produce reports in sub directories -----------------------------

# Loop over unique county IDs to produce RMarkdown HTML reports
for(i in ctys_id_v){
  # Generate 'County, State' title to pass to the rmarkdown script as a parameter
  sta_id = str_sub(i, 1, 2) #state id
  cty_sf <- filter(ctys_sf, COUNTYID == i) #dataframe with county names
  sta_sf <- filter(stas_sf, STATEFP == sta_id) #dataframe with state names
  cty_name <- cty_sf$NAMELSAD #county name
  sta_name <- sta_sf$NAME #state name
  loc_name <- paste(cty_name, ', ', sta_name, sep = '') #concatenated 'County, State' name
  
  # Filter SF objects for county of interest
  cty_df <- ctys_df %>%
    filter(COUNTYID == i) %>%
    mutate(cumYieldGap = cumsum(yieldgap),
           treeArea = (treeArea*0.000247105))
  
  sta_id = str_sub(i, 1, 2)
  sta_ctys_sf <- filter(ctys_sf, STATEFP == sta_id)
  
  #generate rmarkdown report
  rmarkdown::render("C:/Users/eric/Desktop/CountyForage/CountyForage_df.Rmd",
                    output_file = paste("C:/Users/eric/Desktop/CountyForage/Outputs/_test", str_replace_all(sta_name, fixed(" "), ""), '/', str_replace_all(cty_name, fixed(" "), ""), '.html', sep = ''),
                    params = list(new_title = loc_name,
                                  cty_df = cty_df,
                                  cty_sf = cty_sf,
                                  sta_sf = sta_sf,
                                  sta_ctys_sf = sta_ctys_sf,
                                  current_year = current_year))
}

# Note: Probably makes more sense to write this as a nested loop -- create directories if they don't exist

