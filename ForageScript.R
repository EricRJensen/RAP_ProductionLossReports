library(tidyverse)
library(sf)
library(rmarkdown)
library(here)
library(stringr)

# ------------------------------- Read in CSV and shapefiles -------------------------------------

#Read-in and filter data
# Read in production csv and filter for county of interest
ctys_df <- read_csv('data/csv/Agricultural_Statistics_County_Exported_20210625.csv') %>%
  mutate(COUNTYID = paste(STATEFP,COUNTYFP, sep = '')) #FIPS code

# Read in SF objects for states and counties
ctys_sf <- st_read('data/shp/tl_2020_us_county/tl_2020_us_county.shp') %>%
  mutate(COUNTYID = paste(STATEFP,COUNTYFP, sep = '')) %>% #FIPS code
  st_transform(4326)
stas_sf <- st_read('data/shp/tl_2020_us_state/tl_2020_us_state.shp') %>% 
  st_transform(4326)


# ----------------- Generate vector of  FIPS codes for RAP counties -----------------------------

# Loop over countys dataframe to produce vector of FIPS codes strings
stas_id_v <- c()
ctys_id_v <- c()
ctys_id_df <- ctys_df %>%
  select(c('STATEFP', 'COUNTYFP')) %>%
  unique()
for(i in 1:nrow(ctys_id_df)){ #1:nrow(ctys_id_df)
  sta_fp <- ctys_id_df[i,1] %>% unlist()
  cty_fp <- ctys_id_df[i,2] %>% unlist()
  cty_id <- paste(sta_fp, cty_fp, sep = '')
  ctys_id_v <- c(ctys_id_v, cty_id)
  stas_id_v <- c(stas_id_v, sta_fp) %>%unique()
}

remove(sta_fp, cty_fp, cty_id, i, ctys_id_df)

# Get current year to pass to the rmarkdown script as a parameter 
current_year = max(ctys_df$year)

# ---------------------- Loop to produce reports in sub directories -----------------------------

for(i in stas_id_v){
  
  # Subset state sf object for current state
  sta_sf <- filter(stas_sf, STATEFP == i) 
  
  # Get list of county ids for state
  sta_ctys_v <- grep(paste('^',i,sep = ''), ctys_id_v,value=TRUE)
  
  # Create state directory
  sta_name <- sta_sf$NAME
  sta_dir <- paste('Outputs/', str_replace(sta_name, ' ', ''), '/', sep = '')
  dir.create(sta_dir)
  
  # Subset county sf object for current state's counties
  sta_ctys_sf <- ctys_sf %>%
    filter(STATEFP == i) %>%
    mutate(path = paste(here(), '/', sta_dir, str_replace_all(NAME, ' ', ''), '/County.html', sep = ''))
  
  # Generate RAP url for county
  coords <- st_coordinates(st_centroid(sta_sf))
  rap_sta_url = paste('https://rangelands.app/rap/?biomass_t=herbaceous&ll=', coords[2], ',', coords[1], '&z=8&landcover_t=tree&landcover_v=true', sep = '')
  
  # Sum counties
  sta_df <- ctys_df %>%
    filter(STATEFP == i) %>%
    group_by(year) %>%
    mutate(treeArea = sum(treeArea),
           treeCover = mean(treeCover),
           biomass = sum(biomass),
           yieldgap = sum(yieldgap),
           classWoodlands = sum(classWoodlands),
           classEncroached = sum(classEncroached),
           classRecruitment = sum(classRecruitment),
           classDispersal = sum(classDispersal),
           analysisArea = sum(analysisArea)) %>%
    select(-c(COUNTYFP, NAME, treedArea, COUNTYID, countyArea)) %>%
    ungroup() %>%
    unique() %>%
    mutate(cumYieldGap = cumsum(yieldgap),
           treeArea = (treeArea*0.000247105))
  
  # State report url
  str <- paste(here(), '/', sta_dir, sep = '')
  sta_subs <- strsplit(str, "/") %>% unlist() %>% head(n = 7)
  sta_path <- paste(sta_subs[1], '/', sta_subs[2], '/', sta_subs[3], '/', sta_subs[4], '/', sta_subs[5], '/', sta_subs[6], '/', sta_subs[7], '/', 'State.html', sep ='')

  #generate rmarkdown report
  sta_csv_path = paste(here(), '/', sta_dir, 'State.csv', sep = '')
  write_csv(sta_df, path = sta_csv_path)
  rmarkdown::render("Scripts/StateCounty.Rmd",
                    output_file = paste(here(), '/', sta_dir, 'State.html', sep = ''),
                    params = list(new_title = sta_name,
                                  main_df = sta_df,
                                  cty_sf = NULL,
                                  sta_sf = sta_sf,
                                  sta_ctys_sf = sta_ctys_sf,
                                  current_year = current_year,
                                  rap_url = rap_sta_url,
                                  sta_path = NULL,
                                  csv_path = sta_csv_path,
                                  type = 'State'))
  

      for(j in sta_ctys_v){

        # Subset to county sf object and name
        cty_sf <- filter(ctys_sf, COUNTYID == j) #dataframe with county names

        #Create county directory
        cty_name_s <- str_replace_all(cty_sf$NAME, ' ', '')
        cty_name_l <- cty_sf$NAMELSAD #county name
        cty_dir <- paste(sta_dir, cty_name_s, sep = '')
        dir.create(cty_dir)

        # # Location name as title parameter for RMD
        loc_name <- paste(cty_name_l, ', ', sta_name, sep = '') #concatenated 'County, State' name

        # # Filter SF objects for county of interest
        cty_df <- ctys_df %>%
          filter(COUNTYID == j) %>%
          mutate(cumYieldGap = cumsum(yieldgap),
                 treeArea = (treeArea*0.000247105))

        # Generate RAP url for county
        coords <- st_coordinates(st_centroid(cty_sf))
        rap_cty_url = paste('https://rangelands.app/rap/?biomass_t=herbaceous&ll=', coords[2], ',', coords[1], '&z=9&landcover_t=tree&landcover_v=true', sep = '')

        #generate rmarkdown report
        cty_csv_path <- paste(here(), '/', sta_dir, cty_name_s, '/County.csv', sep = '')
        write_csv(cty_df %>% select(c(1:5,8:9,17)),path = cty_csv_path)
        rmarkdown::render("Scripts/StateCounty.Rmd",
                          output_file = paste(here(), '/', sta_dir, cty_name_s, '/County.html', sep = ''),
                          params = list(new_title = loc_name,
                                        main_df = cty_df,
                                        cty_sf = cty_sf,
                                        sta_sf = sta_sf,
                                        sta_ctys_sf = sta_ctys_sf,
                                        current_year = current_year,
                                        rap_url = rap_cty_url,
                                        sta_path = sta_path,
                                        csv_path = cty_csv_path,
                                        type = 'County'))
      }
}

