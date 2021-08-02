library(tidyverse)
library(sf)
library(rmarkdown)
library(here)
library(stringr)
library(data.table)

# ------------------------------- Read in CSV and shapefiles -------------------------------------

#Read-in and filter data
# Read in production csv and filter for county of interest
ctys_df <- read_csv('data/csv/Agricultural_Statistics_County_Exported_20210730.csv') %>%
  mutate(COUNTYID = paste(STATEFP,COUNTYFP, sep = ''),
         biomass = biomass / 2000,
         yieldgap = yieldgap / 2000,
         classIntact = (analysisArea - (classWoodlands + classModCover + classLowCover + classAtRisk)) * 0.000247105,
         classWoodlands = classWoodlands * 0.000247105,
         classModCover = classModCover * 0.000247105,
         classLowCover = classLowCover * 0.000247105,
         classAtRisk = classAtRisk * 0.000247105,
         countyArea = countyArea * 0.000247105,
         analysisArea = analysisArea * 0.000247105,
         treeArea = treeArea * 0.000247105,
         treedArea = treedArea * 0.000247105) #FIPS code

# Read in SF objects for states and counties
ctys_sf <- st_read('data/shp/tl_2020_us_county/tl_2020_us_county.shp') %>%
  mutate(COUNTYID = paste(STATEFP,COUNTYFP, sep = '')) %>% #FIPS code
  st_transform(4326)
stas_sf <- st_read('data/shp/tl_2020_us_state/tl_2020_us_state.shp') %>% 
  st_transform(4326)


# ------------------------------- Calculate slopes for viz -------------------------------------

# Load data
data <- fread("C:/users/eric/desktop/countyforage/data/csv/Agricultural_Statistics_County_Exported_20210730.csv")

# CLEAN THIS UP
data$classIntact <- data$analysisArea - (data$classAtRisk + data$classLowCover + data$classModCover + data$classWoodlands)

getVisData <- function(inputData = inputData) {
  
  ## outputs normalized county level data 
  # is30uw: intact area slope 30 years, unweighted. 
  # is30w: intact area slope 30 years, weighted. 
  # is10uw: intact area slope 10 years, unweighted. 
  # is10w: intact area slope 10 years, weighted. 
  # is05uw: intact area slope 05 years, unweighted. 
  # is05w: intact area slope 05 years, weighted. 
  # ygs10uw: yield-gap slope 10 years, unweighted.
  # ygs10w: yield-gap slope 10 years, weighted.
  # ygs05uw: yield-gap slope 05 years, unweighted.
  # ygs05w: yield-gap slope 05 years, weighted.
  
  # Weighting is based on (intact + at risk area) divided by county area. 
  # Approximates the amount of rangeland in the county. 
  
  
  
  calcSlopes <- function(inputData, fipscode) {
    
    x <- subset(inputData, cindex == fipscode)
    intactArea <- max(x$classIntact + x$classAtRisk + x$classLowCover) / x$countyArea[1] # Weight 
    
    intact.slope.30yr.unweighted <- as.vector(lm( classIntact ~ year, data = x)$coefficients[2])
    intact.slope.30yr.weighted <- intact.slope.30yr.unweighted * intactArea 
    
    x10 <- subset(inputData, cindex == fipscode  & year >= 2010)
    intact.slope.10yr.unweighted <- as.vector(lm( classIntact ~ year, data = x10)$coefficients[2])
    intact.slope.10yr.weighted <- intact.slope.10yr.unweighted * intactArea 
    yieldgap.slope.10yr.unweighted <- as.vector(lm( yieldgap ~ year, data = x10)$coefficients[2])
    yieldgap.slope.10yr.weighted <- yieldgap.slope.10yr.unweighted * intactArea
    
    x05 <- subset(inputData, cindex == fipscode  & year >= 2014)
    intact.slope.05yr.unweighted <- as.vector(lm( classIntact ~ year, data = x05)$coefficients[2])
    intact.slope.05yr.weighted <- intact.slope.05yr.unweighted * intactArea 
    yieldgap.slope.05yr.unweighted <- as.vector(lm( yieldgap ~ year, data = x05)$coefficients[2])
    yieldgap.slope.05yr.weighted <- yieldgap.slope.05yr.unweighted * intactArea
    
    return(
      data.table(
        STATEFP = x$STATEFP[1], 
        COUNTYFP = x$COUNTYFP[1],
        FIPS = x$cindex[1],
        is30uw = intact.slope.30yr.unweighted,
        is30w = intact.slope.30yr.weighted,
        is10uw = intact.slope.10yr.unweighted,
        is10w = intact.slope.10yr.weighted,
        is05uw = intact.slope.05yr.unweighted,
        is05w = intact.slope.05yr.weighted,
        ygs10uw = yieldgap.slope.10yr.unweighted,
        ygs10w = yieldgap.slope.10yr.weighted,
        ygs05uw = yieldgap.slope.05yr.unweighted,
        ygs05w = yieldgap.slope.05yr.weighted)
    )
  }
  
  normalize_state_data <- function(inputData, fipscode) { 
    # apply normalize() function to each of the slope columns
    x <- subset(inputData, STATEFP == fipscode) # only consider past 10 years for plotting
    x$is30uw <- normalizeV2(x$is30uw)
    x$is30w <- normalizeV2(x$is30w)
    x$is10uw <- normalizeV2(x$is10uw)
    x$is10w <- normalizeV2(x$is10w)
    x$is05uw <- normalizeV2(x$is05uw)
    x$is05w <- normalizeV2(x$is05w)
    x$ygs10uw <- normalizeV3(x$ygs10uw)
    x$ygs10w <- normalizeV3(x$ygs10w)
    x$ygs05uw <- normalizeV3(x$ygs05uw)
    x$ygs05w <- normalizeV3(x$ygs05w)
    
    return(x)
  }
  
  # normalize <- function(x) { 
  #   # rescale data from 0 to 1 base on the 5th to 95th perccentile.
  #   x.min <- stats::quantile(x, probs = c(0.05), na.rm  = TRUE, names = FALSE)
  #   x.max <- stats::quantile(x, probs = c(0.95), na.rm  = TRUE, names = FALSE)
  #   norm.x <- (x - x.min) / (x.max - x.min)
  #   
  #   norm.x[which(norm.x < 0)] <- 0
  #   norm.x[which(norm.x > 1)] <- 1
  #     
  #   return(norm.x)
  # }
  
  normalizeV2 <- function(x) { 
    # rescale data from 0 to 1 base on the 5th to 95th perccentile.
    
    # Negative Values
    x.neg.idx <- which(x < 0)
    x.neg <- x[x.neg.idx]
    x.neg.min <- stats::quantile(x.neg, probs = c(0.02), na.rm  = TRUE, names = FALSE)
    norm.x.neg <- ((x.neg - x.neg.min) / (0 - x.neg.min)) -1
    norm.x.neg[norm.x.neg < -1] <- -1
    
    # Positive Values
    x.pos.idx <- which(x > 0)
    x.pos <- x[x.pos.idx]
    norm.x.pos <- (x.pos) / (abs(x.neg.min))
    norm.x.pos[norm.x.pos > 1] <- 1
    
    norm.x <- x
    norm.x[x.neg.idx] <- norm.x.neg
    norm.x[x.pos.idx] <- norm.x.pos
    
    return(norm.x)
  }
  
  normalizeV3 <- function(x) { 
    
    # Pos Values
    x.pos.idx <- which(x > 0)
    x.pos <- x[x.pos.idx]
    x.pos.max <- stats::quantile(x.pos, probs = c(0.98), na.rm  = TRUE, names = FALSE)
    norm.x.pos <- (x.pos ) / (x.pos.max)
    norm.x.pos[norm.x.pos > 1] <- 1
    
    
    # Negative Values
    x.neg.idx <- which(x < 0)
    x.neg <- x[x.neg.idx]
    norm.x.neg <- ((x.neg + x.pos.max) / (0 + x.pos.max)) -1
    norm.x.neg[norm.x.neg < -1] <- -1
    
    norm.x <- x
    norm.x[x.neg.idx] <- norm.x.neg
    norm.x[x.pos.idx] <- norm.x.pos
    
    return(norm.x)
  }
  
  
  inputData$cindex <- paste(sprintf("%02d", inputData$STATEFP),sprintf("%03d", inputData$COUNTYFP), sep = "")
  fips <- unique(inputData$cindex)
  
  #Create list to hold intermediate datasl
  slopeData <- list()
  
  for(i in seq_along(fips)) { 
    #fipscode <- fips[i]  
    slopeData[[i]] <-calcSlopes(inputData, fips[i])
  } 
  
  # coerce list to data.table
  slopeData <- rbindlist(slopeData)
  
  # Get unique list of state fips
  state.index <- unique(slopeData$STATEFP)
  
  # Create list to hold final data
  slopeDataNormalized <- list()
  
  for(i in seq_along(state.index)) { 
    slopeDataNormalized[[i]] <- normalize_state_data(slopeData, state.index[i])  
  } 
  
  # coerce list to data.table and return
  slopeDataNormalized <- rbindlist(slopeDataNormalized)
  
  return(slopeDataNormalized)
}

data.out <- getVisData(inputData = data)


# ----------------- Generate vector of FIPS codes for RAP counties -----------------------------

# Loop over countys dataframe to produce vector of FIPS codes strings
stas_fp_v <- c()
ctys_fp_v <- c()
ctys_fp_df <- ctys_df %>%
  select(c('STATEFP', 'COUNTYFP')) %>%
  unique()
for(i in 1:nrow(ctys_fp_df)){ #1:nrow(ctys_fp_df)
  sta_fp <- ctys_fp_df[i,1] %>% unlist()
  cty_fp <- ctys_fp_df[i,2] %>% unlist()
  cty_id <- paste(sta_fp, cty_fp, sep = '')
  ctys_fp_v <- c(ctys_fp_v, cty_id)
  stas_fp_v <- c(stas_fp_v, sta_fp) %>%unique()
}

# Zoom level vector
stas_zoom_df <- tibble(STUSPS = c('AZ', 'CA', 'CO', 'ID', 'KS', 'MT', 'NE', 'NV', 'NM', 'ND', 'OK', 'OR', 'SD', 'TX', 'UT', 'WA', 'WY'),
                      zoom = c(6.5, 5.8, 7, 6, 7, 6.5, 7, 6, 6.5, 7, 7, 6.5, 7, 5.7, 6.5, 7, 6.8))

stas_sf <- stas_sf %>%
  filter(STATEFP %in% stas_fp_v) %>%
  left_join(stas_zoom_df, by = 'STUSPS') 

remove(sta_fp, cty_fp, cty_id, i, ctys_fp_df, stas_zoom_df, data, getVisData)

# Get current year to pass to the rmarkdown script as a parameter 
current_year = max(ctys_df$year)

ctys_yieldgap_df <- ctys_df %>%
  select(c(COUNTYID, year, yieldgap, analysisArea)) %>%
  group_by(COUNTYID) %>%
  mutate(yieldgap_cumulative = sum(yieldgap)) %>%
  filter(year == current_year) %>%
  mutate(yieldgap_2019_norm = yieldgap / analysisArea) %>%
  ungroup() %>%
  select(c(FIPS = COUNTYID, yieldgap_2019 = yieldgap, yieldgap_cumulative, yieldgap_2019_norm))

# ---------------------- Nested loop to generate state and county reports -----------------------------
for(i in stas_fp_v ){ #stas_fp_v 
  
  # Subset state sf object for current state
  sta_sf <- filter(stas_sf, STATEFP == i) 
  
  # Get list of county ids for state
  sta_ctys_v <- grep(paste('^',i,sep = ''), ctys_fp_v,value=TRUE)
  
  # Create state directory
  sta_name <- sta_sf$NAME
  sta_dir <- paste('Outputs/', str_replace(sta_name, ' ', ''), '/', sep = '')
  dir.create(sta_dir)
  
  # Subset county sf object for current state's counties
  sta_ctys_sf <- ctys_sf %>%
    filter(STATEFP == i) %>%
    mutate(up_path = paste('../',str_replace_all(NAME, ' ', ''), '/index.html', sep = ''),
           dn_path = paste('./',str_replace_all(NAME, ' ', ''), '/index.html', sep = '')) %>%
    rename(FIPS = GEOID) %>%
    left_join(ctys_yieldgap_df, by = 'FIPS') %>%
    left_join(data.out, by = 'FIPS')
  
  # Generate RAP url for county
  coords <- st_coordinates(st_centroid(sta_sf))
  rap_sta_url = paste('https://rangelands.app/rap/?biomass_t=herbaceous&ll=', coords[2], ',', coords[1], '&z=8&landcover_t=tree&landcover_v=true', sep = '')
  
  # Sum counties to generate 
  sta_df <- ctys_df %>%
    filter(STATEFP == i) %>%
    group_by(year) %>%
    mutate(treeArea = sum(treeArea),
           treeCover = mean(treeCover),
           biomass = sum(biomass),
           yieldgap = sum(yieldgap),
           classWoodlands = sum(classWoodlands),
           classModCover = sum(classModCover),
           classLowCover = sum(classLowCover),
           classAtRisk = sum(classAtRisk),
           classIntact = sum(classIntact),
           analysisArea = sum(analysisArea),
           totalArea = sum(countyArea)) %>%
    select(-c(COUNTYFP, NAME, treedArea, COUNTYID, countyArea)) %>%
    ungroup() %>%
    unique() %>%
    mutate(cumYieldGap = cumsum(yieldgap),
           treeArea = (treeArea))
  
  # Generate rmarkdown reports
  sta_csv_path = paste(here(), '/', sta_dir, 'State.csv', sep = '')
  write_csv(sta_df, path = sta_csv_path)
  rmarkdown::render("Scripts/ForageReports_html.Rmd",
                    output_file = paste(here(), '/', sta_dir, 'index.html', sep = ''),
                    output_format = html_document(),
                    params = list(new_title = sta_name,
                                  main_df = sta_df,
                                  cty_sf = NULL,
                                  sta_sf = sta_sf,
                                  sta_ctys_sf = sta_ctys_sf,
                                  current_year = current_year,
                                  rap_url = rap_sta_url,
                                  type = 'State'))
  
  rmarkdown::render("Scripts/ForageReports_pdf.Rmd",
                    output_file = paste(here(), '/', sta_dir, 'index.pdf', sep = ''),
                    output_format = pdf_document(),
                    params = list(new_title = sta_name,
                                  main_df = sta_df,
                                  cty_sf = NULL,
                                  sta_sf = sta_sf,
                                  sta_ctys_sf = sta_ctys_sf,
                                  current_year = current_year,
                                  rap_url = rap_sta_url,
                                  type = 'State'))

      for(j in sta_ctys_v){

        # Subset to county sf object and name
        cty_sf <- filter(ctys_sf, COUNTYID == j) #dataframe with county names

        #Create county directory
        cty_name_s <- str_replace_all(cty_sf$NAME, ' ', '')
        cty_name_l <- cty_sf$NAMELSAD #county name
        cty_dir <- paste(sta_dir, cty_name_s, sep = '')
        dir.create(cty_dir)

        # Location name as title parameter for RMD
        loc_name <- paste(cty_name_l, ', ', sta_name, sep = '') #concatenated 'County, State' name

        # Filter SF objects for county of interest
        cty_df <- ctys_df %>%
          filter(COUNTYID == j) %>%
          mutate(cumYieldGap = cumsum(yieldgap),
                 totalArea = countyArea) %>%
          select(-countyArea)

        # Generate RAP url for county
        coords <- st_coordinates(st_centroid(cty_sf))
        rap_cty_url = paste('https://rangelands.app/rap/?biomass_t=herbaceous&ll=', coords[2], ',', coords[1], '&z=9&landcover_t=tree&landcover_v=true', sep = '')

        # Generate rmarkdown reports
        cty_csv_path <- paste(here(), '/', sta_dir, cty_name_s, '/County.csv', sep = '')
        write_csv(cty_df %>% select(-c(COUNTYID, cumYieldGap)), path = cty_csv_path) #FIX THIS
        rmarkdown::render("Scripts/ForageReports_html.Rmd",
                          output_file = paste(here(), '/', sta_dir, cty_name_s, '/index.html', sep = ''),
                          output_format = html_document(),
                          params = list(new_title = loc_name,
                                        main_df = cty_df,
                                        cty_sf = cty_sf,
                                        sta_sf = sta_sf,
                                        sta_ctys_sf = sta_ctys_sf,
                                        current_year = current_year,
                                        rap_url = rap_cty_url,
                                        type = 'County'))

      rmarkdown::render("Scripts/ForageReports_pdf.Rmd",
                        output_file = paste(here(), '/', sta_dir, cty_name_s, '/index.pdf', sep = ''),
                        output_format = pdf_document(),
                        params = list(new_title = loc_name,
                                      main_df = cty_df,
                                      cty_sf = cty_sf,
                                      sta_sf = sta_sf,
                                      sta_ctys_sf = sta_ctys_sf,
                                      current_year = current_year,
                                      rap_url = rap_cty_url,
                                      type = 'County'))
      }
 }
