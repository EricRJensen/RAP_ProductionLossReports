library(tidyverse)
library(sf)
library(rmarkdown)
library(here)
library(stringr)
library(raster)
library(dt)
library(tableHTML)

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

wgs_wkt <- 'GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4326"]]'

# Read in SF objects for states and counties
ctys_sf <- st_read('data/shp/tl_2020_us_county/tl_2020_us_county.shp') %>%
  mutate(COUNTYID = paste(STATEFP,COUNTYFP, sep = '')) %>% #FIPS code
  st_transform(wgs_wkt)
stas_sf <- st_read('data/shp/tl_2020_us_state/tl_2020_us_state.shp') %>%
  st_transform(wgs_wkt)


# ----------------- Generate vector of FIPS codes for RAP counties -----------------------------

# Loop over countys dataframe to produce vector of FIPS codes strings
stas_fp_v <- c()
ctys_fp_v <- c()
ctys_fp_df <- ctys_df %>%
  dplyr::select(c('STATEFP', 'COUNTYFP')) %>%
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
                      zoom = c(6.6, 5.8, 7, 6.05, 7, 6.5, 7, 6.22, 6.6, 7, 7, 6.8, 7, 5.75, 6.65, 7, 6.8))

stas_sf <- stas_sf %>%
  filter(STATEFP %in% stas_fp_v) %>%
  left_join(stas_zoom_df, by = 'STUSPS') 

remove(sta_fp, cty_fp, cty_id, i, ctys_fp_df, stas_zoom_df)

# Get current year to pass to the rmarkdown script as a parameter 
current_year = max(ctys_df$year)

ctys_yieldgap_df <- ctys_df %>%
  dplyr::select(c(COUNTYID, year, yieldgap, analysisArea)) %>%
  group_by(COUNTYID) %>%
  mutate(yieldgap_cumulative = sum(yieldgap)) %>%
  filter(year == current_year) %>%
  mutate(yieldgap_2019_norm = yieldgap / analysisArea) %>%
  ungroup() %>%
  dplyr::select(c(FIPS = COUNTYID, yieldgap_2019 = yieldgap, yieldgap_cumulative, yieldgap_2019_norm))


# ---------------------- Nested loop to generate state and county reports -----------------------------
for(i in stas_fp_v){ #stas_fp_v
  
  # Subset state sf object for current state
  sta_sf <- filter(stas_sf, STATEFP == i) %>%
    st_simplify(preserveTopology = FALSE, dTolerance = .02)
  
  # Get list of county ids for state
  sta_ctys_v <- grep(paste('^',i,sep = ''), ctys_fp_v,value=TRUE)
  
  # Create state directory
  sta_name <- sta_sf$NAME
  sta_dir <- paste('Outputs/', str_replace(sta_name, ' ', ''), '/', sep = '')
  dir.create(sta_dir)
  
  # Get state raster
  sta_rast <- raster(paste('data/tif/v5/960m/', str_replace_all(sta_name, ' ', '_'), '.tif', sep = ''))
  sta_rast[1,1] <- 6
  
  # Subset county sf object for current state's counties
  sta_ctys_sf <- ctys_sf %>%
    filter(STATEFP == i) %>%
    mutate(up_path = paste('../',str_replace_all(NAME, ' ', ''), '/index.html', sep = ''),
           dn_path = paste('./',str_replace_all(NAME, ' ', ''), '/index.html', sep = '')) %>%
    rename(FIPS = GEOID) %>%
    left_join(ctys_yieldgap_df, by = 'FIPS') %>%
    st_simplify(preserveTopology = FALSE, dTolerance = .02)
  
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
    dplyr::select(-c(COUNTYFP, NAME, treedArea, COUNTYID, countyArea)) %>%
    ungroup() %>%
    unique() %>%
    mutate(cumYieldGap = cumsum(yieldgap),
           treeArea = (treeArea))
  
  sta_df_csv <- sta_df %>%
    mutate(staName = sta_name) %>%
    dplyr::select(c(staName, year, totalArea, analysisArea, biomass, yieldGap = yieldgap, cumYieldGap, treeCover, treeArea, classWoodlands, classModCover, classLowCover, classAtRisk, classIntact))
  
  # Write out html table to add to report
  data_html <- sta_df %>%
    mutate(Year = round(year, 0),
           `Production (tons)` = format(round(biomass, 0), big.mark=","),
           `Production gain/loss (tons)` = ifelse(yieldgap >= 0, paste(format(round(-yieldgap, 0), big.mark=",")), str_replace_all(paste('+', format(round(-yieldgap, 0), big.mark=","), sep = ''), ' ', '')),
           `Tree area (acres)` = format(round(treeArea, 0), big.mark=",")) %>%
    dplyr::select(c(Year, `Production (tons)`, `Production gain/loss (tons)`, `Tree area (acres)`)) %>%
    tableHTML(widths = c(100,150,150,150), rownames = FALSE) %>% 
    add_css_row(css = list(c('background-color', 'text-align', 'height', 'font-family'), c('#f2f2f2', 'center', '30px', 'Open Sans')),
                rows = odd(1:(nrow(sta_df)+1))) %>%
    add_css_row(css = list(c('background-color', 'text-align', 'height', 'font-family'), c('#FFFFFF', 'center', '30px', 'Open Sans')),
                rows = even(1:nrow(sta_df))) 
  
  write_tableHTML(data_html, paste(here(), '/', sta_dir, 'data.html', sep = ''))
  
  # Generate rmarkdown reports
  sta_csv_path = paste(here(), '/', sta_dir, 'data.csv', sep = '')
  write_csv(sta_df_csv, path = sta_csv_path)
  rmarkdown::render("Scripts/ForageReports_html.Rmd",
                    output_file = paste(here(), '/', sta_dir, 'index', sep = ''),
                    output_format = html_document(),
                    params = list(new_title = sta_name,
                                  main_df = sta_df,
                                  cty_sf = NULL,
                                  sta_sf = sta_sf,
                                  sta_rast = sta_rast,
                                  sta_ctys_sf = sta_ctys_sf,
                                  current_year = current_year,
                                  rap_url = rap_sta_url,
                                  main_dir = sta_dir,
                                  type = 'State'))
  
  
  # Export supplementary maps for download
  # Annual production plot
  png(file = paste(here(), '/', sta_dir, 'production.png', sep = ''), width=6, height=4, units="in", res=300)
  prod_map <- ggplot()+
    geom_line(sta_df, mapping = aes(x = year, y = biomass), color = 'grey20', size = 1)+
    geom_point(sta_df, mapping = aes(x = year, y = biomass), color = 'grey20', size = 2)+
    labs(title = paste('Herbaceous production for', sta_name), y = 'Annual production (tons)')+
    scale_y_continuous(labels = comma, limits = c(0, (max(sta_df$biomass))*1.1))+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    theme_minimal()+
    theme(axis.title.x=element_blank(),
          plot.title.position = "plot")
  print(prod_map)
  dev.off()
  
  # Tree cover area
  png(file = paste(here(), '/', sta_dir, 'tree.png', sep = ''), width=6, height=4, units="in", res=300)
  tree_map <- ggplot()+
    geom_line(sta_df, mapping = aes(x = year, y = treeArea), color = '#0E8E00', size = 1)+
    geom_point(sta_df, mapping = aes(x = year, y = treeArea), color = '#0E8E00', size = 2)+
    geom_hline(yintercept = head(sta_df$treeArea, n=1), linetype = 'dashed', color = 'grey30', size = 1)+
    labs(title = paste('Tree cover for', sta_name), y = 'Tree cover (acres)')+
    scale_y_continuous(labels = comma, limits = c(0, max(sta_df$treeArea)*1.1))+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    theme_minimal()+
    theme(axis.title.x=element_blank(),
          plot.title.position = "plot")
  print(tree_map)
  dev.off()
  
  # rmarkdown::render("Scripts/ForageReports_pdf.Rmd",
  #                   output_file = paste(here(), '/', sta_dir, 'index.pdf', sep = ''),
  #                   output_format = pdf_document(),
  #                   params = list(new_title = sta_name,
  #                                 main_df = sta_df,
  #                                 cty_sf = NULL,
  #                                 sta_sf = sta_sf,
  #                                 sta_ctys_sf = sta_ctys_sf,
  #                                 current_year = current_year,
  #                                 rap_url = rap_sta_url,
  #                                 type = 'State'))

      # for(j in sta_ctys_v){
      # 
      #   # Subset to county sf object and name
      #   cty_sf <- filter(ctys_sf, COUNTYID == j) #dataframe with county names
      # 
      #   #Create county directory
      #   cty_name_s <- str_replace_all(cty_sf$NAME, ' ', '')
      #   cty_name_l <- cty_sf$NAMELSAD #county name
      #   cty_dir <- paste(sta_dir, cty_name_s, '/', sep = '')
      #   dir.create(cty_dir)
      # 
      #   # Location name as title parameter for RMD
      #   loc_name <- paste(cty_name_l, ', ', sta_name, sep = '') #concatenated 'County, State' name
      # 
      #   # Filter SF objects for county of interest
      #   cty_df <- ctys_df %>%
      #     filter(COUNTYID == j) %>%
      #     mutate(cumYieldGap = cumsum(yieldgap),
      #            totalArea = countyArea) %>%
      #     dplyr::select(-countyArea)
      # 
      #   cty_df_csv <- cty_df %>%
      #     mutate(staName = sta_name) %>%
      #     dplyr::select(c(ctyName = NAME, staName, totalArea, analysisArea, biomass, yieldGap = yieldgap, cumYieldGap, treeCover, treeArea, classWoodlands, classModCover, classLowCover, classAtRisk, classIntact))
      # 
      #   # Generate RAP url for county
      #   coords <- st_coordinates(st_centroid(cty_sf))
      #   rap_cty_url = paste('https://rangelands.app/rap/?biomass_t=herbaceous&ll=', coords[2], ',', coords[1], '&z=9&landcover_t=tree&landcover_v=true', sep = '')
      # 
      #   # Write out html table to add to report
      #   data_html <- cty_df %>%
      #     mutate(Year = round(year, 0),
      #            `Production (tons)` = format(round(biomass, 0), big.mark=","),
      #            `Production gain/loss (tons)` = ifelse(yieldgap >= 0, paste(format(round(-yieldgap, 0), big.mark=",")), str_replace_all(paste('+', format(round(-yieldgap, 0), big.mark=","), sep = ''), ' ', '')),
      #            `Tree area (acres)` = format(round(treeArea, 0), big.mark=",")) %>%
      #     dplyr::select(c(Year, `Production (tons)`, `Production gain/loss (tons)`, `Tree area (acres)`)) %>%
      #     tableHTML(widths = c(100,150,150,150), rownames = FALSE) %>% 
      #     add_css_row(css = list(c('background-color', 'text-align', 'height', 'font-family'), c('#f2f2f2', 'center', '30px', 'Open Sans')),
      #                 rows = odd(1:(nrow(sta_df)+1))) %>%
      #     add_css_row(css = list(c('background-color', 'text-align', 'height', 'font-family'), c('#FFFFFF', 'center', '30px', 'Open Sans')),
      #                 rows = even(1:nrow(sta_df))) 
      #   
      #   write_tableHTML(data_html, paste(here(), '/', cty_dir, 'data.html', sep = ''))
      #   
      #   # Generate rmarkdown reports
      #   cty_csv_path <- paste(here(), '/', sta_dir, cty_name_s, '/data.csv', sep = '')
      #   write_csv(cty_df_csv, path = cty_csv_path)
      #   rmarkdown::render("Scripts/ForageReports_html.Rmd",
      #                     output_file = paste(here(), '/', sta_dir, cty_name_s, '/index', sep = ''),
      #                     output_format = html_document(),
      #                     params = list(new_title = loc_name,
      #                                   main_df = cty_df,
      #                                   cty_sf = cty_sf,
      #                                   sta_rast = sta_rast,
      #                                   sta_sf = sta_sf,
      #                                   sta_ctys_sf = sta_ctys_sf,
      #                                   current_year = current_year,
      #                                   rap_url = rap_cty_url,
      #                                   main_dir = cty_dir,
      #                                   type = 'County'))
      # 
      # # rmarkdown::render("Scripts/ForageReports_pdf.Rmd",
      # #                   output_file = paste(here(), '/', sta_dir, cty_name_s, '/index.pdf', sep = ''),
      # #                   output_format = pdf_document(),
      # #                   params = list(new_title = loc_name,
      # #                                 main_df = cty_df,
      # #                                 cty_sf = cty_sf,
      # #                                 sta_sf = sta_sf,
      # #                                 sta_ctys_sf = sta_ctys_sf,
      # #                                 current_year = current_year,
      # #                                 rap_url = rap_cty_url,
      # #                                 type = 'County'))
      # 
      #   # Export supplementary maps for download
      #   # Annual production plot
      #   png(file = paste(here(), '/', sta_dir, cty_name_s, '/production.png', sep = ''), width=6, height=4, units="in", res=300)
      #   prod_map <- ggplot()+
      #     geom_line(cty_df, mapping = aes(x = year, y = biomass), color = 'grey20', size = 1)+
      #     geom_point(cty_df, mapping = aes(x = year, y = biomass), color = 'grey20', size = 2)+
      #     labs(title = paste('Herbaceous production for ', cty_name, ', ', sta_name, sep = ''), y = 'Annual production (tons)')+
      #     scale_y_continuous(labels = comma, limits = c(0, (max(cty_df$biomass))*1.1))+
      #     scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      #     theme_minimal()+
      #     theme(axis.title.x=element_blank(),
      #           plot.title.position = "plot")
      #   print(prod_map)
      #   dev.off()
      # 
      #   # Tree cover area
      #   png(file = paste(here(), '/', sta_dir, cty_name_s, '/tree.png', sep = ''), width=6, height=4, units="in", res=300)
      #   tree_map <- ggplot()+
      #     geom_line(cty_df, mapping = aes(x = year, y = treeArea), color = '#0E8E00', size = 1)+
      #     geom_point(cty_df, mapping = aes(x = year, y = treeArea), color = '#0E8E00', size = 2)+
      #     geom_hline(yintercept = head(cty_df$treeArea, n=1), linetype = 'dashed', color = 'grey30', size = 1)+
      #     labs(title = paste('Tree cover for ', cty_name, ', ', sta_name, sep = ''), y = 'Tree cover (acres)')+
      #     scale_y_continuous(labels = comma, limits = c(0, max(cty_df$treeArea)*1.1))+
      #     scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      #     theme_minimal()+
      #     theme(axis.title.x=element_blank(),
      #           plot.title.position = "plot")
      #   print(tree_map)
      #   dev.off()
      #  }
 }

