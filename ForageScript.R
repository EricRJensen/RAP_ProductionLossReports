# ----------------------------------- Load in packages -------------------------------------------
library(tidyverse)
library(sf)
library(rmarkdown)
library(here)
library(stringr)
library(raster)
library(DT)
library(reshape2)
library(tableHTML)
library(extrafont)
font_import('C:/Users/eric/Documents/NTSG/Projects/RAP/Scripts/CountyForage/Font', prompt = FALSE)
loadfonts(device = "win")

# ------------------------------- Read in CSV and shapefiles -------------------------------------

options(scipen = 999)

# Read-in and filter data
# Read in production csv and filter for county of interest
ctys_df <-
  read_csv(
    'C:/Users/eric/Documents/NTSG/Projects/RAP/Scripts/CountyForage/data/csv/Agricultural_Statistics_County_Exported_20211218.csv'
  ) %>%
  mutate(
    COUNTYID = paste(STATEFP, COUNTYFP, sep = ''),
    #FIPS code
    biomass = biomass / 2000,
    yieldgap = yieldgap / 2000,
    classIntact = (
      analysisArea - (classWoodlands + classModCover + classLowCover + classAtRisk)
    ) * 0.000247105,
    classWoodlands = classWoodlands * 0.000247105,
    classModCover = classModCover * 0.000247105,
    classLowCover = classLowCover * 0.000247105,
    classAtRisk = classAtRisk * 0.000247105,
    countyArea = countyArea * 0.000247105,
    analysisArea = analysisArea * 0.000247105,
    treeArea = treeArea * 0.000247105,
    treedArea = treedArea * 0.000247105
  ) %>%
  left_join(
    read_csv(
      'C:/Users/eric/Documents/NTSG/Projects/RAP/Scripts/CountyForage/data/csv/BPS_ForestedArea_Exported_20211029.csv'
    ) %>%
      mutate(
        COUNTYID = paste(STATEFP, COUNTYFP, sep = ''),
        BPS_Forest = BPS_Forest * 0.000247105
      ) %>%
      dplyr::select(-c(STATEFP, COUNTYFP, NAME)),
    by = "COUNTYID"
  )

# Projection string
wkt <-
  'GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4326"]]'

# Read in SF objects for states and counties
ctys_sf <-
  st_read(
    'C:/Users/eric/Documents/NTSG/Projects/RAP/Scripts/CountyForage/data/shp/tl_2020_us_county_westwide/tl_2020_us_county.shp',
    quiet = TRUE
  ) %>%
  mutate(COUNTYID = paste(STATEFP, COUNTYFP, sep = '')) %>% #FIPS code
  st_transform(wkt)

ctys_sf_hiRes <-
  st_read(
    'C:/Users/eric/Documents/NTSG/Projects/RAP/Scripts/CountyForage/data/shp/tl_2020_us_county/tl_2020_us_county.shp',
    quiet = TRUE
  ) %>%
  mutate(COUNTYID = paste(STATEFP, COUNTYFP, sep = ''))  #FIPS code

# Generate state boundaries from the union of coutnies in the state to ensure topological consistency
dissolve_sta <- function(FIPS) {
  ctys_sf %>%
    dplyr::select(STATEFP) %>%
    filter(STATEFP == FIPS) %>%
    st_union() %>%
    st_as_sf(crs = wkt) %>%
    mutate(STATEFP = FIPS)
}
state_fp_v <- ctys_sf$STATEFP %>%
  unique() %>%
  unlist()
stas_sf <- map(state_fp_v, dissolve_sta) %>%
  bind_rows() %>%
  left_join(
    st_read(
      'C:/Users/eric/Documents/NTSG/Projects/RAP/Scripts/CountyForage/data/shp/tl_2020_us_state/tl_2020_us_state.shp',
      quiet = TRUE
    ) %>% st_drop_geometry(),
    by = 'STATEFP'
  ) %>%
  mutate(path = paste('./', str_replace_all(NAME, ' ', ''), '/', sep = ''))


rm(dissolve_sta, state_fp_v, wkt)

# ----------------- Generate vector of FIPS codes for RAP counties -----------------------------

# Loop over countys dataframe to produce vector of FIPS codes strings
stas_fp_v <- c()
ctys_fp_v <- c()
ctys_fp_df <- ctys_df %>%
  dplyr::select(c('STATEFP', 'COUNTYFP')) %>%
  unique()
for (i in 1:nrow(ctys_fp_df)) {
  #1:nrow(ctys_fp_df)
  sta_fp <- ctys_fp_df[i, 1] %>% unlist()
  cty_fp <- ctys_fp_df[i, 2] %>% unlist()
  cty_id <- paste(sta_fp, cty_fp, sep = '')
  ctys_fp_v <- c(ctys_fp_v, cty_id)
  stas_fp_v <- c(stas_fp_v, sta_fp) %>% unique()
}

# Zoom level vector
stas_zoom_df <-
  tibble(
    STUSPS = c(
      'AZ',
      'CA',
      'CO',
      'ID',
      'KS',
      'MT',
      'NE',
      'NV',
      'NM',
      'ND',
      'OK',
      'OR',
      'SD',
      'TX',
      'UT',
      'WA',
      'WY'
    ),
    zoom = c(6.6, 5.8, 7, 6.05, 7, 6.5, 7, 6.22, 6.6, 7, 7, 6.8, 7, 5.75, 6.65, 7, 6.8)
  )

# Filter states by RAP vector and join to zoom level dataframe
stas_sf <- stas_sf %>%
  filter(STATEFP %in% stas_fp_v) %>%
  left_join(stas_zoom_df, by = 'STUSPS')

remove(sta_fp, cty_fp, cty_id, i, ctys_fp_df, stas_zoom_df)

# Get current year to pass to the rmarkdown script as a parameter
current_year = max(ctys_df$year)

# Sum up counties to the state level and calculate yield gap values cumulatively through current year and for current year itself
ctys_yieldgap_df <- ctys_df %>%
  dplyr::select(c(COUNTYID, year, yieldgap, analysisArea)) %>%
  group_by(COUNTYID) %>%
  mutate(yieldgap_cumulative = sum(yieldgap)) %>%
  filter(year == current_year) %>%
  mutate(yieldgap_curYear_norm_area = yieldgap / analysisArea) %>%
  ungroup() %>%
  dplyr::select(c(
    FIPS = COUNTYID,
    analysisArea,
    yieldgap_curYear = yieldgap,
    yieldgap_cumulative
  ))

# Join sf to summed dataframe
ctys_sf <- ctys_sf %>%
  rename(FIPS = GEOID) %>%
  right_join(ctys_yieldgap_df, by = 'FIPS')

# Generate new columns and translate data for plotting
ctys_df_yg_curYear <- ctys_df %>%
  filter(year == current_year) %>%
  mutate(
    FIPS = paste(STATEFP, COUNTYFP, sep = ''),
    yieldgap_curYear_norm = yieldgap / (yieldgap + biomass) * 100,
    pct_analysis = analysisArea / countyArea
  ) %>%
  dplyr::select(c(FIPS, pct_analysis, yieldgap_curYear_norm))

# Join sf to normalized dataframe
ctys_sf <- ctys_sf %>%
  dplyr::select(c(NAME, FIPS, COUNTYFP, STATEFP, NAMELSAD, yieldgap_curYear)) %>%
  left_join(ctys_df_yg_curYear, by = 'FIPS')

# Separate forested from non forested counties using LANDFIRE BPS data
# Initial mask prior to selection by WLFW Science Team
# analysisThreshold = 0.15
# masked_ctys_sf <- filter(ctys_sf, pct_analysis < analysisThreshold) %>%
#   st_transform(4326)
# unmask_ctys_sf <- filter(ctys_sf, pct_analysis > analysisThreshold) %>%
#   st_transform(4326)

# Read in CSV informed by initial mask and selection by WLFW Science Team
masked_ctys_df <-
  read_csv(
    "C:/Users/eric/Documents/NTSG/Projects/RAP/Scripts/CountyForage/data/csv/Mask_BPSandExpert_2022012022.csv"
  )
masked_ctys_sf <- filter(ctys_sf, FIPS %in% masked_ctys_df$FIPS)
`%notin%` <- Negate(`%in%`)
unmask_ctys_sf <- filter(ctys_sf, FIPS %notin% masked_ctys_df$FIPS)

# Calculate states yield gap statistics for generating labels on leaflet map
calc_states <- function(FIP) {
  ctys_df %>%
    filter(STATEFP == FIP) %>%
    group_by(year) %>%
    mutate(
      biomass = sum(biomass),
      yieldgap = sum(yieldgap),
      analysisArea = sum(analysisArea)
    ) %>%
    ungroup() %>%
    filter(year == current_year) %>%
    mutate(yieldgap_curYear_norm = yieldgap / (yieldgap + biomass) * 100) %>%
    dplyr::select(STATEFP, yieldgap_curYear = yieldgap, yieldgap_curYear_norm, biomass) %>%
    unique()
}
stas_df <- map(stas_fp_v, calc_states) %>% bind_rows()

# Join states yield gap statistics to states SF object
stas_sf <- left_join(stas_sf, stas_df, by = 'STATEFP')

rm(ctys_yieldgap_df,
   ctys_df_yg_curYear,
   analysisThreshold,
   calc_states,
   `%notin%`,
   masked_ctys_df)

# ---------------------- Nested loop to generate state and county reports -----------------------------
for (i in stas_fp_v) {
  #stas_fp_v
  
  # Subset state sf object for current state
  sta_sf <- filter(stas_sf, STATEFP == i)
  
  # Get list of county ids for state
  sta_ctys_v <-
    grep(paste('^', i, sep = ''), ctys_fp_v, value = TRUE)
  
  # Create state directory
  sta_name <- sta_sf$NAME
  sta_dir <-
    paste(
      'C:/Users/eric/Documents/NTSG/Projects/RAP/Scripts/CountyForage/Outputs/',
      str_replace(sta_name, ' ', ''),
      '/',
      sep = ''
    )
  dir.create(sta_dir)
  
  # Subset county sf object for current state's counties that are historically forested
  masked_sta_ctys_sf <- masked_ctys_sf %>%
    filter(STATEFP == i)
  
  # Subset county sf object for current state's counties that are historically nonforested
  unmask_sta_ctys_sf <- unmask_ctys_sf %>%
    filter(STATEFP == i)
  
  # Subset county sf object for current state's counties for labels and on-click functionality
  sta_ctys_sf <- ctys_sf %>%
    filter(STATEFP == i) %>%
    mutate(
      up_path = paste('../', str_replace_all(NAME, ' ', ''), '/index.html', sep = '') %>% stringr::str_replace_all("�", "n"),
      dn_path = paste('./', str_replace_all(NAME, ' ', ''), '/index.html', sep = '') %>% stringr::str_replace_all("�", "n")
    )
  
  # Generate RAP url for county
  coords <- sta_sf %>%
    st_transform(crs = 5070) %>%
    st_centroid() %>%
    st_transform(crs = 4326) %>%
    st_coordinates()
  rap_sta_url = paste(
    'https://rangelands.app/rap/?biomass_t=herbaceous&ll=',
    coords[2],
    ',',
    coords[1],
    '&z=8&landcover_t=tree&landcover_v=true',
    sep = ''
  )
  
  # Sum counties to generate state dataframe
  sta_df <- ctys_df %>%
    filter(STATEFP == i) %>%
    group_by(year) %>%
    mutate(
      treeArea = sum(treeArea),
      biomass = sum(biomass),
      yieldgap = sum(yieldgap),
      classWoodlands = sum(classWoodlands),
      classModCover = sum(classModCover),
      classLowCover = sum(classLowCover),
      classAtRisk = sum(classAtRisk),
      classIntact = sum(classIntact),
      analysisArea = sum(analysisArea),
      totalArea = sum(countyArea),
      treeCover = (treeArea / analysisArea) * 100
    ) %>%
    dplyr::select(-c(COUNTYFP, NAME, treedArea, COUNTYID, countyArea, BPS_Forest)) %>%
    ungroup() %>%
    unique() %>%
    mutate(cumYieldGap = cumsum(yieldgap))
  
  # Select attributes for CSV file
  sta_df_csv <- sta_df %>%
    mutate(staName = sta_name) %>%
    dplyr::select(
      c(
        staName,
        year,
        totalArea,
        analysisArea,
        biomass,
        yieldGap = yieldgap,
        cumYieldGap,
        treeCover,
        treeArea
        # classWoodlands,
        # classModCover,
        # classLowCover,
        # classAtRisk,
        # classIntact
      )
    ) %>%
    unique()
  
  # # Write out html data summary table to add to report
  # data_html <- sta_df %>%
  #   mutate(
  #     Year = round(year, 0),
  #     `Production (tons)` = format(round(biomass, 0), big.mark = ","),
  #     `Production gain/loss (tons)` = ifelse(yieldgap >= 0, paste(format(
  #       round(-yieldgap, 0), big.mark = ","
  #     )), str_replace_all(paste(
  #       '+', format(round(-yieldgap, 0), big.mark = ","), sep = ''
  #     ), ' ', '')),
  #     `Production gain/loss (%)` = ifelse(
  #       yieldgap >= 0,
  #       paste(format(round(
  #         -(yieldgap / (yieldgap + biomass)) * 100, 2
  #       ), nsmall = 2), '%', sep = ''),
  #       str_replace_all(paste('+', format(
  #         round(-(yieldgap / (yieldgap + biomass)) * 100, 2), nsmall = 2
  #       ), '%', sep = ''), ' ', '')
  #     ),
  #     `Tree area (acres)` = format(round(treeArea, 0), big.mark = ",")
  #   ) %>%
  #   dplyr::select(
  #     c(
  #       Year,
  #       `Production (tons)`,
  #       `Production gain/loss (tons)`,
  #       `Production gain/loss (%)`,
  #       `Tree area (acres)`
  #     )
  #   ) %>%
  #   tableHTML(widths = c(100, 150, 150, 150, 150),
  #             rownames = FALSE) %>%
  #   add_css_row(css = list(
  #     c('background-color', 'text-align', 'height', 'font-family'),
  #     c('#f2f2f2', 'center', '30px', 'Open Sans')
  #   ),
  #   rows = odd(1:(nrow(sta_df) + 1))) %>%
  #   add_css_row(css = list(
  #     c('background-color', 'text-align', 'height', 'font-family'),
  #     c('#FFFFFF', 'center', '30px', 'Open Sans')
  #   ),
  #   rows = even(1:nrow(sta_df)))
  # 
  # write_tableHTML(data_html, paste(sta_dir, 'data.html', sep = ''))
  
  # Write out html summary table to add to report
  char_df <- tibble(init = 10)
  char_df[, 'Total area'] <-
    paste(as.character(round(sta_df$totalArea[1]), 0), 'acres')
  char_df[, 'Analysis area'] <-
    paste(as.character(round(sta_df$analysisArea[1]), 0), 'acres')
  char_df[, 'Area excluded from analysis'] <-
    paste(as.character(round(
      sta_df$totalArea[1] - sta_df$analysisArea[1]
    ), 0), 'acres')
  char_df <- char_df[, 2:4]
  
  char_df <- char_df %>%
    t() %>%
    melt() %>%
    mutate(Value = prettyNum(value, big.mark = ",")) %>%
    dplyr::select(c(` ` = Var1, Area = Value))
  
  char_html <- char_df %>%
    tableHTML(
      widths = c(300, 200),
      rownames = FALSE
    ) %>%
    add_css_row(css = list(
      c(
        'background-color',
        'text-align',
        'height',
        'font-family',
        'font-size',
        'border-color'
      ),
      c(
        '#FFFFFF',
        'center',
        '30px',
        'Open Sans',
        '0.9rem',
        '#00000055 #FFFFFF00 #00000055 #FFFFFF00'
      )
    ), rows = 1:4)  %>%
    add_css_caption(css = list(
      c('font-size', 'font-family', 'text-align', 'margin-bottom'),
      c('1rem', 'Open sans', 'left', '8px')
    ))
  
  write_tableHTML(char_html, paste(sta_dir, 'poly_data.html', sep = ''))
  
  # # Export supplementary plots for download
  # # Annual production plot
  # png(
  #   file = paste(sta_dir, 'production.png', sep = ''),
  #   width = 9,
  #   height = 6,
  #   units = "in",
  #   res = 200
  # )
  # prod_plot <- ggplot() +
  #   geom_line(
  #     sta_df,
  #     mapping = aes(x = year, y = biomass),
  #     color = 'grey20',
  #     size = 1.5
  #   ) +
  #   geom_point(
  #     sta_df,
  #     mapping = aes(x = year, y = biomass),
  #     color = 'grey20',
  #     size = 3
  #   ) +
  #   labs(title = paste('Herbaceous production for', sta_name),
  #        y = 'Annual production (tons)') +
  #   scale_y_continuous(labels = scales::comma, limits = c(0, (max(sta_df$biomass)) *
  #                                                           1.1)) +
  #   scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  #   theme_minimal() +
  #   theme(axis.title.x = element_blank(),
  #         plot.title.position = "plot")
  # print(prod_plot)
  # dev.off()
  # 
  # # Tree cover area plot
  # png(
  #   file = paste(sta_dir, 'tree.png', sep = ''),
  #   width = 9,
  #   height = 6,
  #   units = "in",
  #   res = 200
  # )
  # tree_plot <- ggplot() +
  #   geom_line(
  #     sta_df,
  #     mapping = aes(x = year, y = treeArea),
  #     color = '#0E8E00',
  #     size = 1.5
  #   ) +
  #   geom_point(
  #     sta_df,
  #     mapping = aes(x = year, y = treeArea),
  #     color = '#0E8E00',
  #     size = 3
  #   ) +
  #   geom_hline(
  #     yintercept = head(sta_df$treeArea, n = 1),
  #     linetype = 'dashed',
  #     color = 'grey30',
  #     size = 1
  #   ) +
  #   labs(title = paste('Tree cover for', sta_name), y = 'Tree cover (acres)') +
  #   scale_y_continuous(labels = scales::comma, limits = c(0, max(sta_df$treeArea) *
  #                                                           1.1)) +
  #   scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  #   theme_minimal() +
  #   theme(axis.title.x = element_blank(),
  #         plot.title.position = "plot")
  # print(tree_plot)
  # dev.off()
  
  # Analysis Area Map
  # Load state raster
  sta_anArea_rast <-
    raster(
      paste(
        'C:/Users/eric/Documents/NTSG/Projects/RAP/Scripts/CountyForage/data/tif/AnalysisArea/',
        str_replace_all(sta_name, ' ', '_'),
        '.tif',
        sep = ''
      )
    )
  
  # Convert raster to dataframe for plotting
  sta_anArea_rast_df <-
    raster::as.data.frame(sta_anArea_rast, xy = TRUE) %>%
    rename(anArea_val = 3) %>%
    mutate(anArea = cut(
      anArea_val,
      breaks = c(-Inf, 0.99, 1.99, Inf),
      labels = c(NA,
                 "Area excluded from analysis",
                 "Analysis area")
    )) %>%
    filter(anArea %in% c("Analysis area",
                         "Area excluded from analysis"))
  # Factor the column for binary analyzed/not analyzed so that legend items show up in desired order
  sta_anArea_rast_df$anArea <-
    factor(sta_anArea_rast_df$anArea,
           levels = c("Analysis area",
                      "Area excluded from analysis"))
  
  # Aspect factor for setting height of exported map
  aspect_factor <-
    (max(sta_anArea_rast_df$x) - min(sta_anArea_rast_df$x)) / (max(sta_anArea_rast_df$y) - min(sta_anArea_rast_df$y))
  
  if (aspect_factor > 1.3) {
    png(
      file = paste(sta_dir, 'anArea.png', sep = ''),
      width = 7,
      height = 7 * (1/aspect_factor)+2,
      units = "in",
      res = 300
    )
  } else if (aspect_factor < 0.7) {
    png(
      file = paste(sta_dir, 'anArea.png', sep = ''),
      width = 7 * (aspect_factor)-2,
      height = 7,
      units = "in",
      res = 300
    )
  } else{
    png(
      file = paste(sta_dir, 'anArea.png', sep = ''),
      width = 6 * (aspect_factor)-1,
      height = 6 * (1/aspect_factor),
      units = "in",
      res = 300
    )
  }
  
  anArea_map <- ggplot() +
    geom_raster(sta_anArea_rast_df, mapping = aes(x = x, y = y, fill = anArea)) +
    geom_sf(
      sta_sf,
      mapping = aes(),
      color = "#343434",
      fill = "#FFFFFFFF",
      alpha = 0,
      size = 1.5
    ) +
    geom_sf(
      sta_ctys_sf,
      mapping = aes(),
      color = "#343434",
      fill = "#FFFFFFFF",
      alpha = 0,
      size = 1
    ) +
    labs(
      x = '',
      y = ''
    ) +
    scale_fill_manual(values = c(
      "Analysis area" = "#2c9e37",
      "Area excluded from analysis" = "#e5e5e5"
    )) +
    theme_minimal() +
    theme(
      text=element_text(family='Open Sans'),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      legend.title = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      legend.position = "bottom"
      
    )
  print(anArea_map)
  dev.off()
  
  # # Rangeland vulnerability map
  # # Load state raster
  # sta_vuln_rast <-
  #   raster(
  #     paste(
  #       'C:/Users/eric/Documents/NTSG/Projects/RAP/Scripts/CountyForage/data/tif/Vulnerability/240m_median/',
  #       str_replace_all(sta_name, ' ', '_'),
  #       '.tif',
  #       sep = ''
  #     )
  #   )
  # sta_vuln_rast[1, 1] <- 6
  #
  # # Convert raster to dataframe for plotting
  # sta_vuln_rast_df <-
  #   raster::as.data.frame(sta_vuln_rast, xy = TRUE) %>%
  #   rename(vuln_val = 3) %>%
  #   mutate(vuln = cut(
  #     vuln_val,
  #     breaks = c(-Inf, 0.99, 1.99, 2.99, 3.99, Inf),
  #     labels = c(
  #       NA,
  #       "Intact rangeland",
  #       "Intact rangeland, at risk",
  #       "Low/moderate tree cover",
  #       "Forest/woodland"
  #     )
  #   )) %>%
  #   filter(
  #     vuln %in% c(
  #       "Intact rangeland",
  #       "Intact rangeland, at risk",
  #       "Low/moderate tree cover",
  #       "Forest/woodland"
  #     )
  #   )
  #
  # # Aspect factor for setting height of exported map
  # aspect_factor <-
  #   (max(sta_vuln_rast_df$x) - min(sta_vuln_rast_df$x)) / (max(sta_vuln_rast_df$y) - min(sta_vuln_rast_df$y))
  #
  # png(
  #   file = paste(sta_dir, 'vuln.png', sep = ''),
  #   width = 9,
  #   height = 9 / aspect_factor + 2,
  #   units = "in",
  #   res = 150
  # )
  # vuln_map <- ggplot() +
  #   geom_raster(sta_vuln_rast_df, mapping = aes(x = x, y = y, fill = vuln)) +
  #   geom_sf(
  #     sta_sf,
  #     mapping = aes(),
  #     color = "#343434",
  #     fill = "#FFFFFFFF",
  #     alpha = 0,
  #     size = 1.5
  #   ) +
  #   geom_sf(
  #     sta_ctys_sf,
  #     mapping = aes(),
  #     color = "#343434",
  #     fill = "#FFFFFFFF",
  #     alpha = 0,
  #     size = 1
  #   ) +
  #   labs(
  #     title = paste('Rangeland vulnerability map for', sta_name),
  #     x = '',
  #     y = ''
  #   ) +
  #   scale_fill_manual(
  #     values = c(
  #       "Intact rangeland" = "#00B050",
  #       "Intact rangeland, at risk" = "#FDE64B",
  #       "Low/moderate tree cover" = "#FF0000",
  #       "Forest/woodland" = "grey40"
  #     )
  #   ) +
  #   theme_minimal() +
  #   theme(
  #     axis.title.x = element_blank(),
  #     axis.text.x = element_blank(),
  #     axis.ticks.y = element_blank(),
  #     axis.text.y = element_blank(),
  #     legend.title = element_blank(),
  #     panel.grid.major = element_blank(),
  #     panel.grid.minor = element_blank(),
  #     panel.background = element_blank(),
  #     legend.position = "bottom"
  #   )
  # print(vuln_map)
  # dev.off()
  
  # Generate rmarkdown reports
  sta_csv_path = paste(sta_dir, 'data.csv', sep = '')
  write_csv(sta_df_csv, file = sta_csv_path)
  rmarkdown::render(
    "C:/Users/eric/Documents/NTSG/Projects/RAP/Scripts/CountyForage/Scripts/ForageReports_html.Rmd",
    output_file = paste(sta_dir, 'index', sep = ''),
    params = list(
      new_title = sta_name,
      main_df = sta_df,
      aspect_factor = aspect_factor,
      cty_sf = NULL,
      sta_sf = sta_sf,
      sta_ctys_sf = sta_ctys_sf,
      unmask_sta_ctys_sf = unmask_sta_ctys_sf,
      masked_sta_ctys_sf = masked_sta_ctys_sf,
      current_year = current_year,
      rap_url = rap_sta_url,
      main_dir = sta_dir,
      type = 'State'
    )
  )
  
  # pagedown::chrome_print(input = paste(sta_dir, 'index.html', sep = ''),
  #                        output = paste(sta_dir, 'index_paged.pdf', sep = ''),
  #                        format = "pdf")
  
  
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

    for (j in sta_ctys_v) {
      #sta_ctys_v

      # Subset to county sf object and name
      cty_sf <-
        filter(ctys_sf, FIPS == j) #dataframe with county names
      cty_sf_hiRes <-
        filter(ctys_sf_hiRes, COUNTYID == j) #dataframe with county names

      #Create county directory
      cty_name_s <- str_replace_all(cty_sf$NAME, ' ', '')
      cty_name_l <- cty_sf$NAMELSAD #county name
      cty_dir <- paste(sta_dir, cty_name_s, '/', sep = '') %>%
        stringr::str_replace_all("�", "n")
      dir.create(cty_dir)

      # Location name as title parameter for RMD
      loc_name <-
        paste(cty_name_l, ', ', sta_name, sep = '') #concatenated 'County, State' name

      # Filter SF objects for county of interest
      cty_df <- ctys_df %>%
        filter(COUNTYID == j) %>%
        mutate(cumYieldGap = cumsum(yieldgap),
               totalArea = countyArea) %>%
        dplyr::select(-countyArea)

    cty_df_csv <- cty_df %>%
      mutate(staName = sta_name) %>%
      dplyr::select(
        c(
          ctyName = NAME,
          staName,
          totalArea,
          analysisArea,
          biomass,
          yieldGap = yieldgap,
          cumYieldGap,
          treeCover,
          treeArea
          # classWoodlands,
          # classModCover,
          # classLowCover,
          # classAtRisk,
          # classIntact
        )
      )

      # Generate RAP url for county
      coords <- cty_sf %>%
        st_transform(crs = 5070) %>%
        st_centroid() %>%
        st_transform(crs = 4326) %>%
        st_coordinates()
      rap_cty_url = paste(
        'https://rangelands.app/rap/?biomass_t=herbaceous&ll=',
        coords[2],
        ',',
        coords[1],
        '&z=9&landcover_t=tree&landcover_v=true',
        sep = ''
      )

      # # Write out html table to add to report
      # data_html <- cty_df %>%
      #   mutate(
      #     Year = round(year, 0),
      #     `Production (tons)` = format(round(biomass, 0), big.mark =
      #                                    ","),
      #     `Production gain/loss (tons)` = ifelse(
      #       yieldgap >= 0,
      #       paste(format(round(-yieldgap, 0), big.mark = ",")),
      #       str_replace_all(paste(
      #         '+', format(round(-yieldgap, 0), big.mark = ","), sep = ''
      #       ), ' ', '')
      #     ),
      #     `Production gain/loss (%)` = ifelse(
      #       yieldgap >= 0,
      #       paste(format(round(
      #         -(yieldgap / (yieldgap + biomass)) * 100, 2
      #       ), nsmall = 2), '%', sep = ''),
      #       str_replace_all(paste(
      #         '+', format(round(-(
      #           yieldgap / (yieldgap + biomass)
      #         ) * 100, 2), nsmall = 2), '%', sep = ''
      #       ), ' ', '')
      #     ),
      #     `Tree area (acres)` = format(round(treeArea, 0), big.mark =
      #                                    ",")
      #   ) %>%
      #   dplyr::select(
      #     c(
      #       Year,
      #       `Production (tons)`,
      #       `Production gain/loss (tons)`,
      #       `Production gain/loss (%)`,
      #       `Tree area (acres)`
      #     )
      #   ) %>%
      #   tableHTML(widths = c(100, 150, 150, 150, 150),
      #             rownames = FALSE) %>%
      #   add_css_row(css = list(
      #     c('background-color', 'text-align', 'height', 'font-family'),
      #     c('#f2f2f2', 'center', '30px', 'Open Sans')
      #   ),
      #   rows = odd(1:(nrow(cty_df) + 1))) %>%
      #   add_css_row(css = list(
      #     c('background-color', 'text-align', 'height', 'font-family'),
      #     c('#FFFFFF', 'center', '30px', 'Open Sans')
      #   ),
      #   rows = even(1:nrow(cty_df)))
      # write_tableHTML(data_html, paste(cty_dir, 'data.html', sep = ''))

      # Write out html summary table to add to report
      char_df <- tibble(init = 10)
      char_df[, 'Total area'] <-
        paste(as.character(round(cty_df$totalArea[1]), 0), 'acres')
      char_df[, 'Analysis area'] <-
        paste(as.character(round(cty_df$analysisArea[1]), 0), 'acres')
      char_df[, 'Area excluded from analysis'] <-
        paste(as.character(round(
          cty_df$totalArea[1] - cty_df$analysisArea[1]
        ), 0), 'acres')
      char_df <- char_df[, 2:4]

      char_df <- char_df %>%
        t() %>%
        melt() %>%
        mutate(Value = prettyNum(value, big.mark = ",")) %>%
        dplyr::select(c(` ` = Var1, Area = Value))

      char_html <- char_df %>%
        tableHTML(
          widths = c(300, 200),
          rownames = FALSE
        ) %>%
        add_css_row(css = list(
          c(
            'background-color',
            'text-align',
            'height',
            'font-family',
            'font-size',
            'border-color'
          ),
          c(
            '#FFFFFF',
            'center',
            '30px',
            'Open Sans',
            '0.9rem',
            '#00000055 #FFFFFF00 #00000055 #FFFFFF00'
          )
        ), rows = 1:4)  %>%
        add_css_caption(css = list(
          c('font-size', 'font-family', 'text-align', 'margin-bottom'),
          c('1rem', 'Open sans', 'left', '8px')
        ))

      write_tableHTML(char_html, paste(cty_dir, 'poly_data.html', sep = ''))

      # Write out csv of county data
      write_csv(cty_df_csv, path = paste(cty_dir, 'data.csv', sep = ''))

      # # Export supplementary maps for download
      # # Annual production plot
      # png(
      #   file = paste(cty_dir, 'production.png', sep = ''),
      #   width = 6,
      #   height = 4,
      #   units = "in",
      #   res = 300
      # )
      # prod_map <- ggplot() +
      #   geom_line(
      #     cty_df,
      #     mapping = aes(x = year, y = biomass),
      #     color = 'grey20',
      #     size = 1
      #   ) +
      #   geom_point(
      #     cty_df,
      #     mapping = aes(x = year, y = biomass),
      #     color = 'grey20',
      #     size = 2
      #   ) +
      #   labs(
      #     title = paste('Herbaceous production for ', cty_name, ', ', sta_name, sep = ''),
      #     y = 'Annual production (tons)'
      #   ) +
      #   scale_y_continuous(labels = comma, limits = c(0, (max(cty_df$biomass)) *
      #                                                   1.1)) +
      #   scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      #   theme_minimal() +
      #   theme(axis.title.x = element_blank(),
      #         plot.title.position = "plot")
      # print(prod_map)
      # dev.off()
      # 
      # # Tree cover area
      # png(
      #   file = paste(cty_dir, 'tree.png', sep = ''),
      #   width = 6,
      #   height = 4,
      #   units = "in",
      #   res = 300
      # )
      # tree_map <- ggplot() +
      #   geom_line(
      #     cty_df,
      #     mapping = aes(x = year, y = treeArea),
      #     color = '#0E8E00',
      #     size = 1
      #   ) +
      #   geom_point(
      #     cty_df,
      #     mapping = aes(x = year, y = treeArea),
      #     color = '#0E8E00',
      #     size = 2
      #   ) +
      #   geom_hline(
      #     yintercept = head(cty_df$treeArea, n = 1),
      #     linetype = 'dashed',
      #     color = 'grey30',
      #     size = 1
      #   ) +
      #   labs(title = paste('Tree cover for ', loc_name, sep = ''),
      #        y = 'Tree cover (acres)') +
      #   scale_y_continuous(labels = comma, limits = c(0, max(cty_df$treeArea) *
      #                                                   1.1)) +
      #   scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      #   theme_minimal() +
      #   theme(axis.title.x = element_blank(),
      #         plot.title.position = "plot")
      # print(tree_map)
      # dev.off()

      # Analysis Area Map
      # Load state raster
      cty_anArea_rast <-
        raster(
          paste(
            'C:/Users/eric/Documents/NTSG/Projects/RAP/Scripts/CountyForage/data/tif/AnalysisArea/',
            str_replace_all(sta_name, ' ', ''),
            '/',
            str_replace_all(cty_sf$NAME, ' ', '_'),
            '.tif',
            sep = ''
          )
        )

      # Convert raster to dataframe for plotting
      cty_anArea_rast_df <-
        raster::as.data.frame(cty_anArea_rast, xy = TRUE) %>%
        rename(anArea_val = 3) %>%
        mutate(anArea = cut(
          anArea_val,
          breaks = c(-Inf, 0.99, 1.99, Inf),
          labels = c(NA,
                     "Area excluded from analysis",
                     "Analysis area")
        )) %>%
        filter(anArea %in% c("Analysis area",
                             "Area excluded from analysis"))
      # Factor the column for binary analyzed/not analyzed so that legend items show up in desired order
      cty_anArea_rast_df$anArea <-
        factor(cty_anArea_rast_df$anArea,
               levels = c("Analysis area",
                          "Area excluded from analysis"))

      # Aspect factor for setting height of exported map
      aspect_factor <-
        (max(cty_anArea_rast_df$x) - min(cty_anArea_rast_df$x)) / (max(cty_anArea_rast_df$y) - min(cty_anArea_rast_df$y))

      if (aspect_factor > 1.3) {
        png(
          file = paste(cty_dir, 'anArea.png', sep = ''),
          width = 7,
          height = 7 * (1/aspect_factor)+2,
          units = "in",
          res = 300
        )
      } else if (aspect_factor > 0.7) {
        png(
          file = paste(cty_dir, 'anArea.png', sep = ''),
          width = 6 * (aspect_factor)-1,
          height = 6 * (1/aspect_factor),
          units = "in",
          res = 300
        )
      } else if (aspect_factor > 0.4) {
        png(
          file = paste(cty_dir, 'anArea.png', sep = ''),
          width = 7 * (aspect_factor),
          height = 7,
          units = "in",
          res = 300
        )
      } else{
        png(
          file = paste(cty_dir, 'anArea.png', sep = ''),
          width = 7 * (aspect_factor)+1.5,
          height = 7,
          units = "in",
          res = 300
        )
      }

      anArea_map <- ggplot() +
        geom_raster(cty_anArea_rast_df, mapping = aes(x = x, y = y, fill = anArea)) +
        geom_sf(
          cty_sf_hiRes,
          mapping = aes(),
          color = "#343434",
          fill = "#FFFFFFFF",
          alpha = 0,
          size = 1.5
        ) +
        labs(
          x = '',
          y = ''
        ) +
        scale_fill_manual(values = c(
          "Analysis area" = "#2c9e37",
          "Area excluded from analysis" = "#e5e5e5"
        )) +
        theme_minimal() +
        theme(
          text=element_text(family='Open Sans'),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.position = "bottom"

        )
      print(anArea_map)
      dev.off()

      # Generate rmarkdown reports
      rmarkdown::render(
        "C:/Users/eric/Documents/NTSG/Projects/RAP/Scripts/CountyForage/Scripts/ForageReports_html.Rmd",
        output_file = paste(cty_dir, 'index', sep = ''),
        params = list(
          new_title = loc_name,
          main_df = cty_df,
          cty_sf = cty_sf,
          sta_sf = sta_sf,
          aspect_factor = aspect_factor,
          sta_ctys_sf = sta_ctys_sf,
          unmask_sta_ctys_sf = unmask_sta_ctys_sf,
          masked_sta_ctys_sf = masked_sta_ctys_sf,
          current_year = current_year,
          rap_url = rap_cty_url,
          main_dir = cty_dir,
          type = 'County'
        )
      )

      # rmarkdown::render("Scripts/ForageReports_pdf.Rmd",
      #                   output_file = paste(here(), '/', sta_dir, cty_name_s, '/index.pdf', sep = ''),
      #                   output_format = pdf_document(),
      #                   params = list(new_title = loc_name,
      #                                 main_df = cty_df,
      #                                 cty_sf = cty_sf,
      #                                 sta_sf = sta_sf,
      #                                 sta_ctys_sf = sta_ctys_sf,
      #                                 current_year = current_year,
      #                                 rap_url = rap_cty_url,
      #                                 type = 'County'))

    }
}

