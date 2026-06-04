# Introduction to Maps
# USDA AMS/AAEA GSS Local Food Economics Data Visualization Challenge
# Andrew Van Leuven | University of Vermont | June 4, 2026

library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)
library(rnassqs)
library(mapgl)
library(classInt)
library(RColorBrewer)
library(scales)
library(glue)
library(viridisLite)

options(tigris_use_cache = TRUE)


# ============================================================
# PART 2: MAPPING IN GGPLOT2
# ============================================================

states_sf <- states(cb = TRUE, resolution = "20m") %>%
  filter(!STUSPS %in% c("AK", "HI", "PR", "VI", "GU", "AS", "MP"))

counties_sf <- counties(cb = TRUE, resolution = "20m") %>%
  filter(!STATEFP %in% c("02", "15", "72", "78", "66", "60", "69"))

# your first map
ggplot(states_sf) +
  geom_sf()

# why does this look flat and weird?

# with a projection
ggplot(states_sf) +
  geom_sf() +
  coord_sf(crs = 5070)

# styled
ggplot(states_sf) +
  geom_sf(
    fill      = "dodgerblue",
    color     = "white",
    linewidth = 0.3
  ) +
  coord_sf(crs = 5070) +
  theme_void()

# --- Real data: LACE (Lack of Air Conditioning Estimate) ---
# Source: U.S. Census Bureau -- newly released 2023 county-level estimates
# https://www2.census.gov/programs-surveys/demo/datasets/lace/2023/

lace <- read_csv("https://www2.census.gov/programs-surveys/demo/datasets/lace/2023/LACE_23_County.csv") %>%
  mutate(GEOID = paste0(STATE, COUNTY))

lace_map <- counties_sf %>%
  left_join(lace, by = "GEOID")

# start with most arguments commented out -- uncomment one at a time to show
# what each layer/argument contributes to the finished map
ggplot(lace_map) +
  geom_sf(aes(fill = NO_AC_PE), color = NA) +
  # geom_sf(data = states_sf, fill = NA, color = "black", linewidth = 0.1) +
  # coord_sf(crs = 5070) +
  # scale_fill_distiller(
  #   palette = "YlGn",
  #   direction = 1,
  #   labels = percent,
  #   name = "Share without A/C"
  # ) +
  theme_void() +
  theme(
    # legend.position = "bottom",
    # plot.title = element_text(face = "bold", hjust = .5)
  ) +
  labs(
    # title = "Estimate for the percentage of occupied households\nwithout any kind of air conditioning"
  )


# ============================================================
# PART 3: CENSUS AND NASS DATA
# ============================================================

# --- tidycensus ---

# one-time setup: get a free key at api.census.gov/data/key_signup.html
# census_api_key("YOUR_KEY_HERE", install = TRUE)

# browse available variables
v22 <- load_variables(2022, "acs5", cache = TRUE)

# ACS data is estimated, not a headcount like the decennial census.
# The smaller the geography, the larger the margin of error.
v22 %>%
  filter(str_detect(label, regex("snap|food stamp", ignore_case = TRUE))) %>%
  select(name, label, concept)

# pull SNAP data with geometry attached
snap_households <- get_acs(
  geography = "county",
  variables = c(snap  = "B22003_002",
                total = "B22003_001"),
  year      = 2022,
  geometry  = TRUE
) %>%
  select(GEOID, NAME, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(snap_pct = snap / total) %>%
  filter(!str_starts(GEOID, "02|15|72"))

ggplot(snap_households) +
  geom_sf(aes(fill = snap_pct), color = NA) +
  geom_sf(data = states_sf, fill = NA, color = "white", linewidth = 0.4) +
  coord_sf(crs = 5070) +
  scale_fill_viridis_c(
    name      = "Share of households receiving SNAP",
    labels    = percent_format(accuracy = 1),
    option    = "rocket",
    direction = 1
  ) +
  theme_void(base_family = "Verdana") +
  theme(
    plot.title   = element_text(face = "bold", hjust = .5),
    legend.title = element_text(face = "bold", hjust = .5),
    plot.subtitle = element_text(hjust = .5),
    legend.position = "bottom"
  ) +
  guides(fill = guide_colourbar(barheight = 0.35, barwidth = 15, title.position = "top")) +
  labs(title    = "SNAP Participation by County",
       subtitle = "2018-2022 5-year ACS Estimates")

# smaller geographic unit: census tract
# the same get_acs() call works -- just change geography and add state
get_acs(
  geography = "tract",
  state     = "RI",
  variables = c(snap  = "B22003_002",
                total = "B22003_001"),
  year      = 2024,
  geometry  = TRUE
) %>%
  select(GEOID, NAME, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(snap_pct = snap / total) %>%
  ggplot() +
  geom_sf(aes(fill = snap_pct), color = NA) +
  scale_fill_viridis_c(
    name      = "Share of households receiving SNAP",
    labels    = percent_format(accuracy = 1),
    option    = "A",
    direction = -1,
    na.value  = "grey90"
  ) +
  theme_void(base_family = "Verdana") +
  theme(
    plot.title   = element_text(face = "bold", hjust = .5),
    legend.title = element_text(face = "bold", hjust = .5),
    plot.subtitle = element_text(hjust = .5),
    legend.position = "bottom"
  ) +
  guides(fill = guide_colourbar(barheight = 0.35, barwidth = 15, title.position = "top")) +
  labs(title    = "SNAP Participation by Census Tract",
       subtitle = "2020-2024 5-year ACS Estimates")

# --- rnassqs ---

# one-time setup per session: get a free key at quickstats.nass.usda.gov/api
# nassqs_auth("YOUR_API_KEY")

# prototype your query at quickstats.nass.usda.gov, then translate to a nassqs() call
corn_acres <- nassqs(
  commodity_desc    = "CORN",
  short_desc        = "CORN - ACRES PLANTED",
  domain_desc       = "TOTAL",
  agg_level_desc    = "COUNTY",
  year              = c(2015)
) %>%
  mutate(GEOID = paste0(state_fips_code, county_code)) %>%
  select(GEOID, year, corn_acres_planted = Value)

soybean_acres <- nassqs(
  commodity_desc    = "SOYBEANS",
  short_desc        = "SOYBEANS - ACRES PLANTED",
  domain_desc       = "TOTAL",
  agg_level_desc    = "COUNTY",
  year              = c(2015)
) %>%
  mutate(GEOID = paste0(state_fips_code, county_code)) %>%
  select(GEOID, year, soybean_acres_planted = Value)

corn_acres_sf    <- left_join(counties_sf, corn_acres)
soybean_acres_sf <- left_join(counties_sf, soybean_acres)

ggplot(corn_acres_sf) +
  geom_sf(aes(fill = corn_acres_planted), color = NA) +
  geom_sf(data = states_sf, fill = NA, color = "white", linewidth = 0.4) +
  coord_sf(crs = 5070) +
  scale_fill_viridis_c(
    option   = "A",
    name     = "Corn Acres Planted",
    labels   = comma_format(),
    trans    = "log10",
    na.value = "gray90"
  ) +
  theme_void(base_family = "Verdana") +
  theme(
    plot.title   = element_text(face = "bold", hjust = .5),
    legend.title = element_text(face = "bold", hjust = .5),
    legend.position = "bottom"
  ) +
  guides(fill = guide_colourbar(barheight = 0.35, barwidth = 15, title.position = "top")) +
  labs(
    title   = "Corn Acres Planted by County, 2015",
    caption = "\nSource: USDA NASS QuickStats (2026) \nSuppressed/missing counties shown in gray."
  )

ggplot(soybean_acres_sf) +
  geom_sf(aes(fill = soybean_acres_planted), color = NA) +
  geom_sf(data = states_sf, fill = NA, color = "white", linewidth = 0.4) +
  coord_sf(crs = 5070) +
  scale_fill_viridis_c(
    option   = "A",
    name     = "Soybean Acres Planted",
    labels   = comma_format(),
    trans    = "log10",
    na.value = "gray90"
  ) +
  theme_void(base_family = "Verdana") +
  theme(
    plot.title   = element_text(face = "bold", hjust = .5),
    legend.title = element_text(face = "bold", hjust = .5),
    legend.position = "bottom"
  ) +
  guides(fill = guide_colourbar(barheight = 0.35, barwidth = 15, title.position = "top")) +
  labs(
    title   = "Soybean Acres Planted by County, 2015",
    caption = "\nSource: USDA NASS QuickStats (2026) \nSuppressed/missing counties shown in gray."
  )

# Jenks natural breaks: an algorithm that minimizes variance within each class
# and maximizes it between classes -- finds natural groupings rather than
# arbitrary round numbers. Critical for right-skewed ag data.
corn_vals <- as.numeric(str_remove_all(corn_acres_sf$corn_acres_planted, ","))
soy_vals  <- as.numeric(str_remove_all(soybean_acres_sf$soybean_acres_planted, ","))
combined  <- c(corn_vals, soy_vals)

brks <- classIntervals(combined[!is.na(combined) & combined > 0], n = 5, style = "jenks")$brks %>%
  round(-3)
pal  <- brewer.pal(length(brks), "YlGnBu")

left_map <- maplibre(bounds = states_sf, style = carto_style("voyager")) %>%
  add_fill_layer(
    id         = "corn_acres",
    source     = corn_acres_sf,
    fill_color = interpolate(
      column   = "corn_acres_planted",
      values   = brks,
      stops    = pal,
      na_color = "lightgrey"
    ),
    fill_opacity = 0.9,
    tooltip      = "corn_acres_planted"
  ) %>%
  add_line_layer(
    id         = "states",
    source     = states_sf,
    line_width = .1
  ) %>%
  add_continuous_legend(
    "Corn Acres Planted by County, 2015",
    values   = brks,
    colors   = pal,
    position = "bottom-left",
    draggable = TRUE
  )

right_map <- maplibre(bounds = states_sf, style = carto_style("voyager")) %>%
  add_fill_layer(
    id         = "soybean_acres",
    source     = soybean_acres_sf,
    fill_color = interpolate(
      column   = "soybean_acres_planted",
      values   = brks,
      stops    = pal,
      na_color = "lightgrey"
    ),
    fill_opacity = 0.9,
    tooltip      = "soybean_acres_planted"
  ) %>%
  add_line_layer(
    id         = "states",
    source     = states_sf,
    line_width = .1
  ) %>%
  add_continuous_legend(
    "Soybean Acres Planted by County, 2015",
    values   = brks,
    colors   = pal,
    position = "bottom-left",
    draggable = TRUE
  )

# compare corn and soybean acres planted side by side
compare(left_map, right_map)


# ============================================================
# PART 4: POINT DATA AND BASIC GIS OPERATIONS
# ============================================================

# --- From table to map: USDA inspection establishments ---

# Source: https://www.fsis.usda.gov/inspection/establishments/meat-poultry-and-egg-product-inspection-directory
usda_inspection_estabs <- read_csv("https://andrewvanleuven.github.io/aaea_webinar/data/MPI_Directory_by_Establishment_Number.csv")

glimpse(usda_inspection_estabs)
# just a table, BUT it has latitude and longitude -- all we need

# iconv() strips non-UTF-8 characters that would break mapgl's JSON serialization
usda_inspection_estabs_sf <- usda_inspection_estabs %>%
  mutate(across(where(is.character), ~iconv(., to = "UTF-8", sub = ""))) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F) %>%
  st_transform(crs = 5070)

ggplot() +
  geom_sf(data = usda_inspection_estabs_sf, aes(color = size))

lower48_usda <- st_intersection(
  usda_inspection_estabs_sf,
  st_transform(states_sf, 5070)
) %>%
  filter(size %in% c("Large", "Small", "Very Small"))

ggplot() +
  geom_sf(data = st_transform(states_sf, 5070), color = "black", linewidth = .25) +
  geom_sf(data = lower48_usda, aes(color = size), size = 1, alpha = .7) +
  scale_color_manual(
    values = c("#e41a1c", "#e6ab02", "#377eb8"),
    name   = "Size"
  ) +
  theme_void(base_family = "Verdana") +
  theme(
    plot.title    = element_text(face = "bold", hjust = .5),
    legend.title  = element_text(face = "bold", hjust = .5),
    plot.subtitle = element_text(hjust = .5),
    legend.position = "bottom"
  ) +
  labs(title    = "USDA Meat, Poultry & Egg Product Inspection",
       subtitle = "Establishments by Size Class")

# --- Categorical point maps with mapgl ---

# Source: Overture Maps Foundation
gas_stations <- read_csv("https://andrewvanleuven.github.io/aaea_webinar/data/gas_stations.csv") %>%
  st_as_sf(wkt = "geometry_wkt", crs = 4326) %>%
  st_transform(crs = 5070)

mapboxgl(style = mapbox_style("light"),
         bounds = states_sf) %>%
  add_circle_layer(
    id           = "poi-gas",
    source       = gas_stations,
    circle_color = match_expr(
      "name",
      values = c("Casey's General Store", "Cenex", "Kwik Trip/Kwik Star"),
      stops  = c("#F3D317", "#8a0000", "#0300ad")
    ),
    circle_radius       = 4,
    circle_stroke_color = "#ffffff",
    circle_stroke_width = .4,
    circle_opacity      = 0.7,
    tooltip             = "city_name"
  ) %>%
  add_line_layer(
    id         = "states",
    source     = states_sf,
    line_width = .1
  ) %>%
  add_categorical_legend(
    legend_title = "Midwest Gas Station Chains",
    values       = c("Casey's General Store", "Cenex", "Kwik Trip/Kwik Star"),
    colors       = c("#F3D317", "#8a0000", "#0300ad"),
    patch_shape  = "circle",
    position     = "bottom-left",
    layer_id     = "poi-gas",
    interactive  = TRUE
  )

# Source: Overture Maps Foundation
dollar_stores <- read_csv("https://andrewvanleuven.github.io/aaea_webinar/data/dollar_stores.csv") %>%
  st_as_sf(wkt = "geometry_wkt", crs = 4326) %>%
  st_transform(crs = 5070) %>%
  mutate(tooltip = glue("<b>{name}</b><br>{city_name}"))

mapboxgl(style = mapbox_style("navigation-day"),
         bounds = states_sf) %>%
  add_circle_layer(
    id           = "poi-dollar-store",
    source       = dollar_stores,
    circle_color = match_expr(
      "name",
      values = c("Dollar General", "Dollar Tree", "Family Dollar"),
      stops  = c("#fff200", "#00954C", "#E81F11")
    ),
    circle_radius       = 4,
    circle_stroke_color = "black",
    circle_stroke_width = .4,
    circle_opacity      = 0.7,
    tooltip             = "tooltip"
  ) %>%
  add_categorical_legend(
    legend_title = "Dollar Store Establishments",
    values       = c("Dollar General", "Dollar Tree", "Family Dollar"),
    colors       = c("#fff200", "#00954C", "#E81F11"),
    patch_shape  = "circle",
    position     = "bottom-left",
    layer_id     = "poi-dollar-store",
    interactive  = TRUE
  )

# --- Spatial operations: from points to a county-level metric ---

# st_intersection assigns each dollar store to the county polygon it falls in,
# which lets us count stores per county and join to population for a rate
county_pop_2020 <- get_decennial(
  geography = "county",
  variables = "P1_001N",
  year      = 2020,
  geometry  = TRUE
) %>%
  st_transform(5070) %>%
  filter(!str_starts(GEOID, "02|15|72")) %>%
  select(cty_fips = GEOID, pop20 = value)

dollar_stores_by_county <- st_intersection(dollar_stores, county_pop_2020) %>%
  st_drop_geometry() %>%
  summarize(
    dollar_stores = n(),
    pop_2020      = first(pop20),
    .by = cty_fips
  ) %>%
  replace_na(list(dollar_stores = 0)) %>%
  mutate(dollar_stores_per_capita = dollar_stores / pop_2020)

dollar_store_sf <- county_pop_2020 %>%
  left_join(dollar_stores_by_county)

# raw counts: misleading -- larger counties will always look worse
ggplot() +
  geom_sf(data = dollar_store_sf, aes(fill = dollar_stores), color = NA) +
  geom_sf(data = states_sf, color = "white", linewidth = .35) +
  scale_fill_viridis_c(
    option    = "mako",
    name      = "Number of Establishments",
    trans     = "log10",
    direction = -1,
    na.value  = "gray90"
  ) +
  theme_void(base_family = "Verdana") +
  theme(
    plot.title    = element_text(face = "bold", hjust = .5),
    legend.title  = element_text(face = "bold", hjust = .5),
    plot.subtitle = element_text(hjust = .5),
    legend.position = "bottom"
  ) +
  guides(fill = guide_colourbar(barheight = 0.35, barwidth = 15, title.position = "top")) +
  labs(title   = "Dollar Store Establishments by County, 2025",
       caption = "\nSource: Overture Maps (2026)     \nSuppressed/missing counties shown in grey     ")

# normalized: a more honest picture of concentration
ggplot() +
  geom_sf(data = dollar_store_sf, aes(fill = dollar_stores_per_capita * 10000), color = NA) +
  geom_sf(data = states_sf, color = "black", linewidth = .15, fill = NA) +
  scale_fill_distiller(
    palette   = "YlOrBr",
    name      = "Dollar Stores per 10,000",
    direction = 1,
    na.value  = "white"
  ) +
  theme_void(base_family = "Verdana") +
  theme(
    plot.title    = element_text(face = "bold", hjust = .5),
    legend.title  = element_text(face = "bold", hjust = .5),
    plot.subtitle = element_text(hjust = .5),
    legend.position = "bottom"
  ) +
  guides(fill = guide_colourbar(barheight = 0.35, barwidth = 15, title.position = "top")) +
  labs(title   = "Dollar Store Concentration by County, 2025",
       caption = "\nSource: Overture Maps (2026)     \nSuppressed/missing counties shown in white     ")

# --- Buffer analysis: identifying underserved areas ---

# st_buffer creates a polygon of a fixed distance around each point.
# CRS 6463 (Iowa State Plane North) uses U.S. feet, so 1 mile = 5,280 feet.
iowa_dollar_stores <- dollar_stores %>%
  filter(state == "IA") %>%
  st_transform(6463) %>%
  st_buffer(10 * 5280)

maplibre_view(iowa_dollar_stores, interactive_legend = TRUE)

# dense points are hard to read -- heatmap reveals coverage gaps more clearly
iowa_pts <- dollar_stores %>%
  filter(state == "IA") %>%
  st_transform(4326) %>%
  st_jitter(factor = 0.0001)

maplibre(
  style  = maptiler_style("openstreetmap"),
  bounds = filter(states_sf, STUSPS == "IA")
) %>%
  add_circle_layer(
    id                  = "poi-dollar-store",
    source              = iowa_pts,
    circle_color        = "red",
    circle_stroke_color = "white",
    circle_stroke_width = 1
  ) %>%
  add_heatmap_layer(
    id            = "heatmap",
    source        = iowa_pts,
    heatmap_radius = 25,
    heatmap_color  = interpolate(
      property = "heatmap-density",
      values   = seq(0, 1, 0.2),
      stops    = c("transparent", magma(5))
    )
  )


# ============================================================
# GOING FURTHER: MORE THINGS sf CAN DO
# ============================================================

# sf cheat sheet: https://rstudio.github.io/cheatsheets/sf.pdf

# st_distance()        -- distance between features (in CRS units)
# st_nearest_feature() -- index of the nearest feature in another layer
# st_centroid()        -- geometric center of a polygon
# st_area()            -- area of a polygon (in CRS units)
# st_union()           -- merge/dissolve geometries into one
# st_bbox()            -- bounding box of a layer
# st_crop()            -- clip features to a rectangular extent
# st_within()          -- logical: is feature A fully inside feature B?
# st_touches()         -- logical: do features share a boundary?
# st_snap()            -- snap vertices to nearby features (topology cleanup)
