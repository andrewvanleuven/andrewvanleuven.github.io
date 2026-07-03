# prep_golf_data.R
# Parse POI Factory golf-course data, spatially join to counties, attach 2020
# census population, and write clean GeoJSON outputs used by index.qmd.
#
# Source of raw points: http://www.poi-factory.com/node/29395

library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)

options(tigris_use_cache = TRUE)

# ---- 1. Read & parse raw POI data -------------------------------------------
# Columns (no header): lon, lat, name, description
raw <- read_csv(
  "poi_factory_golf.csv",
  col_names = c("lon", "lat", "name", "description"),
  show_col_types = FALSE
)

golf <- raw |>
  mutate(
    # Course name is everything before the trailing "-City,ST"
    course = str_trim(str_remove(name, "-[^-]+,[A-Z]{2}$")),
    # Access type e.g. (Public), (Municipal), (Private)
    access = str_match(description, "^\\(([^)]+)\\)")[, 2],
    # Number of holes
    holes  = as.integer(str_match(description, "\\((\\d+)\\s*Holes?\\)")[, 2]),
    # State from the "City,ST ZIP" portion of the description
    state  = str_match(description, ",\\s*([A-Z]{2})\\s*\\d{5}")[, 2]
  ) |>
  filter(!is.na(lon), !is.na(lat)) |>
  filter(between(lon, -180, -60), between(lat, 15, 72)) # drop bad coords

golf_sf <- st_as_sf(golf, coords = c("lon", "lat"), crs = 4269, remove = FALSE)

# ---- 2. Spatial join to counties --------------------------------------------
counties_sf <- counties(cb = TRUE, resolution = "20m", year = 2020,
                        progress_bar = FALSE) |>
  filter(as.integer(STATEFP) < 60) |>
  select(GEOID, county = NAMELSAD, st = STUSPS)

golf_sf <- st_join(golf_sf, st_transform(counties_sf, 4269), join = st_within) |>
  filter(!is.na(GEOID)) |>
  mutate(
    # Binary access category: publicly accessible vs. members-only private club.
    # Municipal and semi-private courses allow public play, so group with Public.
    category = if_else(access == "Private", "Private", "Public")
  )

# ---- 3. 2020 population (PL 94-171 redistricting file) -----------------------
pop <- get_decennial(
  geography = "county",
  variables = "P1_001N",   # total population
  year      = 2020,
  sumfile   = "pl",
  progress_bar = FALSE
) |>
  select(GEOID, pop = value)

# ---- 4. County-level counts & per-capita (all / public / private) -----------
county_counts <- golf_sf |>
  st_drop_geometry() |>
  count(GEOID, category) |>
  pivot_wider(names_from = category, values_from = n,
              values_fill = 0, names_prefix = "n_") |>
  transmute(GEOID,
            courses         = coalesce(n_Public, 0L) + coalesce(n_Private, 0L),
            courses_public  = coalesce(n_Public, 0L),
            courses_private = coalesce(n_Private, 0L))

county_data <- counties_sf |>
  left_join(county_counts, by = "GEOID") |>
  left_join(pop, by = "GEOID") |>
  mutate(across(starts_with("courses"), \(x) replace_na(x, 0L))) |>
  mutate(
    per_100k         = if_else(pop > 0, courses         / pop * 1e5, NA_real_),
    per_100k_public  = if_else(pop > 0, courses_public  / pop * 1e5, NA_real_),
    per_100k_private = if_else(pop > 0, courses_private / pop * 1e5, NA_real_)
  )

# ---- 5. Write outputs --------------------------------------------------------
golf_out <- golf_sf |>
  st_drop_geometry() |>
  transmute(course, access, category, holes,
            city_state = str_remove(name, "^.*-"),
            st, county, lon, lat)

st_as_sf(golf_out, coords = c("lon", "lat"), crs = 4326, remove = FALSE) |>
  st_write("golf_points.geojson", delete_dsn = TRUE, quiet = TRUE)

st_transform(county_data, 4326) |>
  st_write("golf_counties.geojson", delete_dsn = TRUE, quiet = TRUE)

write_csv(golf_out, "golf_courses.csv")

message("Wrote ", nrow(golf_out), " courses across ",
        n_distinct(golf_sf$GEOID), " counties.")
