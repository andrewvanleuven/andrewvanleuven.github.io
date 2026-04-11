library(tidyverse)
library(tigris)
library(janitor)
library(sf)
library(scales)
library(mapgl)

# ==========================================================
# PART 0: PULL FROM WIKIPEDIA HIST. POPULATION GITHUB REPO
# ==========================================================

st_list <- states(cb = TRUE, resolution = '20m') %>%
  filter(as.integer(GEOID) < 60, !STUSPS %in% c('AK', 'HI')) %>%
  arrange(GEOID) %>%
  pull(STUSPS)
wiki_hist_pops <- map_dfr(
  .x = st_list,
  .f = function(x) {
    filen <- sprintf(
      'https://github.com/CreatingData/Historical-Populations/raw/refs/heads/master/wikipedia_state_data/%s.csv',
      x
    )
    read_csv(filen, show_col_types = FALSE) %>%
      select(title:y2010, lat, lon, settlement_type) %>%
      arrange(title)
  }
)
write_csv(wiki_hist_pops, 'posts/03_2026_popdecline/wiki_hist_pops.csv')

# ============================================================
# PART 1: CITY/PLACE POPULATION DECLINE (Wikipedia → Census)
# ============================================================

# Build a vector of contiguous U.S. state abbreviations (excl. AK/HI)
st_list <- states(cb = TRUE, resolution = '20m') %>%
  filter(as.integer(GEOID) < 60, !STUSPS %in% c('AK', 'HI')) %>%
  arrange(GEOID) %>%
  pull(STUSPS)

# Settlement types from Wikipedia that correspond to incorporated Census places.
# CDPs are excluded here but are re-admitted for certain states further below.
keep_types <- c(
  'City', 'city', 'General law city', 'Charter city', 'Statutory city',
  'Small city', 'Independent city', 'Consolidated city-county',
  'Consolidated city–county', 'Consolidated city-parish',
  'Town', 'town', 'Statutory town', 'New England town',
  'Incorporated town', 'Suburban city',
  'Village', 'village', 'Village (united states)', 'Incorporated village',
  'Borough', 'Home rule municipality', 'Municipality'
)

# ------------------------------------------------------------
# Load and clean Wikipedia historical population data
# ------------------------------------------------------------
df <- read_csv('posts/03_2026_popdecline/wiki_hist_pops.csv') %>%
  clean_names() %>%

  # Keep only incorporated place types and places with a nonzero 2010 population
  filter(settlement_type %in% keep_types, y2010 > 0) %>%

  # Drop neighborhood/community-area articles whose titles contain a parent city
  # (e.g., "Logan Square, Chicago" should not be treated as a standalone place)
  filter(
    str_detect(title, ', Chicago',   negate = TRUE),
    str_detect(title, ', Louisville', negate = TRUE),
    str_detect(title, ', Boston',    negate = TRUE)
  ) %>%

  # Fix a handful of Wikipedia titles that omit the state suffix
  mutate(
    title = ifelse(title == 'Chicago',       'Chicago, Illinois',        title),
    title = ifelse(title == 'Boston',        'Boston, Massachusetts',    title),
    title = ifelse(title == 'New York City', 'New York City, New York',  title),
    title = ifelse(title == 'Cleveland',     'Cleveland, Ohio',          title)
  ) %>%

  # Reshape from wide (one column per census year) to long (one row per year)
  pivot_longer(cols = y1790:y2010, names_to = 'year', values_to = 'population') %>%
  mutate(year = str_remove_all(year, 'y') %>% as.numeric()) %>%

  # Keep only the standalone "Washington, D.C." article; drop all neighborhood
  # articles whose titles end with ", Washington, D.C."
  filter(
    title == "Washington, D.C." | !str_detect(title, "Washington, D\\.C\\."),
    !str_detect(title, "\\((town|village)\\)")   # drop NY parenthetical variants
  ) %>%

  # Split "City, State" title into separate columns on the last comma
  separate(title, into = c('city', 'state'), sep = ',(?=[^,]*$)') %>%
  mutate(state = str_trim(state)) %>%

  # Drop rows that are clearly not places (bad separations, county articles, NAs)
  filter(
    !state %in% c('Cleveland', 'Manhattan'),
    !str_ends(city, ' County'),
    !is.na(state)
  )

# ------------------------------------------------------------
# Identify peak population and compute decline to 2010
# ------------------------------------------------------------

# One row per city: the year and population at historical peak
peaks <- df %>%
  slice_max(population, n = 1, with_ties = FALSE, by = c(city, state)) %>%
  select(city, state, peak_year = year, peak_pop = population)

# 2010 population for each city (the endpoint of our decline window)
pop_2010 <- df %>%
  filter(year == 2010) %>%
  select(city, state, pop_2010 = population)

# Join peak and 2010 data; compute raw and relative decline.
# Keep only cities that: (a) actually declined from peak to 2010,
# (b) had at least 2,500 residents in 2010, and
# (c) represent the worst-performing city (raw OR relative) within
#     each state × size-class combination.
result <- peaks %>%
  left_join(pop_2010, by = c('city', 'state')) %>%
  filter(pop_2010 != peak_pop, pop_2010 >= 2500) %>%
  mutate(
    decline     = pop_2010 - peak_pop,
    pct_decline = (pop_2010 - peak_pop) / peak_pop,
    type = case_when(
      pop_2010 < 10000  ~ 'Small Town',
      pop_2010 < 50000  ~ 'Town',
      pop_2010 < 250000 ~ 'City',
      TRUE              ~ 'Large City'
    )
  ) %>%
  filter(decline != 0, pct_decline != 0) %>%
  filter(
    decline == min(decline) | pct_decline == min(pct_decline),
    .by = c(state, type)
  ) %>%
  arrange(state, city, type)

# ============================================================
# PART 2: BUILD CENSUS PLACE UNIVERSE FOR SPATIAL JOIN
# ============================================================

# OMB Core-Based Statistical Area delineation file (2023).
# Used to classify each county as Metro, Micro, or Non-Core.
omb <- rio::import(
  'https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2023/delineation-files/list1_2023.xlsx',
  skip = 2
) %>%
  clean_names() %>%
  rename(ctype = 5) %>%
  mutate(
    cty_fips = paste0(fips_state_code, fips_county_code),
    cty_type = case_when(
      str_detect(ctype, 'Metro') ~ 'Metro',
      str_detect(ctype, 'Micro') ~ 'Micro',
      .default                   = 'Non-Core'
    )
  ) %>%
  select(cty_fips, cty_type, central_outlying_county) %>%
  filter(cty_fips != 'NANA')

# USDA Rural-Urban Continuum Codes (2023).
# Joined with OMB to get full rurality classification per county.
rucc <- read_csv(
  'https://ers.usda.gov/sites/default/files/_laserfiche/DataFiles/53251/Ruralurbancontinuumcodes2023.csv',
  show_col_types = FALSE
) %>%
  filter(Attribute == 'RUCC_2023') %>%
  mutate(cty_fips = str_pad(FIPS, width = 5, pad = '0')) %>%
  left_join(omb, by = 'cty_fips') %>%
  mutate(rucc = as.integer(Value)) %>%
  select(cty_fips, rucc, cty_type, cty_centrality = central_outlying_county) %>%
  replace_na(list(cty_centrality = '--', cty_type = 'Non-Core')) %>%
  mutate(
    cty_msa_adj = case_when(
      rucc %in% c(4, 6, 8) ~ 'Adjacent',
      rucc %in% c(5, 7, 9) ~ 'Non-Adjacent',
      rucc %in% c(1, 2, 3) ~ 'In Metro'
    )
  )

# County polygons (TIGER/Line) with rurality attributes attached
cty_univ <- counties(st_list, cb = TRUE, resolution = '5m') %>%
  select(cty_fips = GEOID, cty_name = NAME, state = STUSPS) %>%
  left_join(rucc, by = 'cty_fips') %>%
  st_transform(6350)

# 2020 decennial Census place populations with point geometry (centroids).
# Used to spatially assign each place to a county.
univ_pop <- tidycensus::get_decennial(
  geography = 'place',
  variables = 'P1_001N',
  year      = 2020,
  geometry  = TRUE
) %>%
  select(GEOID, city_name = NAME, pop_20 = value) %>%
  st_transform(6350) %>%
  st_centroid()

# States where CDPs are commonly used for incorporated-like settlements
# (e.g., New England towns that lack separate Census place records)
cdp_exception_states <- c(
  'CT', 'ME', 'MA', 'NH', 'RI', 'VT',
  'NY', 'NJ', 'PA', 'MI', 'MN', 'WI'
)

# Spatial join: assign each Census place to the county whose polygon
# contains its centroid. Filter out CDPs except in exception states,
# and drop places with fewer than 2,500 residents (matches result threshold).
universe_candidates <- st_intersection(univ_pop, cty_univ) %>%
  select(
    city_fips     = GEOID,
    city_name,
    state,
    pop_20,
    cty_fips,
    cty_type,
    cty_centrality,
    cty_msa_adj,
    rucc
  ) %>%
  filter(
    str_detect(city_name, 'CDP', negate = TRUE) | state %in% cdp_exception_states
  ) %>%
  arrange(city_fips)

# ============================================================
# PART 3: NAME NORMALIZATION AND FUZZY MATCHING
# ============================================================

# Strip Census place-type suffixes so names can be matched against
# Wikipedia titles, which generally use bare city names.
normalize_city_name <- function(name) {
  name %>%
    str_replace_all(' city', '') %>%
    str_replace_all(' town', '') %>%
    str_replace_all(' CDP', '') %>%
    str_replace_all(' village', '') %>%
    str_replace_all(' borough', '') %>%
    str_replace_all(' unified government', '') %>%
    str_replace_all(' metropolitan government', '') %>%
    str_replace_all(' consolidated government', '') %>%
    str_replace_all(' Town, Mass', ', Mass') %>%
    str_replace_all(' \\(balance\\)', '') %>%
    str_replace_all('  ', ' ') %>%
    str_trim()
}

# Apply normalization to Census side of the join
tiger_places <- universe_candidates %>%
  st_drop_geometry() %>%
  arrange(city_fips) %>%
  mutate(clean_name = normalize_city_name(city_name)) %>% 
  rename(st = state)

# Apply normalization to Wikipedia side of the join.
# Also handle a few consolidated city-county names that differ between
# Wikipedia and TIGER (Butte, Anaconda, Macon).
hist_pops_clean <- result %>%
  mutate(
    title = paste0(city, ', ', state),
    title = str_replace_all(title, ' \\(village\\), New York', ', New York'),
    title = str_replace_all(title, ' \\(city\\), New York',   ', New York'),
    title = str_replace_all(title, ' \\(city\\), Vermont',    ', Vermont'),
    title = str_replace_all(title, 'Butte, Montana',     'Butte-Silver Bow (balance), Montana'),
    title = str_replace_all(title, 'Anaconda, Montana',  'Anaconda-Deer Lodge County, Montana'),
    title = str_replace_all(title, 'Macon, Georgia',     'Macon-Bibb County, Georgia'),
    title = str_replace_all(title, '[\u2013\u2014]', '-'),  # normalize en/em dashes
    clean_name = normalize_city_name(title)
  ) %>%
  # When a clean_name appears more than once, keep the row with the larger 2010 pop
  group_by(clean_name) %>%
  slice_max(pop_2010, n = 1, with_ties = FALSE) %>%
  ungroup() %>% 
  select(-state)

# Join Census place universe to Wikipedia decline data on normalized name.
# Rows with no Wikipedia match (is.na(city)) are dropped.
joined <- left_join(tiger_places, hist_pops_clean, by = 'clean_name') %>%
  filter(!is.na(city)) %>%
  select(-clean_name, -title, -city)

# Recompute decline metrics using 2020 Census population (pop_20) as the
# endpoint instead of 2010. Re-select the worst performer per state × size
# class and label each row by which criterion selected it.
city_result <- joined %>%
  filter(pop_20 < peak_pop) %>%   # exclude places still growing into 2020
  mutate(
    decline     = pop_20 - peak_pop,
    pct_decline = (pop_20 - peak_pop) / peak_pop
  ) %>%
  mutate(
    is_min_decline = decline     == min(decline),
    is_min_pct     = pct_decline == min(pct_decline),
    .by = c(st, type)
  ) %>%
  filter(is_min_decline | is_min_pct) %>%
  mutate(
    reason = case_when(
      is_min_decline & is_min_pct ~ 'Both',
      is_min_decline              ~ 'Raw Decline',
      is_min_pct                  ~ 'Relative Decline'
    )
  ) %>%
  select(-is_min_decline, -is_min_pct)

write_csv(city_result, 'posts/03_2026_popdecline/city_declines.csv')

# ============================================================
# PART 4: COUNTY POPULATION DECLINE (1900–2020)
# ============================================================

# Historical county populations from 1900–2020 (decennial Census).
# Source: andrewvanleuven.com
df_cty <- read_csv('https://andrewvanleuven.com/files/data/historical_county_populations_v2.csv')

county_seats <- rleuven::cbsaxw %>%
  select(cty_fips, cty_seat = seat, cbsa_type) %>% 
  replace_na(list(cbsa_type = 'Non-Core'))

# For each county: find the peak year/population, compute decline to 2020.
# Keep only counties that peaked before 2020, then retain the single county
# per state × metro group with the greatest raw decline AND the greatest
# relative decline (up to two rows per state × metro group; may be the same
# county). A 'reason' column identifies which criterion selected each row.
cty_result <- df_cty %>%
  filter(cty_fips != 11001) %>%
  left_join(county_seats) %>%
  replace_na(list(cbsa_type = 'Non-Core')) %>%
  # Classify counties as Metro (1) or non-Metro (0) based on CBSA type
  mutate(metro = ifelse(cbsa_type == 'Metro', 1, 0) %>% as.factor()) %>%
  pivot_longer(cols = starts_with('pop_'), names_to = 'year', values_to = 'population') %>%
  mutate(
    year  = str_remove(year, 'pop_') %>% as.integer(),
    state = str_extract(cty, '(?<=, ).+$')
  ) %>%
  slice_max(population, n = 1, with_ties = FALSE, by = c(cty_fips, cty)) %>%
  rename(peak_year = year, peak_pop = population) %>%
  left_join(df_cty %>% select(cty_fips, pop_2020), by = 'cty_fips') %>%
  filter(pop_2020 < peak_pop) %>%   # exclude counties still growing into 2020
  mutate(
    decline     = pop_2020 - peak_pop,
    pct_decline = (pop_2020 - peak_pop) / peak_pop
  ) %>%
  # Flag the worst-performing county on each criterion within state × metro group
  mutate(
    is_min_decline = decline     == min(decline),
    is_min_pct     = pct_decline == min(pct_decline),
    .by = c(state, metro)
  ) %>%
  filter(is_min_decline | is_min_pct) %>%
  mutate(
    reason = case_when(
      is_min_decline & is_min_pct ~ 'Both',
      is_min_decline              ~ 'Raw Decline',
      is_min_pct                  ~ 'Relative Decline'
    )
  ) %>%
  select(-is_min_decline, -is_min_pct) %>%
  arrange(state, metro, cty)

write_csv(cty_result, 'posts/03_2026_popdecline/county_declines.csv')

