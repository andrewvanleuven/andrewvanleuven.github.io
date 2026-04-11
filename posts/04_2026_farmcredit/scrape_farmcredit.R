library(tidyverse)
library(tigris)
library(rvest)
library(httr)
library(janitor)
library(tictoc)
library(mapboxapi)
library(mapgl)
library(fuzzyjoin)

# Config -----------------------------------------------------------------
BASE_URL   <- "https://farmcredit.com/find-a-lender/"
BATCH_SIZE <- 25
SLEEP_SECS <- 2   # pause between requests — be polite

# STATEFP < 60 drops territories (PR, GU, VI, etc.)
states_list <- states(T,'20m') |>
  filter(STATEFP < 60) |>
  arrange(STATEFP) |>
  pull(STUSPS)

counties_list <- counties(states_list, T, '20m') |>
  select(st_abbr = STUSPS, st = STATE_NAME, cty_fips = GEOID, cty = NAME) |>
  arrange(cty_fips) |>
  sf::st_drop_geometry()


# Runtime estimate -------------------------------------------------------

n_batches_total <- counties_list |>
  count(st_abbr) |>
  mutate(batches = ceiling(n / BATCH_SIZE)) |>
  pull(batches) |>
  sum()

FETCH_SECS <- 1.5    # observed ~1-2s per HTTP request
total_secs <- n_batches_total * (SLEEP_SECS + FETCH_SECS) + length(states_list) * SLEEP_SECS
message(sprintf(
  "%d total batches × (%.2gs sleep + %gs fetch) = ~%dm %.0fs (~%.1f hours)",
  n_batches_total, SLEEP_SECS, FETCH_SECS,
  total_secs %/% 60, total_secs %% 60,
  total_secs / 3600
))


# Scraper functions ------------------------------------------------------
parse_row <- function(row, col_names) {
  cells        <- row |> html_elements(".lender-list-item") |> html_text(trim = TRUE)
  counties_svd <- row |> html_element(".details-counties p") |> html_text(trim = TRUE)
  result       <- setNames(as.list(cells), col_names[seq_along(cells)])
  result$counties_served <- if (is.na(counties_svd)) NA_character_ else counties_svd
  result
}

scrape_batch <- function(state, county_vec) {
  county_str <- paste(county_vec, collapse = ",")
  url <- modify_url(BASE_URL, query = list(state = state, county = county_str))

  message("Fetching: ", url)

  resp <- tryCatch(
    GET(url,
        add_headers(
          `User-Agent`      = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36",
          `Accept`          = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
          `Accept-Language` = "en-US,en;q=0.5"
        )),
    error = function(e) { message("  ERROR: ", e$message); return(NULL) }
  )

  if (is.null(resp) || http_error(resp)) {
    message("  Bad response: HTTP ", status_code(resp), " — skipping")
    return(NULL)
  }

  page <- read_html(content(resp, as = "text"))

  col_names <- page |> html_elements(".lender-list-header span") |> html_text(trim = TRUE)
  rows      <- page |> html_elements(".lender-list-item-wrapper")

  if (length(rows) == 0) {
    message("  No rows found")
    return(NULL)
  }

  rows |>
    map(\(r) parse_row(r, col_names)) |>
    bind_rows() |>
    select(-any_of(c("Contact Info", "Details"))) |>
    mutate(query_state = state, .before = 1)
}

scrape_state <- function(state_abbr) {
  tic(state_abbr)
  ctys    <- counties_list |> filter(st_abbr == state_abbr) |> pull(cty)
  batches <- split(ctys, ceiling(seq_along(ctys) / BATCH_SIZE))
  results <- vector("list", length(batches))

  for (i in seq_along(batches)) {
    message(sprintf("  [%s] batch %d / %d", state_abbr, i, length(batches)))
    results[[i]] <- scrape_batch(state_abbr, batches[[i]])
    if (i < length(batches)) Sys.sleep(SLEEP_SECS)
  }

  toc()
  bind_rows(results)
}


# Run all states ---------------------------------------------------------
# Should take about 10 minutes

all_results <- vector("list", length(states_list))
names(all_results) <- states_list

for (st in states_list) {
  message(sprintf("\n=== %s ===", st))
  all_results[[st]] <- scrape_state(st)
  Sys.sleep(SLEEP_SECS)
}

farmcredit_data_all <- bind_rows(all_results) |> clean_names()

# ── Supplemental queries for known site/FIPS mismatches ──────────────────────
# CT: tigris uses new planning-region FIPS; farmcredit.com still uses old counties
ct_fix <- scrape_batch("CT", c("Fairfield", "Hartford", "Litchfield",
                                "Middlesex", "New Haven", "New London",
                                "Tolland", "Windham")) |> clean_names()

# SD: farmcredit.com still lists Shannon County (renamed Oglala Lakota in 2015)
sd_fix <- scrape_batch("SD", "Shannon") |> clean_names()


# Merge all and save -----------------------------------------------------
farmcredit_data_all <- bind_rows(farmcredit_data_all, ct_fix, sd_fix)
farmcredit_data <- farmcredit_data_all |>
  select(organization:office_type) |>
  distinct()

glimpse(farmcredit_data)

write_csv(farmcredit_data_all, "farmcredit_all.csv")
write_csv(farmcredit_data,     "farmcredit.csv")


# Geocode and map --------------------------------------------------------
# Pick up here (after changing the directory) if the scrape is already done 

farmcredit_data <- read_csv("posts/04_2026_farmcredit/farmcredit.csv") |>
  replace_na(list(office_type = 'Branch'))  # a few NA values...all branches

fc_addresses <- farmcredit_data |>
  mutate(
    city = str_remove(city_state_zipcode, ",\\s+[A-Z]{2}.*$"),
    st   = str_extract(city_state_zipcode, "[A-Z]{2}(?=\\s+\\d{5})"),
    zip  = str_extract(city_state_zipcode, "\\d{5}(-\\d{4})?$") |> str_sub(end = 5)
  )

# You will need an access token (API) from Mapbox
# https://docs.mapbox.com/help/dive-deeper/access-tokens/
fc_addresses_sf <- fc_addresses |> 
  mb_batch_geocode(
    address_line1 = "address",
    place = "city",
    region = "st",
    postcode = "zip"
  )

beepr::beep()


# HQ Addresses from FCA Public Directory ---------------------------------

FCA_BASE <- "https://apps.fca.gov/FCSPublicDirectory/"
UA <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36"

# Step 1: Get all institutions and their UNINUMs
inst_page <- read_html(GET(paste0(FCA_BASE, "PubViewInstitutionsBySysDist.aspx"),
  add_headers(`User-Agent` = UA)))

inst_links <- inst_page |>
  html_elements("a[href*='PubViewInst']") |>
  (\(x) tibble(
    fca_name = html_text(x, trim = TRUE),
    uninum   = str_extract(html_attr(x, "href"), "(?<=u=)\\d+")
  ))()

# Step 2: Scrape each detail page for HQ address
get_hq_address <- function(uninum) {
  Sys.sleep(0.75)
  pg <- tryCatch(
    read_html(GET(paste0(FCA_BASE, "PubViewInst.aspx?u=", uninum),
      add_headers(`User-Agent` = UA))),
    error = function(e) NULL
  )
  if (is.null(pg)) return(NA_character_)
  pg |>
    html_element("#ctl00_cphMainContent_fvInstitution_lblCharterAddress") |>
    html_text2() |>
    str_trim()
}

fca_hq <- inst_links |>
  mutate(hq_raw = map_chr(uninum, get_hq_address))

fca_hq_sep <- fca_hq |>
  mutate(
    hq_address       = str_extract(hq_raw, "^[^\n]+"),
    hq_city_st_zip   = str_extract(hq_raw, "[^\n]+$"),
    hq_city          = str_remove(hq_city_st_zip, ",\\s+[A-Z]{2}.*$"),
    hq_st            = str_extract(hq_city_st_zip, "[A-Z]{2}(?=\\s+\\d{5})"),
    hq_zip           = str_extract(hq_city_st_zip, "\\d{5}(-\\d{4})?$") |> str_sub(end = 5)
  ) |>
  select(-hq_raw, -hq_city_st_zip) |> 
  arrange(hq_st)

# Step 3: Fuzzy-join FCA names to farmcredit.com organization names
orgs <- farmcredit_data |> distinct(organization)

hq_matched <- stringdist_left_join(
  orgs, fca_hq_sep,
  by        = c("organization" = "fca_name"),
  method    = "jw",       # Jaro-Winkler works well for org name variants
  max_dist  = 0.2,
  distance_col = "dist"
) |>
  group_by(organization) |>
  slice_min(dist, n = 1, with_ties = FALSE) |>
  ungroup()

# Inspect matches -- fix any bad ones manually before joining to main data
hq_matched |> select(organization, fca_name, dist, hq_address) |> print(n = Inf)

# Fix four mistaken matches
hq_corrections <- fca_hq_sep |>
  filter(uninum %in% c("720131", "710454", "722313", "720899")) |>
  transmute(
    organization = case_match(uninum,
      "720131" ~ "ArborOne Farm Credit",
      "710454" ~ "AgTrust Farm Credit",
      "722313" ~ "Farm Credit of Western Arkansas",
      "720899" ~ "Ag Credit"
    ),
    hq_address_fix = hq_address, hq_city_fix = hq_city, fca_name_fix = fca_name,
    hq_st_fix = hq_st, hq_zip_fix = hq_zip, hq_uninum_fix = uninum
  )

hq_matched_final <- hq_matched |>
  left_join(hq_corrections, by = "organization") |>
  mutate(
    fca_name   = coalesce(fca_name_fix,   fca_name),
    uninum     = coalesce(hq_uninum_fix,  uninum),
    hq_address = coalesce(hq_address_fix, hq_address),
    hq_city    = coalesce(hq_city_fix,    hq_city),
    hq_st      = coalesce(hq_st_fix,      hq_st),
    hq_zip     = coalesce(hq_zip_fix,     hq_zip)
  ) |>
  select(-ends_with("_fix"), -dist)

glimpse(hq_matched_final)

hq_addresses_sf <- hq_matched_final |> 
  mb_batch_geocode(
    address_line1 = "hq_address",
    place = "hq_city",
    region = "hq_st",
    postcode = "hq_zip"
  )


# HQ rows from farmcredit.com are replaced with FCA-sourced addresses,
# which are more authoritative (official charter address per regulator)
all_locations_sf <- bind_rows(
  fc_addresses_sf |>
    filter(office_type != "Headquarters") |>
    select(organization, address, city, st, zip, office_type, geometry),
  hq_addresses_sf |>
    transmute(
      organization,
      address     = hq_address,
      city        = hq_city,
      st          = hq_st,
      zip         = hq_zip,
      office_type = "Headquarters",
      geometry
    )
)


sf::st_write(all_locations_sf, "posts/04_2026_farmcredit/farmcredit_locations.geojson", delete_dsn = TRUE)

# Also export as CSV with latitude and longitude columns
all_locations_sf |>
  mutate(
    longitude = sf::st_coordinates(geometry)[, 1],
    latitude  = sf::st_coordinates(geometry)[, 2]
  ) |>
  sf::st_drop_geometry() |>
  write_csv("posts/04_2026_farmcredit/farmcredit_locations.csv")


# Interactive Map ----------------------------------------------------------------
usa <- counties(cb = T, resolution = '20m') |>
  filter(STATEFP < 60) |>
  select(cty_fips = GEOID, st = STUSPS)
us <- usa |> group_by(st) |> summarise()
us48 <- filter(us, !st %in% c('AK','HI'))

all_locations_sft <- all_locations_sf |>
  mutate(
    type_color = case_when(
      office_type == "Headquarters" ~ "#e34a33",
      .default = "#5b8dd9"
    ),
    tooltip = glue::glue(
      "<b>{organization}</b>",
      "<hr style='margin:1px 0'>",
      "{address}<br>",
      "{city}, {st} {zip}<br>",
      "<i style='color:{type_color}'>{office_type}</i>"
    )
  )

maplibre(bounds = us48, style = carto_style('voyager')) |>
  add_line_layer(
    id = 'st_outlines',
    source = us,
    line_color = 'black',
    line_width = 0.4
  ) |>
  add_circle_layer(
    id = 'fca_locations',
    source = all_locations_sft,
    circle_color = match_expr(
      column = "office_type",
      values = c("Branch", "Headquarters"),
      stops  = c("#5b8dd9", "#e34a33")
    ),
    circle_radius = 5,
    circle_stroke_color = 'white',
    circle_stroke_width = 0.5,
    circle_opacity = 0.85,
    tooltip = "tooltip",
    popup = "tooltip"
  ) |>
  add_categorical_legend(
    legend_title = "Office Type",
    values = c("Branch", "Headquarters"),
    colors = c("#5b8dd9", "#e34a33"),
    layer_id = "fca_locations",
    position = "bottom-left",
    interactive = TRUE
  )
