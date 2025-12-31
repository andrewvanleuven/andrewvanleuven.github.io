yts_checker <- function(ind_vctr = NULL, yr_vctr = NULL, st_vctr = NULL,
                        twn_vctr = NULL, biz_vctr = NULL, 
                        match_type = c("detect", "exact"),
                        max_rows = 25e6, spatial = FALSE, show_map = FALSE,
                        column = NULL) {
  # If show_map is TRUE, spatial must also be TRUE
  if (show_map) {
    spatial <- TRUE
  }
  # Load packages quietly
  suppressPackageStartupMessages({
    require(arrow, quietly = TRUE, warn.conflicts = FALSE)
    require(tidyverse, quietly = TRUE, warn.conflicts = FALSE)
    if (spatial) require(sf, quietly = TRUE, warn.conflicts = FALSE)
    if (show_map) require(mapgl, quietly = TRUE, warn.conflicts = FALSE)
  })
  
  match_type <- match.arg(match_type)
  
  # Open dataset
  ytsdb <- open_dataset('/Users/andrew/Library/CloudStorage/GoogleDrive-vanleuven.andrew@gmail.com/My Drive/research_files/ytsR/data/yts')
  
  # Start with an empty list of filters
  filter_expr <- list()
  
  # Add NAICS filter if provided
  if (!is.null(ind_vctr) && length(ind_vctr) > 0) {
    naics_pattern <- paste0("^", ind_vctr, collapse = "|")
    naics_filter <- expr(str_detect(as.character(naics), !!naics_pattern))
    filter_expr <- c(filter_expr, naics_filter)
  }
  
  # Add year filter if provided
  if (!is.null(yr_vctr) && length(yr_vctr) > 0) {
    yr_int <- as.integer(yr_vctr)
    year_filter <- expr(year %in% !!yr_int)
    filter_expr <- c(filter_expr, year_filter)
  }
  
  # Add state filter if provided
  if (!is.null(st_vctr) && length(st_vctr) > 0) {
    state_filter <- expr(state %in% !!st_vctr)
    filter_expr <- c(filter_expr, state_filter)
  }
  
  # Add city filter if provided
  if (!is.null(twn_vctr) && length(twn_vctr) > 0) {
    city_exact <- toupper(trimws(twn_vctr))
    city_filter <- expr(city %in% !!city_exact)
    filter_expr <- c(filter_expr, city_filter)
  }
  
  # Add company filter if provided
  if (!is.null(biz_vctr) && length(biz_vctr) > 0) {
    if (match_type == "detect") {
      biz_pattern <- paste0(toupper(biz_vctr), collapse = "|")
      company_filter <- expr(str_detect(company, !!biz_pattern))
      filter_expr <- c(filter_expr, company_filter)
    } else if (match_type == "exact") {
      biz_exact <- toupper(trimws(biz_vctr))
      company_filter <- expr(company %in% !!biz_exact)
      filter_expr <- c(filter_expr, company_filter)
    }
  }
  
  # --- Start timing here (everything from now on) ---
  start_time <- Sys.time()
  
  # Count rows without pulling data
  if (length(filter_expr) == 0) {
    n_rows <- ytsdb |> count() |> pull(n, as_vector = TRUE)
  } else {
    n_rows <- ytsdb |> filter(!!!filter_expr) |> count() |> pull(n, as_vector = TRUE)
  }
  
  # Check size and warn if too big
  if (n_rows > max_rows) {
    message(
      paste0(
        "⚠️ Query would return ",
        format(n_rows, big.mark = ","),
        " rows, which exceeds the limit of ",
        format(max_rows, big.mark = ","),
        ".\nPlease add more filters (e.g., states/years) and try again."
      )
    )
    return(invisible(NULL))
  }
  
  # Now collect the data
  if (length(filter_expr) == 0) {
    yts_query <- ytsdb |> collect()
  } else {
    yts_query <- ytsdb |> filter(!!!filter_expr) |> collect()
  }
  
  # If spatial = TRUE, convert to sf and keep latest year per id
  if (spatial) {
    yts_query <- yts_query |>
      mutate(longitude = ifelse(longitude > 0, longitude * -1, longitude)) |>
      sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
      sf::st_transform(6350) |>
      filter(year == max(year), .by = id)
  }
  
  # If show_map = TRUE, show a map (only if spatial = TRUE)
  if (show_map) {
    if (!spatial) {
      warning("show_map = TRUE but spatial = FALSE; cannot map non-spatial data.")
    } else {
      # Check row count
      if (nrow(yts_query) > 50000) {
        message(
          paste0(
            "Too many data points (",
            format(nrow(yts_query), big.mark = ","),
            " rows), mapping not performed."
          )
        )
      } else {
        # Load mapgl quietly
        suppressPackageStartupMessages({
          require(mapgl, quietly = TRUE, warn.conflicts = FALSE)
        })
        
        # Build maplibre_view call
        map_args <- list(
          yts_query,
          style = carto_style('positron'),
          n = 6,
          color = '#007155'
        )
        if (!is.null(column) && column %in% names(yts_query)) {
          map_args$column <- column
        }
        
        # Show the map and force it to print in the viewer
        map_obj <- do.call(maplibre_view, map_args)
        print(map_obj)
        invisible(map_obj)  # so the function still returns yts_query, not the map
      }
    }
  }
  
  # --- End timing here ---
  elapsed <- difftime(Sys.time(), start_time, units = "secs")
  elapsed_sec <- as.numeric(elapsed)
  
  # Build a nice summary message
  states_used <- if (is.null(st_vctr) || length(st_vctr) == 0) "All" else sort(unique(st_vctr))
  years_used  <- if (is.null(yr_vctr) || length(yr_vctr) == 0) "All" else sort(unique(yr_vctr))
  cities_used <- if (is.null(twn_vctr) || length(twn_vctr) == 0) "All" else sort(unique(twn_vctr))
  biz_used    <- if (is.null(biz_vctr) || length(biz_vctr) == 0) "All" else sort(unique(biz_vctr))
  naics_used  <- if (is.null(ind_vctr) || length(ind_vctr) == 0) "All" else sort(unique(ind_vctr))
  
  # Format lists nicely (e.g., "VT, ME, and NH")
  format_list <- function(x) {
    if (length(x) == 0) return("All")
    if (length(x) == 1) return(x)
    if (length(x) == 2) return(paste(x, collapse = " and "))
    paste0(paste(x[-length(x)], collapse = ", "), ", and ", x[length(x)])
  }
  
  # Print clean success message with time
  data_emoji <- if (spatial) "🌎" else "📊"
  
  message(paste0(
    "Pulled ", format(nrow(yts_query), big.mark = ","), " Rows of Data ", data_emoji, "\n",
    "📅 Years: ", format_list(years_used), "\n",
    "🗺️ States: ", format_list(states_used), "\n",
    "🌆 Cities: ", format_list(cities_used), "\n",
    "🏭 NAICS: ", format_list(naics_used), "\n",
    "🏬 Establishments: ", format_list(biz_used), "\n",
    "──────────────\n",
    "✔ Query complete in ", round(elapsed_sec, 1), " sec ⏱︎"
  ))
  
  
  
  yts_query |> 
    select(1:4, state, everything(), -State) |> 
    arrange(company)
}

# ################################################################################
# #                                                                              #
# #                yts_checker() - YourEconomy Time Series Query Helper          #
# #                                                                              #
# ################################################################################
# OVERVIEW
# ========
# A streamlined function for querying the YourEconomy Time Series database 
# (parquet format) with built-in filtering, spatial analysis, and interactive 
# mapping capabilities.
# 
# 
# ARGUMENTS
# =========
# 
# Filter Parameters
# -----------------
#   ind_vctr    → NAICS code(s) - uses prefix matching (e.g., "44" matches all retail)
#   yr_vctr     → Year(s) to include
#   st_vctr     → State abbreviation(s) (e.g., "VT", "ME")
#   twn_vctr    → City name(s) - case-insensitive, auto-trimmed
#   biz_vctr    → Company name(s) to search for
# 
# Search Options
# --------------
#   match_type  → "detect" (pattern match) or "exact" (precise match) for companies
#   max_rows    → Safety limit to prevent massive queries (default: 25 million)
# 
# Spatial Options
# ---------------
#   spatial     → Convert results to sf object with EPSG:6350 projection
#                 Filters to latest year per establishment when TRUE
#   show_map    → Display interactive maplibre map (auto-enables spatial)
#   column      → Variable to color-code map points (optional)
# 
# 
# BEHAVIOR
# ========
# - Efficient querying via Arrow - counts rows before pulling data
# - Auto-blocks queries exceeding max_rows with helpful guidance
# - Corrects positive longitudes to negative (Western Hemisphere)
# - Maps limited to 50,000 points to prevent browser crashes
# - Returns data sorted by company name with key columns prioritized
# - Prints execution time and query summary
# 
# 
# OUTPUT STRUCTURE
# ================
# Returns tibble (or sf object if spatial=TRUE) with:
# 
#   → Establishment details: id, company, address, city, state, zip
#   → Employment: jobs count
#   → Classification: naics, sic codes
#   → Spatial: latitude, longitude (corrected), fips codes
#   → Temporal: year, first_year, last_year
#   → Owner demographics: gender, minority status, veteran status
# 
# 
# EXAMPLE USAGE
# =============
# 
# df <- yts_checker(
#   ind_vctr = c("44", "45"),                  # Retail trade sectors
#   yr_vctr = c(2020, 2021, 2022),             # Multiple years
#   st_vctr = c("VT", "NH", "ME"),             # New England states
#   twn_vctr = c("Burlington", "Montpelier"),  # Specific cities
#   biz_vctr = c("WALMART", "TARGET"),         # Company names to search for
#   match_type = "detect",                     # Use pattern matching for companies
#   max_rows = 50000,                          # Limit result size
#   spatial = TRUE,                            # Convert to sf object
#   show_map = TRUE,                           # Display interactive map
#   column = "jobs"                            # Color map by jobs column
# )
# 
# 
# TIPS & TRICKS
# =============
# - Use broader NAICS codes first (e.g., "44" for all retail), then narrow
# - Combine state + year filters to manage query size
# - "detect" mode for fuzzy company matching, "exact" for precise lookups
# - Enable spatial=TRUE for geographic analysis without mapping overhead
# - Check the summary output to verify your filters worked as expected
# 
# ###############################################################################
