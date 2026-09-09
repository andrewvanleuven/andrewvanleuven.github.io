# Builds the listing thumbnail for this post: a text-free version of the point
# map below. Kept as a standalone script rather than a chunk in index.qmd,
# because this post pins freeze: true and re-executing it rewrites a ~18 MB
# frozen result into git history.

library(tidyverse)
library(sf)
library(tigris)

options(tigris_use_cache = TRUE)

counties_sf <- st_read("golf_counties.geojson", quiet = TRUE)
points_sf   <- st_read("golf_points.geojson",   quiet = TRUE)

col_public  <- "#2e7d32"
col_private <- "#c1440e"

states <- counties_sf |>
  filter(!st %in% c("PR")) |>
  group_by(st) |>
  summarise(.groups = "drop") |>
  shift_geometry()

pts <- points_sf |>
  select(category) |>
  shift_geometry()

p <- ggplot() +
  geom_sf(data = states, fill = "grey96", color = "white", linewidth = 0.25) +
  geom_sf(data = pts, aes(color = category), size = 0.35, alpha = 0.6) +
  scale_color_manual(values = c(Public = col_public, Private = col_private)) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(0, 0, 0, 0),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("thumbnail.png", p, width = 8, height = 6, dpi = 150)
