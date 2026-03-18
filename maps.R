# Script to generate maps demonstrating differences in geographies.
# Compare two different English geographies visually and how they overlap.

library(sf)
library(patchwork)
library(ggplot2)

output_dir <- fs::dir_create(here::here("methods", "leading-indicator", "output"))


set.seed(07734)

ggplot2::theme_set(ggplot2::theme_void())

# turn off spherical geometry for centroid calculation & unions
sf::sf_use_s2(FALSE)


# define location of ONS data for loading in
lad24_uk_buc_url <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local_Authority_Districts_December_2024_Boundaries_UK_BUC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"
nhser24_en_buc_url <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/NHS_England_Regions_January_2024_EN_BSC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"

lad24_uk_buc <- sf::st_read(lad24_uk_buc_url) |>
  # Tidy column names
  janitor::clean_names()

# bring in scotland and wales for context
country <- lad24_uk_buc |>
  dplyr::filter(stringr::str_starts(lad24cd, "W") | stringr::str_starts(lad24cd, "S")) |>
  dplyr::collect() |>
  dplyr::mutate(
    name = dplyr::case_when(
      stringr::str_starts(lad24cd, "W") ~ "Wales",
      stringr::str_starts(lad24cd, "S") ~ "Scotland",
      TRUE ~ "error"
    )
  ) |>
  dplyr::group_by(name) |>
  # merge the local authorities within scotland and wales
  dplyr::summarize(geometry = sf::st_union(geometry)) |>
  dplyr::mutate(type = "country") |>
  sf::st_sf(crs = "epsg:4326") |>
  sf::st_make_valid()

# load regions polygons
regions <- sf::st_read(nhser24_en_buc_url) |>
  # Tidy column names
  janitor::clean_names() |>
  #sf::st_as_sf() |>
  dplyr::mutate(type = "region") |>
  sf::st_set_crs(sf::st_crs(country)) |>
  sf::st_make_valid() |>
  # we are later joining based off order
  dplyr::arrange(fid)


# bring in the English local authorities
local_authorities <- lad24_uk_buc |>
  dplyr::filter(stringr::str_starts(lad24cd, "E")) |>
  dplyr::collect() |>
  dplyr::mutate(type = "Local authorities") |>
  sf::st_sf(crs = "epsg:4326") |>
  sf::st_make_valid() |>
  # lets find the center of each LA to see which region it's in.
  # We need to do point on surface not centroid because not all LA's are
  # convex, so our "centres" may be not in the LA otherwise.
  dplyr::mutate(
    centroid = sf::st_point_on_surface(geometry),
    # note the IDs we get are based on order of `regions` which
    # means we need to order the dataframe to match.
    # Check which centroid is in each region
    region_id = as.integer(sf::st_within(centroid, regions))
  ) |>
  dplyr::left_join(
    regions |>
      dplyr::select(
        "region_id" = fid,
        "nhser24nm"
      ) |>
      sf::st_drop_geometry(),
    by = c("region_id")
  ) |>
  dplyr::mutate(nhser24nm = dplyr::if_else(lad24nm == "Isle of Wight", "South East", nhser24nm))


combined <- dplyr::bind_rows(
  country,
  regions,
  local_authorities
)

combined_plot <- country |>
  ggplot() +
  geom_sf(
    data = combined |> dplyr::filter(type == "country"),
    aes(geometry = geometry),
    color = "grey10",
    fill = "grey95"
  ) +
  geom_sf(
    data = combined |> dplyr::filter(type == "Local authorities"),
    aes(geometry = geometry, linewidth = "Administrative area", fill = nhser24nm),
    color = "grey10",
    alpha = 0.7
  ) +
  geom_sf(
    data = combined |> dplyr::filter(type == "region"),
    aes(geometry = geometry, linewidth = "Health region"),
    color = "grey10",
    linetype = 1,
    fill = NA
  ) +
  scale_linewidth_manual(name = NULL, values = c("Health region" = 1, "Administrative area" = 0.2)) +
  ggplot2::coord_sf(crs = "epsg:27700", ylim = c(0, 700000), xlim = c(100000, 700000)) +
  scale_fill_brewer(palette = "Paired") +
  theme(plot.background = element_rect(fill = "white", colour = NA), legend.position = "bottom") +
  guides(linewidth = guide_legend(order = 2), shape = guide_legend(order = 1), fill = "none") +
  labs(title = stringr::str_wrap("The administrative areas do not all nest within health regions.", width = 50))

combined_plot

ggplot2::ggsave(
  filename = fs::path(output_dir, "map.png"),
  plot = combined_plot,
  width = 8,
  height = 8
)
