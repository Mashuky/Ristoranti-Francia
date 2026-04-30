#!/usr/bin/env Rscript

Sys.setenv(LANG = "en_US.UTF-8", LC_ALL = "en_US.UTF-8")

project_lib <- file.path(getwd(), ".Rlibs")
dir.create(project_lib, showWarnings = FALSE, recursive = TRUE)
.libPaths(c(project_lib, .libPaths()))

suppressPackageStartupMessages({
  required_packages <- c("sf", "dplyr", "readr", "stringr", "ggplot2", "viridis")
  missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_packages) > 0) {
    install.packages(missing_packages, repos = "https://cloud.r-project.org", lib = project_lib)
  }

  invisible(lapply(required_packages, library, character.only = TRUE))
})

csv_url <- "https://raw.githubusercontent.com/holtzy/R-graph-gallery/master/DATA/data_on_french_states.csv"
geojson_url <- "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/communes.geojson"

data_dir <- "data"
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

csv_path <- file.path(data_dir, "ristoranti_francia.csv")
geojson_path <- file.path(data_dir, "communes_francia.geojson")
regioni_sud_geojson <- file.path(data_dir, "regioni_sud_francia.geojson")

download.file(csv_url, destfile = csv_path, mode = "wb")
download.file(geojson_url, destfile = geojson_path, mode = "wb")

cat("Download completato:\n")
cat("-", csv_path, "\n")
cat("-", geojson_path, "\n\n")

region_by_dep <- c(
  "16" = "Nouvelle-Aquitaine", "17" = "Nouvelle-Aquitaine", "19" = "Nouvelle-Aquitaine",
  "23" = "Nouvelle-Aquitaine", "24" = "Nouvelle-Aquitaine", "33" = "Nouvelle-Aquitaine",
  "40" = "Nouvelle-Aquitaine", "47" = "Nouvelle-Aquitaine", "64" = "Nouvelle-Aquitaine",
  "79" = "Nouvelle-Aquitaine", "86" = "Nouvelle-Aquitaine", "87" = "Nouvelle-Aquitaine",
  "09" = "Occitanie", "11" = "Occitanie", "12" = "Occitanie", "30" = "Occitanie",
  "31" = "Occitanie", "32" = "Occitanie", "34" = "Occitanie", "46" = "Occitanie",
  "48" = "Occitanie", "65" = "Occitanie", "66" = "Occitanie", "81" = "Occitanie",
  "82" = "Occitanie",
  "01" = "Auvergne-Rhone-Alpes", "03" = "Auvergne-Rhone-Alpes", "07" = "Auvergne-Rhone-Alpes",
  "15" = "Auvergne-Rhone-Alpes", "26" = "Auvergne-Rhone-Alpes", "38" = "Auvergne-Rhone-Alpes",
  "42" = "Auvergne-Rhone-Alpes", "43" = "Auvergne-Rhone-Alpes", "63" = "Auvergne-Rhone-Alpes",
  "69" = "Auvergne-Rhone-Alpes", "73" = "Auvergne-Rhone-Alpes", "74" = "Auvergne-Rhone-Alpes",
  "04" = "Provence-Alpes-Cote d'Azur", "05" = "Provence-Alpes-Cote d'Azur",
  "06" = "Provence-Alpes-Cote d'Azur", "13" = "Provence-Alpes-Cote d'Azur",
  "83" = "Provence-Alpes-Cote d'Azur", "84" = "Provence-Alpes-Cote d'Azur",
  "2A" = "Corse", "2B" = "Corse"
)

ristoranti_raw <- readr::read_delim(
  file = csv_path,
  delim = ";",
  quote = "\"",
  trim_ws = TRUE,
  col_names = FALSE,
  skip = 1,
  show_col_types = FALSE
)
names(ristoranti_raw) <- c("id", "reg", "dep", "depcom", "dciris", "an", "typequ", "nb_equip")

ristoranti <- ristoranti_raw %>%
  dplyr::filter(typequ == "A504") %>%
  dplyr::mutate(
    dep = stringr::str_pad(as.character(dep), width = 2, side = "left", pad = "0"),
    comune_code = stringr::str_sub(as.character(dciris), 1, 5),
    comune_code = stringr::str_pad(comune_code, width = 5, side = "left", pad = "0"),
    nb_equip = as.numeric(nb_equip)
  ) %>%
  dplyr::group_by(comune_code, dep) %>%
  dplyr::summarise(nb_ristoranti = sum(nb_equip, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(region = dplyr::recode(dep, !!!as.list(region_by_dep), .default = NA_character_)) %>%
  dplyr::filter(!is.na(region))

communes <- sf::st_read(geojson_path, quiet = TRUE) %>%
  dplyr::mutate(
    dep = substr(code, 1, 2),
    region = dplyr::recode(dep, !!!as.list(region_by_dep), .default = NA_character_)
  ) %>%
  dplyr::filter(!is.na(region))

regioni_sud <- communes %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop")

sf::st_write(regioni_sud, regioni_sud_geojson, delete_dsn = TRUE, quiet = TRUE)

communes_con_ristoranti <- communes %>%
  dplyr::left_join(ristoranti, by = c("code" = "comune_code")) %>%
  dplyr::mutate(nb_ristoranti = dplyr::coalesce(nb_ristoranti, 0))

centroidi <- sf::st_point_on_surface(communes_con_ristoranti) %>%
  dplyr::filter(nb_ristoranti > 0)

if (nrow(centroidi) == 0) {
  stop("Nessun punto valido per la densita`: verifica il parsing del CSV o il join con il GeoJSON.")
}

coord <- sf::st_coordinates(centroidi)
centroidi_df <- cbind(
  sf::st_drop_geometry(centroidi),
  x = coord[, "X"],
  y = coord[, "Y"]
)

mappa <- ggplot2::ggplot() +
  ggplot2::stat_bin_2d(
    data = centroidi_df,
    ggplot2::aes(x = x, y = y, weight = nb_ristoranti, fill = after_stat(count)),
    bins = 180,
    alpha = 0.8
  ) +
  ggplot2::geom_sf(data = regioni_sud, fill = NA, color = "grey25", linewidth = 0.35) +
  ggplot2::scale_fill_viridis_c(option = "C", direction = 1, name = "Densita`") +
  ggplot2::labs(
    title = "Densita` dei ristoranti nel Sud della Francia",
    subtitle = "CSV R-Graph-Gallery + confini derivati da communes.geojson",
    x = "Longitudine",
    y = "Latitudine"
  ) +
  ggplot2::theme_minimal(base_size = 12)

output_png <- "mappa_densita_ristoranti_sud.png"
ggplot2::ggsave(output_png, plot = mappa, width = 10, height = 8, dpi = 300)

cat("Creati:\n")
cat("-", regioni_sud_geojson, "\n")
cat("-", output_png, "\n")
