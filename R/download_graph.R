#' Download the São Paulo drive network with OSMnx
#'
#' @param zones_path Path to the OD zones file.
#' @param test If TRUE, only the Sé and República districts (OD zones 1-6).
#' @param neighbors If TRUE, also include the OD zones outside the study area
#'   that touch it (first-order neighbors).
#' @param out_dir Output directory.
#' @return Paths to the graphml, nodes geojson and edges geojson.
download_graph <- function(
    zones_path,
    test = FALSE,
    neighbors = FALSE,
    out_dir = "data"
) {
    all_zones <- sf::st_make_valid(sf::st_read(zones_path, quiet = TRUE))
    in_area <- all_zones$cd_municipio == 36
    if (test) {
        in_area <- in_area &
            all_zones$cd_original_zona_origem_destino %in% 1:6
    }
    zones <- all_zones[in_area, ]
    if (neighbors) {
        area <- sf::st_union(zones)
        touches <- lengths(sf::st_intersects(all_zones, area)) > 0
        zones <- all_zones[in_area | touches, ]
    }

    polygon_path <- tempfile(fileext = ".geojson")
    sf::st_write(zones, polygon_path, quiet = TRUE)

    name <- paste0(
        if (test) "sp_graph_test" else "sp_graph",
        if (neighbors) "_neighbors" else ""
    )
    prefix <- file.path(out_dir, name)
    paths <- paste0(prefix, c(".graphml", "_nodes.geojson", "_edges.geojson"))
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

    status <- system2(
        "uv",
        c("run", "py/download_graph.py", polygon_path, paths)
    )
    if (status != 0) {
        stop("py/download_graph.py failed")
    }

    paths
}
