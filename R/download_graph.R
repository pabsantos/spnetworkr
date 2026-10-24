#' Download the São Paulo drive network with OSMnx
#'
#' @param zones_path Path to the OD zones file.
#' @param test If TRUE, only the Sé and República districts (OD zones 1-6).
#' @param out_dir Output directory.
#' @return Paths to the graphml, nodes geojson and edges geojson.
download_graph <- function(zones_path, test = FALSE, out_dir = "data") {
    zones <- sf::st_read(zones_path, quiet = TRUE)
    zones <- zones[zones$cd_municipio == 36, ]
    if (test) {
        zones <- zones[zones$cd_original_zona_origem_destino %in% 1:6, ]
    }

    polygon_path <- tempfile(fileext = ".geojson")
    sf::st_write(sf::st_make_valid(zones), polygon_path, quiet = TRUE)

    prefix <- file.path(out_dir, if (test) "sp_graph_test" else "sp_graph")
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
