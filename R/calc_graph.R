# library(igraph)
# library(sf)
# library(ggplot2)
# library(DirectedClustering)
#
# graph_path = "data-raw/sptoygraph.graphml"
# nodes_path = "data-raw/sptoygraph_nodes.geojson"
# edges_path = "data-raw/sptoygraph_edges.geojson"
#
# g = read_graph(graph_path, format = "graphml")
# nodes_sf = st_read(nodes_path)
# edges_sf = st_read(edges_path)
#
# e_ij = edge_betweenness(g)
#
# k_i = degree(g)

calc_avg_shortest_dist <- function(g) {
    # Inverse of normalized closeness = mean distance to reachable vertices,
    # without building the n x n distance matrix
    l_i <- 1 / closeness(g, mode = "out", normalized = TRUE)
    l_i[is.nan(l_i)] <- NA
    return(l_i)
}

#
# d = distances(g, mode = "out")
# diag(d) = NA
# d[is.infinite(d)] = NA
# l_i = rowMeans(d, na.rm = TRUE)
# l_i[is.nan(l_i)] = NA

# Sparse port of DirectedClustering::ClustF(type = "directed")$totalCC
# (Fagiolo 2007), avoiding the dense n x n adjacency matrix
calc_directed_clustering <- function(g) {
    w <- as_adjacency_matrix(g)
    diag(w) <- 0
    w <- drop0(w)
    a <- w
    a@x[] <- 1
    # As in ClustF, parallel edges make w weighted: normalize, then cube root
    w <- w / max(w)
    w@x <- w@x^(1 / 3)
    s <- w + t(w)
    # diag(s %*% s %*% s), using the symmetry of s
    num <- rowSums((s %*% s) * s)
    d_tot <- rowSums(a) + colSums(a)
    d_bil <- rowSums(a * t(a))
    c_i <- as.vector(num / (2 * (d_tot * (d_tot - 1) - 2 * d_bil)))
    c_i[is.na(c_i)] <- 0
    return(c_i)
}
#
# res = ClustF(as.matrix(as_adjacency_matrix(g)), type = "directed")
# c_i = res$totalCC |> as.vector()
#
# nodes_sf$k_i = k_i
# nodes_sf$l_i = l_i
# nodes_sf$c_i = c_i
#
# edges_sf$e_ij = e_ij
#
#
# ggplot() +
#     geom_sf(data = edges_sf, color = "grey80") +
#     geom_sf(data = nodes_sf, aes(color = c_i)) +
#     scale_color_viridis_c(transform = "log1p") +
#     theme_void()
#
# k_in = degree(g, mode = "in")
# k_out = degree(g, mode = "out")
#

generate_random_graph <- function(g) {
    g_random <- sample_degseq(
        out.deg = degree(g, mode = "out"),
        in.deg = degree(g, mode = "in"),
        method = "configuration"
    )
    return(g_random)
}

join_vertices_results <- function(v, k, l, c) {
    v$k <- k
    v$l <- l
    v$c <- c
    return(v)
}

join_edge_results <- function(e, eb) {
    e$eb <- eb
    return(e)
}

export_random_graph_params <- function(g_random) {
    c_i_mean <- mean(calc_directed_clustering(g_random), na.rm = TRUE)
    l_i_mean <- mean(calc_avg_shortest_dist(g_random), na.rm = TRUE)
    params <- c(l_i_mean, c_i_mean)
    names(params) <- c("<l_i>", "<c_i>")
    return(params)
}

export_geojson <- function(x, path) {
    # Multi-valued OSM attributes come as list columns, which GDAL can't write
    is_list <- vapply(st_drop_geometry(x), is.list, logical(1))
    for (col in names(is_list)[is_list]) {
        x[[col]] <- vapply(x[[col]], paste, character(1), collapse = "; ")
    }
    unlink(path)
    st_write(x, path, quiet = TRUE)
    return(path)
}
