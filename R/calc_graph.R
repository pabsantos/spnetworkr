#' Average shortest path distance from each vertex
#'
#' Inverse of the normalized out-closeness, i.e. the mean shortest path
#' length from each vertex to all vertices it can reach.
#'
#' @param g An igraph graph.
#' @return Numeric vector with one value per vertex; NA for vertices that
#'   reach no other vertex.
calc_avg_shortest_dist <- function(g) {
    l_i <- 1 / igraph::closeness(g, mode = "out", normalized = TRUE)
    l_i[is.nan(l_i)] <- NA
    return(l_i)
}

#' Weighted directed clustering coefficient
#'
#' Total-cycle clustering coefficient for weighted directed graphs (Fagiolo,
#' 2007). Weights are the edge multiplicities from the adjacency matrix,
#' scaled by their maximum; self-loops are ignored.
#'
#' @param g An igraph graph.
#' @return Numeric vector with one value per vertex; 0 where undefined.
calc_directed_clustering <- function(g) {
    w <- igraph::as_adjacency_matrix(g)
    diag(w) <- 0
    w <- Matrix::drop0(w)
    a <- w
    a@x[] <- 1
    w <- w / max(w)
    w@x <- w@x^(1 / 3)
    s <- w + t(w)
    num <- rowSums((s %*% s) * s)
    d_tot <- rowSums(a) + colSums(a)
    d_bil <- rowSums(a * t(a))
    c_i <- as.vector(num / (2 * (d_tot * (d_tot - 1) - 2 * d_bil)))
    c_i[is.na(c_i)] <- 0
    return(c_i)
}

#' Normalized edge betweenness
#'
#' @param g An igraph graph.
#' @return Numeric vector with one value per edge, divided by n(n - 1).
calc_edge_betweenness <- function(g) {
    n <- igraph::vcount(g)
    eb <- igraph::edge_betweenness(g) / (n * (n - 1))
    return(eb)
}

#' Random graph with the same degree sequence
#'
#' @param g An igraph graph.
#' @return A random igraph graph preserving the in- and out-degrees of `g`
#'   (configuration model).
generate_random_graph <- function(g) {
    g_random <- igraph::sample_degseq(
        out.deg = igraph::degree(g, mode = "out"),
        in.deg = igraph::degree(g, mode = "in"),
        method = "configuration"
    )
    return(g_random)
}

#' Add vertex metrics to the nodes table
#'
#' @param v sf data frame of vertices.
#' @param k Degree per vertex.
#' @param l Average shortest path distance per vertex.
#' @param c Clustering coefficient per vertex.
#' @return `v` with columns `k`, `l` and `c`.
join_vertices_results <- function(v, k, l, c) {
    v$k <- k
    v$l <- l
    v$c <- c
    return(v)
}

#' Add edge metrics to the edges table
#'
#' @param e sf data frame of edges.
#' @param eb Edge betweenness per edge.
#' @return `e` with column `eb`.
join_edge_results <- function(e, eb) {
    e$eb <- eb
    return(e)
}

#' Mean metrics of a random graph
#'
#' @param g_random An igraph graph.
#' @return Named numeric vector with the mean average shortest path distance
#'   (`<l_i>`) and the mean clustering coefficient (`<c_i>`).
export_random_graph_params <- function(g_random) {
    c_i_mean <- mean(calc_directed_clustering(g_random), na.rm = TRUE)
    l_i_mean <- mean(calc_avg_shortest_dist(g_random), na.rm = TRUE)
    params <- c(l_i_mean, c_i_mean)
    names(params) <- c("<l_i>", "<c_i>")
    return(params)
}

#' Write an sf object to GeoJSON
#'
#' List columns are collapsed into "; "-separated strings. Any existing file
#' at `path` is overwritten.
#'
#' @param x sf data frame.
#' @param path Output file path.
#' @return `path`.
export_geojson <- function(x, path) {
    is_list <- vapply(sf::st_drop_geometry(x), is.list, logical(1))
    for (col in names(is_list)[is_list]) {
        x[[col]] <- vapply(x[[col]], paste, character(1), collapse = "; ")
    }
    unlink(path)
    sf::st_write(x, path, quiet = TRUE)
    return(path)
}
