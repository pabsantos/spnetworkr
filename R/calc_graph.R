calc_avg_shortest_dist <- function(g) {
    l_i <- 1 / closeness(g, mode = "out", normalized = TRUE)
    l_i[is.nan(l_i)] <- NA
    return(l_i)
}

calc_directed_clustering <- function(g) {
    w <- as_adjacency_matrix(g)
    diag(w) <- 0
    w <- drop0(w)
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

calc_edge_betweenness <- function(g) {
    n <- vcount(g)
    eb <- edge_betweenness(g) / (n * (n - 1))
    return(eb)
}

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
    is_list <- vapply(st_drop_geometry(x), is.list, logical(1))
    for (col in names(is_list)[is_list]) {
        x[[col]] <- vapply(x[[col]], paste, character(1), collapse = "; ")
    }
    unlink(path)
    st_write(x, path, quiet = TRUE)
    return(path)
}
