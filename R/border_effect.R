#' Edge betweenness restricted to SP -> SP shortest paths
#'
#' Counts, on the extended graph, only the shortest paths whose source and
#' target are both SP vertices (matched by OSM id), so the result covers the
#' same set of pairs as the edge betweenness of the SP graph. Uses the
#' internal `igraph:::edge_betweenness_subset_impl()`, the only igraph
#' function that accepts sets of sources and targets.
#'
#' @param g_nb igraph graph of SP + the neighboring zones.
#' @param ids OSM ids of the SP vertices (`V(g)$id`).
#' @return Raw edge betweenness, one value per edge of `g_nb`.
calc_sp_edge_betweenness <- function(g_nb, ids) {
    sp <- which(igraph::V(g_nb)$id %in% ids)
    igraph:::edge_betweenness_subset_impl(
        g_nb,
        directed = TRUE,
        sources = sp,
        targets = sp
    )
}

#' Compare SP edge betweenness with and without the neighboring zones
#'
#' Matches the edges of the São Paulo graph to the edges of the graph that
#' also covers the first-order neighbor OD zones by their OSM ids
#' (`u`, `v`, `key`). Both betweenness vectors are raw counts over the same
#' SP -> SP pairs, so they share a scale with no normalization: any
#' difference comes from shortest paths that leave SP and come back.
#'
#' @param e sf data frame of the SP edges.
#' @param eb Normalized edge betweenness of the SP graph (per row of `e`).
#' @param n Number of vertices of the SP graph.
#' @param e_nb sf data frame of the extended graph edges.
#' @param eb_nb SP -> SP edge betweenness of the extended graph (per row of
#'   `e_nb`), from `calc_sp_edge_betweenness()`.
#' @return List with `stats` (one-row data frame: edge counts, Spearman
#'   correlation, Wilcoxon rank-sum W, z, p-value and effect size r, medians
#'   and means, share of edges whose betweenness increased) and `pairs`
#'   (row index in `e` and the two raw betweenness values per matched edge).
compare_eb_border <- function(e, eb, n, e_nb, eb_nb) {
    id <- paste(e$u, e$v, e$key)
    id_nb <- paste(e_nb$u, e_nb$v, e_nb$key)
    j <- match(id, id_nb)
    i <- which(!is.na(j))
    pairs <- data.frame(
        i = i,
        eb_sp = eb[i] * n * (n - 1),
        eb_nb = eb_nb[j[i]]
    )

    x <- pairs$eb_sp
    y <- pairs$eb_nb
    test <- stats::wilcox.test(x, y, exact = FALSE)
    # z from the normal approximation with tie correction; the p-value
    # underflows to 0 at this sample size, so it can't be inverted.
    # as double: n1 * n2 overflows R integers for the full city
    n1 <- as.numeric(length(x))
    n2 <- as.numeric(length(y))
    big_n <- n1 + n2
    ties <- table(c(x, y))
    sigma <- sqrt(
        n1 *
            n2 /
            12 *
            ((big_n + 1) - sum(ties^3 - ties) / (big_n * (big_n - 1)))
    )
    z <- (test$statistic - n1 * n2 / 2) / sigma

    stats <- data.frame(
        n_edges = nrow(e),
        n_matched = length(i),
        spearman = stats::cor(x, y, method = "spearman"),
        wilcox_W = unname(test$statistic),
        wilcox_z = unname(z),
        wilcox_p = test$p.value,
        wilcox_r = unname(abs(z) / sqrt(big_n)),
        median_sp = stats::median(x),
        median_nb = stats::median(y),
        mean_sp = mean(x),
        mean_nb = mean(y),
        share_increased = mean(y > x)
    )
    list(stats = stats, pairs = pairs)
}

#' Map SP edge betweenness with and without the neighboring zones
#'
#' Two panels on one shared log color scale (single-hue blue ramp, line width
#' as a redundant channel), showing only the SP edges. An inset with the
#' log-binned distributions of both stands in for the legend.
#'
#' @param e sf data frame of the SP edges.
#' @param pairs `pairs` element of `compare_eb_border()`.
#' @param zones_path Path to the OD zones file (for the city outline).
#' @param test If TRUE, the outline is OD zones 1-6 only.
#' @param path Output PNG path.
#' @return `path`.
plot_eb_border_maps <- function(e, pairs, zones_path, test, path) {
    utm23s <- 31983
    font <- "Helvetica"
    fig_w_cm <- 16
    ramp <- c("#b7d3f6", "#6da7ec", "#2a78d6", "#184f95", "#0d366b")
    ink <- "grey15"
    ink_2 <- "grey30"
    labels <- c(
        eb_sp = "São Paulo only",
        eb_nb = "São Paulo + neighboring zones"
    )

    zones <- sf::st_read(zones_path, quiet = TRUE)
    in_area <- zones$cd_municipio == 36
    if (test) {
        in_area <- in_area & zones$cd_original_zona_origem_destino %in% 1:6
    }
    limits <- sf::st_transform(
        sf::st_union(sf::st_make_valid(zones[in_area, ])),
        utm23s
    )

    geom <- sf::st_geometry(e)[pairs$i]
    geom <- sf::st_transform(geom, utm23s)
    # 20 m is sub-pixel at this scale but cuts the vertex count sharply
    geom <- sf::st_simplify(geom, dTolerance = 20, preserveTopology = FALSE)

    # Shared scale: the floor is the 6th percentile of the positive values
    # of both graphs, so a color means the same in both panels.
    pooled <- c(pairs$eb_sp, pairs$eb_nb)
    eb_floor <- stats::quantile(pooled[pooled > 0], 0.06, names = FALSE)
    eb_max <- max(pooled)
    lims <- c(eb_floor, eb_max)

    bb <- sf::st_bbox(limits)
    ratio <- unname((bb["ymax"] - bb["ymin"]) / (bb["xmax"] - bb["xmin"]))

    map_panel <- function(col) {
        d <- sf::st_sf(eb = pmax(pairs[[col]], eb_floor), geometry = geom)
        # high-betweenness edges paint last, on top of the mesh
        d <- d[order(d$eb), ]
        ggplot2::ggplot(d) +
            ggplot2::geom_sf(
                data = limits,
                fill = NA,
                colour = "grey70",
                linewidth = 0.2
            ) +
            ggplot2::geom_sf(
                ggplot2::aes(colour = eb, linewidth = eb),
                lineend = "round"
            ) +
            ggplot2::scale_colour_gradientn(
                colours = ramp,
                transform = "log10",
                limits = lims,
                oob = scales::squish,
                guide = "none"
            ) +
            ggplot2::scale_linewidth_continuous(
                transform = "log10",
                limits = lims,
                range = c(0.04, 0.45),
                guide = "none"
            ) +
            ggplot2::coord_sf(expand = FALSE) +
            ggplot2::labs(title = labels[[col]]) +
            ggplot2::theme_void(base_size = 8, base_family = font) +
            ggplot2::theme(
                plot.title = ggplot2::element_text(
                    colour = ink,
                    face = "bold",
                    size = 8,
                    hjust = 0,
                    margin = ggplot2::margin(0, 0, 4, 0)
                ),
                plot.background = ggplot2::element_rect(
                    fill = "white",
                    colour = NA
                ),
                plot.margin = ggplot2::margin(2, 6, 2, 2)
            )
    }

    # Log binning in quarter decades; zero-betweenness edges cannot sit on a
    # log axis but stay in the denominator (share of all matched edges).
    pos <- pooled[pooled > 0]
    brk <- seq(
        floor(log10(min(pos)) * 4) / 4,
        ceiling(log10(max(pos)) * 4) / 4,
        by = 0.25
    )
    bins <- function(x) {
        cnt <- as.integer(table(cut(
            log10(x[x > 0]),
            brk,
            right = FALSE,
            include.lowest = TRUE
        )))
        data.frame(
            mid = 10^(utils::head(brk, -1) + 0.125),
            lo = 10^utils::head(brk, -1),
            hi = 10^utils::tail(brk, -1),
            pct = 100 * cnt / length(x)
        )
    }
    b_sp <- bins(pairs$eb_sp)
    b_sp <- b_sp[b_sp$pct > 0, ]
    b_nb <- bins(pairs$eb_nb)
    b_nb <- b_nb[b_nb$pct > 0, ]
    # step outline of the extended graph's histogram
    step_nb <- data.frame(
        x = as.vector(rbind(b_nb$lo, b_nb$hi)),
        pct = rep(b_nb$pct, each = 2)
    )
    base <- min(b_sp$pct, b_nb$pct) * 0.6
    x_breaks <- 10^seq(ceiling(log10(min(pos))), floor(log10(max(pos))), 2)

    hist <- ggplot2::ggplot() +
        ggplot2::geom_rect(
            data = b_sp,
            ggplot2::aes(
                xmin = lo,
                xmax = hi,
                ymin = base,
                ymax = pct,
                fill = mid
            )
        ) +
        ggplot2::geom_step(
            data = step_nb,
            ggplot2::aes(x, pct),
            colour = "grey20",
            linewidth = 0.35,
            direction = "hv"
        ) +
        ggplot2::scale_fill_gradientn(
            colours = ramp,
            transform = "log10",
            limits = lims,
            oob = scales::squish,
            guide = "none"
        ) +
        ggplot2::scale_x_log10(
            name = "Edge betweenness (shortest paths)",
            breaks = x_breaks,
            labels = scales::label_math(10^.x, format = log10)
        ) +
        ggplot2::scale_y_log10(
            name = "% of edges",
            breaks = 10^(-3:1),
            labels = c("0.001", "0.01", "0.1", "1", "10")
        ) +
        ggplot2::theme_classic(base_size = 7, base_family = font) +
        ggplot2::theme(
            axis.line = ggplot2::element_line(
                colour = ink_2,
                linewidth = 0.3
            ),
            axis.ticks = ggplot2::element_blank(),
            axis.title = ggplot2::element_text(size = 7, colour = ink),
            axis.text = ggplot2::element_text(size = 6, colour = ink_2),
            plot.background = ggplot2::element_rect(
                fill = "white",
                colour = NA
            ),
            plot.margin = ggplot2::margin(6, 8, 4, 4)
        )

    # Key for the two series: the colored bars and the outline. Text stays in
    # neutral ink; the marks carry identity.
    x_rng <- range(log10(c(b_sp$lo, b_sp$hi, b_nb$lo, b_nb$hi)))
    # The key sits in headroom above the tallest bar so it never covers data
    y_top <- log10(max(b_sp$pct, b_nb$pct)) + 1.5
    kx <- 10^(x_rng[1] + c(0, 0.06) * diff(x_rng))
    ky <- 10^(y_top - c(0, 0.7))
    hist <- hist +
        ggplot2::expand_limits(y = 10^(y_top + 0.2)) +
        ggplot2::annotate(
            "rect",
            xmin = kx[1],
            xmax = kx[2],
            ymin = ky / 10^0.12,
            ymax = ky * 10^0.12,
            fill = c(ramp[3], NA),
            colour = c(NA, "grey20"),
            linewidth = 0.35
        ) +
        ggplot2::annotate(
            "text",
            x = kx[2] * 10^(0.03 * diff(x_rng)),
            y = ky,
            label = c("São Paulo only", "São Paulo + neighbors"),
            hjust = 0,
            size = 6 / ggplot2::.pt,
            family = font,
            colour = ink
        )

    p_sp <- map_panel("eb_sp")
    p_nb <- map_panel("eb_nb") +
        ggspatial::annotation_scale(
            location = "br",
            width_hint = 0.25,
            height = grid::unit(0.1, "cm"),
            line_width = 0.4,
            bar_cols = c("grey25", "white"),
            line_col = "grey25",
            text_cex = 0.55,
            text_col = "grey25",
            text_family = font,
            pad_x = grid::unit(0.2, "cm"),
            pad_y = grid::unit(0.2, "cm")
        ) +
        ggspatial::annotation_north_arrow(
            location = "tr",
            height = grid::unit(0.6, "cm"),
            width = grid::unit(0.6, "cm"),
            pad_x = grid::unit(0.2, "cm"),
            pad_y = grid::unit(0.2, "cm"),
            style = ggspatial::north_arrow_minimal(
                line_width = 0.8,
                line_col = "grey25",
                fill = "grey25",
                text_size = 6,
                text_col = "grey25",
                text_family = font
            )
        )
    # The southeast quadrant of the municipality is empty, so the
    # distribution rides inside the first map instead of costing a panel.
    p_sp <- p_sp +
        patchwork::inset_element(
            hist,
            left = 0.45,
            bottom = 0.08,
            right = 1,
            top = 0.42,
            align_to = "panel"
        )

    p <- patchwork::wrap_plots(p_sp, p_nb, nrow = 1)

    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
    ragg::agg_png(
        path,
        width = fig_w_cm,
        height = fig_w_cm / 2 * ratio + 0.6,
        units = "cm",
        res = 300,
        background = "white"
    )
    print(p)
    grDevices::dev.off()
    return(path)
}
