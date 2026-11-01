#' Edge betweenness estimated from a sample of source vertices
#'
#' Brandes & Pich (2007) pivot estimator: shortest paths are counted only
#' from `round(frac * n)` random source vertices and the result is scaled by
#' `n / k`. Normalized by n(n - 1), like `calc_edge_betweenness()`. Uses the
#' internal `igraph:::edge_betweenness_subset_impl()`, the only igraph
#' function that accepts a set of sources.
#'
#' @param g An igraph graph.
#' @param frac Fraction of vertices to use as sources, in (0, 1].
#' @return Numeric vector with one value per edge.
calc_sampled_edge_betweenness <- function(g, frac) {
    n <- igraph::vcount(g)
    k <- round(frac * n)
    sources <- sample(n, k)
    eb <- igraph:::edge_betweenness_subset_impl(
        g,
        directed = TRUE,
        sources = sources
    )
    eb <- eb * n / k / (n * (n - 1))
    return(eb)
}

#' Compare a sampled edge betweenness with the exact one
#'
#' @param g An igraph graph.
#' @param eb Exact normalized edge betweenness of `g`.
#' @param frac Fraction of vertices to use as sources.
#' @param rep Repetition number (only recorded).
#' @return One-row data frame with `frac`, `rep`, the elapsed time of the
#'   sampled calculation in seconds (`secs`), and the Spearman correlation,
#'   RMSE and MAE against `eb`.
eval_sampled_edge_betweenness <- function(g, eb, frac, rep) {
    secs <- system.time(
        eb_hat <- calc_sampled_edge_betweenness(g, frac)
    )[["elapsed"]]
    err <- eb_hat - eb
    data.frame(
        frac = frac,
        rep = rep,
        secs = secs,
        spearman = stats::cor(eb_hat, eb, method = "spearman"),
        rmse = sqrt(mean(err^2)),
        mae = mean(abs(err))
    )
}

#' Plot the sampled edge betweenness benchmark
#'
#' One panel per metric (Spearman, RMSE, MAE, elapsed time) against the
#' sample fraction: repetitions as faint points, their mean as a solid point.
#'
#' @param metrics Data frame from `eval_sampled_edge_betweenness()` rows.
#' @param path Output PNG path.
#' @return `path`.
plot_eb_sampling <- function(metrics, path) {
    labels <- c(
        spearman = "Spearman",
        rmse = "RMSE",
        mae = "MAE",
        secs = "Time (s)"
    )
    long <- do.call(
        rbind,
        lapply(names(labels), function(m) {
            data.frame(
                frac = metrics$frac,
                metric = labels[[m]],
                value = metrics[[m]]
            )
        })
    )
    long$metric <- factor(long$metric, levels = labels)
    means <- stats::aggregate(value ~ frac + metric, long, mean)

    series <- "#2a78d6"
    ink <- "#0b0b0b"
    ink_2 <- "#52514e"
    axis_line <- "#9c9b96"

    p <- ggplot2::ggplot(long, ggplot2::aes(frac, value)) +
        ggplot2::geom_point(colour = series, alpha = 0.35, size = 1.5) +
        ggplot2::geom_point(data = means, colour = series, size = 2.5) +
        ggplot2::facet_wrap(~metric, scales = "free_y", axes = "all_x") +
        ggplot2::scale_x_log10(
            breaks = sort(unique(metrics$frac)),
            labels = scales::label_percent()
        ) +
        ggplot2::scale_y_continuous(
            labels = function(x) {
                if (max(abs(x), na.rm = TRUE) < 0.01) {
                    scales::label_scientific()(x)
                } else {
                    format(x, trim = TRUE)
                }
            }
        ) +
        ggplot2::labs(x = "Fraction of sampled vertices", y = NULL) +
        ggplot2::theme_classic(base_size = 11) +
        ggplot2::theme(
            axis.ticks = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(colour = axis_line),
            axis.text = ggplot2::element_text(colour = ink_2),
            axis.title = ggplot2::element_text(colour = ink_2),
            strip.background = ggplot2::element_blank(),
            strip.text = ggplot2::element_text(
                colour = ink,
                hjust = 0,
                face = "bold"
            ),
            legend.position = "none"
        )

    ggplot2::ggsave(
        path,
        p,
        width = 8,
        height = 6,
        dpi = 300,
        create.dir = TRUE
    )
    return(path)
}
