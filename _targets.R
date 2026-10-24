library(targets)

tar_option_set(
    packages = c("sf", "igraph", "Matrix")
)

tar_source()

list(
    # TRUE downloads only the Sé and República districts
    tar_target(test, FALSE),
    tar_target(
        zones_file,
        "data-raw/zona_origem_destino.geojson",
        format = "file"
    ),
    tar_target(download_script, "py/download_graph.py", format = "file"),
    tar_target(
        graph_files,
        {
            download_script
            download_graph(zones_file, test = test)
        },
        format = "file"
    ),
    tar_target(g, read_graph(graph_files[1], format = "graphml")),
    tar_target(v, st_read(graph_files[2])),
    tar_target(e, st_read(graph_files[3])),
    tar_target(eb, calc_edge_betweenness(g)),
    tar_target(k, degree(g)),
    tar_target(l, calc_avg_shortest_dist(g)),
    tar_target(c, calc_directed_clustering(g)),
    tar_target(g_random, generate_random_graph(g)),
    tar_target(v_results, join_vertices_results(v, k, l, c)),
    tar_target(e_results, join_edge_results(e, eb)),
    tar_target(g_random_params, export_random_graph_params(g_random)),
    tar_target(
        v_results_file,
        export_geojson(
            v_results,
            if (test) {
                "data/sp_graph_test_nodes_results.geojson"
            } else {
                "data/sp_graph_nodes_results.geojson"
            }
        ),
        format = "file"
    ),
    tar_target(
        e_results_file,
        export_geojson(
            e_results,
            if (test) {
                "data/sp_graph_test_edges_results.geojson"
            } else {
                "data/sp_graph_edges_results.geojson"
            }
        ),
        format = "file"
    )
)
