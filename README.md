# spnetworkr

Network analysis of the São Paulo street network. The pipeline downloads the
drivable road graph from OpenStreetMap (via OSMnx), restricted to the São Paulo
municipality zones of the Origin–Destination survey, and computes
node- and edge-level metrics with igraph.

## Metrics

- **Nodes**
  - `k`: degree
  - `l`: average shortest-path distance (inverse of normalized out-closeness)
  - `c`: weighted directed clustering coefficient
- **Edges**
  - `eb`: edge betweenness, normalized by `n(n - 1)`
- **Random baseline**: mean `l` and `c` for a degree-preserving random graph
  (configuration model)

## Requirements

- R with [renv](https://rstudio.github.io/renv/) (packages: `targets`, `sf`,
  `igraph`, `Matrix`)
- [uv](https://docs.astral.sh/uv/) for the Python download script
  (dependencies are declared inline in `py/download_graph.py`)

## Usage

```r
renv::restore()
targets::tar_make()
```

To run on a small area (Sé and República districts only), set
`tar_target(test, TRUE)` in `_targets.R`.

## Outputs

Written to `data/`:

- `sp_graph.graphml`, `sp_graph_nodes.geojson`, `sp_graph_edges.geojson`: raw network
- `sp_graph_nodes_results.geojson`: nodes with `k`, `l`, `c`
- `sp_graph_edges_results.geojson`: edges with `eb`

Test runs use the `sp_graph_test` prefix. Random-graph parameters are available
via `targets::tar_read(g_random_params)`.

## Structure

```
_targets.R               pipeline definition
R/download_graph.R       builds the study-area polygon and calls the Python script
R/calc_graph.R           metric calculations and exports
py/download_graph.py     OSMnx download and GraphML/GeoJSON export
data-raw/                OD survey zones
```
