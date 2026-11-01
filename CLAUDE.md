# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Network analysis of the São Paulo drivable street network. A `targets` pipeline downloads the OSM road graph (Python/OSMnx, run via `uv`), then computes node and edge metrics in R with igraph and writes the results to GeoJSON in `data/`. See `README.md` for the metric definitions and output files.

## Commands

```r
renv::restore()                    # install R dependencies (renv is activated by .Rprofile)
targets::tar_make()                # run the pipeline
targets::tar_make(names = c)       # build one target (and its upstream deps)
targets::tar_read(g_random_params) # read a target's value
targets::tar_visnetwork()          # view the dependency graph
```

The Python script is run by the pipeline, but it can also be run on its own: `uv run py/download_graph.py <polygon.geojson> <graphml> <nodes.geojson> <edges.geojson>`. Its dependencies are declared inline (PEP 723), so there's no separate Python environment to set up.

There are no tests or lint config. R files are formatted with `air` (4-space indent).

## Architecture

- `_targets.R` defines the whole pipeline. `tar_source()` loads every file in `R/`, and the packages `sf`, `igraph` and `Matrix` are attached through `tar_option_set`.
- `R/` holds plain functions, one per pipeline step. They are not a package. Calls to igraph, Matrix and sf functions use the `pkg::` prefix, and they are documented with roxygen-style comments (no roxygen build).
- **Test mode:** `tar_target(test, TRUE)` in `_targets.R` limits the download to OD zones 1–6 (Sé and República) and switches every output to the `sp_graph_test` prefix.
- **Data flow:** `download_graph()` filters `data-raw/zona_origem_destino.geojson` to the São Paulo municipality (`cd_municipio == 36`) and writes a temporary polygon. It then shells out to `py/download_graph.py`, which writes three files: a graphml and node/edge GeoJSONs. The graph `g` is read from the graphml, and `v`/`e` are read as sf from the GeoJSONs.
- **Positional joins:** `join_vertices_results` / `join_edge_results` attach metrics computed on `g` to `v`/`e` by row order, not by ID. This only works because OSMnx writes the graphml and the GDFs in the same node/edge order. Keep that order intact when you change the download or read steps.
- The graph is a directed multigraph. `calc_directed_clustering` uses edge multiplicities from `as_adjacency_matrix` as weights (Fagiolo's directed clustering). It relies on Matrix sparse methods for `rowSums`, `t` and `%*%`.
- `data/` and `cache/` (the OSMnx HTTP cache) are gitignored. From the `_targets/` store, only `_targets/meta/meta` is committed.
