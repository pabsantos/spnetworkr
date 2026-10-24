# /// script
# requires-python = ">=3.12"
# dependencies = [
#     "osmnx>=2.0.7",
# ]
# ///
"""Download the drive network inside a polygon and export it.

Usage: uv run py/download_graph.py <polygon.geojson> <graphml> <nodes.geojson> <edges.geojson>
"""

import sys

import geopandas as gpd
import osmnx as ox


def stringify_lists(gdf: gpd.GeoDataFrame) -> gpd.GeoDataFrame:
    # OSM attributes can be lists (e.g. merged ways), which GeoJSON drivers reject
    for col in gdf.columns.drop("geometry"):
        if gdf[col].map(lambda x: isinstance(x, list)).any():
            gdf[col] = gdf[col].map(lambda x: str(x) if isinstance(x, list) else x)
    return gdf


def main(polygon_path, graphml_path, nodes_path, edges_path):
    polygon = gpd.read_file(polygon_path).to_crs(4326).geometry.union_all()
    graph = ox.graph_from_polygon(polygon, network_type="drive")
    ox.save_graphml(graph, graphml_path)

    nodes, edges = ox.graph_to_gdfs(graph)
    stringify_lists(nodes.reset_index()).to_file(nodes_path, driver="GeoJSON")
    stringify_lists(edges.reset_index()).to_file(edges_path, driver="GeoJSON")


if __name__ == "__main__":
    main(*sys.argv[1:5])
