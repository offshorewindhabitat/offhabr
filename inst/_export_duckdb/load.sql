COPY datasets FROM '/Users/bbest/Github/ecoquants/offhabr/inst/_export_duckdb/datasets.csv' (FORMAT 'csv', quote '"', delimiter ',', header 0);
COPY lyrs FROM '/Users/bbest/Github/ecoquants/offhabr/inst/_export_duckdb/lyrs.csv' (FORMAT 'csv', quote '"', delimiter ',', header 0);
COPY lyr_zone_stats FROM '/Users/bbest/Github/ecoquants/offhabr/inst/_export_duckdb/lyr_zone_stats.csv' (FORMAT 'csv', quote '"', delimiter ',', header 0);
COPY iris FROM '/Users/bbest/Github/ecoquants/offhabr/inst/_export_duckdb/iris.csv' (FORMAT 'csv', quote '"', delimiter ',', header 0);
COPY taxa_wm FROM '/Users/bbest/Github/ecoquants/offhabr/inst/_export_duckdb/taxa_wm.csv' (FORMAT 'csv', quote '"', delimiter ',', header 0);
COPY lyr_rgn_ds FROM '/Users/bbest/Github/ecoquants/offhabr/inst/_export_duckdb/lyr_rgn_ds.csv' (FORMAT 'csv', quote '"', delimiter ',', header 0);
COPY taxa_rl FROM '/Users/bbest/Github/ecoquants/offhabr/inst/_export_duckdb/taxa_rl.csv' (FORMAT 'csv', quote '"', delimiter ',', header 0);
COPY taxa_rl_catscores FROM '/Users/bbest/Github/ecoquants/offhabr/inst/_export_duckdb/taxa_rl_catscores.csv' (FORMAT 'csv', quote '"', delimiter ',', header 0);
