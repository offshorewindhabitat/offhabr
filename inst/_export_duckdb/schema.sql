


CREATE TABLE datasets(ds_id INTEGER, ds_key VARCHAR, active BOOLEAN, name_short VARCHAR, name_long VARCHAR, "year" INTEGER, "type" VARCHAR, units VARCHAR, taxa_classes VARCHAR, rgn_pacific BOOLEAN, rgn_atlantic BOOLEAN, rgn_gomex BOOLEAN, source_urls VARCHAR, source_url_titles VARCHAR, res_space VARCHAR, res_time VARCHAR, notes VARCHAR, description VARCHAR, n_taxa BOOLEAN, citation VARCHAR, citation_url VARCHAR, meta_url VARCHAR);
CREATE TABLE lyrs(path_tif VARCHAR, lyr_key VARCHAR, ds_key VARCHAR, aphia_id INTEGER, val_min DOUBLE, val_max DOUBLE, rescale_min DOUBLE, rescale_max DOUBLE, ds_id DOUBLE, is_ds_prime BOOLEAN);
CREATE TABLE lyr_zone_stats(lyr_key VARCHAR, ds_key VARCHAR, aphia_id INTEGER, zone_id DOUBLE, area_km2 DOUBLE, mean DOUBLE, "notNA" DOUBLE);
CREATE TABLE iris("Sepal.Length" DOUBLE, "Sepal.Width" DOUBLE, "Petal.Length" DOUBLE, "Petal.Width" DOUBLE, "Species" "Species");
CREATE TABLE taxa_wm(aphia_id INTEGER, url VARCHAR, scientificname VARCHAR, authority VARCHAR, status VARCHAR, unacceptreason VARCHAR, taxon_rank_id INTEGER, rank VARCHAR, valid_aphia_id INTEGER, valid_name VARCHAR, valid_authority VARCHAR, parent_name_usage_id INTEGER, kingdom VARCHAR, phylum VARCHAR, "class" VARCHAR, "order" VARCHAR, "family" VARCHAR, genus VARCHAR, citation VARCHAR, lsid VARCHAR, is_marine INTEGER, is_brackish INTEGER, is_freshwater INTEGER, is_terrestrial INTEGER, is_extinct INTEGER, match_type VARCHAR, modified VARCHAR);
CREATE TABLE lyr_rgn_ds(lyr_key VARCHAR, region VARCHAR, ds_id DOUBLE);
CREATE TABLE taxa_rl(aphia_id INTEGER, taxa VARCHAR, taxonid INTEGER, scientific_name VARCHAR, kingdom VARCHAR, phylum VARCHAR, "class" VARCHAR, "order" VARCHAR, "family" VARCHAR, genus VARCHAR, main_common_name VARCHAR, authority VARCHAR, published_year INTEGER, assessment_date VARCHAR, category VARCHAR, criteria VARCHAR, population_trend VARCHAR, marine_system BOOLEAN, freshwater_system BOOLEAN, terrestrial_system BOOLEAN, assessor VARCHAR, reviewer VARCHAR, aoo_km2 VARCHAR, eoo_km2 VARCHAR, elevation_upper INTEGER, elevation_lower INTEGER, depth_upper DOUBLE, depth_lower DOUBLE, errata_flag BOOLEAN, errata_reason VARCHAR, amended_flag BOOLEAN, amended_reason VARCHAR);
CREATE TABLE taxa_rl_catscores(category VARCHAR, rl_score INTEGER, description VARCHAR);


CREATE UNIQUE INDEX lyr_zone_stats_idx ON lyr_zone_stats (lyr_key, zone_id);
CREATE UNIQUE INDEX IF NOT EXISTS lyr_rgn_ds_unique_idx ON lyr_rgn_ds(lyr_key, region);
CREATE  INDEX IF NOT EXISTS lyr_rgn_ds_ds_id_idx ON lyr_rgn_ds(ds_id);
CREATE UNIQUE INDEX IF NOT EXISTS taxa_rl_unique_idx ON taxa_rl(aphia_id);
CREATE  INDEX IF NOT EXISTS taxa_rl_category_idx ON taxa_rl(category);
CREATE  INDEX IF NOT EXISTS taxa_rl_catscores_category_idx ON taxa_rl_catscores(category);


