CREATE TABLE cells_ds AS
    SELECT
      *,
      val_z * rl_score AS val_rl
    FROM
    -- datasets by polygon
    (
SELECT
  ds.ds_key, ds.mdl_id, ds.aphia_id, ds.val_type,
  dv.ply_id, dc.cell_id,
  dv.val, dv.val_z
FROM
  (SELECT ds_key, mdl_id, aphia_id, val_type
   FROM ds_gm) AS ds
LEFT JOIN
  (SELECT mdl_id, ply_id,
   density AS val,
   density / MAX(density) OVER (PARTITION BY mdl_id) AS val_z
   FROM ds_gm_ply_vals) AS dv
  USING (mdl_id)
LEFT JOIN
  (SELECT ply_id, cell_id
   FROM cells_ds_ply
   WHERE ds_key = 'gm') AS dc
  USING (ply_id)
) AS ds_gm
    -- cell attributes
    LEFT JOIN
      (SELECT
        cell_id, block_id, zone_id, elev
      FROM cells) AS c
      USING (cell_id)
    -- taxa attributes
    LEFT JOIN
      (SELECT
         aphia_id, scientificname, taxon_rank_id, rank,
         kingdom, phylum, class, "order", family, genus
       FROM taxa_wm
       GROUP BY
        aphia_id, scientificname, taxon_rank_id, rank,
         kingdom, phylum, class, "order", family, genus) AS tw
      USING (aphia_id)
    LEFT JOIN
      (SELECT
         aphia_id, category, population_trend
       FROM taxa_rl
       GROUP BY
        aphia_id, category, population_trend) AS tr
      USING (aphia_id)
    LEFT JOIN
      (SELECT category, rl_score
      FROM taxa_rl_catscores) trc
      USING (category)
    -- filters
    WHERE
      zone_id IS NOT NULL
