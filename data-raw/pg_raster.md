
* [PostGIS Raster and Crunchy Bridge](https://www.crunchydata.com/blog/postgis-raster-and-crunchy-bridge)

```sh
url=http://tgftp.nws.noaa.gov/SL.us008001/ST.opnl/DF.gr2/DC.ndfd/AR.conus/VP.001-003/ds.pop12.bin

gdal_translate \
  -a_nodata 255 \
  -co COMPRESS=DEFLATE \
  -co ZLEVEL=9 \
  -co PREDICTOR=2 \
  -co TILED=YES \
  -ot Byte \
  /vsicurl/$url \
  pop12.tif          
```
