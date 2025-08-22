library(sf)
library(terra)

plots <- st_read(dsn = "./data/_vector/sampling_sites_large_int_ext_seminat.gpkg")

subplots <- st_read("./data/_vector/subplots_rectangles.gpkg")
#st_write(subplots, dsn = "./data/_vector/subplots_2.gpkg", layer = "subplots")

st_crs(subplots)
plots <- st_transform(plots, st_crs(x = subplots))
st_crs(subplots)
st_crs(plots)

# now spatially extract info to subplots df
join <- st_join(subplots, y = plots, join = st_intersects)

###
# load an original img
img <- terra::rast("./data/_raster/site8_202409812_aligned.tif")
plot(img[[3]])
plotRGB(img, r=3,g=2,b=1, stretch="lin")
