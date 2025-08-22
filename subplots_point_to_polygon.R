# This script takes the GPS location points of the subplots for the hubland meadows
# project and creates quadratic polygons (2x2 meters) around them

library(sf)
library(dplyr)

points <- st_read("./data/_vector/sampling_sites_small.gpkg")
bff <- st_buffer(points, 1)
# then bbox around the buffered points:
rect_around_point <- function(x){st_as_sfc(st_bbox(x))}

bboxes <- lapply(bff$geom, rect_around_point)

for (i in 1:39){
  st_geometry(bff[i,]) <- bboxes[[i]]
}

plot(bff$geom[1])

st_write(bff, dsn = "./data/_vector/sampling_sites_small_rectangles.gpkg", layer = "plots_karla",
         driver = "GPKG")
