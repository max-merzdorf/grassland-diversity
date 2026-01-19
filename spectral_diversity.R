library(terra)

rstack <- rast("./data/_raster/planet/Planet_8bit.tif")
apr <- rstack[[grep("20240408", names(rstack))]]
may <- rstack[[grep("20240513", names(rstack))]]
jul <- rstack[[grep("20240709", names(rstack))]]
aug <- rstack[[grep("20240812", names(rstack))]]
monthly <- list(apr, may, jul, aug)

# make smaller for testing
plots <- sf::st_read("./data/_vector/sampling_sites_large_int_ext_seminat.gpkg")
plots <- st_transform(plots, "EPSG:32632")
sf_use_s2(FALSE) # turn off spherical geometry use
plots <- st_buffer(plots, dist = 30)
aoi_vec <- vect(plots)
i <- 3
aoi_i <- aoi_vec[i]
r   <- crop(apr, aoi_i)
r   <- mask(r,  aoi_i)

X <- values(r, mat = TRUE)
ok <- complete.cases(X)
X <- X[ok, ]
Xn <- scale(X)

pc <- prcomp(X, center = FALSE, scale. = FALSE)
pcn <- prcomp(Xn, center = FALSE, scale. = FALSE)
pc
pcn
# Use first 2–3 components
Z <- pc$x[, 1:3]

######### DISTANCE TO SPECTRAL CENTROID WITH PCA
X <- values(r, mat = TRUE)
ok <- complete.cases(X)
Xsc <- scale(X)

pc <- prcomp(Xsc, center = FALSE, scale. = FALSE)

npc <- 3
centroid_pc <- colMeans(pc$x[, 1:npc])

disp_r <- app(r, fun = function(v) {
  v <- scale(
    v,
    center = attr(Xsc, "scaled:center"),
    scale  = attr(Xsc, "scaled:scale")
  )
  
  # project into PCA space
  z <- v %*% pc$rotation[, 1:npc]
  
  sqrt(rowSums((z - centroid_pc)^2))
})

# do for all sites -------------------------------------------------------
library(terra)
library(purrr)
library(sf)
library(dplyr)

plots <- sf::st_read("./data/_vector/sampling_sites_large_int_ext_seminat.gpkg")
plots <- st_transform(plots, "EPSG:32632")
sf_use_s2(FALSE) # turn off spherical geometry use
plots <- st_buffer(plots, dist = 30)
aoi_vec <- vect(plots)

ls <- list.files("./data/_raster/planet/", pattern = "bandmath.tif$", full.names = T)
r <- lapply(ls, rast)

# make list of PCAs for every month
X <- lapply(r, values, mat = T)
ok <- lapply(X, complete.cases)

# remove non compleete cases
Xok <- map(X, ~ .x[complete.cases(.x), ])
Xscaled <- lapply(Xok, scale)

pcs <- lapply(Xscaled, prcomp, center = F, .scale = F) # DO NOT center!! because centroid calculation later

centroids <- lapply(pcs, function(pc) {
  colMeans(pc$x[, 1:3, drop = FALSE])
})

dist_to_centroid <- function(pc, centroid, pcs = 1:3) {
  scores <- pc$x[, pcs, drop = FALSE]
  sqrt(rowSums((scores - centroid)^2))
}

distances <- Map(
  function(pc, cent) dist_to_centroid(pc, cent),
  pcs,
  centroids
)

dist_rasters <- Map(function(rast_obj, ok, dist_vec) {
  
  # initialize output raster
  out <- rast(rast_obj, nlyrs = 1)
  values(out) <- NA_real_
  
  # assign distances to valid pixels
  vals <- values(out)
  vals[ok] <- dist_vec
  values(out) <- vals
  
  out
  
}, r, ok, distances)

# rename the rasters with the corresponding date
dates <- c(20240408, 20240513, 20240709, 20240812)

dist_rasters <- Map(function(r, nm){
  names(r) <- nm
  r
}, dist_rasters, dates)

for (i in plots$Nummer){
  
  aoi <- aoi_vec[aoi_vec$Nummer == i]
  distrast_site <- lapply(dist_rasters, crop, aoi)
  # mask to site
  distrast_site <- lapply(distrast_site, mask, aoi)
  
  # get the mean distance per site
  v <- lapply(distrast_site, values)
  m <- lapply(v, mean, na.rm = T)
  
  # turn into data frame with date oclumn and value column to match the other dataframes
  d <- data.frame(SSD = unlist(m),
                  date = as.integer(unlist(lapply(distrast_site, names))))
  
  # read the corresponding site-metric result df
  res <- read.csv(paste0("./results/metric_tables/Planet_site",i,"_metrics.csv"))
  class(res$date)
  class(d$date)
  res_c <- left_join(res, d)
  write.csv(x = res_c, file = paste0("./results/metric_tables/Planet_site",i,"_metrics.csv"), row.names = F)
  
}

t <- read.csv("./results/metric_tables/Planet_site1_metrics.csv")
