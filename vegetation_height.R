library(terra)
library(lidR)
library(sf)

#s8_aug <- lidR::readLAS("./data/_lidar/2024-07-31_site10_section1.laz")

s2 <- readLAS("./data/_lidar/20240408_E_rescaled.las")
plot(s2)
#s2 <- classify_ground(s2, algorithm = pmf(ws = 5000, th = 3000))
