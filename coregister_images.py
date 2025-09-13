import os
import re
import rasterio as rio
from arosics import COREG
from geoarray import GeoArray
from osgeo import gdal

# Try single coreg:

img_ref = "C:/EAGLE/InnoLab/data/_raster/20240408_site8_3cm_orthomosaic.tif"
img_target = "C:/EAGLE/InnoLab/data/_raster/20240523_site8_3cm_georef.tif"

img = rio.open(img_ref)
width = img.width
height = img.height
print("width = ", width, "height = ", height)


# Use GeoArray to clean Metadata, avoiding arosics "STATISTICS_APPROXIMATE" Error
ga_ref = GeoArray(img_ref)
ga_target = GeoArray(img_reprojected)
ga_ref.metadata.band_meta = {}
ga_target.metadata.band_meta = {}



# Setting the window size close to the actual image size helps
CR = COREG(ga_ref, ga_target, binary_ws= False, ws=(width*0.9, height*0.9),
	max_shift=200, calc_corners = True, max_iter=50,
	path_out="C:/EAGLE/InnoLab/data/_raster/20240523_site8_3cm_globcoreg.tif",
	fmt_out="GTIFF")
CR.calculate_spatial_shifts()
CR.correct_shifts()

