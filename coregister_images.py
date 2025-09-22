import os
import re
import rasterio as rio
import difflib
from arosics import COREG
from geoarray import GeoArray
from osgeo import gdal
from rasterio.crs import CRS

# Try single coreg:

img_ref = "C:/EAGLE/InnoLab/data/_raster/20240408_site8_3cm_georef.tif"
img_target = "C:/EAGLE/InnoLab/data/_raster/20240523_site8_3cm_georef.tif"

img = rio.open(img_ref)
width = img.width
height = img.height
print("width = ", width, "height = ", height)
img.close()

# At some point in the processing chain "TOWGS84[0,0,0,0,0,0,0]" was introduced to the proj4 WKT string which interferes with
# AROSICS. Therefore the rasters are reprojected cleanly to EPSG:32632

'''
new_crs = CRS.from_epsg(32632)

with rio.open(img_ref, "r+") as dst:
    dst.update_tags(ns="crs", **new_crs.to_dict())

with rio.open(img_target, "r+") as dst:
    dst.update_tags(ns="crs", **new_crs.to_dict())

# changes permanently:
with rio.open(img_ref) as img:
	ref_crs = img.crs.to_wkt()
	print("REF wkt: ", ref_crs, "\n", "\n")

with rio.open(img_target) as img:
	trg_crs = img.crs.to_wkt()
	print("REF wkt: ", trg_crs, "\n", "\n")

print("Projection compare: ", ref_crs == trg_crs)

'''

# Normalize Projection before running arosics

'''
def normalize_crs(path):
    with rio.open(path, "r+") as dst:
        epsg = dst.crs.to_epsg()
        print("Input CRS: ", epsg, "\n")
        if epsg:
            clean_crs = CRS.from_epsg(epsg)
            print("Clean CRS string: ", clean_crs.to_wkt(), "\n")
            dst.update_tags(ns="crs", **clean_crs.to_dict())

normalize_crs(img_ref)
normalize_crs(img_target)
'''

clean_crs = CRS.from_epsg(32632)

with rio.open(img_ref, "r+") as img:
	img.crs = clean_crs
	print("REF wkt: ", img.crs.to_wkt(), "\n", "\n")

with rio.open(img_target, "r+") as img:
	img.crs = CRS.from_epsg(32632)
	print("REF wkt: ", img.crs.to_wkt(), "\n", "\n") 

'''
# Use GeoArray to clean Metadata, avoiding arosics "STATISTICS_APPROXIMATE" Error
ga_ref = GeoArray(img_ref)
ga_target = GeoArray(img_target)
ga_ref.metadata.band_meta = {}
ga_target.metadata.band_meta = {}


'''
# Setting the window size close to the actual image size helps
CR = COREG(img_ref, img_target, binary_ws= False, ws=(width*0.9, height*0.9),
	max_shift=200, calc_corners = True, max_iter=50,
	path_out="C:/EAGLE/InnoLab/data/_raster/20240523_site8_3cm_globcoreg.tif",
	fmt_out="GTIFF")
CR.calculate_spatial_shifts()
CR.correct_shifts()
