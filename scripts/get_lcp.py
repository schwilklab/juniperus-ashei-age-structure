#!/usr/bin/python3
# get_lcp.py

__version__ =    '''0.1'''
__program__ =    '''get_lcppy'''
__author__  =    '''Dylan Schwilk'''
__usage__   =    '''get_lcp.py [options] -b bounding_box output_file'''

import os
import landfire # https://github.com/FireSci/landfire-python
from osgeo import gdal
import sys
import numpy as np
import rasterio as rio

import logging
logging.basicConfig(format='%(levelname)s: %(message)s')
logger = logging.getLogger('lcp_logger')


def get_lcp_data(sbbox, name, opath):
    """sbbox is string such as "-98.11047 30.28860  -98.07929 30.31566"
    name is used to create filename. Returns an 8 band geotiff."""
    lf = landfire.Landfire(bbox=sbbox)
    lf.request_data(layers=["ELEV2020",   # elevation
                        "SLPD2020",   # slope degrees
                        "ASP2020",    # aspect in azimuth degrees, -1 for flat
                        "220F40_22",  # fuel models
                        "220CC_22",   # canopy cover
                        "220CH_22",   # canopy height
                        "220CBH_22",  # canopy base height
                        "220CBD_22"], # canopy bulk density
                    output_path = opath + name + ".zip")
    return()

# def fix_aspect_band1(geot_file):
#     """Replace -1 values with zeros in the 3rd band of a multi-band geotiff. Uses
# rasterio and numpy."""
#     # in-place method. I could modify to make a copy while doing this?
#     with rio.open(geot_file, "r+") as src:
#         meta = src.meta
#         bands = [x + 1 for x in list(range(src.count))]
#         badbands = [3] # aspect
#         for ID, b in enumerate(bands,1):
#             if b in badbands:
#                 bdata = src.read(b)
#                 bdata[bdata < 0] = 0
#                 src.write(bdata, indexes = b)
#     src.close()
#     return(true)

def fix_aspect_band(infile, outfile):
    """Replace -1 values with zeros in the 3rd band of a multi-band geotiff. Uses
rasterio and numpy. Create a new file and leave original unchanged"""
    badbands = [3] # aspect
    with rio.open(infile, "r") as src:
        profile = src.profile
        bands = [x + 1 for x in list(range(src.count))]
        with rio.open(outfile, 'w', **profile ) as dst:
            for ID, b in enumerate(bands,1):
                if b in badbands:
                    bdata = src.read(b)
                    bdata[bdata < 0] = 0
                    dst.write(bdata, ID)
                else:
                    ddata = src.read(b)
                    dst.write(ddata, ID)
        dst.close()
    src.close


def geotiff2lcp(infile, outfile):
    """Use gdal to convert an 8-band geotiff to a farsite4 style lcp for use with
the fire spread model. Note that aspect layers such as ASP2020 from landfire
will have -1 values where the slope is flat. fix_aspect_band will convert those
to zeros which I think is fine. Not converting causes the gdal LCP dirver to crash, see 

    """
    ds = gdal.Open(os.path.abspath(infile))
    driver = gdal.GetDriverByName("LCP")
    outdata = driver.CreateCopy(os.path.abspath(outfile), ds,
                 options=["ELEVATION_UNIT=METERS",
                          "SLOPE_UNIT=DEGREES",
                          "ASPECT_UNIT=AZIMUTH_DEGREES",
                          "FUEL_MODEL_OPTION=NO_CUSTOM_AND_NO_FILE",
                          "CANOPY_COV_UNIT=PERCENT",
                          "CANOPY_HT_UNIT=METERS_X_10",
                          "CBH_UNIT=METERS_X_10",
                          "CBD_UNIT=KG_PER_CUBIC_METER_X_100",
                          "DUFF_UNIT=MG_PER_HECTARE_X_10"])            



# example:
def run_example():
    gt = "./results/Hays-Travis_2022/j365f451e55d241a390edcf03458afed3.tif"
    ot = "./results/test.tif"
    fix_aspect_band(gt, ot)
    geotiff2lcp(ot, "./results/test.lcp" )
    

    
def main():
    
    '''Command line program.  Usage: '''
    import sys   
    from optparse import OptionParser

    # make sure stdin and stdout is in unicode
    #sys.stdout = codecs.getwriter('utf-8')(sys.stdout)
    #sys.stdout.reconfigure(encoding='utf-8')
    
    parser = OptionParser(usage=__usage__, version ="%prog " + __version__)
    parser.add_option("-p", "--path", action="store", type="string",
                      dest="opath",
                      default="./results", help="Output path")
    parser.add_option("-b", "--bbox", action="store", type="string",
                      dest="bbox",
                      default="", help="Output path")
    parser.add_option("-v", "--verbose", action="store_true", dest="verbose", default=False,
                       help="Print INFO messages to stdout, default=%default")    

    (options, args) = parser.parse_args()

    if (options.bbox=="" or len(args) < 1):
        print("Must supply bounding box and file name")
        exit(-1)
        
    if options.verbose:
        logger.setLevel(logging.INFO)

    logger.debug("bounding box: " + options.bbox + ", " + "output file: " + args[0])
    get_lcp_data(options.bbox, args[0], options.opath)

    return(0)

if __name__== "__main__":
    main()

