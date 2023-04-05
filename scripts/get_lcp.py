# get_lcp.py

import os
import landfire
from osgeo import gdal
import logging
logging.basicConfig(format='%(levelname)s: %(message)s')
logger = logging.getLogger('lcp_logger')


def get_lcp(sbbox, name):
    """sbbox is string such as "-98.11047 30.28860  -98.07929 30.31566"
    name is used to create filename"""
    lf = landfire.Landfire(bbox=sbbox)
    lf.request_data(layers=["ELEV2020",   # elevation
                        "SLPD2020",   # slope degrees
                        "ASP2020",    # aspect
                        "220F40_22",  # fuel models
                        "220CC_22",   # canopy cover
                        "220CH_22",   # canopy height
                        "220CBH_22",  # canopy base height
                        "220CBD_22"], # canopy bulk density
                    output_path="./results/" + name + ".zip")
    return()

    
    



    
# # Obtain required layers for FlamMap landscape file
# lf = landfire.Landfire(bbox="-98.11047 30.28860  -98.07929 30.31566")
# lf.request_data(layers=["ELEV2020",   # elevation
#                         "SLPD2020",   # slope degrees
#                         "ASP2020",    # aspect
#                         "220F40_22",  # fuel models
#                         "220CC_22",   # canopy cover
#                         "220CH_22",   # canopy height
#                         "220CBH_22",  # canopy base height
#                         "220CBD_22"], # canopy bulk density
#                 output_path="./results/Hays-Travis_2022.zip")



def main():
    '''Command line program.  '''
    import sys   
    from optparse import OptionParser

    # make sure stdin and stdout is in unicode
    #sys.stdout = codecs.getwriter('utf-8')(sys.stdout)
    sys.stdout.reconfigure(encoding='utf-8')
    
    parser = OptionParser(usage=__usage__, version ="%prog " + __version__)
    parser.add_option("-b", "--bbox", action="store", type="string",
                      dest="bbox",  default = "",
                      help="Bounding box string")
    parser.add_option("-n", "--name", action="store", type="string",
                      dest="fname",
                      default="", help="name")
     parser.add_option("-v", "--verbose", action="store_true", dest="verbose", default=False,
                       help="Print INFO messages to stdout, default=%default")    

    (options, args) = parser.parse_args()

    if options.verbose:
        logger.setLevel(logging.INFO)

    get_lcp(options.bbox, options.fname)

    return(0)

if __name__== "__main__":
    main()
