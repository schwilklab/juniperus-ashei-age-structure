# juniper-fire
Alex Bowers' MS project


## Organization ##

- `./data` folder has data in version control
- `./scripts` folder contains R code. All code in this folder assumes working directory is the project root. Enforce this locally according to your editor (eg use a `Rproj` file if using RStudio).
- `./results` folder is for figures and temporary data not tracked by version control. A `.gitignore` file in this folder ensures the folder itself is available on a fresh clone.

## Project Goals ##

TODO


## Resources ##



### Fire growth model resources ###

We might produce "burn probability" type maps to characterize and identify potential topographic fire refugia. This would be reasonably large undertaking, but some of the work required would aid other lab projects. Although the windows gui flammap has some capabilites like this and FSPro has more, our preferred workflow would be scriptable and use the open-source farsite fire growth implementation

- Our Farsite fork: https://github.com/dschwilk/farsite
- Landfire data site: https://landfire.gov/getdata.php and pdf on web API: https://lfps.usgs.gov/arcgis/rest/services/LandfireProductService/GPServer
- Python code for landfire data retrieval: https://landfire-python.firesci.io/en/latest/
- R package for producing farsite input files: https://github.com/mbedward/farsitebatch
- R package for dealing with RAWS weather data needed by farsite: https://github.com/MazamaScience/RAWSmet


I envision: python script to grab landfire data and creating lcps (pretty much done), then R steering code to grab weather data, produce input files, execute farsite, grab results and summarize. All thee steps should be wrapped up in a separate git repo as they are not specific to the juniper project but I'm making a note here for now.
