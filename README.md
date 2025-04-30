# juniper-fire
Alex Bowers' MS project


## Organization ##

- `./data` folder has data in version control
- `./scripts` folder contains R code. All code in this folder assumes working directory is the project root. Enforce this locally according to your editor (eg use a  `Rproj` file if using RStudio).
- `./results` folder is for figures and temporary data not tracked by version control. A `.gitignore` file in this folder ensures the folder itself is available on a fresh clone.

## Organization ##

- `./scripts` folder should be run with tree_analysis.R first for the main anaylsis. The other script print_figures.R creates the figures located in the `./results` folder.

