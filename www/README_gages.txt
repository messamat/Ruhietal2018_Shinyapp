Metadata for data download from the online app accompanying Ruhi et al.(2017) 'Consistent freshwater resource monitoring for a changing world'
Author: Mathis Messager
Date: 08/02/2017
Contact information: messamat@uw.edu
Refer to https://messamat.shinyapps.io/Map_5/ for complete methodology

Ruhi et al. 2017: URL

Be aware that gages and HUCs IDs are in character format.
In Excel, use 'Get external data from text' to import data with the appropriate column types (delimited, data has headers, comma)
In R, use the following command line:
basingages_df <- read.csv('selected_basin_gagedata.csv', colClasses=c(rep('character',3), rep('numeric',156)))


These data are licensed under a 
Creative Common License Attribution-NonCommercial 4.0 International (CC BY-NC 4.0)
See https://creativecommons.org/licenses/by-nc/4.0/ for details