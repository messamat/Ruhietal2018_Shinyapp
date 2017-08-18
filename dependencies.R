# List the package and version as below
dependencies <- read.csv(textConnection("
                                        Package,     Min.Version
                                        foreign,     0.8-69
                                        leaflet,       1.1.0
                                        leaflet.extras,      0.2
                                        tidyverse,     1.1.1
                                        sp,       1.2-4
                                        rgeos,      0.3-22
                                        maps,     3.1.1
                                        rgdal,       1.2-5
                                        dplyr,      0.5.0
                                        rmapshaper,     0.2.0
                                        shiny,       1.0.0
                                        shinyBS,      0.61
                                        ggplot2,     2.2.0
                                        DT,       0.2
                                        rsconnect,      0.8
                                        grid,       0.4-7
                                        gridExtra,      2.2.1
                                        HatchedPolygons,       0.1.0
                                        "), stringsAsFactors = FALSE, strip.white = TRUE)

## No changes necessary below. ##

# Import installed package versions
pkgs <- installed.packages()
rownames(pkgs) <- c()
pkgs <- data.frame(pkgs, stringsAsFactors = FALSE)

# Compare requirements to installed packages
pkgs <- merge(dependencies, pkgs, by="Package", all.x=TRUE)

# Filter out packages meeting minimum version requirement
pkgs <- pkgs[mapply(compareVersion, pkgs$Min.Version, pkgs$Version) > 0, ]

# Install missing and newer packages
lapply(pkgs$Package, install.packages, repos="http://cran.us.r-project.org")

# Require dependencies [optional]
lapply(dependencies$Package, require, character.only=TRUE)
