#--------------------------------------------------------------------------------
# setup.R         Setup project
#--------------------------------------------------------------------------------

# set computer
cpu <- "laptop"  # "pc", "mac". "server"

# Set main data paths
if(cpu == "laptop") main_dir <- "C:/YOUR_PATH"

# 2. Create data paths
input_data <- paste(main_dir, "input", sep="/")
if (!dir.exists(input_data)) dir.create(input_data, recursive = TRUE)

output_data <- paste(main_dir, "output", sep="/")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)

# Load required packages
pacman::p_load(dplyr, raster, sf, lubridate, ggplot2, rnaturalearth, rnaturalearthdata, CopernicusMarine, doParallel, 
               beepr, tidyverse, dplyr, move, data.table, splitstackshape, Hmisc, dismo, scam, tidyr, parallel,
               egg, gifski, ncdf4, ramify, ecmwfr, av, gganimate, foreach, stringr, groupdata2, ggBRT, fmsb,
               reshape2, gbm, devtools, rJava, animalsensor, availability, moments, magrittr, grid, corrplot,
               nlme, ape, caper, car, bbmle, install = FALSE) 

#devtools::install_github("JBjouffray/ggBRT") # will take several minutes to install
library(ggBRT)
