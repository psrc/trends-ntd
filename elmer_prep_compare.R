#' ---
#' title: National Transit Database Updates or Revisions
#' ---

#' ### Compare Versions  
#' The table below captures records of transit agencies in the Central Puget Sound region for the previous month that have changed since the last monthly release 
#+ echo = FALSE, message = FALSE, results = 'hide'
library(data.table)
library(tidyverse)
library(openxlsx)
library(odbc)
library(DBI)
library(knitr)

dir <- "C:/Users/CLam/Desktop/trends-ntd"
setwd(dir)
month <- "December" # most current downloaded month
year <- 2018 # most current downloaded year

source("lookups.R")
source("functions.R")

comp.dt <- compare.ntd.versions(month, year, psrc.region = "TRUE")

#+ echo = FALSE 
knitr::kable(comp.dt)