library(data.table)
library(tidyverse)
library(openxlsx)
library(odbc)
library(DBI)

dir <- "C:/Users/CLam/Desktop/trends-ntd"
setwd(dir)
month <- "December" # most current downloaded month
year <- 2018 # most current downloaded year

source("lookups.R")
source("functions.R")

compare.ntd.versions.share.start.date(month, year)

# Update Elmer ------------------------------------------------------------
# dt <- transform.ntd.non.master(month, year)
# dbWriteTable(elmer_connection, "NationalTransitDatabase_Estimates", as.data.frame(dt))
# dbDisconnect(elmer_connection)