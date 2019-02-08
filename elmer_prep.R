library(data.table)
library(tidyverse)
library(openxlsx)

dir <- "C:/Users/CLam/Desktop/trends-ntd"
setwd(dir)
month <- "November"
year <- 2018

source("lookups.R")

# download file
# source("download_files.R")
# download.ntd(dir, month, year)

# transform non-master data 
transform.ntd.non.master <- function(month, year) {
  local.file <- paste0("ntd-monthly-", month, year, '.xlsx')
  sheets <- c("UPT", "VRM", "VRH")
  dts <- NULL
  for (sheet in sheets) {
    cat("\nReading in", sheet, "worksheet for", month, year)
    t <- read.xlsx(local.file, sheet = sheet) 
    setDT(t)
    datecols <- colnames(t)[str_which(colnames(t), "\\d+$")]
    idcols <- c("5.digit.NTD.ID" = "NtdID",
                # "4.digit.NTD.ID" = ,
                "Agency" = "Agency",
                "Active" = "Active",
                "Reporter.Type" = "ReporterType",
                "UZA" = "UrbanArea",
                "UZA.Name" = "UrbanAreaName",
                "Modes" = "Modes",
                "TOS" = "TypeOfService")
    setnames(t, names(idcols), idcols)
    allcols <- c(idcols, datecols)
    t <- t[!is.na(NtdID), ..allcols]
    dt <- melt.data.table(t, id.vars = idcols, measure.vars = datecols, value.name = "Value")
    dt[is.na(Value), Value := 0]
    dt[, month := str_to_title(str_extract(variable, "[[:alpha:]]+"))]
    dt[, Date := paste0("20", str_extract(variable, "\\d+$"), "-", str_pad(match(month, month.abb), 2, pad = "0"))
       ][, Date := lubridate::ymd(Date, truncated = 1)]
    dt[, ValueType := sheet]
    dt[, `:=` (month = NULL, variable = NULL)]
    setcolorder(dt, c(idcols, "Date", "Value"))
    dts[[sheet]] <- dt
  }
  dtsall <- rbindlist(dts, use.names = T)
  return(dtsall)
}

# query non-master data (previous and current month)
query.ntd.non.master <- function(month, year) {
  dt <- transform.ntd.non.master(month, year)
  month.num <- match(month, month.name)
  curr.date <- paste(year, month.num, "01", sep = "-")
  prev.date <- paste(year, month.num-1, "01", sep = "-")
  t <- dt[Date %between% c(prev.date, curr.date),]
}

# compare start dates
compare.ntd.versions.start.date <- function(latest.month, latest.year) {
  dtelm <- transform.ntd.non.master("October", year) # an old file, replace with elmer connection
  dt <- transform.ntd.non.master(latest.month, latest.year)
  
  dtelm.date <- min(dtelm$Date) 
  dt.date <- min(dt$Date)
  
  if (dtelm.date == dt.date) {
    cat("\nThe", latest.month, latest.year, "file and Elmer share the same beginning reporting month and year:", 
                month.abb[lubridate::month(dt.date)], lubridate::year(dt.date))
  } else {
    cat("\nThe", latest.month, latest.year, "file and what is in Elmer do not share the same beginning reporting month and year.\n",
        "The", latest.month, latest.year, "file starts with", month.abb[lubridate::month(dt.date)], lubridate::year(dt.date), "\n",
        "What is in Elmer starts with", month.abb[lubridate::month(dtelm.date)], lubridate::year(dtelm.date))
  }
}

