library(data.table)
library(tidyverse)
library(openxlsx)

dir <- "C:/Users/CLam/Desktop/trends-ntd"
setwd(dir)
month <- "November"
year <- 2018

# download file
# source("download_files.R")
# download.ntd(dir, month, year)

# transform non-master data 
transform.ntd.non.master <- function(month, year) {
  local.file <- paste0("ntd-monthly-", month, year, '.xlsx')
  sheets <- c("UPT", "VRM", "VRH")
  dts <- NULL
  for (sheet in sheets) {
    cat("\nReading in ", sheet, " worksheet\n")
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





