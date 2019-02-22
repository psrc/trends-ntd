# Table Name from Elmer
working.dbtable <- "Christy.NationalTransitDatabase_Estimates"

db.connect <- function() {
  elmer_connection <- dbConnect(odbc(),
                                driver = "SQL Server",
                                server = "sql2016\\DSADEV",
                                database = "Sandbox",
                                trusted_connection = "yes"
  )
}

# read table
read.dt <- function() {
  elmer_connection <- db.connect()
  dtelm <- dbReadTable(elmer_connection, SQL(working.dbtable))
  dbDisconnect(elmer_connection)
  setDT(dtelm)
}

# transform non-master data from raw
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

# query non-master data from raw (previous and current month)
query.ntd.non.master <- function(month, year) {
  dt <- transform.ntd.non.master(month, year)
  month.num <- match(month, month.name)
  curr.date <- paste(year, month.num, "01", sep = "-")
  prev.date <- paste(year, month.num-1, "01", sep = "-")
  t <- dt[Date %between% c(prev.date, curr.date),]
}

# compare start dates
compare.ntd.versions.share.start.date <- function(month, year) {
  # dtelm <- transform.ntd.non.master("October", year) # an old file or use elmer connection
  dtelm <- read.dt()
  
  dt <- transform.ntd.non.master(month, year)
  
  dtelm.date <- min(dtelm$Date) 
  dt.date <- min(dt$Date)
  
  if (dtelm.date == dt.date) {
    cat("\nThe", month, year, "file and Elmer share the same beginning reporting month and year:\n", 
        month.abb[lubridate::month(dt.date)], lubridate::year(dt.date))
    t <- TRUE
  } else {
    cat("\nThe", month, year, "file and what is in Elmer do not share the same beginning reporting month and year.\n",
        "The", month, year, "file starts with", month.abb[lubridate::month(dt.date)], lubridate::year(dt.date), "\n",
        "What is in Elmer starts with", month.abb[lubridate::month(dtelm.date)], lubridate::year(dtelm.date))
    t <- FALSE
  }
  return(t)
}

# View differences between versions for the previous month as a table
compare.ntd.versions <- function(month, year, psrc.region = c("TRUE", "FALSE")) {
  
  # dtelm <- transform.ntd.non.master("October", year) # an old file or use elmer connection
  dtelm <- read.dt()
  
  dt <- query.ntd.non.master(month, year) # new file
  
  # new version
  cols <- colnames(dt)[!(colnames(dt) %in% c("Active", "ReporterType"))]
  newdt <- dt[Date == min(Date), ..cols] # select previous month
  setnames(newdt, "Value", "Value_new")
  
  # existing
  olddt <- dtelm[Date == max(Date), ..cols][, Date := lubridate::ymd(Date)] 
  join.cols <- setdiff(cols, "Value")
  
  compdt <- merge(olddt, newdt, by = join.cols)
  diffdt <- compdt[Value != Value_new][, Diff := Value_new- Value]
  
  if (psrc.region == "TRUE") {
    psrc <- names(abbr)
    t <- diffdt[Agency %in% psrc,]
  } else {
    t <- diffdt[order(Diff)]
  }
  return(t)
}