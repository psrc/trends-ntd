# Table Name from Elmer
working.dbtable.ntd <- "Christy.NationalTransitDatabase_Estimates"
working.dbtable.uzapop <- "Craig.national_urban_area_population"
working.dbtable.fipcode <- "Craig.fips_codes"

db.connect <- function() {
  elmer_connection <- dbConnect(odbc(),
                                driver = "SQL Server",
                                server = "sql2016\\DSADEV",
                                database = "Sandbox",
                                trusted_connection = "yes"
  )
}

# read table
read.dt <- function(atable) {
  elmer_connection <- db.connect()
  dtelm <- dbReadTable(elmer_connection, SQL(atable))
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
  dtelm <- read.dt(working.dbtable.ntd)
  
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
  dtelm <- read.dt(working.dbtable.ntd)
  
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



# Summarise: Agency & Mode ------------------------------------------------



create.agency.lu <- function() {
  agency.fnames <- map(agency.list, ~pluck(., "fullname")) %>% unlist
  agency.alias <- map(agency.list, ~pluck(., "alias")) %>% unlist
  lu <- data.table(fnames = agency.fnames, alias = agency.alias)
}

summarise.boardings.by.agency.mode <- function() {
  dt <- read.dt(working.dbtable.ntd)
  lu <- create.agency.lu()
  dtu <- dt[ValueType == "UPT" & Agency %in% lu$fnames, ] # select boardings
  dtu <- merge(dtu, modes.lookup, by = "Modes")
  dtu <- merge(dtu, lu, by.x = "Agency", by.y = "fnames")
  dtagg <- dtu[, lapply(.SD, sum), .SDcols = "Value", by = .(alias, AModes, year(Date))]
  dtc <- dcast.data.table(dtagg, alias + AModes ~ paste0("cy", year), value.var = "Value")
  t <- dtc[order(AModes)][, AModes := str_to_title(str_replace_all(AModes, "_", " "))]
  setnames(t, c("alias", "AModes"), c("Agency", "Mode"))
  return(t)
}

summarise.boardings.by.mode <- function() {
  dt <- read.dt(working.dbtable.ntd)
  lu <- create.agency.lu()
  dtu <- dt[ValueType == "UPT" & Agency %in% lu$fnames, ] # select boardings
  dtu <- merge(dtu, modes.lookup, by = "Modes")
  dtagg <- dtu[, lapply(.SD, sum), .SDcols = "Value", by = .(AModes, year(Date))]
  dtc <- dcast.data.table(dtagg, AModes ~ paste0("cy", year), value.var = "Value")
  t <- dtc[, alias := "All CPS Agencies"][order(AModes)][, AModes := str_to_title(str_replace_all(AModes, "_", " "))]
  setnames(t, c("alias", "AModes"), c("Agency", "Mode"))
  return(t)
}

calc.change.and.shares <- function(table) {
  beg.dec.yr <- "2010"
  cycols <- str_subset(colnames(table), "^cy")
  beg.dec <- str_subset(cycols, beg.dec.yr)
  cols1 <- c(tail(cycols, -1), max(cycols))
  cols2 <- c(head(cycols, -1), beg.dec)
  diffcols <- paste0(cols1, "-", cols2)
  sharecols <- paste0("share_", diffcols)
  table[, (diffcols) := mapply(function(x, y) .SD[[x]] - .SD[[y]], cols1, cols2, SIMPLIFY = F)
        ][, (sharecols) := mapply(function(x, y) .SD[[x]]/.SD[[y]], diffcols, cols2, SIMPLIFY = F)]
  t <- table[, lapply(.SD, function(x) replace(x, which(is.nan(x)|is.infinite(x)), 0))]
}

export.excel <- function(table) {
  yrscols <- str_subset(colnames(table), "^cy|share")
  sumcols <- setdiff(colnames(table), yrscols)
  newfilenm <- paste0(paste0(sumcols, collapse = "_"), "_Summary_")
  wb <- createWorkbook()
  addWorksheet(wb, newfilenm)
  modifyBaseFont(wb, fontSize = 10, fontName = "Segoe UI Semilight")
  pct <- createStyle(numFmt="0%")
  num <- createStyle(numFmt = "#,##0")
  writeData(wb, sheet = newfilenm, table)
  addStyle(wb, newfilenm, style = pct, cols = str_which(colnames(table), "^share_"), rows = c(2:(nrow(table)+1)), gridExpand = T)
  addStyle(wb, newfilenm, style = num, cols = str_which(colnames(table), "^cy"), rows = c(2:(nrow(table)+1)), gridExpand = T, stack = T)
  saveWorkbook(wb, file = file.path(dir, paste0(newfilenm, Sys.Date(), ".xlsx")), overwrite = T)
}


# Summarise: Urban Area (UZA) ---------------------------------------------

summarize.annual.uza.pop <- function() {
  # read uza pop data
  pop <- read.dt(working.dbtable.uzapop)
  lu <- read.dt(working.dbtable.fipcode)
  uza.lu <- fread("uza_ua_codes.txt", colClasses= c("character", "integer", "character"))
  
  l <- lu[category == "urban area", .(geoid, name)]
  dtm <- merge(pop, l, by = "geoid", all.x = T)
  max.yr <- max(dtm$year)
  metros <- dtm[value > 1000000 & year %in% max.yr, ] # criteria
  t <- dtm[name %in% c(metros$name, "Bremerton, WA"), ]
  tt <- dcast.data.table(t, geoid + name ~ paste0("pop_", year))
  tm <- merge(tt, uza.lu, by.x = "geoid", by.y = "UACE", all.x = T)
  
  # merge Seattle and Bremerton
  cps.names <- str_subset(tm$name, "Seattle|Bremerton")
  cps.dt <- tm[name %in% cps.names, lapply(.SD, sum), .SDcols = str_subset(colnames(tm), "^pop")
               ][, `:=` (name = "Central Puget Sound", NAME = "Central Puget Sound", geoid = "00000", UZA2010 = "00000")] #keep UZA2010?, keep NAME?
  ntm <- rbindlist(list(tm[!(name %in% cps.names)], cps.dt), use.names = T, fill = T)
}

summarize.annual.uza.boardings <- function() {
  dt <- read.dt(working.dbtable.ntd)
  dtagg <- dt[ValueType == "UPT" & Active == "Active", ][, lapply(.SD, sum), .SDcols = "Value", by = .(UrbanArea, UrbanAreaName, year(Date))]
  tt <- dcast.data.table(dtagg, UrbanArea + UrbanAreaName ~ paste0("boarding_", year), value.var = "Value")
  
  cps.names <- str_subset(tt$UrbanAreaName, "Seattle|Bremerton")
  cps.dt <- tt[UrbanAreaName %in% cps.names, lapply(.SD, sum), .SDcols = str_subset(colnames(tt), "^boarding")
               ][, `:=` (UrbanAreaName = "Central Puget Sound", UrbanArea = "00000")]
  ntt <- rbindlist(list(tt[!(UrbanAreaName %in% cps.names)], cps.dt), use.names = T, fill = T)
}

summarize.annual.uza.pop.boardings <- function() {
  pop <- summarize.annual.uza.pop()
  brd <- summarize.annual.uza.boardings()
  t <- merge(pop, brd, by.x = "UZA2010", by.y = "UrbanArea", all.x = T)
  beg.year <- min(str_subset(colnames(t), "^pop") %>% str_extract("\\d+"))
  brd.cols <- str_subset(str_subset(colnames(t), "^boarding"), paste0(seq(beg.year, max(str_subset(colnames(t), "^boarding") %>% str_extract("\\d+"))), collapse = "|"))
  cols <- c(str_subset(colnames(t), "^UZA|geoid|UrbanAreaName"), str_subset(colnames(t), "pop"), brd.cols)
  tt <- t[, ..cols]
}

calc.annual.uza.pop.boardings.ranks <- function(table) {
  # dt <- summarize.annual.uza.pop.boardings()
  brd.cols <- str_subset(colnames(table), "^boarding")
  max.p <- max(str_subset(colnames(table), "^pop"))
  max.b <- max(brd.cols)
  prv.b <- brd.cols[(length(brd.cols)-1)]
  yrs.b <- paste0(str_extract(c(max.b, prv.b), "\\d+"), collapse = "-")
  table[, boardings_growth := get(eval(max.b))-get(eval(prv.b))
     ][, boardings_rate := boardings_growth/get(eval(prv.b))
       ][, boardings_capita := get(eval(max.b))/get(eval(max.p))]
  b.rank.cols <- str_subset(colnames(table), "^boardings")
  rank.cols <- c(max.p, max.b, b.rank.cols)
  b.rank.cols.date <- c(max.p, max.b, paste(b.rank.cols, c(rep(yrs.b, 2), str_extract(max.b, "\\d+")), sep = "_" ))
  rank.colsnm <- paste0("rank_", b.rank.cols.date)
  tt <- table[, (rank.colsnm) := mapply(function(x) rank(-.SD[[x]]), rank.cols, SIMPLIFY = F)]
}


# Summarise: Peer Urban Areas (UZA) ---------------------------------------


# top XX uza boardings of most recent full year
find.peer.uza <- function(valuetype = c("UPT", "VRM", "VRH"), n) {
  dtelm <- read.dt(working.dbtable.ntd)
  curr.date <- max(dtelm$Date)
  if (month(curr.date) < 12) {
    prev.yr <- year(curr.date) - 1
    st.prev.yr <- paste0(year(curr.date)-1, "-01-01")
    end.prev.yr <- paste0(year(curr.date)-1, "-12-01")
    dt <- copy(dtelm)
    dtsub <- dt[ValueType == valuetype & (Date %between% c(st.prev.yr, end.prev.yr)), ]
  } else {
    st.yr <- paste0(year(curr.date), "-01-01")
    dt <- copy(dtelm)
    dtsub <- dt[ValueType == valuetype & (Date %between% c(st.yr, curr.date)), ]
  }
  dtagg <- dtsub[, lapply(.SD, sum), .SDcols = c("Value"), by = c("UrbanAreaName", "UrbanArea")]
  dtpeer <- dtagg[order(-Value)][1:n]
} 

create.dt.peer.uza.ytd <- function(valuetype, n) {
  dtelm <- read.dt(working.dbtable.ntd)
  curr.date <- max(dtelm$Date)
  curr.month <- month(curr.date)
  prev.yr <- paste0(year(curr.date)-1, "-01-01")
  
  dt <- dtelm[ValueType == valuetype & Active == "Active", ][Date %between% c(prev.yr, curr.date),][month(Date) %in% c(1:curr.month)]
  dtsum <- dt[, lapply(.SD, sum), .SDcols = c("Value"), by = .(UrbanArea, UrbanAreaName, year(Date))]
  dtpeer <- find.peer.uza(valuetype, n) # select those in top 25 uzas
  dtsum2 <- dtsum[UrbanArea %in% dtpeer$UrbanArea | UrbanAreaName %in% c("Bremerton, WA"), ]
  years <- paste0("yr", unique(dtsum2$year))
  dtc <- dcast.data.table(dtsum2, UrbanArea + UrbanAreaName ~ paste0("yr", year), value.var = "Value")
  dtc[, diff := get(eval(years[2]))-get(eval(years[1]))
      ][, share := diff/get(eval(years[1]))
        ][UrbanArea %in% c(14, 180), color := "TRUE" # 14 = Seattle, 180 = Bremerton
          ][!(UrbanArea %in% c(14, 180)), color := "FALSE"
            ][, ytd := month.abb[curr.month]]
  return(dtc)
}

# create.dotplot.peer.uza.ytd <- function(table) {
#   yrcols <- colnames(table)[str_which(colnames(table), "^yr")]
#   years <- str_extract(yrcols, "\\d+")
# 
#   g <- ggplot(table) +
#     geom_point(aes(x = diff, y = share, color = color)) +
#     geom_text_repel(aes(x = diff, y = share, label = UrbanAreaName, color = color), size = 2.5) +
#     scale_color_manual(values = c("TRUE" = "purple", "FALSE" = "#3d3d44")) +
#     geom_hline(aes(yintercept = 0), alpha = .2) +
#     geom_vline(aes(xintercept = 0), alpha = .2) +
#     scale_y_continuous(labels=scales::percent) +
#     scale_x_continuous(limits = c(min(table$diff) + -5000000, max(table$diff) + 20000000), labels=scales::comma) +
#     ylab("") +
#     xlab(paste0("Growth YTD (Jan-", unique(table$ytd), ") ", years[1], "-", years[2])) +
#     theme(legend.position = "none",
#           axis.title.x = element_text(size = 10, margin = margin(t = 20, r = 0, b = 0, l = 0)))
#   g
# }
# 
# dtc <- create.dt.peer.uza.ytd("UPT", 15)
# g <- create.dotplot.peer.uza.ytd(dtc)
# print(g)
