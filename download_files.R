download.ntd <- function(dir, month, year) {
  url <- "https://www.transit.dot.gov/sites/fta.dot.gov/files/"
  url.file <- paste0(url, month, "%20", year, "%20Adjusted%20Database.xlsx")
  local.file <- file.path(dir, paste0("ntd-monthly-", month, year, '.xlsx'))
  download.file(url.file, local.file, mode="wb")
  return(local.file)
}


