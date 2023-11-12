#' @import DBI yaml
#' @export
open_mariadb <- function(dbinfo, dbname = 1) {
  if (is.numeric(dbname)) dbname <- dbinfo$dbname[dbname]
  bold(ok(sprintf("[info] opening db: %s", dbname)))

  # dev = odbc::odbc()
  # odbc::odbcListDrivers()
  dev <- RMariaDB::MariaDB()
  port <- 3306
  if (!is.null(dbinfo$port)) port <- dbinfo$port

  con <- dbConnect(dev,
    load_data_local_infile = TRUE,
    host = dbinfo$host, port = port,
    user = dbinfo$user,
    password = as.character(dbinfo$password),
    database = dbname,
    dbname = dbname
  )

  str(dbGetInfo(con))
  return(con)
}

#' @export
db_info <- function(con) {
  str(dbGetInfo(con))
}

#' @export
read_met_daily <- function(con, site0, table = "China_Mete2000_daily_1951_2019") {
  .tbl <- tbl(con, table)
  t <- system.time({
    d_day <- filter(.tbl, site == site0) |> collect()
  })
  print(t)
  d_day
}
