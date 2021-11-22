
ncdata <- nc_open(".\\original data\\NCEP-NCAR\\pr_wtr.mon.mean.nc")
lon <- ncvar_get(ncdata, "lon")
lat <- ncvar_get(ncdata, "lat")
time <- ncvar_get(ncdata, "time")
pw <- ncvar_get(ncdata, "pr_wtr")
nc_close(ncdata)
unit <- str_extract(ncdata$dim$time$units, "[0-9]{4}.+0\\.0") # unit
time1 <- as.PCICt(x = unit, cal = "gregorian") + time * 3600 # good job
time1[1]

#------------------------pw_north-------------------------------------------
date <- seq(as.Date("1948-1-1"), as.Date("2018-12-1"), by = "month")[1:852]
year <- substring(date, 1, 4) %>% as.numeric()
month <- substring(date, 6, 7) %>% as.numeric()
loc1 <- which(month %in% 6:11)

pw.mean <- apply(pw, c(1, 2), function(x) {
    aggregate(x[loc1], by = list(year[loc1]), mean, na.rm = T)$x
})

years <- year(dates) %>% unique() # unique
phase <- fread("data-raw/EL_year.csv")
ind_warm <- phase[NH1 == "warm", match(year, years)]
ind_cold <- phase[NH1 == "cold", match(year, years)]

meann <- apply(pw.mean, c(2, 3), function(x) {
    phase <- phase[, 1:3]
    warm.year <- phase[which(phase$NH1 == "warm"), ]$year
    loc.warm <- which(1951:2018 %in% warm.year)
    cold.year <- phase[which(phase$NH1 == "cold"), ]$year
    loc.cold <- which(1951:2018 %in% cold.year)
    m <- (mean(x[loc.warm + 3]) - mean(x[loc.cold + 3])) # /mean(x[loc.cold+3])
    m
})

pn <- apply(pw.mean, c(2, 3), function(x) {
    phase <- phase[, 1:3]
    warm.year <- phase[which(phase$NH1 == "warm"), ]$year
    loc.warm <- which(1951:2018 %in% warm.year)
    cold.year <- phase[which(phase$NH1 == "cold"), ]$year
    loc.cold <- which(1951:2018 %in% cold.year)
    p <- t.test(x = x[loc.warm + 3], y = x[loc.cold + 3])$p.value
    p
})

#-------------------------pw_south-------------------------------
date1 <- seq(as.Date("1948-1-1"), as.Date("2018-12-1"), by = "month")[1:845]
year1 <- substring(date1, 1, 4) %>% as.numeric()
xx <- c(rep(1948, 11), rep(1949:2017, 12)[order(rep(1949:2017, 12))], rep(2018, 6))
year1 <- xx
month1 <- substring(date1, 6, 7) %>% as.numeric()
loc2 <- which(month1 %in% c(12, 1, 2, 3, 4, 5))

pw.mean1 <- apply(pw, c(1, 2), function(x) {
    aggregate(x[loc2], by = list(year1[loc2]), mean, na.rm = T)$x
})

means <- apply(pw.mean1, c(2, 3), function(x) {
    phase <- phase[, 1:3]
    warm.year <- phase[which(phase$SH1 == "warm"), ]$year
    loc.warm <- which(1951:2018 %in% warm.year)
    cold.year <- phase[which(phase$SH1 == "cold"), ]$year
    loc.cold <- which(1951:2018 %in% cold.year)
    m <- (mean(x[loc.warm + 3]) - mean(x[loc.cold + 3])) # /mean(x[loc.cold+3])
    m
})

ps <- apply(pw.mean1, c(2, 3), function(x) {
    lphase <- phase[, 1:3]
    warm.year <- phase[which(phase$SH1 == "warm"), ]$year
    loc.warm <- which(1951:2018 %in% warm.year)
    cold.year <- phase[which(phase$SH1 == "cold"), ]$year
    loc.cold <- which(1951:2018 %in% cold.year)
    p <- t.test(x = x[loc.warm + 3], y = x[loc.cold + 3])$p.value
    p
})

p <- pn
p[, 37:73] <- ps[, 37:73]
pw <- meann
pw[, 37:73] <- means[, 37:73]
write_tc <- function(lon, lat, pw, p, outname) {
    lon <- ncdim_def(name = "lon", units = "longitude", vals = lon)
    lat <- ncdim_def(name = "lat", units = "latitude", vals = lat)
    var.pw <- ncvar_def(
        name = "pw", units = "kg/m2", missval = NA,
        dim = list(lon, lat), longname = "pw - warm minus cold")
    var.p <- ncvar_def(
        name = "p", units = "", missval = NA,
        dim = list(lon, lat), longname = "p - warm and cold")
    nc <- nc_create(
        filename = outname,
        force_v4 = TRUE,
        vars = list(var.pw, var.p))
    ncvar_put(nc = nc, varid = var.pw, vals = pw)
    ncvar_put(nc = nc, varid = var.p, vals = p)
    nc_close(nc)
}

write_tc(lon = lon, lat = lat, pw = pw, p = p, outname = "OUTPUT/pw_minus.nc")
