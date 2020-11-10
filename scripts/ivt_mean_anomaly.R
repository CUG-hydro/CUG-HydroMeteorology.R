source('scripts/main_pkgs.R')


# 1. 读取数据
read_nc <- function(file, varname, count = c(-1, -1, 8, -1), ...) {
    fid <- nc_open(file)
    on.exit(nc_close(fid))
    if (is.numeric(varname)) varname <- names(fid$var)[varname]
    ncvar_get(fid, varname, ..., count = count)
}

{
    levels = c(1000, 925, 850, 700, 600, 500, 400, 300, 250, 200, 150, 100, 70, 50, 30, 20, 10)
    # dates: 1948-01:2020-03
    vwind <- read_nc("data-raw/NCEP-NCAR/vwnd.mon.mean.nc", 1)
    uwind <- read_nc("data-raw/NCEP-NCAR/uwnd.mon.mean.nc", 1)
    shum <- read_nc("data-raw/NCEP-NCAR/shum.mon.mean.nc", 1) / 1000 # g/kg to kg/kg, only available in 1000hPa to 300hPa

    fid <- nc_open("data-raw/NCEP-NCAR/uwnd.mon.mean.nc")
    lon <- ncvar_get(fid, "lon")
    lat <- ncvar_get(fid, "lat")
    time <- ncvar_get(fid, "time")
    nc_close(fid)
}

levs <- levels[1:8] # selected levels, 1000hPa - 300hPa
ivt <- IVT(uwind, vwind, shum, levs)

#------------------------ivt_u_north-------------------------------------------
dates  <- seq(as.Date('1948-1-1'), as.Date('2018-12-01'), by = 'month')
nmonth <- length(dates)
years  <- year(dates)
months <- month(dates)
ind_season <- which(months %in% 6:11) # summer and autumn

# 每年的6-11月平均
uwind.mean = apply_3d(ivt_u[, , ind_season], by = years[ind_season])

year <- unique(year[loc1])
phase <- fread("data-raw/EL_year.csv")

## 计算anomaly
ind_ref = match(1951:1980, unique(years))
u_ref = apply_3d(uwind.mean[,,ind_ref]) %>% set_dim(c(144, 73, 1))
uwnd.jpn = uwind.mean - u_ref

uwnd.jpn <- apply(uwind.mean, c(2,3), function(x){
    loc <- which(year %in% 1951:1980)
    x - mean(x[loc], na.rm = T)
})

# dims = uwind.mean,
uwind.mean = arr
class(uwind.mean) <- c("array")
uwind.mean - u_ref

class(uwind.mean)
`-.array`(uwind.mean, u_ref)
(uwind.mean - u_ref)


uwnd.jpn <- uwnd.jpn[4:71,,]
ivt_u.wn <- apply(uwnd.jpn,c(2,3),function(x){
    loc <- which(phase$NH1 == "warm")
    mean <- mean(x[loc],na.rm = T)
})


ivt_u.cn <- apply(uwnd.jpn, c(2,3), function(x){
        loc <- which(phase$NH1 == "cold")
        mean <-mean(x[loc],na.rm = T)
})

rm(date,year,month,loc1,uwind.mean,uwnd.jpn)
#-----------------ivt_v_north-------------------------------------------
vwind.mean <- apply(ivt_v, c(1,2), function(x){
    aggregate(x[loc1], by = list(year[loc1]), mean, na.rm = T)$x
})

year <- unique(year[loc1])
vwnd.jpn <- apply(vwind.mean, c(2,3), function(x){
    loc <- which(year %in% 1951:1980)
    x - mean(x[loc], na.rm = T)
})

vwnd.jpn <- vwnd.jpn[4:71,,]

ivt_v.wn <- apply(vwnd.jpn,c(2,3),function(x){
    loc <- which(phase$NH1 == "warm")
    mean <- mean(x[loc],na.rm = T)
})

ivt_v.cn <- apply(vwnd.jpn, c(2,3), function(x){
    loc <- which(phase$NH1 == "cold")
    mean <-mean(x[loc],na.rm = T)
})
rm(date,year,month,loc1,vwind.mean,vwnd.jpn)
#-------------------------ivt_u_south-------------------------------
date1 <- seq(as.Date('1948-1-1'), as.Date('2018-12-1'), by = 'month')[1:845]
year1 <- substring(date1, 1, 4) %>% as.numeric()
xx <- c(rep(1948,11),rep(1949:2017,12)[order(rep(1949:2017,12))],rep(2018,6))
year1 <- xx
month1 <- substring(date1, 6, 7) %>% as.numeric()
loc2 <- which(month1 %in% c(12,1,2,3,4,5))

uwind.mean1 <- apply(ivt_u, c(1,2), function(x){
        aggregate(x[loc2], by = list(year1[loc2]), mean, na.rm = T)$x
})

year1 <- unique(year1[loc2])

uwnd.jps <- apply(uwind.mean1, c(2,3), function(x){
        loc <- which(year1 %in% 1951:1980)
        x - mean(x[loc], na.rm = T)
})

uwnd.jps <- uwnd.jps[4:71,,]

ivt_u.ws <- apply(uwnd.jps,c(2,3),function(x){
        loc <- which(phase$SH1 == "warm")
        mean <- mean(x[loc],na.rm = T)
})

ivt_u.cs <- apply(uwnd.jps, c(2,3), function(x){
        loc <- which(phase$SH1 == "cold")
        mean <-mean(x[loc],na.rm = T)
})
rm(date1,year1,xx,month1,loc2,uwind.mean1,uwnd.jps)
#-----------------------ivt_v_south------------------------------------
date1 <- seq(as.Date('1948-1-1'), as.Date('2018-12-1'), by = 'month')[1:845]
year1 <- substring(date1, 1, 4) %>% as.numeric()
xx <- c(rep(1948,11),rep(1949:2017,12)[order(rep(1949:2017,12))],rep(2018,6))
year1 <- xx
month1 <- substring(date1, 6, 7) %>% as.numeric()
loc2 <- which(month1 %in% c(12,1,2,3,4,5))

vwind.mean1 <- apply(ivt_v, c(1,2), function(x){
        aggregate(x[loc2], by = list(year1[loc2]), mean, na.rm = T)$x
})

year1 <- unique(year1[loc2])

vwnd.jps <- apply(vwind.mean1, c(2,3), function(x){
        loc <- which(year1 %in% 1951:1980)
        x - mean(x[loc], na.rm = T)
})

vwnd.jps <- vwnd.jps[4:71,,]

ivt_v.ws <- apply(vwnd.jps,c(2,3),function(x){
        loc <- which(phase$SH1 == "warm")
        mean <- mean(x[loc],na.rm = T)
})

ivt_v.cs <- apply(vwnd.jps, c(2,3), function(x){
        loc <- which(phase$SH1 == "cold")
        mean <-mean(x[loc],na.rm = T)
})
rm(date1,xx,year1,month1,loc2,vwind.mean1,vwnd.jps)

################################save_as_a_nc_file#########################################
ivt_u.w <- ivt_u.wn
ivt_u.w[,37:73] <- ivt_u.ws[,37:73]
ivt_v.w <- ivt_v.wn
ivt_v.w[,37:73] <- ivt_v.ws[,37:73]
ivt_u.c <- ivt_u.cn
ivt_u.c[,37:73] <- ivt_u.cs[,37:73]
ivt_v.c <- ivt_v.cn
ivt_v.c[,37:73] <- ivt_v.cs[,37:73]
write_tc <- function(lon, lat, warmu,coldu,warmv,coldv, outname){
    lon <- ncdim_def(name = 'lon', units = 'longitude', vals = lon)
    lat <- ncdim_def(name = 'lat', units = 'latitude', vals = lat)
    var.warmu <- ncvar_def(name = 'warm_ivtu', units = 'kg/m/s', missval = NA,
                            dim = list(lon, lat), longname = 'ivtu during the warm period')
    var.coldu <- ncvar_def(name = 'cold_ivtu',  units = 'kg/m/s', missval = NA,
                            dim = list(lon, lat), longname = 'ivtu during the cold period')
    var.warmv <- ncvar_def(name = 'warm_ivtv', units = 'kg/m/s', missval = NA,
                            dim = list(lon, lat), longname = 'ivtv during the warm period')
    var.coldv <- ncvar_def(name = 'cold_ivtv',  units = 'kg/m/s', missval = NA,
                            dim = list(lon, lat), longname = 'ivtv during the cold period')
    nc <- nc_create(filename = outname,
                    force_v4 = TRUE,
                    vars = list(var.warmu, var.coldu, var.warmv, var.coldv))
    ncvar_put(nc = nc, varid = var.warmu, vals = warmu)
    ncvar_put(nc = nc, varid = var.coldu, vals = coldu)
    ncvar_put(nc = nc, varid = var.warmv, vals = warmv)
    ncvar_put(nc = nc, varid = var.coldv, vals = coldv)
    nc_close(nc)
}

write_tc(lon = lon, lat = lat, warmu = ivt_u.w, coldu = ivt_u.c, warmv = ivt_v.w, coldv = ivt_v.c,
         outname = "./outputted data/IVT/ivt_anomaly_mean.nc")
