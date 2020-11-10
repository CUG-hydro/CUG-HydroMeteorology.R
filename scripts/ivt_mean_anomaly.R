ncdata <- nc_open(".\\original data\\NCEP-NCAR\\uwnd.mon.mean.nc")
lon <- ncvar_get(ncdata,"lon")
lat <- ncvar_get(ncdata,"lat")
time <- ncvar_get(ncdata,"time")
level <- ncvar_get(ncdata,"level")
uwind <- ncvar_get(ncdata,"uwnd")
nc_close(ncdata)
uwind1 <- uwind[,,1:8,]
rm(uwind)

ncdata1 <- nc_open(".\\original data\\NCEP-NCAR\\shum.mon.mean.nc")
shum <- ncvar_get(ncdata1,"shum")
level1 <- ncvar_get(ncdata1,"level")
nc_close(ncdata1)
shum <- shum[,,1:8,]/1000

ncdata2 <- nc_open(".\\original data\\NCEP-NCAR\\vwnd.mon.mean.nc")
vwind <- ncvar_get(ncdata2,"vwnd")
nc_close(ncdata2)
vwind1 <- vwind[,,1:8,]
rm(vwind)

ivt_u <- (uwind1[,,1,]*shum[,,1,]+uwind1[,,2,]*shum[,,2,])*37.5 + 
        (uwind1[,,2,]*shum[,,2,]+uwind1[,,3,]*shum[,,3,])*37.5 +
        (uwind1[,,3,]*shum[,,3,]+uwind1[,,4,]*shum[,,4,])*75 +
        (uwind1[,,4,]*shum[,,4,]+uwind1[,,5,]*shum[,,5,])*50 + 
        (uwind1[,,5,]*shum[,,5,]+uwind1[,,6,]*shum[,,6,])*50+
        (uwind1[,,6,]*shum[,,7,]+uwind1[,,7,]*shum[,,7,])*50+
        (uwind1[,,7,]*shum[,,8,]+uwind1[,,8,]*shum[,,8,])*50
ivt_v <- (vwind1[,,1,]*shum[,,1,]+vwind1[,,2,]*shum[,,2,])*37.5 + 
        (vwind1[,,2,]*shum[,,2,]+vwind1[,,3,]*shum[,,3,])*37.5 +
        (vwind1[,,3,]*shum[,,3,]+vwind1[,,4,]*shum[,,4,])*75 +
        (vwind1[,,4,]*shum[,,4,]+vwind1[,,5,]*shum[,,5,])*50 + 
        (vwind1[,,5,]*shum[,,5,]+vwind1[,,6,]*shum[,,6,])*50+
        (vwind1[,,6,]*shum[,,6,]+vwind1[,,7,]*shum[,,7,])*50+
        (vwind1[,,7,]*shum[,,7,]+vwind1[,,8,]*shum[,,8,])*50
#------------------------ivt_u_north-------------------------------------------
date <- seq(as.Date('1948-1-1'), as.Date('2018-12-1'), by = 'month')[1:852]
year <- substring(date, 1, 4) %>% as.numeric()
month <- substring(date, 6, 7) %>% as.numeric()
loc1 <- which(month %in% 6:11)

uwind.mean <- apply(ivt_u, c(1,2), function(x){
        aggregate(x[loc1], by = list(year[loc1]), mean, na.rm = T)$x
})

year <- unique(year[loc1]) 

phase <- read.csv(".\\original data\\ENSO\\EL year.csv",stringsAsFactors = F)

uwnd.jpn <- apply(uwind.mean, c(2,3), function(x){
        loc <- which(year %in% 1951:1980)
        x - mean(x[loc], na.rm = T)
})

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
date <- seq(as.Date('1948-1-1'), as.Date('2018-12-1'), by = 'month')[1:852]
year <- substring(date, 1, 4) %>% as.numeric()
month <- substring(date, 6, 7) %>% as.numeric()
loc1 <- which(month %in% 6:11)

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
         outname = ".\\outputted data\\IVT\\ivt_anomaly_mean.nc")
