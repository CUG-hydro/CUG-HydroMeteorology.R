source("scripts/main_pkgs.R")


#############################moistrue_flux#######################################
fid <- nc_open("OUTPUT/ivt_anomaly_mean.nc")
warm_ivtu <- ncvar_get(fid, "warm_ivtu")
warm_ivtv <- ncvar_get(fid, "warm_ivtv")
cold_ivtu <- ncvar_get(fid, "cold_ivtu")
cold_ivtv <- ncvar_get(fid, "cold_ivtv")
ivtu <- warm_ivtu - cold_ivtu
ivtv <- warm_ivtv - cold_ivtv
lon <- ncvar_get(fid, "lon")
lat <- ncvar_get(fid, "lat")
nc_close(fid)
rm(cold_ivtu, cold_ivtv, warm_ivtu, warm_ivtv)

lonlat <- expand.grid(lon = lon, lat = lat)
ivt <- cbind(lonlat, u = as.vector(ivtu), v = as.vector(ivtv))
ivt$wind <- sqrt((ivt$u)^2+(ivt$v)^2)

######convert 2.5 *2.5 grid box to 5 * 5 ones
indlon <- which(ivt$lon %% 5 == 0)
indlat <- which(ivt$lat %% 5 == 0)
ind <- intersect(indlon,indlat)
ivt <- ivt[ind,]

###############################precipitable water###################################
ncdata1 <- nc_open("OUTPUT/pw_minus.nc")
var1 <- ncvar_get(ncdata1, "pw")
var1p <- ncvar_get(ncdata1, "p")
lon1 <- ncvar_get(ncdata1, "lon")
lat1 <- ncvar_get(ncdata1, "lat")
nc_close(ncdata1)

lonlat1 <- expand.grid(lon = lon1, lat = lat1)
var1 <- as.vector(var1)
var1p <- as.vector(var1p)
var1 <- cbind(lonlat1, var1, var1p)
colnames(var1) = c("lon", "lat", "value","p")
range(var1$value)
#############################fake_legend################################################
# get colors
col <- colorRampPalette(brewer.pal(11, "RdYlBu")[c(2:5, 7:10)])
rcol <- col(22)[c(1:10, 13:22)]
show_col(rcol)

rcol1 <- c(rcol[1:10], "white", rcol[11:20], "white")
show_col(rcol1)

lgd <- make_legend()

####################################main_figures###################################################
brks = c(-9, -3.5, -3, -2.5, -2, -1.75, -1.5, -1.25, -1, -0.75, -0.5,
    0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.5, 3, 3.5, 9)
labs_lon = c("", "30°E", "60°E", "90°E", "120°E", "150°E", "180°",
    "150°W", "120°W", "90°W", "60°W", "30°W", "")
labs_lat = c("", "75°S", "60°S", "45°S", "30°S", "15°S", "0°",
    "15°N", "30°N", "45°N", "60°N", "75°N", "")
fig.a <- show_VaporTransport(data = var1, uv = 2,
    breaks = brks,
    col = rev(rcol1),
    lag1 = 30, lag2 = 15,
    labels1 = labs_lon, labels2 = labs_lat,
    labb = "(a) MF and PW in East Aisa",
    lons1 = 70, lons2 = 210, lats1 = 2.5, lats2 = 70)
fig.a
fig.b <- show_VaporTransport(data = var1, uv = 3,
    breaks = brks, col = rev(c(rcol1)),
    lag1 = 30, lag2 = 15,
    labels1 = labs_lon, labels2 = labs_lat,
    labb = "(b) MF and PW in North America",
    lons1 = 210, lons2 = 350, lats1 = 2.5, lats2 = 70)
fig.b
fig.c <- show_VaporTransport(data = var1, uv = 0.9,
    breaks = brks, col = rev(c(rcol1)),
    lag1 = 30, lag2 = 15,
    labels1 = labs_lon, labels2 = labs_lat,
    labb = "(c) MF and PW in Australia",
    lons1 = 90, lons2 = 170, lats1 = -50, lats2 = -2.5)
fig.c

fig.all <- ggdraw() +
    draw_plot(fig.a,  0.002, 0.62, width = 0.44,  height =   0.33) +
    draw_plot(lgd, 0.2445,  0.654, width = 0.43,  height =  0.259)+
    draw_plot(fig.b,  0.002, 0.328, width = 0.44,  height =  0.33) +
    draw_plot(lgd,   0.2445,  0.362, width = 0.43,  height =  0.259)+
    draw_plot(  fig.c,  0.002,  -0.065, width = 0.44, height =  0.5) +
    draw_plot(lgd,   0.2445,  0.04, width = 0.43,  height =  0.289)

ggsave(fig.all, filename = 'ivt_pw.pdf',
    width = 800*0.3, height = 800*0.3, units = 'mm', dpi = 600)
