source("scripts/main_pkgs.R")
library(purrr)

# 1. 读取数据
read_nc <- function(file, varname, count = c(-1, -1, 8, -1), ...) {
    fid <- nc_open(file)
    on.exit(nc_close(fid))
    if (is.numeric(varname)) varname <- names(fid$var)[varname]
    ncvar_get(fid, varname, ..., count = count)
}

{
    levels <- c(1000, 925, 850, 700, 600, 500, 400, 300, 250, 200, 150, 100, 70, 50, 30, 20, 10)
    # dates: 1948-01:2020-03
    vwind <- read_nc("data-raw/NCEP-NCAR/vwnd.mon.mean.nc", 1)
    uwind <- read_nc("data-raw/NCEP-NCAR/uwnd.mon.mean.nc", 1)

    # g/kg to kg/kg, only available in 1000hPa to 300hPa
    shum <- read_nc("data-raw/NCEP-NCAR/shum.mon.mean.nc", 1) / 1000

    file <- "data-raw/NCEP-NCAR/uwnd.mon.mean.nc"
    fid <- nc_open("data-raw/NCEP-NCAR/uwnd.mon.mean.nc")
    lon <- ncvar_get(fid, "lon")
    lat <- ncvar_get(fid, "lat")
    # time <- ncvar_get(fid, "time")
    dates <- nc_date(file)
    nc_close(fid)
}

# unit <- str_extract(ncdata$dim$time$units, "[0-9]{4}.+0\\.0") # unit
levs <- levels[1:8] # selected levels, 1000hPa - 200hPa
ivt <- IVT(uwind, vwind, shum, levs)
ivt$wind <- sqrt((ivt$u)^2 + (ivt$v)^2)

ncread("E:/Research/metehydro/CUG-MeteoroHydrology/data-raw/NCEP-NCAR/hgt.mon.mean.nc", "level")
hgt = read_nc("data-raw/NCEP-NCAR/hgt.mon.mean.nc", 1, count = c(-1, -1, 1, -1), start = c(1, 1, 6, 1)) # 500hpa
# brick(n)
## 挑选某一日期，进行绘图
time

i = which(format(dates) == make_date(2020, 7, 1))

loc = expand.grid(lon = lon, lat = lat) %>% data.table()
d = purrr::map(ivt, ~.x[,,i] %>% as.numeric) %>% as.data.table() %>%
    cbind(loc, ., hgt = as.numeric(hgt[,,i]/10)) %>%
    plyr::mutate(hgt_col = ifelse(hgt >= 588, "red", "blue"))

{

    ggplot(d, aes(lon, lat, z = hgt)) +
        # geom_raster(aes(fill = hgt))
        stat_contour_fill(breaks = brks) +
        geom_path(data = worldmap, inherit.aes = F,
                  mapping = aes(x = long, y = lat, group = group), color = "#999999") +
        geom_contour(breaks = brks) +
        geom_label_contour(breaks = brks, col = "red") +
    scale_fill_gradientn(colours = cols)
}

plot_ivt(d, 0.01, "2020 July", outfile = "a.pdf", arrow.len = 0.6, arrow.size = 0.15)
# library(lattice)
# levelplot(ivt_i$wind)
