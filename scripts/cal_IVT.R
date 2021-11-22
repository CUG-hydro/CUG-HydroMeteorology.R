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

    file = "data-raw/NCEP-NCAR/uwnd.mon.mean.nc"
    fid <- nc_open("data-raw/NCEP-NCAR/uwnd.mon.mean.nc")
    lon <- ncvar_get(fid, "lon")
    lat <- ncvar_get(fid, "lat")
    # time <- ncvar_get(fid, "time")
    time = nc_date(file)
    nc_close(fid)
}


# unit <- str_extract(ncdata$dim$time$units, "[0-9]{4}.+0\\.0") # unit
levs <- levels[1:8] # selected levels, 1000hPa - 300hPa
ivt <- IVT(uwind, vwind, shum, levs)
ivt$wind <- sqrt((ivt$u)^2+(ivt$v)^2)

## 计算ivt的多年平均

dates  <- seq(as.Date('1948-1-1'), as.Date('2019-12-01'), by = 'month')
nmonth <- length(dates)
months <- month(dates)
ntime  <- length(dates)
#------------------------ivt_u_north-------------------------------------------
library(matrixStats)
library(purrr)
ivt_mean = map(ivt, ~ apply_3d(.x[,,1:ntime]))

# 补充lon, lat的数据
loc <- expand.grid(lon = lon, lat = lat) %>% data.table()
df <- cbind(loc, u = as.numeric(ivt_mean$u), v = as.numeric(ivt_mean$v)) %>%
    mutate(wind = sqrt(u^2 + v^2))


#' ggplot_ivt
#'
#' @param df with the column of `lon`, `lat`, `u`, `v`
#' @param uv_scalar
#'
#' global Environment: worldmap
#' @export
ggplot_ivt <- function(df, uv_scalar = 0.04, title = NULL, outfile = NULL) {
    cols_br <- alpha(rcolors::get_color("RdYlBu"))
    cols <- alpha(rcolors::get_color("Blues"))[-(1:2)] %>% c("red")

    cols <- colorRampPalette(brewer.pal(11, "RdYlBu")[c(2:5, 7:10)])
    rcols <- cols(22)[c(1:10, 13:22)]

    title <- ifelse(is.null(title), expression("IVT intensity (kg " * m^-1 ~ s^-1 * ")"), title)
    p <- ggplot(df, aes(lon, lat)) +
        geom_segment(
            # data = ivt, inherit.aes = F,
            aes(x = lon, y = lat, xend = lon + u * uv_scalar, yend = lat + v * uv_scalar, color = wind),
            arrow = arrow(length = unit(0.3, "mm"), type = "closed"), size = 0.2
        ) +
        geom_path(
            data = worldmap, inherit.aes = F,
            mapping = aes(x = long, y = lat, group = group), color = "#999999"
        ) +
        geom_hline(yintercept = 0, size = 0.5, color = "black", linetype = 2) +
        coord_cartesian(xlim = c(0, 360), ylim = c(-60, 90), expand = FALSE) +
        # geom_label(aes(x = 350, y = -55, label = "4.0 m/s\n"), size = 3, label.padding = unit(0.5, "lines")) +
        # scale_fill_gradientn(colours = cols) +
        scale_color_gradientn(colours = rcols) +
        scale_x_continuous(breaks = seq(0, 360, 60)) +
        scale_y_continuous(breaks = seq(-90, 90, 30)) +
        # labs(title = expression("Annual average IVT intensity (kg "*m^-1~s^-1*", 1000hPa-300hPa) during 1948-2019"),
        labs(title = title, color = "IVT", x = NULL, y = NULL) +
        theme_bw(base_size = 16) +
        theme(panel.border = element_rect(color = "black", fill = "transparent"),
              axis.ticks.length = unit(2, "mm"))
        # scale_y_continuous(expand = c(0, 0))
    p
    write_fig(p, outfile, 10, 5)
}

ggplot_ivt(df, uv_scalar = 0.06, 
    expression("Annual average IVT intensity (kg " * m^-1 ~ s^-1 * ", 1000hPa-300hPa) during 1948-2019"),
    "Annual average IVT intensity (1000-300hPa).pdf")




years <- year(dates) %>% unique() # unique
phase <- fread("data-raw/EL_year.csv")
ind_warm <- phase[NH1 == "warm", match(year, years)]
ind_cold <- phase[NH1 == "cold", match(year, years)]
ind_ref <- match(1951:1980, years)

## IVT anomaly in the North Hemisphere -------------------------------------
ind_season <- which(months %in% 6:11) # summer and autumn

# horizontal
uwind.mean <- apply_3d(ivt$u[, , ind_season], by = year(dates[ind_season])) # 每年的6-11月平均
uwnd.jpn <- get_anomaly(uwind.mean, ind_ref) # 取1951-2018年数据
ivt_u.wn <- apply_3d(uwnd.jpn[, , ind_warm])
ivt_u.cn <- apply_3d(uwnd.jpn[, , ind_cold])

# vertical
vwind.mean <- apply_3d(ivt$v[, , ind_season], by = year(dates[ind_season])) # 每年的6-11月平均
vwnd.jpn <- get_anomaly(vwind.mean, ind_ref) # 取1951-2018年数据
ivt_v.wn <- apply_3d(vwnd.jpn[, , ind_warm])
ivt_v.cn <- apply_3d(vwnd.jpn[, , ind_cold])
