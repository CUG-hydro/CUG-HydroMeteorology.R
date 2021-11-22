write_tc <- function(lon, lat, warmu, coldu, warmv, coldv, outname) {
    lon <- ncdim_def(name = "lon", units = "longitude", vals = lon)
    lat <- ncdim_def(name = "lat", units = "latitude", vals = lat)
    var.warmu <- ncvar_def(
        name = "warm_ivtu", units = "kg/m/s", missval = NA,
        dim = list(lon, lat), longname = "ivtu during the warm period")
    var.coldu <- ncvar_def(
        name = "cold_ivtu", units = "kg/m/s", missval = NA,
        dim = list(lon, lat), longname = "ivtu during the cold period")
    var.warmv <- ncvar_def(
        name = "warm_ivtv", units = "kg/m/s", missval = NA,
        dim = list(lon, lat), longname = "ivtv during the warm period")
    var.coldv <- ncvar_def(
        name = "cold_ivtv", units = "kg/m/s", missval = NA,
        dim = list(lon, lat), longname = "ivtv during the cold period")
    nc <- nc_create(
        filename = outname,
        force_v4 = TRUE,
        vars = list(var.warmu, var.coldu, var.warmv, var.coldv))

    ncvar_put(nc = nc, varid = var.warmu, vals = warmu)
    ncvar_put(nc = nc, varid = var.coldu, vals = coldu)
    ncvar_put(nc = nc, varid = var.warmv, vals = warmv)
    ncvar_put(nc = nc, varid = var.coldv, vals = coldv)
    nc_close(nc)
}
