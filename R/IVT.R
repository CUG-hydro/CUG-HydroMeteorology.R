#' Integrated Vapor Transport
#'
#' @param uwind,vwind horizontal and vertical wind (m/s), in the dimension of `[nlat, nlon, nlev, ntime]`
#' @param shum specific humidity (kg/kg), in the same dimension of `[nlat, nlon, nlev, ntime]`
#' @param levs air pressure (hPa), a numberic vector with the length of `nvel`
#'
#' @return IVT (kg m-1 s-1)
#' - IVT_u: horizontal
#' - IVT_t: vertical
#' @export
IVT <- function(uwind, vwind, shum, levs) {
    IVT_u <- IVT_v <- 0
    g <- 9.80065 # gravity constant

    delta_Pa <- abs(diff(levs)) * 100 # hPa to Pa
    for (i in seq(1, length(levs) - 1)) {
        IVT_u <- IVT_u + delta_Pa[i] / g / 2 * (uwind[, , i, ] * shum[, , i, ] + uwind[, , i + 1, ] * shum[, , i + 1, ])
        IVT_v <- IVT_v + delta_Pa[i] / g / 2 * (vwind[, , i, ] * shum[, , i, ] + vwind[, , i + 1, ] * shum[, , i + 1, ])
    }
    list(u = IVT_u, v = IVT_v)
}
