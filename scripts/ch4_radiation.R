library(ncdf4)
library(terra)
library(nctools)

file = "I:/CUG-HydroMeteorology/data/ERA5L_2020_China_010deg.nc"

fid = nc_open(file)
vars = nc_info(file)

info = map_chr(fid$var, "longname") %>% data.table(var = names(.), name = .)



# 15:   slhf                              Surface latent heat flux
# 16:    ssr                           Surface net solar radiation
# 17:    str                         Surface net thermal radiation
# 18:     sp                                      Surface pressure
# 19:    sro                                        Surface runoff
# 20:   sshf                            Surface sensible heat flux
# 21:   ssrd                     Surface solar radiation downwards
# 22:   strd                   Surface thermal radiation downwards
# 23:      e                                           Evaporation
# 24:     tp                                   Total precipitation


library(lattice.layers)
library(sf2)
library(rcolors)

shp = "D:/Documents/ArcGIS/china/bou2_4p_ChinaProvince.shp"
prov = rgdal::readOGR(shp)
sp_layout = list("sp.lines", prov, first = F)

as_grid <- function(b) {
    as_SpatialPixelsDataFrame(raster::raster(crop(b, vect(shp))))
}
days = make_date(2020, 1:12) %>% days_in_month()

make_title <- function(label, cex = 1.4) {
    listk(label, fontfamily = "Times", cex = cex)
}
## Figure1: prcp
{
    b = rast(file, subds = "tp")*1000 # to mm #%>% mean()
    b = sum(b * days)
    g = as_grid(b)

    brks = c(seq(200, 2000, 100)) %>% c(-Inf, .,  Inf)
    cols = get_color(rcolors$amwg256)
    p = sp_plot(g, colors = cols,
                # brks = brks,
                sp.layout = sp_layout,
                main = make_title("Total Precipitation in 2020 (mm/y)"),
            unit = "mm/y", unit.adj = 0.65) +
        theme_lattice(layout.heights = list(main = 2, main.key.padding = 1))
    write_fig(p, "ERA5_prcp_masked.pdf", 9, 6)
}

## Figure2: Rn
# J / d
b_raw = rast(file, subds = "ssr") # to mm #%>% mean()
b_mean = mean(b_raw / 86400)
{
    g = as_grid(b_mean)
    brks = c(seq(200, 2000, 100)) %>% c(-Inf, .,  Inf)
    cols = get_color(rcolors$amwg256) # length(brks) - 1
    p = sp_plot(g, colors = cols, sp.layout = sp_layout,
                main = make_title("Surface net solar radiation in 2020 (Rn, W/m2)"),
                unit = "W/m2", unit.adj = 0.65) +
        theme_lattice(layout.heights = list(main = 2, main.key.padding = 1))
    write_fig(p, "ERA5_Rn_masked.pdf", 9, 6)
}

# convert into mm/d
b_mm = sum(b_raw/1e6/2.5 * days) # MJ/d, mm/d
{
    g = as_grid(b_mm)
    brks = c(seq(200, 2000, 100)) %>% c(-Inf, .,  Inf)
    cols = get_color(rcolors$amwg256) # length(brks) - 1
    p = sp_plot(g, colors = cols,
                sp.layout = sp_layout,
                main = make_title("Surface net solar radiation in 2020 (Rn, mm/y)"),
                unit = "mm/d", unit.adj = 0.65) +
        theme_lattice(layout.heights = list(main = 2, main.key.padding = 1))
    write_fig(p, "ERA5_Rn_mask_mm.pdf", 9, 6)
}

## Figure3: ET
{
    b = -rast(file, subds = "e")*1000 # to mm #%>% mean()
    b = sum(b * days)
    g = as_grid(b)

    brks = c(seq(200, 2000, 100)) %>% c(-Inf, .,  Inf)
    cols = get_color(rcolors$amwg256)
    p = sp_plot(g, colors = cols,
                # brks = brks,
                sp.layout = sp_layout,
                main = make_title("Total Evaporation in 2020 (mm/y)"),
                unit = "mm/y", unit.adj = 0.65) +
        theme_lattice(layout.heights = list(main = 2, main.key.padding = 1))
    write_fig(p, "ERA5_ET_masked.pdf", 9, 6)
}
