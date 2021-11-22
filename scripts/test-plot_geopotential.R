library(metR)
data(geopotential)
geopotential <- data.table::copy(geopotential)
geopotential[date == date[1], c("u", "v") := GeostrophicWind(gh, lon, lat)]

library("rnaturalearth")
library("rnaturalearthdata")


{
    worldmap <- map_data("world", wrap = c(0, 360))
    # worldmap[which(worldmap$long < 0), 1] <- worldmap$long[which(worldmap$long < 0)] + 360

    ggplot() +
        geom_path(data = worldmap,
                  # inherit.aes = F,
          mapping = aes(x = long, y = lat, group = group), color = "#999999")
}

library(ggplot2)
# , skip = 2
ggplot(geopotential[date == date[1]], aes(lon, lat)) +
    geom_path(data = worldmap,
              # inherit.aes = F,
              mapping = aes(x = long, y = lat, group = group), color = "#999999")

{
    p <- ggplot(geopotential[date == date[1]], aes(lon, lat)) +
        geom_polygon(data = worldmap, aes(x = long, y = lat, group = group),
                     fill = "grey85", color = "black", size = 0.2) +
        # geom_path(data = worldmap, inherit.aes = F,
        #           mapping = aes(x = long, y = lat, group = group),color = "#999999", size = 0.2)+
        geom_contour(aes(z = gh, color = after_stat(factor(level)))) +
        geom_label_contour(aes(z = gh)) +
        geom_vector(aes(dx = u, dy = v), skip = 2, size = 0.3) +
        scale_mag() +
        labs(color = "level", x = NULL, y = NULL) +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.2)) +
        coord_cartesian(expand = FALSE) +
        scale_x_continuous(breaks = seq(0, 360, 60))
    write_fig(p, "a.pdf")
}
