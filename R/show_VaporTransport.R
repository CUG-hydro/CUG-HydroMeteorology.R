make_legend <- function() {
    breaks1 <- c(-9, 
        sprintf("%0.2f", c(
            -3.5, -3, -2.5, -2, -1.75, -1.5, -1.25, -1, -0.75, -0.5,
            0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.5, 3, 3.5)), 9)
    nbrks = length(breaks1) - 1
    values1 <- seq(0, nbrks)
    group1 <- letters[values1 + 1]
    brks <- c(0, diff(values1))
    df_lgd <- data.frame(x = 1, y = breaks1, brks, group1, stringsAivtactors = FALSE)

    lgd <- ggplot(df_lgd, aes(x, y = brks, fill = group1)) +
        geom_col(width = 0.13, show.legend = F, col = "black") +
        annotate(
            geom = "text", x = 1.38, y = values1[c(2, 4, 6, 8, 10, 13, 15, 17, 19, 21)],
            label = breaks1[c(2, 4, 6, 8, 10, 13, 15, 17, 19, 21)], 
            size = 4.5, family = "serif", fontface = "bold") +
        scale_x_continuous(expand = c(2, 2)) +
        scale_fill_manual(values = rev(rcol1)) +
        ylab(label = "") +
        xlab(label = "") +
        # coord_flip() +
        theme_void()
}

show_VaporTransport <- function(data, breaks, col, uv,
                lag1, lag2, labels1, labels2,
                labb, lons1, lons2, lats1, lats2) {
    fig <- ggplot(data = data, aes(x = lon, y = lat)) +
        stat_contour_filled(
            data = data, inherit.aes = F, aes(x = lon, y = lat, z = value),
            breaks = breaks
        ) +
        scale_fill_manual(values = rev(col)) +
        geom_path(
            data = worldmap, inherit.aes = F,
            mapping = aes(x = long, y = lat, group = group), color = "#999999"
        ) +
        stat_subset(
            inherit.aes = F,
            aes(x = lon, y = lat, subset = p <= 0.1), color = "deeppink2",
            geom = "point", size = 0.7
        ) +
        geom_segment(
            data = ivt, inherit.aes = F,
            aes(x = lon, y = lat, xend = lon + u * uv, yend = lat + v * uv),
            arrow = arrow(length = unit(0.75, "mm"), type = "closed"), size = 0.25
        ) +
        # this not important
        coord_quickmap(xlim = c(lons1, lons2), ylim = c(lats1, lats2)) +
        scale_x_continuous(
            expand = c(0, 0), limits = c(0.5, 359.5),
            breaks = seq(0, 360, lag1),
            labels = labels1
        ) +
        scale_y_continuous(
            expand = c(0, 0), limits = c(-87.5, 87.5),
            breaks = seq(-90, 90, lag2), labels = labels2
        ) +
        labs(x = NULL, y = NULL, title = labb) +
        theme_bw() +
        theme(
            panel.border = element_rect(),
            panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text = element_text(size = 11.7, colour = "black", family = "serif", face = "bold"),
            legend.position = "none",
            plot.background = element_rect(fill = "white"),
            plot.title = element_text(hjust = 0.5, size = 14, family = "serif", face = "bold"),
            plot.margin = unit(c(0, 0, 0, 0), "mm")
        )
    fig
}
