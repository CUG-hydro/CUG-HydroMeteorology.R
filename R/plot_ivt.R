#' ggplot_ivt
#'
#' @param df with the column of `lon`, `lat`, `u`, `v` and `wind`
#' @param uv_scalar
#'
#' global Environment: worldmap
#' @import data.table
#' @export
plot_ivt <- function(df, uv_scalar = 0.04, title = NULL, 
    arrow.len = 0.3, arrow.size = 0.2, 
    outfile = NULL) 
{
    brks = c(-Inf, seq(500, 600, 4), Inf)
    nbrk = length(brks) - 1
    cols = get_color("amwg256", 12)[1:10]
    cols_hgt = colorRampPalette(cols)(nbrk)

    brks_ivt = c(-Inf, seq(0, 500, 50), Inf)
    # cols_br <- alpha(rcolors::get_color("RdYlBu"))
    # cols <- alpha(rcolors::get_color("Blues"))[-(1:2)] %>% c("red")
    cols_ivt <- colorRampPalette(brewer.pal(11, "RdYlBu")[c(2:5, 7:10)])(22)[c(1:10, 13:22)]
    cols_ivt <- colorRampPalette(cols_ivt)(length(brks_ivt) - 1)
    df$ivt_col = cols_ivt[cut(df$wind, brks_ivt)]

    title <- ifelse(is.null(title), expression("IVT intensity (kg " * m^-1 ~ s^-1 * ")"), title)
    p <- ggplot(df, aes(lon, lat, z = hgt)) +
        # stat_contour_fill(breaks = brks) + 
        geom_path(data = worldmap, inherit.aes = F,
            mapping = aes(x = long, y = lat, group = group), color = "#999999", size = 0.5) +
        geom_contour(data = df, breaks = brks, size = 0.2, color = "black") +
        # geom_contour(data = df[hgt < 588], breaks = brks, size = 0.2, color = "blue") +
        geom_label_contour(breaks = brks, col = "red") +
        geom_segment(
            # data = ivt, inherit.aes = F,
            aes(x = lon, y = lat, xend = lon + u * uv_scalar, yend = lat + v * uv_scalar, color = cut(wind, brks_ivt)),
            arrow = arrow(length = unit(arrow.len, "mm"), type = "open"), size = arrow.size
        ) +
        geom_hline(yintercept = 0, size = 0.5, color = "black", linetype = 2) +
        # geom_label(aes(x = 350, y = -55, label = "4.0 m/s\n"), size = 3, label.padding = unit(0.5, "lines")) +
        scale_fill_gradientn(colours = cols_hgt) +
        # scale_color_gradientn(colours = rcols) +
        scale_x_continuous(breaks = seq(0, 360, 60)) +
        scale_y_continuous(breaks = seq(-90, 90, 30)) +
        # labs(title = expression("Annual average IVT intensity (kg "*m^-1~s^-1*", 1000hPa-300hPa) during 1948-2019"),
        labs(title = title, color = "IVT", x = NULL, y = NULL) +
        theme_bw(base_size = 16) +
        theme(panel.border = element_rect(color = "black", fill = "transparent"),
              axis.ticks.length = unit(2, "mm")) + 
        coord_cartesian(xlim = c(0, 360), ylim = c(-60, 90), expand = FALSE)
        # coord_map(projection = "robinson", xlim = c(0, 360), ylim = c(-60, 90), expand = FALSE)
        # scale_y_continuous(expand = c(0, 0))
    if (is.null(outfile)) p else write_fig(p, outfile, 10, 5)
}
