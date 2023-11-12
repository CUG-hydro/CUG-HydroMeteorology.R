source("scripts/main_pkgs.R")
library(Ipaper)
library(plyr)
library(purrr)

modify_year <- function(date, year) make_date(year, month(date), day(date))
get_ampl <- function(x) range(x, na.rm = TRUE) %>% diff()

files = dir(path.mnt("N:/ChinaWater.R/OUTPUT/mete2000/2021_FinalVersion"), "*.csv", full.names = TRUE)
# 57494_wuhan
site = 57494
lst = map(files, ~fread(.x)[Station_Id_C == site, ])
df_hour = do.call(rbind, lst) %>% unique()

df_daily = df_hour[, lapply(.SD, mean, na.rm = TRUE), .(date(date)), .SDcols = colnames(df_hour)[-(1:2)]]
df_ampl = df_hour[, lapply(.SD, get_ampl), .(date(date)), .SDcols = colnames(df_hour)[-(1:2)]]

date_range = c(make_date(2021, 10, 1), df_hour$date %>% max() %>% as.Date())
date_range[2] = make_date(2021, 12, 31)
d_box = data.table(
    x = date_range[c(1:2, 2:1)],
    y = c(-Inf, -Inf, Inf, Inf))

dat_cli = d_cli[variable == "Tair_avg", ] %>%
    mutate(date = modify_year(date, 2021), mean = mean/10, sd = sd/10)

{
    theme_set(theme_gray(base_family = "Times"))
    p1 <- ggplot(df_daily[date >= make_date(2021, 1, 1), ], aes(date, TEM)) +
        geom_ribbon(data = d_box, aes(x, y,
                                      ymin = -Inf, ymax = Inf, xmin = date_range[1], xmax = date_range[2]),
                    fill = "blue", alpha = 0.2) +
        geom_ribbon(data = dat_cli, aes(date, mean, ymin = mean-sd, ymax = mean+sd), fill = "red", alpha = 0.2) +
        geom_line(data = dat_cli, aes(date, mean), color = "red") +
        geom_line(color = "black") +
        geom_smooth() +
        labs(y = "Daily Temperature (℃)", x = "Date") +
        coord_cartesian(ylim = c(1, 31)) +
        scale_x_date(limits = c(make_date(2021, 1, 1), date_range[2]))
    p2 <- ggplot(df_hour[date >= make_date(2021, 10, 1), ], aes(date, TEM)) +
        geom_point(size = 0.4) +
        geom_line() +
        geom_smooth() +
        labs(y = "Hourly Temperature (℃)", x = "Date")
    p3 <- ggplot(df_ampl[date >= make_date(2021, 10, 1), ], aes(date, TEM)) +
        geom_point(size = 0.4) +
        geom_line() +
        geom_smooth() +
        labs(y = "Diurnal Temperature Range (℃)", x = "Date")
    # p <- plot_grid(p1, p3, p2, rel_widths = c(10, 6, 6), nrow = 2)
    p = (p1 | (p3 / p2)) +
        plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") &
        theme(plot.tag.position = c(0.085, 0.97),
              plot.tag = element_text(size = 14, hjust = 0, vjust = 1))
    write_fig(p, "wuhan_2021CW_wholeYear.pdf")
    # write_fig(p, "wuhan_2021CW_winter.pdf")
}
