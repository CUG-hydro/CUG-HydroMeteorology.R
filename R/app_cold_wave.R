coldwave_tidy_hourly <- function(df_hour) {
  d2023_hour <- df_hour %>%
    select(date, starts_with(c("TEM", "RHU", "WIN_S_Avg_2mi"))) %>%
    data.table() %>%
    mutate(date = date + dhours(8))

  d2023_day <- d2023_hour[, .(date, TEM)] %>%
    .[, map(.SD, mean), date(date)]

  list(hour = d2023_hour, day = d2023_day)
}

cal_clim <- function(d_day) {
  data = d_day |>
    data.table() %>%
    select(date, U2 = WIN_Avg, starts_with(c("Tair", "RH"))) %>%
    dplyr::mutate(across(U2:Tair_min, \(x) x / 10))
  
  data_clim <- data[date >= make_date(1981) & date <= make_date(2010, 12, 31)]
  clim_mean <- data_clim[, map(.SD, mean, na.rm = TRUE), .(md = format(date, "%m%d"))]
  clim_sd <- data_clim[, map(.SD, sd, na.rm = TRUE), .(md = format(date, "%m%d"))]

  clim <- list(mean = clim_mean, sd = clim_sd) %>%
    map(~ melt(.x, id.vars = "md")) |>
    melt_list("type") |>
    dcast(md + variable ~ type, value.var = "value") %>%
    mutate(date = convert_md2date(md, 2023)) %>%
    .[!is.na(date), ] %>%
    select(-md) %>%
    relocate(date)
  clim
}

#' coldwave_draw_main
#'
#' @param clim data table, with columns: date, variable, mean, sd
#' @param dat_day data.table with columns: date, TEM
#' @export
coldwave_draw_main <- function(
    dat_day, clim,
    time_beg = make_datetime(2023, 8, 1),
    time_end = make_datetime(2023, 11, 15)) {
  
  date_beg <- as.Date(time_beg)
  date_end <- as.Date(time_end)

  col_clim <- "red"
  p1 <- ggplot(clim[variable == "Tair_avg"], aes(date, mean)) +
    geom_rect(
      data = data.table(xmin = date_beg), aes(x = NULL, y = NULL),
      xmin = date_beg, xmax = date_end, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.1
    ) +
    geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.2, fill = "red") + # 气候态置信区间
    geom_line(color = col_clim, linewidth = 1.5) +
    # 2023年气温
    geom_line(data = dat_day, aes(date, TEM), color = "black") +
    geom_smooth(data = dat_day, aes(date, TEM)) +
    theme_gray(base_family = "Times", base_size = 12) +
    labs(y = glue("Daily mean air Temperature ({degc})"), x = NULL)

  p2 <- p1 +
    theme(
      plot.margin = margin(l = -5),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    scale_x_date(limits = c(date_beg, date_end), date_minor_breaks = "2 month") + labs(y = NULL)
  p1 <- p1 +
    annotate_richtext_npc(
      x = 0.01, y = 0.99, "2023",
      size = 10, hjust = 0, vjust = 1, color = "blue"
    )
  p1 + p2 + plot_layout(widths = c(3.5, 1))
}

#' draw_anorm
#'
#' @param data_anorm data.table with columns of `date`, `mean`, `TEM`
coldwave_draw_anorm <- function(
    data_anorm,
    time_beg = make_datetime(2023, 8, 1),
    time_end = make_datetime(2023, 11, 15)) {
  data_anorm %<>% mutate(anom = TEM - mean, anom_mov = movmean(anom, 7))

  date_beg <- as.Date(time_beg)
  date_end <- as.Date(time_end)

  p <- ggplot(data_anorm, aes(date, anom)) +
    geom_line(linewidth = 0.6, linetype = 1) +
    geom_line(aes(y = anom_mov, color = "15day movmean"), linewidth = 1.2, show.legend = TRUE) +
    geom_abline(slope = 0, intercept = 0, linetype = 1) +
    geom_rect(
      data = data.table(x = date_beg),
      aes(x = NULL, y = NULL),
      xmin = make_date(2023, 11, 1), xmax = make_date(2023, 11, 15), ymin = -Inf, ymax = Inf,
      fill = "blue", alpha = 0.1
    ) +
    scale_x_date(date_minor_breaks = "1 month", limits = c(date_beg, date_end), expand = c(0, 0)) +
    theme_gray(base_size = 12) +
    theme(
      legend.position = c(0, 1), legend.justification = c(0, 1),
      legend.background = element_rect(fill = "transparent", colour = NA)
    ) +
    labs(y = "Anomaly of daily Tmean", x = NULL, colour = NULL)
  p
}

#' draw_DRT
#'
#' @param dat_hour data.table with columns of `date`, `TEM`
#' @import data.table
#' @export
coldwave_draw_DRT <- function(
    dat_hour,
    time_beg = make_datetime(2023, 8, 1),
    time_end = make_datetime(2023, 11, 15),
    ...,
    lims_Tair = c(5, 40),
    lims_DRT = c(0, 16)) {
  p1 <- ggplot(dat_hour, aes(date, TEM)) +
    geom_line() +
    geom_rect(
      data = data.table(x = time_beg),
      aes(x = NULL, y = NULL),
      xmin = make_datetime(2023, 11, 1),
      xmax = make_datetime(2023, 11, 15), ymax = Inf, ymin = -Inf,
      fill = "blue", alpha = 0.1
    ) +
    theme(
      plot.margin = margin(b = 0),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
    ) +
    labs(y = "3hourly Temperature", x = NULL) +
    coord_cartesian(ylim = lims_Tair, clip = "on") +
    scale_x_datetime(limits = c(time_beg, time_end), expand = c(0, 0))

  d_range <- dat_hour[, .(range = get_ampl(TEM)), .(date(date))]
  p2 <- ggplot(d_range, aes(date, range)) +
    geom_line() +
    scale_y_continuous(limits = lims_DRT) +
    scale_x_date(limits = c(as.Date(time_beg), as.Date(time_end)), expand = c(0, 0)) +
    labs(y = "Dirual Temperature Range", x = NULL)
  p <- p2 / p1
}

date <- as.Date

rm_xticks <- function() {
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
}

rm_yticks <- function() {
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
}

convert_md2date <- function(md, year = 2023) {
  str <- sprintf("%d%s", year, md)
  as.Date(str, "%Y%m%d")
}

get_ampl <- function(x) range(x, na.rm = TRUE) %>% diff()

# modify_year <- function(date, year) make_date(year, month(date), day(date))

#' @importFrom Ipaper degc
#' @export
coldwave_flame <- function(data) {
  p = ggplot(data = data, aes(x = date)) +
    geom_flame(aes(y = thresh_clim_year, y2 = temp), fill = "steelblue3", show.legend = F) +
    geom_line(aes(y = temp, colour = "temp")) +
    geom_line(aes(y = thresh_clim_year, colour = "thresh"), size = 1.0) +
    # geom_line(aes(y = seas_clim_year, colour = "seas"), size = 1.2) +
    scale_colour_manual(
      name = "Line Colour",
      values = c("temp" = "black", "thresh" = "forestgreen", "seas" = "grey80")
    ) +
    labs(y = glue("Temperature ({degc})"), x = NULL)
  p
}

theme_set(theme_gray(base_family = "Times", base_size = 12))
