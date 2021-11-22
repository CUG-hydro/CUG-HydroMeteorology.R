source("scripts/main_pkgs.R")

df_raw = fread("data-raw/processed_57494_武汉.csv") %>%
    select(!contains(c("_D_", "Prcp_20-08", "Prcp_02"))) %>%
    dplyr::rename(Prcp = `Prcp_20-20`)
df = df_raw %>% melt(c("site", "date"))

# df_raw[, .(date, Tair = Tair_avg/10)]
# 02-29 will be NA, and removed
d_cli = df[date <= "2010-12-31" & date >= "1981-01-01",
           .(mean = mean(value, na.rm = TRUE),
             sd = sd(value, na.rm = TRUE)),
            .(variable, date = format(date, "2010-%m-%d") %>% as.Date())] %>%
    .[!is.na(date), ]
l_cli <- list(
    mean = d_cli %>% dcast(date ~ variable, value.var = "mean"),
    sd = d_cli %>% dcast(date ~ variable, value.var = "sd"))
{
    # 1. 武汉市1981-2010气候态温度
    brks_date = make_date(2010, seq(1, 12, 1))
    brks_doy  = diff(brks_date) %>% as.numeric() %>% c(1, .) %>% cumsum()
    col = "blue"
    d = d_cli[grep("^T", variable)] %>%
        mutate(mean = mean/10,
               sd   = sd/10,
               ymax = mean + sd,
               ymin = mean - sd,
               var = str_extract(variable, ".*(?=_)"),
               type = str_extract(variable, "(?<=_).*"), )
    d$type %<>% factor(c("max", "avg", "min"))
    p <- ggplot(d, aes(yday(date), mean, fill = type)) +
        geom_ribbon(aes(ymax = ymax, ymin = ymin), alpha = 0.2) +
        # geom_point(size = 1) +
        geom_line(aes(color = type)) +
        # geom_line(aes(y = mov), size = 1, color = col) +
        # geom_line(data = df[year(date) == 2019], aes(date, Tair), color = "red", size = 0.8) +
        theme_bw(base_size = 16, base_family = "Times") +
        theme(panel.grid.minor = element_line(linetype = 2, size = 0.2, color = "grey")) +
        labs(x = "Date", y = "Temperature (℃)") +
        facet_wrap(~var, scales= "free_x", ncol = 2) +
        scale_x_continuous(breaks = brks_doy, labels = 1:12) +
        labs(x = "Month")
        # scale_x_date(breaks = brks, date_labels = "%b %d", expand = c(1, 1)*0.01)
    # p
    write_fig(p, "wuhan_Tair.pdf", 12, 5)
}

{
    brks = make_date(2010, seq(1, 12, 2))
    col = "blue"
    d_yearly = df[date >= "2015-01-01" & variable == "Tair_avg"] %>%
        mutate(doy = yday(date), year = as.factor(year(date)), value = value/10)
    d = d_cli[grep("^Tair", variable)] %>%
        mutate(mean = mean/10) %>%
        dcast(date~variable, value.var = "mean")
    p <- ggplot(d, aes(yday(date), ymax = Tair_max, y = Tair_avg, ymin = Tair_min)) +
        geom_ribbon(fill = col, alpha = 0.2) +
        # geom_point(size = 1) +
        geom_line(color = col) +
        # # geom_line(aes(y = mov), size = 1, color = col) +
        # # geom_line(data = df[year(date) == 2019], aes(date, Tair), color = "red", size = 0.8) +
        # theme_bw(base_size = 16, base_family = "Times") +
        geom_line(data = d_yearly[year == "2019"], aes(doy, value, color = year, ymax = NULL, ymin = NULL)) +
        labs(x = "Month", y = "Temperature (℃)") +
        scale_x_continuous(breaks = brks_doy, labels = 1:12, expand = c(1, 1)*0.01) +
        theme(legend.position = c(1, 1)*0.98, legend.justification = c(1, 1))
        # facet_wrap(~var, scales= "free_x", ncol = 2) +
        # scale_x_date(breaks = brks, date_labels = "%b %d", expand = c(1, 1)*0.01)
    # p
    write_fig(p, "wuhan_Tair2.pdf", 8, 4)
    # write_fig(p, "wuhan_Tair.pdf", 16, 10, show = TRUE)
}


{
    # Figure 1-1. 武汉市相对湿度变化
    d = df_raw[year(date) == 2019] %>%
        mutate(Prcp = Prcp/10)
    sec <- with(d, train_sec(RH_avg, Prcp))
    p = ggplot(d, aes(date, RH_avg, prcp = Prcp)) +
        geom_line() +
        geom_line(aes(y = sec$fwd(Prcp)), colour = "blue") +
        geom_line(aes(y = Tair_avg/10), colour = "red") +
        # geom_histogram(aes(y = sec$fwd(Prcp/10)),
        #                stat = "identity", colour = "blue", center = 40) +
        scale_y_continuous(sec.axis = sec_axis(~sec$rev(.), name = "Prcp"))
    ggplotly(p)
}

library(hydroTools)
q = RH2q(d$RH_avg, d$Pa_avg/100, d$Tair_avg/10) # in %, kPa, Celsius

# ggplot(d, aes(date, q*1000)) +
#     geom_line() +
#     labs(y = "Specific humidity q (g/kg)")
# Figure 1-2. 武汉市比湿变化
p = ggplot(d, aes(date, q*1000, prcp = Prcp)) +
    geom_line() +
    geom_histogram(aes(y = (Prcp)/10), colour = "blue", stat = "identity") +
    geom_line(aes(y = Tair_avg/10), colour = "red")
    # geom_histogram(aes(y = sec$fwd(Prcp/10)),
    #                stat = "identity", colour = "blue", center = 40) +
    # scale_y_continuous(sec.axis = sec_axis(~sec$rev(.), name = "Prcp"))
ggplotly(p)
