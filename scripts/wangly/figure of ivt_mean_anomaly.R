source("scripts/main_pkgs.R")


#############################moistrue_flux#######################################
ncdata <- nc_open(".\\outputted data\\IVT\\ivt_anomaly_mean.nc")
warm_ivtu <- ncvar_get(ncdata, "warm_ivtu")
warm_ivtv <- ncvar_get(ncdata, "warm_ivtv")
cold_ivtu <- ncvar_get(ncdata, "cold_ivtu")
cold_ivtv <- ncvar_get(ncdata, "cold_ivtv")
ivtu <- warm_ivtu - cold_ivtu
ivtv <- warm_ivtv - cold_ivtv
lon <- ncvar_get(ncdata, "lon")
lat <- ncvar_get(ncdata, "lat")
nc_close(ncdata)
rm(cold_ivtu, cold_ivtv, warm_ivtu, warm_ivtv)

lonlat <- expand.grid(lon = lon, lat = lat)
ivtu <- as.vector(ivtu)
ivtv <- as.vector(ivtv)
ivt <- cbind(lonlat, u = ivtu, v = ivtv)
ivt$wind <- sqrt((ivt$u)^2+(ivt$v)^2)

######convert 2.5 *2.5 grid box to 5 * 5 ones
indlon <- which(ivt$lon %% 5 == 0)
indlat <- which(ivt$lat %% 5 == 0)
ind <- intersect(indlon,indlat)
ivt <- ivt[ind,]

###############################precipitable water###################################
ncdata1 <- nc_open(".\\outputted data\\sst_etc\\pw_minus.nc")
var1 <- ncvar_get(ncdata1, "pw")
var1p <- ncvar_get(ncdata1, "p")
lon1 <- ncvar_get(ncdata1, "lon")
lat1 <- ncvar_get(ncdata1, "lat")
nc_close(ncdata1)

lonlat1 <- expand.grid(lon = lon1, lat = lat1)
var1 <- as.vector(var1)
var1p <- as.vector(var1p)
var1 <- cbind(lonlat1, var1, var1p)
colnames(var1) = c("lon", "lat", "value","p")
range(var1$value)
#############################fake_legend################################################
breaks1 <- c(-9,sprintf("%0.2f",c(-3.5,-3,-2.5,-2,-1.75,-1.5,-1.25,-1,-0.75,-0.5,
                                   0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.5, 3,3.5)), 9)
length(breaks1)
group1 <- letters[1:22]
y1 <- vector(mode = "numeric", length = 0)
for(i in 1:21){
        y1[i]<-i
} 
values1<-c(0,y1)
diff_values1 <- c(0,diff(values1))
cbar_df1 <- data.frame(x = 1, y = breaks1, diff_values1, group1, stringsAivtactors = FALSE)
cbar_df1

col <- colorRampPalette(brewer.pal(11,'RdYlBu')[c(2:5,7:10)])
rcol <- col(22)[c(1:10, 13:22)]
show_col(rcol)
rcol
rcol1 <- c(rcol[1:10],'white',rcol[11:20],'white')
show_col(rcol1)

cbar_plot1 <- ggplot(cbar_df1,aes(x, y = diff_values1,
                                  fill = group1))+
        geom_col(width = 0.13, show.legend = F, col = 'black')+
        annotate(geom = 'text', x = 1.38, y = values1[c(2,4,6,8,10,13,15,17,19,21)], 
                 label = breaks1[c(2,4,6,8,10,13,15,17,19,21)],size = 4.5,family = 'serif', fontface = "bold") +
        scale_x_continuous(expand = c(2,2)) + 
        scale_fill_manual(values = rev(rcol1))+
        ylab(label = "") +
        xlab(label = "")+
        #coord_flip() +
        theme_void()
cbar_plot1
##################################figure_function##############################################
fig <- function(data, breaks, col,uv,
                lag1, lag2, labels1, labels2,
                labb,lons1, lons2, lats1, lats2){
        fig <- ggplot(data = data, aes(x = lon, y = lat))+
                stat_contour_filled(data = data, inherit.aes = F, aes(x = lon,y = lat,z = value),
                                    breaks = breaks)+
                scale_fill_manual(values = rev(col))+
                scale_x_continuous(expand = c(0,0), limits = c(0.5,359.5),
                                   breaks = seq(0,360,lag1),
                                   labels = labels1)+
                scale_y_continuous(expand = c(0,0), limits = c(-87.5,87.5),
                                   breaks = seq(-90,90,lag2),
                                   labels = labels2)+
                geom_path(data = worldmap, inherit.aes = F,
                          mapping = aes(x = long, y = lat, group = group),color = "#999999")+
                stat_subset(inherit.aes = F,
                            aes(x = lon, y = lat, subset = p <= 0.1), color = "deeppink2",
                            geom = "point",size = 0.7)+
                geom_segment(data = ivt, inherit.aes = F,
                             aes(x = lon, y = lat,xend = lon + u*uv, yend = lat + v*uv),
                             arrow = arrow(length = unit(0.75,"mm"),type = "closed"), size = 0.25)+
                coord_quickmap(xlim = c(lons1,lons2),ylim = c(lats1,lats2))+
                ylab(label = "") +
                xlab(label = "")+
                labs(title = labb)+
                theme_bw()+
                theme(panel.border = element_rect(),
                      panel.background = element_blank(),
                      panel.grid = element_blank(),
                      axis.title = element_blank(),
                      axis.text=element_text(size=11.7,colour = "black",family='serif',face='bold'),
                      legend.position = "none",
                      plot.background = element_rect(fill = "white"),
                      plot.title = element_text(hjust = 0.5,size=14,family='serif',face='bold'),
                      plot.margin = unit(c(0,0,0,0),"mm"))
        fig 
}

####################################main_figures###################################################
fig.a <- fig(data = var1, uv = 2,
             breaks = c(-9,-3.5,-3,-2.5,-2,-1.75,-1.5,-1.25,-1,-0.75,-0.5,
                        0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.5, 3,3.5,9),
             col = rev(rcol1),
             lag1 = 30,
             lag2 = 15,
             labels1 = c("","30°E","60°E","90°E","120°E","150°E","180°",
                         "150°W","120°W","90°W","60°W","30°W",""),
             labels2 = c("","75°S","60°S","45°S","30°S","15°S","0°",
                         "15°N","30°N","45°N","60°N","75°N",""),
             labb = "(a) MF and PW in East Aisa",
             lons1 = 70, lons2 = 210, lats1 = 2.5, lats2 = 70)
fig.a
fig.b <- fig(data = var1, uv = 3,
             breaks = c(-9,-3.5,-3,-2.5,-2,-1.75,-1.5,-1.25,-1,-0.75,-0.5,
                        0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.5, 3,3.5,9),
             col = rev(c(rcol1)),
             lag1 = 30,
             lag2 = 15,
             labels1 = c("","30°E","60°E","90°E","120°E","150°E","180°",
                         "150°W","120°W","90°W","60°W","30°W",""),
             labels2 = c("","75°S","60°S","45°S","30°S","15°S","0°",
                         "15°N","30°N","45°N","60°N","75°N",""),
             labb = "(b) MF and PW in North America",
             lons1 = 210, lons2 = 350, lats1 = 2.5, lats2 = 70)
fig.b
fig.c <- fig(data = var1,uv = 0.9,
             breaks = c(-9,-3.5,-3,-2.5,-2,-1.75,-1.5,-1.25,-1,-0.75,-0.5,
                        0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.5, 3,3.5,9),
             col = rev(rcol1),
             lag1 = 30,
             lag2 = 15,
             labels1 = c("","30°E","60°E","90°E","120°E","150°E","180°",
                         "150°W","120°W","90°W","60°W","30°W",""),
             labels2 = c("","75°S","60°S","45°S","30°S","15°S","0°",
                         "15°N","30°N","45°N","60°N","75°N",""),
             labb = "(c) MF and PW in Australia",
             lons1 = 90, lons2 = 170, lats1 = -50, lats2 = -2.5)
fig.c

fig.all <- ggdraw() + 
        draw_plot(fig.a,  0.002, 0.62, width = 0.44,  height =   0.33) +
        draw_plot(cbar_plot1,   0.2445,  0.654, width = 0.43,  height =  0.259)+
        
        draw_plot(  fig.b,  0.002, 0.328, width = 0.44,  height =  0.33) +
        draw_plot(cbar_plot1,   0.2445,  0.362, width = 0.43,  height =  0.259)+
        
        draw_plot(  fig.c,  0.002,  -0.065, width = 0.44,  height =  0.5) + 
        draw_plot(cbar_plot1,   0.2445,  0.04, width = 0.43,  height =  0.289)
ggsave(fig.all,
       filename = '.\\figure\\IVT\\ivt_pw.jpg',  
       width = 800*0.3, height = 800*0.3, units = 'mm', dpi = 600)
