library(dplyr)
library(ggplot2)
library(patchwork)
library(sp)  # spdf
library(sf)  # st_bbox
library(rgdal)  
library(gstat) # IDW

# vorher "farmlab export Uni Rostock GG.csv" mit Notepad++ in UTF-8 umwandeln
input <- read.csv("input/farmlab export Uni Rostock GG.csv", sep =",", dec= ".", header = TRUE)
input <- rbind(input,(read.csv("input/farmlab export Uni Rostock GG(1).csv", sep =",", dec= ".", header = TRUE)))
input <- rbind(input,(read.csv("input/farmlab export Uni Rostock GG(2).csv", sep =",", dec= ".", header = TRUE)))
input <- rbind(input,(read.csv("input/farmlab export Uni Rostock GG(3).csv", sep =",", dec= ".", header = TRUE)))

df <- input[, c("Timestamp","Nmin..kg.ha.","P..mg.100g.","K..beta...mg.100g.","Moisture....",
                "SOC....","pH.","Mg..mg.100g.","Soil.Temperature...C.","Texture","Field.name")]
#df <- input[, c(3,5,6,8,14,16,18,20,22,28,30,37,4)]

str(df)
df <- df %>% 
  rename(
    timestamp = Timestamp,
    Nmin = Nmin..kg.ha.,
    P = P..mg.100g.,
    K = K..beta...mg.100g.,
    MC  = Moisture....,
    SOC = SOC....,
    pH = pH.,
    Mg = Mg..mg.100g.,
    Soil_Temp = Soil.Temperature...C.,
    Texture = Texture,
    field = Field.name
  )

df[, 2:9] <- lapply(df[, 2:9], as.numeric)

# Add Date column
df$date <- as.Date(df$timestamp)

# If dates are more than 3 days in one field apart, overwrite with the newer date
for (x in c(1:1000)){
df <- df %>% 
  group_by(field) %>%
  arrange(timestamp) %>% 
  mutate(date = ifelse(abs(date - lag(date, default = date[3])) <= 3, 
                               lag(date, default = date[3]), date))
}


df$date <- as.Date(df$date, origin = "1970-01-01")

df <- df %>%
  mutate(field = case_when(
    field == 'Kassow_107'  ~ 'Kassow',
    field == 'Gut Dummerstorf' ~ 'Gut Dummerstorf',
    field == 'Teterow' ~ 'Teterow',
    field == 'Groß_schwiesow' ~ 'Groß Schwiesow'))

str(df)

##############################################################################
## Lab Data

#lab <- read.csv("input/2022_kassow_lab_data.csv", sep =",", dec= ".", header = TRUE,fileEncoding="utf-8")

#ggplot() +
#  geom_point(data=lab, aes(x = long, y = lat,color = pH)) +
#  scale_color_distiller(type="div",na.value = NA)

#  scale_color_distiller(type="seq", palette="YlOrRd",name="Ton",na.value = NA) +
##############################################################################
## Point Plots

plot_points <- function(data, x, y, title, ylab){
  ggplot(data = data, aes(x = {{x}}, y = {{y}}, color=field)) +
    geom_point(shape = 21,alpha=0.4,size=2.5) +
    geom_smooth(fill = NA) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
    labs(title = title, y = ylab) +
    theme(axis.title.x = element_blank()) +
    theme(legend.title = element_blank()) +
    scale_x_date(date_breaks = "1 months") + 
    scale_x_date(date_labels = "%b-%Y") +
    theme(axis.text.x = element_text(size=8,angle=30,vjust=1,hjust=1)) +
    guides(colour = guide_legend(override.aes = list(shape=16, alpha=1)))
}

p_N   <- plot_points(df,date,Nmin,"Nmin","Nmin [kg/ha]")
p_P   <- plot_points(df,date,P,"Phospor","P [mg/100g]")
p_K   <- plot_points(df,date,K,"K - Potassium","K [mg/100g]")
p_SOC <- plot_points(df,date,SOC,"SOC","Soil Organic Carbon [%]")
p_MC  <- plot_points(df,date,MC,"Moisture Content","MC [%]")
p_pH  <- plot_points(df,date,pH,"pH","pH")
p_Mg  <- plot_points(df,date,Mg,"Mg","Magnesium [mg/100g]")
p_Tmp <- plot_points(df,date,Soil_Temp,"Soil Temperature","Temperature [°C]")

combined <- (p_N|p_P|p_K|p_Mg) / (p_SOC|p_MC|p_pH|p_Tmp) & theme(legend.position="bottom")
combined + plot_layout(guides = "collect")
p <- combined + plot_layout(guides = "collect")
ggsave(paste0("Points_all_fields_all_parameter.png"), p,width=10,height=5)

rm(p,p_N,p_P,p_K,p_SOC,p_MC,p_pH,p_Mg,p_Tmp)
rm(combined)

##############################################################################
## Line Plots
# Group the data by date and
# calculate the mean and standard deviation for column 4 to 12 for each date
df_summary <- df %>% 
  group_by(date,field) %>% 
  summarise_at(vars(2:9), list(median=median, sd=sd))
str(df_summary)

plot_lines <- function(data, x, y, sd, title, ylab){
  ggplot(data, aes(x = {{x}}, y = {{y}}, color=field, group=field)) +
    geom_point() +
    geom_line(linewidth = 1) + 
    geom_errorbar(aes(ymin={{y}}-{{sd}}, ymax={{y}}+{{sd}}),width=5,linewidth =0.6,alpha=0.4) +
    theme_classic() +
    labs(title = title, y = ylab) +
    theme(axis.title.x = element_blank()) +
    theme(legend.title = element_blank()) +
    scale_x_date(date_breaks = "1 months") + 
    scale_x_date(date_labels = "%b-%Y") +
    theme(axis.text.x = element_text(size=8,angle=30,vjust=1,hjust=1)) +
    guides(colour = guide_legend(override.aes = list(shape=16, alpha=1)))
}


p_N   <- plot_lines(df_summary,date,Nmin_median,Nmin_sd,"Nmin","Nmin [kg/ha]")
p_P   <- plot_lines(df_summary,date,P_median,P_sd,"Phospor","P [mg/100g]")
p_K   <- plot_lines(df_summary,date,K_median,K_sd,"K - Potassium","K [mg/100g]")
p_SOC <- plot_lines(df_summary,date,SOC_median,SOC_sd,"SOC","Soil Organic Carbon [%]")
p_MC  <- plot_lines(df_summary,date,MC_median,MC_sd,"Moisture Content","MC [%]")
p_pH  <- plot_lines(df_summary,date,pH_median,pH_sd,"pH","pH")
p_Mg  <- plot_lines(df_summary,date,Mg_median,Mg_sd,"Mg","Magnesium [mg/100g]")
p_Tmp <- plot_lines(df_summary,date,Soil_Temp_median,Soil_Temp_sd,"Soil Temperature","Temperature [°C]")

combined <- (p_N|p_P|p_K|p_Mg) / (p_SOC|p_MC|p_pH|p_Tmp) & theme(legend.position="bottom")
combined + plot_layout(guides = "collect")
p <- combined + plot_layout(guides = "collect")
ggsave(paste0("Lines_all_fields_all_parameter.png"), p,width=10,height=5)

rm(p,p_N,p_P,p_K,p_SOC,p_MC,p_pH,p_Mg,p_Tmp)

##############################################################################
## Z-Maps
# Normalization
df_z <- df %>%
  group_by(date,field) %>%
  mutate_at(vars(4:11), ~scale(.)) %>%
  ungroup() %>%
  mutate(across(4:11, round, digits = 2))

df_z_outliers <- df_z %>%
  filter(rowSums(abs(.[4:10]) > 2.5) > 2) 

# Filter out rows where more than 2 values are greater than 2.5
df_z <- df_z %>%
  filter(rowSums(abs(.[4:10]) > 2.5) < 2)

print(paste0(nrow(df)-nrow(df_z)," from ",nrow(df)," deleted"))

dev.off() #sometimes ggplot doesn't work without this

plot_map <- function(data, x, y, color){
  ggplot(data = data, aes(x = {{x}}, y = {{y}}, color = {{color}})) +
    geom_point(size = 1.5) +
    scale_colour_gradientn(colours = rainbow(5)) +
    theme_classic() +
    theme(axis.title.x = element_blank()) +
    theme(axis.title.y = element_blank()) +
    theme(axis.text.x = element_text(size=5)) +
    theme(axis.text.y = element_text(size=5,angle=90,hjust=0.5))
}

map_T <- ggplot(data = df, aes(x = long, y = lat, color = factor(texture))) +
  geom_point(size = 1) +
#  scale_color_gradient2(low ="red",mid="white",high="blue", limits = c(-3,3)) +
#  scale_color_distiller(type="div",palette ="Spectral") +
#  scale_color_fermenter(type="div") #continuous -> discrete scale
  scale_color_brewer(type="seq", palette="OrRd",name="Textur") +
  theme_classic() +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.x = element_text(size=5)) +
  theme(axis.text.y = element_text(size=5,angle=90,hjust=0.5))

map_N   <- plot_map(df_z,long,lat,color=Nmin)
map_P   <- plot_map(df_z,long,lat,color=P)
map_SOC <- plot_map(df_z,long,lat,color=SOC)
map_MC  <- plot_map(df_z,long,lat,color=MC)
map_pH  <- plot_map(df_z,long,lat,color=pH)
map_Mg  <- plot_map(df_z,long,lat,color=Mg)
map_K   <- plot_map(df_z,long,lat,color=K)

(map_N|map_P|map_K|map_Mg)/( map_SOC|map_MC|map_pH|map_T)
p_map <- (map_N|map_P|map_K|map_Mg)/( map_SOC|map_MC|map_pH|map_T)
ggsave(paste0("zmap_",field_name,".png"),p_map,width=10,height=3.5)

rm(p_map,map_T,map_N,map_P,map_SOC,map_MC,map_pH,map_Mg,map_K)

##############################################################################
## k-means clustering

#Plot all the data, to see if there are some relations between them
plot(df_z[,c(4:11)])

df_k <- df_z[,c(4:11)]
library (hopkins)

# Hopkins Test to test the probability that the data has a uniform random distribution 
# Lower is better, e.g. 0.5 indicates high degree of spatial randomness
hopkins <- hopkins (df_k, n=nrow(df_k)-1)
print(hopkins) 

# Transform df into a spatial points dataframe
#coordinates(df_k) <- c("long", "lat")

vars <- df_k[, c("MC","pH","K","Mg","texture")]

k <- kmeans(vars, 4)

df_z$cluster <- k$cluster

# Calculate Silhouette Width (The higher the better)
library(cluster)
sil_width <- silhouette(df_k$cluster, dist(vars))
cat("Silhouette Width:", mean(sil_width[,3]), "\n")

# Calculate Calinski-Harabasz Index
#library(fpc)
#ch <- calinski_harabasz(data[,2:6], data$cluster)

# Print the validation measures
cat("Within-cluster Sum of Squares (WSS):", sum(wss), "\n")


print(sil_width)

ggplot(df_z, aes(x = long, y = lat, color = as.factor(cluster))) +
  geom_point(size=3) +
  labs(title = "Spatial Clustering with K-means", color = "Cluster") +
  theme_bw()

#Nmin + SOC = 0.33
#"K","MC","pH" = 0.32
#MC + pH = 0.36

# Make a SpatialPointsDataFrame
#data        <- df_z[ , c(4:12)]
#coords      <- df_z[ , c("long", "lat")]
#crs         <- CRS("+init=epsg:4326") # => [+proj=longlat +datum=WGS84]
#spdf <- SpatialPointsDataFrame(coords             = coords,
                               data        = data, 
                               proj4string = crs)

#plot(spdf)

# Create an empty grid using the extends of the spdf with Pixel Size 5 meter
#bbox <- st_bbox(spdf)

#cell_size <- 5
#x <- seq(bbox$xmin, bbox$xmax, by=cell_size)
#y <- seq(bbox$ymin, bbox$ymax, by=cell_size)
#grd <- expand.grid(x=x, y=y)
#names(grd)       <- c("X", "Y")
#coordinates(grd) <- c("X", "Y")
#gridded(grd)     <- TRUE  # Create SpatialPixel object
#fullgrid(grd)    <- TRUE  # Create SpatialGrid object
# Add P's projection information to the empty grid
#proj4string(grd) <- proj4string(sensor.spdf)

#spdf <- spTransform(spdf, CRS=CRS("+init=epsg:32623"))

##############################################################################
#Plot schleife alternative Das als Funktion schreiben:

#
#Spalte = character()
#Spalte <- colnames(df)
#
#for(colnames in 4:10) {
#  print(Spalte[colnames])
#  
#  p <- ggplot(data = df, aes(x = date, y = Spalte[colnames], group=interaction(date))) +
#    geom_boxplot(outlier.size=0.5) +
#    theme_bw() +
#    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
#    labs(title="N min", y = "Nmin [kg/ha]", x = "") +
#    scale_x_date(date_breaks = "1 months") + 
#    scale_x_date(date_labels = "%b-%Y")
#  p
#}