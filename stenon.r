install.packages("patchwork")
library(dplyr)
library(ggplot2)
library(patchwork)
library(sp)  # spdf
library(sf)  # st_bbox
library(rgdal)  
library(gstat) # IDW

# vorher "farmlab export Uni Rostock GG.csv" mit Notepad++ in UTF-8 umwandeln
input <- read.csv2("farmlab export Uni Rostock GG.csv", sep =";", dec= ",", header = TRUE)

df <- input[, c(2,4,5,7,10,11,12,13,14,23)]
str(df)
df <- df %>% 
  rename(
    Nmin = Nmin..kg.ha.,
    P = P..mg.100g.,
    K = K..beta...mg.100g.,
    MC  = Moisture....,
    SOC = SOC....
  )

df[, 2:10] <- lapply(df[, 2:10], as.numeric)

# Add Date column
df$date <- as.Date(df$timestamp)

str(df)

##############################################################################
## Calculate the summaries of each measurement series

# Group the data by date and
# calculate the mean and standard deviation for column 4 to 12 for each date
df_summary <- df %>% 
  group_by(date) %>% 
  summarise_at(vars(4:10), list(mean=mean, sd=sd))

str(df_summary)

plot_boxplot <- function(data, x, y, title, ylab){
  ggplot(data = data, aes(x = {{x}}, y = {{y}}, group = interaction({{x}}))) +
    geom_boxplot(outlier.size=0.5) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
    labs(title = title, y = ylab) +
    scale_x_date(date_breaks = "1 months") + 
    scale_x_date(date_labels = "%b-%Y")
}

p_N   <- plot_boxplot(df,date,Nmin,"Nmin","Nmin [kg/ha]")
p_P   <- plot_boxplot(df,date,P,"Phospor","P [mg/100g]")
p_K   <- plot_boxplot(df,date,K,"K - Potassium","K [mg/100g]")
p_SOC <- plot_boxplot(df,date,SOC,"SOC","Soil Organic Carbon [%]")
p_MC  <- plot_boxplot(df,date,MC,"Moisture Content","MC [%]")
p_pH  <- plot_boxplot(df,date,pH,"pH","pH")

( p_N  | p_P  | p_K) /
( p_SOC | p_MC | p_pH)

#(p_N | p_P) /
#(p_K | p_SOC) /
#(p_MC| p_pH)

df_z <- df %>%
  group_by(date) %>%
  mutate_at(vars(4:9), ~scale(.)) %>%
  ungroup() %>%
  mutate(across(4:9, round, digits = 2))

#df_z <- df_z %>% 
#  mutate(across(4:9, ~ifelse(abs(.) > 2.5, NA, .)))

# Filter for rows where less than 2 values are greater than 2.5
df_z <- df_z %>%
  filter(rowSums(abs(.[4:10]) > 2.5) < 2) 

print(paste0(nrow(df)-nrow(df_z)," from ",nrow(df)," deleted"))

dev.off()

plot_map <- function(data, x, y, color, colors, size = 2){
  ggplot(data = data, aes(x = {{x}}, y = {{y}}, color = {{color}})) +
    geom_point(size = size) +
    scale_color_gradientn(colors = colors) +
    labs(title = "", y = "", x = "") +
    theme_classic()
}

map_N <- plot_map(df_z,long,lat,color=Nmin,rainbow(4))
map_P <- plot_map(df_z,long,lat,color=P,rainbow(4))
map_K <- plot_map(df_z,long,lat,color=K,rainbow(4))
map_SOC <- plot_map(df_z,long,lat,color=SOC,rainbow(4))
map_MC <- plot_map(df_z,long,lat,color=MC,rainbow(4))
map_pH <- plot_map(df_z,long,lat,color=pH,rainbow(4))

( map_N  | map_P  | map_K) /
( map_SOC | map_MC | map_pH)

# Make a SpatialPointsDataFrame
data        <- df_z[ , c(4:12)]
coords      <- df_z[ , c("long", "lat")]
crs         <- CRS("+init=epsg:4326") # => [+proj=longlat +datum=WGS84]
spdf <- SpatialPointsDataFrame(coords             = coords,
                                      data        = data, 
                                      proj4string = crs)

plot(spdf)

# Create an empty grid using the extends of the spdf with Pixel Size 5 meter
bbox <- st_bbox(spdf)

cell_size <- 5
x <- seq(bbox$xmin, bbox$xmax, by=cell_size)
y <- seq(bbox$ymin, bbox$ymax, by=cell_size)
grd <- expand.grid(x=x, y=y)
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object
# Add P's projection information to the empty grid
proj4string(grd) <- proj4string(sensor.spdf)

spdf <- spTransform(spdf, CRS=CRS("+init=epsg:32623"))









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
  
