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

p_N <- ggplot(data = df, aes(x = date, y = Nmin, group=interaction(date))) +
  geom_boxplot(outlier.size=0.5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  labs(title="N min", y = "Nmin [kg/ha]", x = "") +
  scale_x_date(date_breaks = "1 months") + 
  scale_x_date(date_labels = "%b-%Y")

p_P <- ggplot(data = df, aes(x = date, y = P, group=interaction(date))) +
  geom_boxplot(outlier.size=0.5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title="P - Phosphor", y = "P [mg/100g]", x = "") + 
  scale_x_date(date_breaks = "1 months") + 
  scale_x_date(date_labels = "%b-%Y")

p_K <- ggplot(data = df, aes(x = date, y = K, group=interaction(date))) +
  geom_boxplot(outlier.size=0.5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title="K - Potassium", y = "K [mg/100g]", x = "") + 
  scale_x_date(date_breaks = "1 months") + 
  scale_x_date(date_labels = "%b-%Y")

p_SOC <- ggplot(data = df, aes(x = date, y = SOC, group=interaction(date))) +
  geom_boxplot(outlier.size=0.5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title="SOC - Soil Organic Carbon", y = "SOC [%]", x = "") + 
  scale_x_date(date_breaks = "1 months") + 
  scale_x_date(date_labels = "%b-%Y")

p_MC <- ggplot(data = df, aes(x = date, y = MC, group=interaction(date))) +
  geom_boxplot(outlier.size=0.5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title="Moisture content", y = "Moisture content [%]", x = "") + 
  scale_x_date(date_breaks = "1 months") + 
  scale_x_date(date_labels = "%b-%Y")

p_pH <- ggplot(data = df, aes(x = date, y = pH, group=interaction(date))) +
  geom_boxplot(outlier.size=0.5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title="pH", y = "pH", x = "") + 
  scale_x_date(date_breaks = "1 months") + 
  scale_x_date(date_labels = "%b-%Y")

#(p_N | p_P) /
#(p_K | p_SOC) /
#(p_MC| p_pH)

( p_N  | p_P  | p_K) /
( p_SOC | p_MC | p_pH)


df_z <- df %>%
  group_by(date) %>%
  mutate_at(vars(4:9), ~scale(.)) %>%
  ungroup()

df_z <- df_z %>% 
  mutate(across(4:9, ~ifelse(abs(.) > 2.5, NA, .)))

dev.off()
ggplot(data = df_z, aes(x = long, y = lat, color = SOC)) +
  geom_point(size = 2) +
  scale_colour_gradientn(colours=rainbow(4)) +
  theme_classic()


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
  
