library(dplyr)
library(ggplot2)
library(patchwork)

# vorher "farmlab export Uni Rostock GG.csv" mit Notepad++ in UTF-8 umwandeln
input <- read.csv2("farmlab export Uni Rostock GG.csv", sep =";", dec= ",", header = TRUE)

df <- input[, c(2,4,5,7,8,10,11,12,13,14,23)]
str(df)
df <- df %>% 
  rename(
    Nmin_kg_ha = Nmin..kg.ha.,
    NO3_kg_ha = NO3.N..kg.ha.,
    P_mg_100g = P..mg.100g.,
    K_mg_100g = K..beta...mg.100g.,
    Moisture  = Moisture....,
    SOC = SOC....
  )

df[, 4:11] <- lapply(df[, 4:11], as.numeric)

# Add Date column
df$date <- as.Date(df$timestamp)

str(df)

##############################################################################
## Calculate the summaries of each measurement series

# Group the data by date and
# calculate the mean and standard deviation for column 4 to 12 for each date
df_summary <- df %>% 
  group_by(date) %>% 
  summarise_at(vars(4:11), list(mean=mean, sd=sd))

str(df_summary)

par(mfrow=c(3,2))
plot(df_summary$date, df_summary$Nmin_kg_ha_mean, main = "Nmin mean", xlab="", ylab = "Nmin [kg/ha]")
plot(df_summary$date, df_summary$P_mg_100g_mean, main = "P mean", xlab="", ylab = "P [mg/100g]")
plot(df_summary$date, df_summary$K_mg_100g_mean, main = "K mean", xlab="", ylab = "K [mg/100g]")
plot(df_summary$date, df_summary$SOC_mean, main = "SOC mean", xlab="", ylab = "SOC [%]")
plot(df_summary$date, df_summary$Moisture_mean, main = "Moisture mean", xlab="", ylab = "Moisture content [%]")
plot(df_summary$date, df_summary$pH_mean, main = "pH mean", xlab="", ylab = "pH")


p_N <- ggplot(data = df, aes(x = date, y = Nmin_kg_ha, group=interaction(date))) +
  geom_boxplot(outlier.size=0.5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  labs(title="N min", y = "Nmin [kg/ha]", x = "") +
  scale_x_date(date_breaks = "1 months") + 
  scale_x_date(date_labels = "%b-%Y")

p_P <- ggplot(data = df, aes(x = date, y = P_mg_100g, group=interaction(date))) +
  geom_boxplot(outlier.size=0.5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title="P - Phosphor", y = "P [mg/100g]", x = "") + 
  scale_x_date(date_breaks = "1 months") + 
  scale_x_date(date_labels = "%b-%Y")

p_K <- ggplot(data = df, aes(x = date, y = K_mg_100g, group=interaction(date))) +
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

p_MC <- ggplot(data = df, aes(x = date, y = Moisture, group=interaction(date))) +
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

(p_N | p_P) /
(p_K | p_SOC) /
(p_MC| p_pH)

( p_N  | p_P  | p_K) /
( p_SOC | p_MC | p_pH)


##############################################################################

df_z <- df %>%
  group_by(date) %>%
  mutate_at(vars(4:11), ~scale(.)) %>%
  ungroup()

Spalte = character()
Spalte <- colnames(df)

for(colnames in 4:10) {
  print(Spalte[colnames])
  p <- ggplot(data = df, aes(x = date, y = Spalte[colnames], group=interaction(date))) +
    geom_boxplot(outlier.size=0.5) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
    labs(title="N min", y = "Nmin [kg/ha]", x = "") +
    scale_x_date(date_breaks = "1 months") + 
    scale_x_date(date_labels = "%b-%Y")
  p
}
  
