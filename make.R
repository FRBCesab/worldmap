#' @title worldmap: Chloropeth World Map in Robinson Projection
#' 
#' @description 
#' Chloropeth World Map in Robinson Projection.
#' 
#' @author Nicolas Casajus \email{nicolas.casajus@fondationbiodiversite.fr}
#' 
#' @date 2021/03/10



## Install Dependencies (listed in DESCRIPTION) ----

if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_deps(upgrade = "never")


## Load Project Addins (R Functions and Packages) ----

devtools::load_all()


## Download Base map ----

# get_basemap()


## Read IPBES Countries ----

world <- sf::st_read(here::here("data", "ipbes-regions", 
                                "IPBES_Regions_Subregions2.shp"))
# world <- world[which(world$Area != "Antarctica"), ]


## Project in Robinson ----

robin <-  "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
world <- sf::st_transform(world, robin)


# Create Graticules ----

lat <- c( -90,  -60, -30, 0, 30,  60,  90)
lon <- c(-180, -120, -60, 0, 60, 120, 180)

grat <- graticule::graticule(lons = lon, lats = lat, proj = robin, 
                             xlim = range(lon), ylim = range(lat))


## Data to map ----
## ... a data frame with two columns: 'country' and 'data'
## ... (in this example)

tab <- read.csv(here::here("data", "data-demo.csv"))


## Add Data to Shape file ----

world$data <- NA

for (i in 1:nrow(tab)) {
  
  lignes <- which(world$Area == tab[i, "country"])
  
  # Detect misspelled country name (to be changed in xlsx not the SHP)
  if (!length(lignes)) stop(paste0(i, " : ", tab[i, "country"]))
  
  world[lignes, "data"] <- tab[i, "data"]
}


## Define Colors ----

classes <- data.frame(from  = c(0, 1, 1, 4,  9, 15, 20),   # x >  from
                      to    = c(0, 1, 4, 9, 15, 20, 9999), # x <= to
                      label = c("0", "1", "2", "4",  "9", "15", ">20"),
                      color = c("#FFFFFF", "#FFCBFE", "#FF98FD", "#FF63FC", 
                                "#FF00FC", "#CF00C9", "#680064"))

world$color <- NA

for (i in 1:nrow(classes)) {
  
  pos <- which(world[ , "data", drop = TRUE] >  classes[i, "from"] & 
                 world[ , "data", drop = TRUE] <= classes[i, "to"])
  
  if (length(pos)) world[pos, "color"] <- classes[i, "color"]
}


# NA values...
pos <- which(is.na(world[ , "data", drop = TRUE]))
if (length(pos)) world[pos, "color"] <- "#f0f0f0"

# Other colors...
borders <- "#c8c8c8"
texte   <- "#666666"



if (!dir.exists(here::here("figures"))) dir.create(here::here("figures"))


## Graphical Device ----

grDevices::pdf(file = here::here("figures", "map-demo.pdf"),
               width = 12, height = 7.5)


## Base map + Data + Graticules ----

par(mar = rep(1, 4), family = "serif")

sp::plot(grat, lty = 1, lwd = 0.2, col = borders)

plot(sf::st_geometry(world), col = world$"color", border = borders, 
     lwd = 0.2, add = TRUE)


## Legend ----

x_length <- 1000000
x_start  <- -1 * (x_length * (nrow(classes) / 2))

if (nrow(classes) %% 2 == 0) x_start <- x_start - (x_length / 2)

y_height <-    500000
y_middle <- -10000000


par(xpd = TRUE)

for (i in 1:nrow(classes)) {
  
  rect(xleft   = x_start + (i - 1) * x_length, 
       xright  = x_start + i * x_length,
       ybottom = y_middle - y_height, 
       ytop    = y_middle + y_height,
       col     = classes[i, "color"], border = borders)
  
  text(x      = x_start + (i - 1) * x_length, 
       y      = y_middle - y_height, 
       labels = classes[i, "label"],
       pos = 1, cex = 0.9, col = texte)
}


## Title ----

text(x = 0, y = y_middle + y_height, col = texte, font = 2, pos = 3,
     labels = "Legend Title")

par(xpd = FALSE)

dev.off()
