#  Cargamos las Librerias ----------------------------------------------------------
library(sf)
library(ggplot2)
library(tidyverse)
library(ggnewscale)
library(raster)
library(extrafont)      # custom font
library(hrbrthemes)     # to use import_roboto_condensed()
library(ggthemes)
library(elevatr)
library(ggspatial)
library(tmap)
library(ggpubr)
library(ggrepel)
library(rnaturalearth)                                   #Continentes
library(rnaturalearthdata)    
# Cargammos los SHp del Peru ---------------------------------------------------------------
Bolivia           <- getData('GADM', country='Bolivia', level=0) %>% st_as_sf()
Brazil            <- getData('GADM', country='Brazil', level=0) %>% st_as_sf()
Chile             <- getData('GADM', country='Chile', level=0) %>% st_as_sf()
Paraguay          <- getData('GADM', country='Paraguay', level=0) %>% st_as_sf()
Uruguay           <- getData('GADM', country='Uruguay', level=0) %>% st_as_sf()
Argentina         <- getData('GADM', country='Argentina', level=1) %>% st_as_sf()
Peru              <- getData('GADM', country='Peru', level=1) %>% st_as_sf()
Anta_Elev         <- get_elev_raster(Argentina, z=6)
Argentina_xy <- cbind(Argentina , st_coordinates(st_centroid(Argentina$geometry)))
Argentina_alt    <- crop(Anta_Elev,Argentina )                           #   
Argentina_alt     <- Argentina_alt  <- mask(Argentina_alt , Argentina)

Geo_data       <-  rasterToPoints(Argentina_alt )
Geo_data_frame <-  data.frame(Geo_data) 

colores<- c("#015B26", "#3F8433", "#8BAB56", "#CFCF7D", "#C49450", "#AA5B32", "#872921")


world        <- ne_countries(scale= "small", returnclass = "sf") # Continentes del mundo
world.SA     <- subset(world, continent=="South America")     # Sur America


Per=ggplot()+
  geom_sf(data = Argentina, fill="black", color="black")+
  geom_sf(data=world.SA, fill=NA, color="black", size=0.5)+
  coord_sf(xlim = c(-81.41094, -34.72999), ylim = c(-55.61183 ,12.4373),expand = FALSE)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))


Peru.grob  <- ggplotGrob(Per)

Mapa =ggplot()+
  geom_raster(data = Geo_data_frame  ,aes(x,y, fill = file737838074ca6))+
  scale_fill_gradientn(colours = colores, 
                       breaks = c(0,500,1000,2000,3000,4000,5000,6000),
                       na.value = 'white',
                       labels = c("[0 - 499] ","[500 - 999]", "[1000 - 1999]", "[2000 - 2999]", "[3000 - 3999]", "[4000 - 4999]",
                                  "[5000 - 5999]","[6000 - 6543]"),
                       name='Elevacion \n(msnm)')+
  geom_sf(data = Argentina, fill=NA, color="black")+
  geom_label_repel(data = Argentina_xy, aes(x = X, y = Y, label = NAME_1), 
                   family="serif", box.padding = unit(0.9, "lines"), size = 2.4, face = "bold",color = 'black',
                   point.padding = unit(0.9, "lines"))+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  theme_bw()+
  theme(plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        panel.grid.major = element_line(color = gray(.4),
                                        linetype = "dashed", size = 0.4),
        axis.text.x  = element_text(face="bold", color="white", size=8),
        axis.text.y  = element_text(angle = 90,face="bold", color="white", size=8),
        legend.position = c(0.90,0.4),
        plot.title = element_text(size = 16, hjust = 0.5, color = "white", family="serif", face = "italic"),
        plot.subtitle = element_text(size = 11, hjust = 0.5, face = "italic", color = "white", family="serif"),
        plot.caption = element_text(size = 10, hjust = 0.95, color = "white", family="serif", face = "italic"),
        legend.key.size = unit(0.4, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.7,"cm"), #ancho de cuadrados de referencia 
        legend.direction = "horizontal", #dirección de la leyenda
        legend.title=element_text(size=8, face = "bold"), #tamaño de titulo de leyenda
        text = element_text(size = 9, family = "Tahoma", color="black"),
        axis.title = element_text(face="bold", color="white"),
        legend.text=element_text(size=8))+
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="white"))+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="white")+
  ggtitle("Mapa de Relieve de Argentina")+
  labs(subtitle="Ing.Gorky Florez Castillo",  x="Longitud",y="Latitud",tag="B)",
       caption="Fuente: Data: https://srtm.csi.cgiar.org/")+
  annotation_custom(Peru.grob, xmin = -53.59184, xmax = -60, ymin =-55.06153, ymax=-48)


ggsave(plot = Mapa ,"Mapa/Mapa elevacion Argentina.png", 
       units = "cm", width = 21,height = 29, dpi = 1000) 






