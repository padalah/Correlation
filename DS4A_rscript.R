## fire locations and extent california


library(cowplot)
library(ggrepel)
library(ggspatial)
library(rgeos)
library(ggmap)
library(rgdal)

library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2)
install.packages("map_data")
library(map_data)

library(tigris)



calfires2<-st_read("C:/Users/Casey/Downloads/California_Fire_Perimeters_(all)/California_Fire_Perimeters__all_.shp")

testyr <- calfires2 %>% filter(YEAR_ == 2022)
ggplot()+
  geom_sf(data = testyr, color="black",
          aes(fill= FIRE_NAME), size=0.25, show.legend = FALSE)

county_areas<-counties(state = "california")


calfires2_st<-st_transform(calfires2,crs(county_areas))
crs(calfires2_st)

st_is_valid(calfires2_st)
calfires2_st<-st_make_valid(calfires2_st)


ggplot()+
  geom_sf(data = county_areas, color="black",
        fill="white", size=0.25)



st_intersection_faster <- function(x,y,...){
  #faster replacement for st_intersection(x, y,...)
  
  y_subset <-
    st_intersects(x, y) %>%
    unlist() %>%
    unique() %>%
    sort() %>%
    {y[.,]}
  
  st_intersection(x, y_subset,...)
}


##burned <- st_intersection(testyr, county_areas)#test for one year of data
burned <- st_intersection_faster(calfires2_st, county_areas) ##for all the years (slow)


st_write(burned, "burned_areas_shapefile_cali.shp")


attArea <- burned %>% 
  mutate(burnedarea = st_area(.) %>% as.numeric())

burnedbycounty<-attArea %>% 
  #as_tibble() %>% ##leave out if you want a shapefile, or add in to return a tibble
  group_by(NAME, YEAR_) %>% 
  summarize(total_burn_pyr = sum(burnedarea))

 
  
  
burnedbycounty_st<-st_make_valid(burnedbycounty)

st_write(burnedbycounty, "sum of burned area per county_year.shp")
#write.csv(burnedarea, "burnedareaforDS4.csv")
#burnedarea<-read.csv("burnedareaforDS4.csv")

county_areas_sum<-county_areas %>% mutate(fullarea = st_area(.) %>% as.numeric()) ##units are m2

ggplot()+
  geom_sf(data = burned, color="black",
          aes(fill= CAUSE), size=0.25, show.legend = FALSE)





burnperc2<-
  right_join(county_areas_sum, 
            burnedbycounty_st%>% as_tibble()%>% dplyr::select(NAME, YEAR_,total_burn_pyr), 
            by= join_by("NAME"),multiple = "all")

burnperc2<- burnperc2%>% mutate(perc_burn = (total_burn_pyr/fullarea)*100)

st_write(burnperc, "final_perc.burned_allyears.shp")


ggplot()+
  geom_sf(data=county_areas, color="black", size=0.25)+
  geom_sf(data = burnperc2%>% filter( YEAR_==2000), aes(geometry =geometry,  fill=perc_burn), color="black",
         size=0.25)+
  scale_fill_viridis(option= "plasma")+
  theme_minimal()

  
  
##making a series of plots
install.packages("gganimate")
install.packages("transformr")
install.packages('gifski')
install.packages('png')
library(gifski)
library(png)
library(gganimate)
library(transformr)
library(lubridate)


burnperc2$YEAR_ <-as.character(burnperc2$YEAR_)
burnperc2$YEAR_  <- as.Date(burnperc2$YEAR_, format="%Y")
class(as.DAteyear(burnperc2$YEAR_))

burnperc2$YEAR_  <- burnperc2$YEAR_ %>% as.Date() %>%year()

timeseries<-ggplot()+
  geom_sf(data=county_areas, color="black", size=0.25)+
  geom_sf(data = burnperc2 %>%filter(YEAR_ > 2000), aes(geometry =geometry,  fill=perc_burn), color="black",
          size=0.25)+
  scale_fill_viridis(option= "plasma")+
  theme_minimal()+
  transition_states(YEAR_) +
  labs(title = "Year: {previous_state}")+
  labs(fill = "% of county burned by fire")+
  theme(legend.position= "top")



gganimate::animate(timeseries)
                 
anim_save("timeseries.gif", animation = last_animation())      

final_burn_table<- burnperc2 %>% st_drop_geometry()
head(final_burn_table)
write.csv(final_burn_table, "final_burn_csv.csv")
