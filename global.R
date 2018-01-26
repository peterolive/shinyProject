library(shiny)
library(data.table)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(maps)
library(sp)
library(ggmap)
library(shinydashboard)





map_data_1 = fread('~/R_learning/CopyOfCopyOfCopyOfshinyProject/San_Francisco_Restaurant_Scores.csv')
map_data_1 = data.frame(map_data_1)
map_data_1 = map_data_1 %>% filter(risk_category != '') %>% 
  mutate(business_phone_number = as.character(business_phone_number)) %>%
  mutate(business_phone_number = paste(substring(business_phone_number, 2, 4), 
                                       substring(business_phone_number, 5, 7),
                                       substring(business_phone_number, 8, 11), sep = '-')) %>%
  mutate(business_phone_number = replace(business_phone_number, 
                                         business_phone_number == 'NA-NA-NA', 
                                         'NA'))
risk = c('100-90', '89-80', '79-70', 'below 70')

########## leaflet map 1 __markers on the map__ ###########
#### make some graphic icons for the markers on the map
reIcons <- iconList('100-90' = makeIcon('thumbs-up-sign_1f44d.png', iconWidth = 24, iconHeight =32),
                    '89-80' = makeIcon('Slightly_Smiling_Face_Emoji.png', iconWidth = 24, iconHeight =32),
                    '79-70' = makeIcon('fearful-face_1f628.png', iconWidth = 24, iconHeight =32),
                    'below 70' = makeIcon('face_vomitting.png', iconWidth = 24, iconHeight =32))

##########leaflet map2
map_data_2 = map_data_1 %>% filter(business_postal_code != '') %>%
  filter(business_postal_code != 'Ca') %>%
  filter(business_postal_code != 'CA') %>%
  mutate(business_postal_code= as.numeric(business_postal_code)) %>%
  filter(business_postal_code != 0) %>%
  filter(business_postal_code < 100000) %>%
  arrange(business_postal_code) 

zipcode = unique(map_data_2$business_postal_code)

########## leaflet map 3
### add address column to the dataframe for label the polyline using map_data_1
map_street = splitadress(map_data_1$business_address)
map_street_1 = cbind(map_data_1, map_street)
map_street_1 = map_street_1 %>%
  mutate(map_street = paste(toupper(substring(map_street, 1,1)), 
                            tolower(substring(map_street, 2)), sep=""))
street = c('Most clean streets', 'Most unclean streets') 

#############get the lat and long of street name of bottom 5 ################
### Ellis
street_Ellis = map_street_1 %>%
  filter(map_street == 'Ellis') %>%
  arrange(latitude) %>% filter(row_number() == 1 | row_number() == n())
el = street_Ellis %>% select(lat = latitude, lon =longitude)
### Broadway
street_Broadway = map_street_1 %>%
  filter(map_street == 'Broadway') %>%
  arrange(longitude) %>% filter(row_number() == 1 | row_number() == n())
br = street_Broadway %>% select(lat = latitude, lon =longitude)
### Jackson
street_Jackson = map_street_1 %>%
  filter(map_street == 'Jackson') %>%
  arrange(latitude) %>% filter(row_number() == 1 | row_number() == n())
ja = street_Jackson %>% select(lat = latitude, lon =longitude)
### Stockton
street_Stockton = map_street_1 %>%
  filter(map_street == 'Stockton') %>%
  arrange(longitude) %>% filter(row_number() == 1 | row_number() == n())
st = street_Stockton %>% select(lat = latitude, lon =longitude)  
### Front
street_Front = map_street_1 %>%
  filter(map_street == 'Front') %>%
  arrange(longitude) %>% filter(row_number() == 1 | row_number() == n())
fr = street_Front %>% select(lat = latitude, lon =longitude) 
###############fancy multiple polylines drawing bottom 5  ################
lst2 = list(el,br,ja,st,fr)
dt2 <- rbindlist(lst2, idcol = "id")

lst_lines2 <- lapply(unique(dt2$id), function(x){
  ## the order of the 'lon' and 'lat' fields is important
  Lines(Line(dt2[id == x, .(lon, lat)]), ID = x)
})
spl_lst2 <- SpatialLines(lst_lines2)
#############get the lat and long of street name of top 5 ################
### Diamond
street_Diamond = map_street_1 %>%
  filter(map_street == 'Diamond') %>%
  arrange(latitude) %>% filter(row_number() == 1 | row_number() == n())
di = street_Diamond %>% select(lat = latitude, lon =longitude)      
### Post
street_Post = map_street_1 %>%
  filter(map_street == 'Post') %>%
  arrange(latitude) %>% filter(row_number() == 1 | row_number() == n())
po = street_Post %>% select(lat = latitude, lon =longitude)      
### 22nd
street_22nd = map_street_1 %>%
  filter(map_street == '22nd') %>%
  arrange(latitude) %>% filter(row_number() == 1 | row_number() == n())
nd = street_22nd %>% select(lat = latitude, lon =longitude)      
### 20th
######## function to differentiate st from ave
street_20th = map_street_1 %>%
  filter(map_street == '20th')

splitadress_st= function(x){
  store = c()
  for (i in 1:length(x)){
    a = tail(unlist(strsplit(x[i], split = ' ')), n=2)[2]
    store = c(store, a)
  }
  return(store)
}
map_street_st = splitadress_st(street_20th$business_address)
street_20th = cbind(street_20th, map_street_st)
street_20th = street_20th %>%
  mutate(map_street_st = paste(toupper(substring(map_street_st, 1,1)), 
                            tolower(substring(map_street_st, 2)), sep=""))

street_20th = street_20th %>%
  filter(map_street_st == 'St') %>%
  arrange(latitude) %>% filter(row_number() == 1 | row_number() == n())
th = street_20th %>% select(lat = latitude, lon =longitude)      
### Noe
street_Noe = map_street_1 %>%
  filter(map_street == 'Noe') %>%
  arrange(longitude) %>% filter(row_number() == 1 | row_number() == n())
no = street_Noe %>% select(lat = latitude, lon =longitude)      
###############fancy multiple polylines drawing top 5  ################
lst1 = list(di,po,nd,no,th)
dt1 <- rbindlist(lst1, idcol = "id")

lst_lines1 <- lapply(unique(dt1$id), function(x){
  ## the order of the 'lon' and 'lat' fields is important
  Lines(Line(dt1[id == x, .(lon, lat)]), ID = x)
})
spl_lst1 <- SpatialLines(lst_lines1)

icons1 <- awesomeIcons(
  icon = 'thumbs-o-up',
  iconColor = 'black',
  library = 'fa',
  markerColor = 'blue'
)

icons2 <- awesomeIcons(
  icon = 'thumbs-o-down',
  iconColor = 'black',
  library = 'fa',
  markerColor = 'orange'
)










