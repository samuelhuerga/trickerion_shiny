library(tidyverse)
library(readxl)
library(janitor)
library(magick)
library(glue)
library(magrittr)

resources_image <- image_read("www/images/resources_original.jpg")
x <- 0:3
y <- 0:2

recorta <- function(x,y, name){
  size <- 137
  offset <- 12
  geometry <- glue("{size}x{size}+{1 + size*x + offset*(x)}+{1 + size*y + offset*(y)}")
  print(geometry)
  img <- resources_image %>% image_crop(geometry) %>% image_modulate(brightness = 125)
  img %>% image_write(glue("www/images/{name}.png"))
}


resources_df %$% 
  pwalk(list(x,y,resource),recorta)



resources_empty_image <- image_read("www/images/resources_empty.png")
recorta_empty <- function(x){
  size <- 97
  offset <- 28.5
  geometry <- glue("{size}x{size}+{12 + size*x + offset*x}+10")
  print(geometry)
  img <- resources_empty_image %>% image_crop(geometry) %>% image_colorize(opacity = 75, color = "white")
  img %>% image_write(glue("www/images/empty{x+1}.png"))
  
  
}
0:5 %>% walk(recorta_empty)

marketrow_empty_image <- image_read("www/images/marketrow.png")
recorta_market <- function(x,y){
  size <- 109
  offset <- 29.5
  geometry <- glue("{size}x{size}+{size*x + offset*(x)}+{size*y + offset*(y)}")
  print(geometry)
  img <- marketrow_empty_image %>% image_crop(geometry) %>% image_colorize(opacity = 75, color = "white")
  img %>% image_write(glue("www/images/marketrow{x+y*2+1}.png"))
  
  
}
walk2(c(0,0,1,1),c(0,1,0,1),recorta_market)

# Redimension images according to sizes shown:

# height: 25px
c('www/images/fame.png' ,
'www/images/coin.png' ,
'www/images/shard.png',
'www/images/fame_req.png' ,
'www/images/resources_needed.png' ,
'www/images/coin_spend.png',
'www/images/fame.png' ,
'www/images/coin.png' ,
'www/images/shard.png',
'www/images/uses.png' ,
'www/images/actions_spend.png') %>% 
  walk(~.x %>% image_read %>% image_resize("x25") %>% image_write(.x))



# heigth: 30px

c('www/images/trick_type_escape.png' ,
'www/images/trick_type_mechanical.png' ,
'www/images/trick_type_optic.png' ,
'www/images/trick_type_spiritual.png' ,
'www/images/fame_req_1.png' ,
'www/images/fame_req_16.png' ,
'www/images/fame_req_36.png' ) %>% 
  walk(~.x %>% image_read %>% image_resize("x30") %>% image_write(.x))
  

#Resources 30 px:

resources_df %$%
  walk(resource,~image_read(glue("www/images/{.x}.png")) %>% 
         image_resize("x30") %>% 
         image_write(glue("www/images/resources_30/{.x}.png")))
#Resources 45 px:

resources_df %$%
  walk(resource,~image_read(glue("www/images/{.x}.png")) %>% 
         image_resize("x45") %>% 
         image_write(glue("www/images/resources_45/{.x}.png")))


#Resources 90 px:

resources_df %$%
  walk(resource,~image_read(glue("www/images/{.x}.png")) %>% 
         image_resize("x90") %>% 
         image_write(glue("www/images/resources_90/{.x}.png")))

c(paste0("empty",1:6),paste0("marketrow",1:4)) %>%
  walk(~image_read(glue("www/images/{.x}.png")) %>% 
         image_resize("x90") %>% 
         image_write(glue("www/images/resources_90/{.x}.png")))
