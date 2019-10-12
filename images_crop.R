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
