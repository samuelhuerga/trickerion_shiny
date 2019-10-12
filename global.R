library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(lubridate)
library(colourpicker)
library(shinyWidgets)
library(DT)
library(fs)
library(glue)
library(janitor)
library(readxl)
library(magrittr)

# source("override.R", local = TRUE) # override 'icon' and 'valueBox'

rv <- reactiveValues()

validColors <-  c("red","yellow","aqua","blue","light-blue","green","navy","teal","olive","lime","orange","fuchsia","purple","maroon","black")


tricks_df <- read_excel("Tricks_overview.xlsx", "MASTER") %>% clean_names
tricks_df <- tricks_df %>% mutate_if(is.numeric,~coalesce(.,0))

resources_df <- read_excel("Tricks_overview.xlsx", "RESOURCES") %>% clean_names
resource_cost_df <- resources_df %>% select(resource,resource_cost)

tricks_resources_df <- tricks_df %>% 
  select(trick_name,trick_type,one_of(resources_df$resource))

tricks_goals_df <- tricks_df %>% 
  select(trick_name,trick_type,fame_req,uses,action_cost,fame, money,shards)


optimize_tricks <- function(own_resources_df, sorting_order){
  tricks_resources_df %>% 
    gather(resource,number_resources,-starts_with("trick")) %>% 
    left_join(own_resources_df) %>% 
    arrange(trick_name) %>% 
    mutate(own_resources = own_resources %>% coalesce(0L)) %>% 
    left_join(resource_cost_df) %>% 
    mutate(needed = pmax(number_resources - own_resources, 0)) %>% 
    # filter(needed != 0) %>% 
    group_by_at(vars(starts_with("trick"))) %>% 
    mutate(cost = sum(needed *resource_cost)) %>% 
    select(-number_resources, -own_resources) %>% 
    nest(-starts_with("trick") ,-cost) %>% 
    mutate(data =  map(data, ~.x %>% filter(needed != 0))) %>% 
    left_join(tricks_goals_df) %>% 
    arrange(cost, -!!sym(sorting_order))
  
}

resources_empty_df <- data_frame(resource = paste0("empty",1:6),own_resources = 1)
marketrow_empty_df <- data_frame(resource = paste0("marketrow",1:4))