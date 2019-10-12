library(tidyverse)
library(readxl)
library(janitor)
library(magick)
library(glue)
library(magrittr)

tricks_df <- read_excel("Tricks_overview.xlsx", "MASTER") %>% clean_names
resources_df <- read_excel("Tricks_overview.xlsx", "RESOURCES") %>% clean_names

tricks_df <- tricks_df %>% mutate_if(is.numeric,~coalesce(.,0))
resource_cost_df <- resources_df %>% select(resource,resource_cost)
own_resources_df <- data_frame(resource = c("wood","mirror","saw"),own_resources = c(2,2,1))

tricks_resources_df <- tricks_df %>% 
  select(trick_name,trick_type,one_of(resources_df$resource))

tricks_goals_df <- tricks_df %>% 
  select(trick_name,trick_type,fame_req,uses,action_cost,fame, money,shards)

optimize_tricks <- function(own_resources_df){
tricks_resources_df %>% 
  gather(resource,number_resources,-starts_with("trick")) %>% 
  left_join(own_resources_df) %>% 
  arrange(trick_name) %>% 
  mutate(own_resources = own_resources %>% coalesce(0)) %>% 
  left_join(resource_cost_df) %>% 
  mutate(needed = pmax(number_resources - own_resources, 0)) %>% 
  # filter(needed != 0) %>% 
  group_by_at(vars(starts_with("trick"))) %>% 
  mutate(cost = sum(needed *resource_cost)) %>% 
  select(-number_resources, -own_resources) %>% 
  nest(-starts_with("trick") ,-cost) %>% 
  mutate(data =  map(data, ~.x %>% filter(needed != 0))) %>% 
    left_join(tricks_goals_df) %>% 
  arrange(cost, -fame)
  
}

own_resources_df %>% optimize_tricks()%>% ungroup %>% slice(3) %>% pull(data)

own_resources_df 
