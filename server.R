function(input,output,session){
  
  rv <- reactiveValues()
  
  rv$own_resources_df <-  data_frame(resource = "",own_resources = 0L) %>% slice(-1)
  rv$marketrow_resources_df<-  data_frame(resource = "") %>% slice(-1)
 
  walk(resources_df$resource,
       ~observeEvent(input[[glue("resource_{.x}")]],{
         if(.x %in% isolate(rv$own_resources_df %>% pull(resource))){
           rv$own_resources_df <<- isolate(rv$own_resources_df) %>% 
             mutate(own_resources = ifelse(resource == .x,pmin(own_resources + 1L,3L),own_resources))
         }else {
           rv$own_resources_df <<- isolate(rv$own_resources_df) %>% 
             bind_rows(data_frame(resource = .x,own_resources = 1L))
         }
       }
       )
  )
  

  
  output$own_resources_html <- renderUI({
    # req(nrow(rv$own_resources_df) != 0)
    
    rv$own_resources_df %>% 
      bind_rows(resources_empty_df %>% 
                  slice(1:6 %>% setdiff(rv$own_resources_df %>% 
                                          mutate(rn = row_number()) %>% 
                                          pull))) %>% 
      mutate(own_resources = ifelse(own_resources == 1,"",own_resources))%$% 
      map2(resource, own_resources,
           ~paste0("<div class='resource'>",
                   actionButton(glue("resource_own_remove_{.x}"),
                                label = HTML(glue("<img src='images/resources_90/{.x}.png' width = '90px'>
                  <div class='bottom-right'>{.y}</div>"))),
              "</div>")) %>% 
      reduce(paste,"\n") %>% 
      paste("<div class='resources-own'>",.,"</div>") %>% 
      HTML
  })

  walk(resources_df$resource,
       ~observeEvent(input[[glue("resource_own_remove_{.x}")]],{
           rv$own_resources_df <<- isolate(rv$own_resources_df) %>% 
             filter(resource != .x)
       }
       )
  )
  
  
  output$tricks_DT <- renderDataTable({
    req(rv$own_resources_df)
    
    rv$own_resources_df %>% 
      optimize_tricks(input$sort_yield) %>% 
      ungroup %>% 
      filter(trick_type %in% input$filter_trick_type, fame_req %in% input$filter_fame_req) %>% 
      filter(!input$components_in_market_row | map_lgl(data, ~nrow(suppressMessages({.x %>% anti_join(rv$marketrow_resources_df)})) == 0)) %>% 
      mutate(data = map_chr(data, function(data){if(nrow(data) == 0){""} else{data %$% 
          map2(resource, needed,
               ~glue("<div class='resource-table'>
              <img src='images/resources_30/{.x}.png' width = '30px'>
                  <div class='bottom-right'>{.y}</div>
              </div>")) %>% 
          reduce(paste,"\n") %>% 
          paste("<div class='resources-needed'>",.,"</div>") %>% 
          HTML}})) %>% 
      # select(-data) %>% 
      mutate(trick_type = glue("<img src='images/trick_type_{stringr::str_to_lower(trick_type)}.png' alt='{trick_type}' height='30'>")) %>% 
      mutate(fame_req = glue("<img src='images/fame_req_{fame_req}.png' alt='{fame_req}' height='30'>")) %>% 
      mutate_at(vars(fame,money,shards), ~na_if(.,0)) %>% 
      select(trick_type,trick_name,fame_req,data,cost,fame,money,shards,uses,action_cost) %>% 
      datatable(escape = F,
                rownames = F,
                colnames = c("",
                             "",
                             "<center><img src='images/fame_req.png' alt='Fame required' height='25'></center>",
                             "<center><img src='images/resources_needed.png' alt='Action cost' height='25'></center>",
                             "<center><img src='images/coin_spend.png' alt='Coins needed' height='25'></center>",
                             "<center><img src='images/fame.png' alt='Fame' height='25'></center>",
                             "<center><img src='images/coin.png' alt='Coin' height='25'></center>",
                             "<center><img src='images/shard.png' alt='Shards' height='25'></center>",
                             "<center><img src='images/uses.png' alt='Uses' width='25'></center>",
                             "<center><img src='images/actions_spend.png' alt='Action cost' height='25'></center>"),
                style = "bootstrap",
                selection = 'none',
                # extensions = "FixedColumns",
                options = list(dom = 't',
                               pageLength = 64,
                               scrollX = TRUE,
                               scrollCollapse= TRUE,
                               # fixedColumns = list(leftColumns = 2),
                               
                               columnDefs = list(list(className = 'dt-center', orderable= F, targets = "_all")))
                ) %>% 
      formatStyle(c(4,6,9),`border-left` = '1px solid #cccccc')
    
  },server=F)

  # Market Row
  
  output$resources_market_row <- renderUI({
    req(input$components_in_market_row)
    
    tagList(
    p("Available components"),
    resources_df %>% 
      mutate(button = map(resource, ~actionButton(glue("resource_marketrow_{.x}"),
                                                  label = HTML(glue("<img src='images/resources_45/{.x}.png' width = '45px'>"))))) %>% 
      group_by(y) %>% 
      summarise(buttons = tagList(button)) %>% 
      mutate(buttons = map(buttons,~ append(.x,tagList(br())))) %>% 
      summarise(buttons = tagList(buttons)) %>% 
      pull %>%
      paste("<div class='resources-market'>",.,"</div>") %>%
      HTML
    )
  })
  
  walk(resources_df$resource,
       ~observeEvent(input[[glue("resource_marketrow_{.x}")]],{
         rv$marketrow_resources_df <<- isolate(rv$marketrow_resources_df) %>% 
           bind_rows(data_frame(resource = .x)) %>% 
           distinct
       }
       )
  )
  output$workshop <- renderUI({
    
      box(width = ifelse(input$components_in_market_row,7,12),title = "WORKSHOP",status = "primary",
          column(width = 4,
                 p("Available components"),
                 resources_df %>% 
                   mutate(button = map(resource, ~actionButton(glue("resource_{.x}"),
                                                               label = HTML(glue("<img src='images/resources_45/{.x}.png' width = '45px'>"))))) %>% 
                   group_by(y) %>% 
                   summarise(buttons = tagList(button)) %>% 
                   mutate(buttons = map(buttons,~ append(.x,tagList(br())))) %>% 
                   summarise(buttons = tagList(buttons)) %>% 
                   pull %>% 
                   paste("<div class='resources-market'>",.,"</div>") %>% 
                   HTML
                 
          ),
          flechas(input$width),
          column(width = 6,
                 p("Components you already own"),
                 htmlOutput("own_resources_html")
          )
      )
    
  })
  
  output$market_row <- renderUI({
    req(input$components_in_market_row)
    req(input$width)
    
      box(title = "MARKET ROW",width = ifelse(input$components_in_market_row,5,12),status = "info",
          column(width = 5,
                 uiOutput("resources_market_row")
          ),
          flechas(input$width),
          column(width = 5,
                 p("Components available in the Market Row"),
                 rv$marketrow_resources_df %>% 
                   bind_rows(marketrow_empty_df %>% 
                               slice(1:4 %>% setdiff(rv$marketrow_resources_df %>% 
                                                       mutate(rn = row_number()) %>% 
                                                       pull))) %$% 
                   map(resource,
                       ~paste0("<div class='resource'>",
                               actionButton(glue("resource_marketrow_remove_{.x}"),
                                            label = HTML(glue("<img src='images/resources_90/{.x}.png' width = '90px'>"))),
                               "</div>")) %>% 
                   reduce(paste,"\n") %>% 
                   paste("<div class='resources_market_row_actual'>",.,"</div>") %>% 
                   HTML
          )
      )
    
  })
  
  
  walk(resources_df$resource,
       ~observeEvent(input[[glue("resource_marketrow_remove_{.x}")]],{
         rv$marketrow_resources_df <<- isolate(rv$marketrow_resources_df) %>% 
           filter(resource != .x)
       }
       )
  )
    
  output$keepAlive <- renderText({
    req(input$count)
    paste("")
  })
}