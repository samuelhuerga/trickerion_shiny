
dashboardPage(
  
  dashboardHeader(title = "TRICKERION"),
  dashboardSidebar(
    br(),
    h5("SORTING PREFERENCE"),
    radioButtons("sort_yield",
                      "Less coins needed to buy components required first, then...",
                      choiceNames = c("<img src='images/fame.png' alt='Fame' height='25'> more fame",
                                      "<img src='images/coin.png' alt='Coin' height='25'> more coins",
                                      "<img src='images/shard.png' alt='Shards' height='25'> more shards") %>% map(HTML),
                      choiceValues = c("fame","money","shards")),
    br(),
    
    h5("TABLE LEGEND"),
    p(HTML("<img src='images/fame_req.png' alt='Fame required' height='25'> Fame threshold"), class = "discrete"),
    p(HTML("<img src='images/resources_needed.png' alt='Action cost' height='25'> Components need to be bought to prepare the trick"), class = "discrete"),
    p(HTML("<img src='images/coin_spend.png' alt='Coins needed' height='25'> Coins needed to buy these components"), class = "discrete"),
    br(),
    p(HTML("<img src='images/fame.png' alt='Fame' height='25'> Yield fame received when trick is performed"), class = "discrete"),
    p(HTML("<img src='images/coin.png' alt='Coin' height='25'> Yield coins received when trick is performed"), class = "discrete"),
    p(HTML("<img src='images/shard.png' alt='Shards' height='25'> Yield shards received when trick is performed"), class = "discrete"),
    br(),
    p(HTML("<img src='images/uses.png' alt='Uses' width='25'> Number of markers this trick can hold when prepared"), class = "discrete"),
    p(HTML("<img src='images/actions_spend.png' alt='Action cost' height='25'> Number of Workshop actions this tricks needs to be prepared"), class = "discrete"),
    br(),
    h5("CREDITS"),
    HTML('<a href="http://www.samuelhuerga.com">Samuel Huerga </a> <a itemprop="sameAs" href="https://orcid.org/0000-0001-6149-4639" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon"></a></br>'),
    HTML('<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/"><img alt="Licencia de Creative Commons" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a><br />'),
    
    collapsed = T)
  ,
  dashboardBody(
    tags$head(
      tags$script(src = "www/js_scripts/js.cookie.js"),
      tags$script(src = "www/js_scripts/script.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/trickerion.css"),
      tags$script('
                        var width = 0;
                        $(document).on("shiny:connected", function(e) {
                          width = window.innerWidth;
                          Shiny.onInputChange("width", width);
                        });
                        $(window).resize(function(e) {
                          width = window.innerWidth;
                          Shiny.onInputChange("width", width);
                        });
                        ')
      
    ),
    fluidPage(
      fluidRow(
      uiOutput("workshop"),
      uiOutput("market_row")
      ),
        
        br(),
        h5("FILTER RECOMMENDATIONS"),
        fluidRow(
          column(4,
                 checkboxGroupInput("filter_trick_type","Type",inline = T,
                                    choiceNames = c("<img src='images/trick_type_escape.png' height='30'>",
                                                    "<img src='images/trick_type_mechanical.png' height='30'>",
                                                    "<img src='images/trick_type_optic.png' height='30'>",
                                                    "<img src='images/trick_type_spiritual.png' height='30'>") %>% map(HTML),
                                    choiceValues= c("Escape","Mechanical","Optic","Spiritual"),
                                    selected =  c("Escape","Mechanical","Optic","Spiritual"))),
          column(4,
                 checkboxGroupInput("filter_fame_req","Fame required", inline = T,
                                    choiceNames = c("<img src='images/fame_req_1.png' height='30'>",
                                                    "<img src='images/fame_req_16.png' height='30'>",
                                                    "<img src='images/fame_req_36.png' height='30'>") %>% map(HTML),
                                    choiceValues= c("1","16","36"),
                                    selected = c("1","16","36"))),
          column(4,
                 checkboxInput("components_in_market_row","Only consider components which can be bought currently in Market Row")
          )
        ),
        dataTableOutput("tricks_DT")
      )
    )
  # )
)

