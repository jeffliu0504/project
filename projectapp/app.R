#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(jsonlite)
library(httr)
library(ggplot2)
api_key = Sys.getenv("RiotGamesAPIKey")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("LOL Summoner Checking System"),
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "region",
                        label = "Select your region:",
                        choices = c("br1", "eun1", "ecw1","jp1","kr","la1","la2","na1","oc1","ru","tr1")),
            textInput(inputId = "summonerName",
                      label = "Enter your ID",),
            actionButton("Check", "Check")
        ),
        mainPanel(
            h3("Current Season Rank"),
            tableOutput("rank"),
            h3("Favorite Champions"),
            dataTableOutput("Top3ChampTable"),
            h3("Rank Games Win Rate"),
            h2(textOutput("winRate")),
            plotOutput(
                "WinRatePlot",
                width = "100%",
                height = "400px",
                click = NULL,
                dblclick = NULL,
                hover = NULL,
                brush = NULL,
                inline = FALSE
            )
            
            
        )
            



)
)
# Define server logic required to draw a histogram
server <- function(input, output) {

    ShowRank = reactive({
        summonerName = input$summonerName %>% str_replace_all(" ","%20")
        region = input$region
        api_key = Sys.getenv("RiotGamesAPIKey")
        endpoint1 = str_glue("https://{region}.api.riotgames.com/lol/summoner/v4/summoners/by-name/{summonerName}?api_key={api_key}")
        r = GET(endpoint1)
        stop_for_status(r)
        json <- content(r, as = "text")
        encryptedSummonerId = fromJSON(json, flatten = TRUE)$id
        endpoint2 = str_glue("https://{region}.api.riotgames.com/lol/league/v4/entries/by-summoner/{encryptedSummonerId}?api_key={api_key}")
        r = GET(endpoint2)
        stop_for_status(r)
        json <- content(r, as = "text")
        rank = fromJSON(json,flatten = TRUE) %>% select("tier","rank")
    })
    ShowChamp = reactive({
        region = input$region
        summonerName = input$summonerName %>% str_replace_all(" ","%20")
        endpoint1 = str_glue("https://{region}.api.riotgames.com/lol/summoner/v4/summoners/by-name/{summonerName}?api_key={api_key}")
        r = GET(endpoint1)
        stop_for_status(r)
        json <- content(r, as = "text")
        encryptedSummonerId = fromJSON(json, flatten = TRUE)$id
        endpoint3 = str_glue("https://{region}.api.riotgames.com/lol/champion-mastery/v4/champion-masteries/by-summoner/{encryptedSummonerId}?api_key={api_key}")
        r = GET(endpoint3)
        stop_for_status(r)
        json <- content(r, as = "text")
        ChampInfo = fromJSON(json,flatten = TRUE) %>% arrange("championPoints") %>% head(3) %>% select("championId","championLevel","championPoints")
        
        ChampData = fromJSON("http://ddragon.leagueoflegends.com/cdn/11.5.1/data/en_US/champion.json")$data
        names(ChampData)=c(1:154)
        
        ChampName = function(MyList){
            id = data.frame(MyList) %>%  select(ends_with("id"))
            id = id[1,]
            
        }
        NameData = lapply(ChampData,ChampName) %>% unlist()
        
        ChampId = function(MyList){
            key = data.frame(MyList) %>%  select(ends_with("key"))
            key = key[1,]
            
        }
        IdData = lapply(ChampData,ChampId) %>% unlist() %>% as.integer()
        ChampTable = tibble("championId" = IdData,"Champname" = NameData)
        MostUsedChamps = tibble(ChampInfo)
        Top3ChampTable = right_join(ChampTable,MostUsedChamps)
    })
    ShowWinRate = reactive({
        summonerName = input$summonerName %>% str_replace_all(" ","%20")
        region = input$region
        endpoint1 = str_glue("https://{region}.api.riotgames.com/lol/summoner/v4/summoners/by-name/{summonerName}?api_key={api_key}")
        r = GET(endpoint1)
        stop_for_status(r)
        json <- content(r, as = "text")
        encryptedSummonerId = fromJSON(json, flatten = TRUE)$id
        
        endpoint2 = str_glue("https://{region}.api.riotgames.com/lol/league/v4/entries/by-summoner/{encryptedSummonerId}?api_key={api_key}")
        r = GET(endpoint2)
        stop_for_status(r)
        json <- content(r, as = "text")
        rank = fromJSON(json,flatten = TRUE) %>% select("tier","rank")
        WinLoss = fromJSON(json,flatten = TRUE) %>% select("wins","losses")
        WinRate = as.integer(WinLoss$wins)/(as.integer(WinLoss$wins)+as.integer(WinLoss$losses))
        dfWinLoss = data.frame(games = c(as.integer(WinLoss$wins),as.integer(WinLoss$losses)),group = c("win","loss"))
        paste("Win Rate:",WinRate)
    })

    output$winrate = renderText(
        ShowWinRate()
    )
    output$WinRatePlot = renderPlot({
        ShowWinRate()
        ggplot(dfWinLoss,mapping = aes(x=1,y=games,fill = group))+
            geom_bar(stat = "identity")+
            coord_polar(theta = "y")
    })
    output$Top3ChampTable = renderDataTable({
        ShowChamp()
    })
    output$rank = renderTable({
        ShowRank()
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
