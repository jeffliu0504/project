library(tidyverse)
library(jsonlite)
library(httr)
library(ggplot2)

region = 'na1'
usethis::edit_r_environ("project")
readRenviron(".Renviron")
summonerName = "IG Daddy Theshy" %>% str_replace_all(" ","%20")
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
WinLoss = fromJSON(json,flatten = TRUE) %>% select("wins","losses")
WinRate = as.integer(WinLoss$wins)/(as.integer(WinLoss$wins)+as.integer(WinLoss$losses))
dfWinLoss = data.frame(games = c(as.integer(WinLoss$wins),as.integer(WinLoss$losses)),group = c("win","loss"))
WinRatePlot = ggplot(dfWinLoss,mapping = aes(x=1,y=games,fill = group))+
  geom_bar(stat = "identity")+
  coord_polar(theta = "y")


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