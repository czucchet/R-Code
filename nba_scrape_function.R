soccer_team_scrape = function(start, limit){
   library(httr)
   library(XML)
   library(data.table)
   library(caroline)
   v_url = rep(NA, length(last))
   v_url = paste("http://insider.espn.com/nba/player/stats/_/id/", first:last,"stephen-curry")
             
   
   
   
   
   lego_movie <- read_html("http://afltables.com/afl/teams/brisbanel/overall_wl.html")
   system.time(read_html("http://afltables.com/afl/teams/brisbanel/overall_wl.html") %>%
      html_nodes("table") %>%
      html_table(fill = T))
read_html("http://afltables.com/afl/teams/brisbanel/overall_wl.html")   
   

 
 
 
 
 

soccer_team_scrape = function(start, limit){
   suppressMessages(library(httr))
   suppressMessages(library(XML))
   suppressMessages(library(data.table))
   suppressMessages(library(caroline))
   v_url = rep(NA, length(start:limit))
   v_get = rep(list(matrix(NA, ncol = 1, nrow = 1)), length(start:limit))
   v_team_list = rep(list(matrix(NA, ncol = 10, nrow = 80)), length(start:limit))
   url_default = 'http://www.transfermarkt.com/real-madrid/startseite/verein/1/saison_id/2016'
   v_url = paste("http://www.transfermarkt.com/real-madrid/startseite/verein/", start:limit,"/saison_id/2016",sep = "")
   for(i in 1:length(start:limit)){
      v_get[[i]] = GET(v_url[i])
      #v_get_all = GET(v_url[i])
      if(v_get[[i]][1] == "http://www.transfermarkt.com/spieler-statistik/wertvollstespieler/marktwertetop"){
         v_get[[i]] = GET(url_default)
      }
      if(length(readHTMLTable(rawToChar(v_get[[i]]$content), stringsAsFactors = F)) < 4){
         v_get[[i]] = GET(url_default)
      }
      v_team_list[[i]] = try(readHTMLTable(rawToChar(v_get[[i]]$content), stringsAsFactors = F)[[4]])
      if(ncol(v_team_list[[i]]) != 6){
         v_team_list[[i]] = NULL
         }
      names(v_team_list[[i]]) = c('v1','v2','v3','v4','v5','v6')
   }
   v_players_value = rbindlist(v_team_list, fill = T)
   v_players_value_file_name = paste("v_players_value",start,"_",limit,".txt", sep = "")
   write.delim(v_players_value,v_players_value_file_name, sep = "~")   
}

system.time(soccer_team_scrape(1,1000))
system.time(soccer_team_scrape(1001,2000))
system.time(soccer_team_scrape(2001,3000))

