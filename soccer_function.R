### Soccer player dashboard
library(rvest)
library(stringr)
library(tidyr)
library(XML)
library(xml2)
library(RCurl)
#Soccer function for getting player data
soccer_scrape = function(limit){
   suppressMessages(library(httr))
   suppressMessages(library(XML))
   suppressMessages(library(data.table))
   v_url = rep(NA, limit)
   v_get = rep(list(matrix(NA, ncol = 3, nrow = 20)), limit)
   v_name = rep(list(matrix(NA, ncol = 1, nrow = 20)), limit)
   V_name_transfer = rep(list(matrix(NA, ncol = 1, nrow = 40)), limit)
   v_player_list = rep(list(matrix(NA, ncol = 2, nrow = 20)), limit)
   v_tranfers_info = rep(list(matrix(NA, ncol = 20, nrow = 30)), limit)
      for(i in 1:limit){
      v_url[i] = paste("http://www.transfermarkt.com/anthony-martial/profil/spieler/",i,sep = "")
      v_get[[i]] = GET(v_url[i])
      v_player_list[[i]] <<-  readHTMLTable(rawToChar(v_get[[i]]$content), stringsAsFactors = F)[[1]]
      v_tranfers_info[[i]] = readHTMLTable(rawToChar(v_get[[i]]$content), stringsAsFactors = F)[[2]]
      v_name[[i]] = data.frame(rep(as.character(unlist(v_get[[i]][1])), nrow(v_player_list_temp[[i]])))
      V_name_transfer[[i]] = data.frame(rep(as.character(unlist(v_get[[i]][1])), nrow(v_tranfers_info_temp[[i]])))
      }
   v_players = rbindlist(v_player_list_temp, fill = T)
   v_transfers = rbindlist(v_tranfers_info_temp, fill = T)
   v_names_players = rbindlist(v_name, fill = T)
   V_names_transfer = rbindlist(V_name_transfer, fill = T)
   write.delim(v_players,'v_players.txt', sep = "~")
   write.delim(v_transfers,'v_transfers.txt', sep = "~")
   write.delim(v_names_players,'v_names_players.txt', sep = "~")
   write.delim(V_names_transfer,'v_names_transfer.txt', sep = "~")
      }

#v_tranfers_info[[i]] <<- ifelse(v_tranfers_info_temp[[i]][3,2] != "1" &&
v_tranfers_info_temp[[i]][3,2] != "2" &&
   v_tranfers_info_temp[[i]][4,2] != "1" &&
   v_tranfers_info_temp[[i]][4,2] != "2",
matrix(NA, ncol = 2, nrow = 20), v_tranfers_info_temp[[i]])



url = 'http://www.transfermarkt.com/silvio-adzic/profil/spieler/109742'
tabs <- GET(url)
z <- readHTMLTable(rawToChar(tabs$content), stringsAsFactors = F)[[2]]



url
x = html_nodes(url,".zentriert")
   
#Pull player data
x = readHTMLTable(url, stringsAsFactors = FALSE)
z = data.frame(x[[1]])
z$name = rep(names(z)[2],nrow(z))



z$name = rep(names(z)[2],nrow(z))

y = x[[2]]
write.csv(z,'z.csv')                 
