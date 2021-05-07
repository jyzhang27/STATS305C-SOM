library(jsonlite)
library(tidyr)
library(reshape2)
library(data.table)
library(plyr)
library(stringi)

# this data is from 
# https://www.kaggle.com/vardan95ghazaryan/top-250-football-transfers-from-2000-to-2018

transfers <-lapply(readLines("~/Documents/STATS305C-SOM/data/transfers.json"), fromJSON)
transfers_df_list <- lapply(transfers, data.frame, stringsAsFactors = FALSE)
transfers_all_df <- rbindlist(transfers_df_list)
colnames(transfers_all_df)
cols_want <- colnames(transfers_all_df)[c(1,3,4,5,7,9,10, 12,17,18,20,25)]
transfers_df <- transfers_all_df[, c(1,3,4,5,7,9,10, 12,17,18,20,25)]
transfers_df <- as.data.frame(transfers_df)

write.csv(transfers_df, 'transfers_other_source.csv')

# this data is from 
# https://github.com/mneedham/football-transfers/blob/master/data/transfers.json
top_transfers <- read.csv('~/Documents/STATS305C-SOM/data/top250-00-19.csv')
top_transfers$Season <- as.factor(substring(top_transfers$Season, 1, 4))
top_transfers <- na.omit(top_transfers)

# make market value and transfer fee in units of 1million
top_transfers$Market_value <- top_transfers$Market_value/1000000
top_transfers$Transfer_fee <- top_transfers$Transfer_fee/1000000

# make a column with difference in price
top_transfers$Price_diff <- top_transfers$Transfer_fee- top_transfers$Market_value

all_leagues <-unique(c(top_transfers$League_from, top_transfers$League_to))
       
# Change some leagues names to match 
top_transfers[top_transfers == '1.Bundesliga'] <- 'Bundesliga'
top_transfers[top_transfers == 'Série A'] <- 'Serie A'
top_transfers[top_transfers == 'Série B'] <- 'Serie B'
top_transfers[top_transfers == 'Serie C - A'] <- 'Serie C'
top_transfers[top_transfers == 'Serie C - B'] <- 'Serie C'
top_transfers[top_transfers == 'Rel. Ligue 1'] <- 'Ligue 1'
top_transfers[top_transfers == 'Segunda División - Segunda Fase'] <- 'Segunda División'

# keep only league in league out, no club in club out 
top_transfers_leagues <- top_transfers[,-c(1,4,6)]

# keep only club in club out, no league in league out 
top_transfers_clubs <- top_transfers[,-c(1,5,7)]


write.csv(top_transfers, 'top_transfers_all.csv')
write.csv(top_transfers_leagues, 'top_transfers_leagues_all.csv')
write.csv(top_transfers_clubs, 'top_transfers_clubs_all.csv')

