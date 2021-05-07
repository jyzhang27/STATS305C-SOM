library(jsonlite)
library(tidyr)
library(reshape2)
library(data.table)
library(plyr)

transfers <-lapply(readLines("C:/Users/Julie Zhang/Documents/1 Stanford 2020-21/Stats305C/transfers.json"), fromJSON)
transfers_df_list <- lapply(transfers, data.frame, stringsAsFactors = FALSE)
transfers_all_df <- rbindlist(transfers_df_list)
colnames(transfers_all_df)
cols_want <- colnames(transfers_all_df)[c(1,3,4,5,7,9,10, 12,17,18,20,25)]
transfers_df <- transfers_all_df[, c(1,3,4,5,7,9,10, 12,17,18,20,25)]
transfers_df <- as.data.frame(transfers_df)


top_transfers <- read.csv('C:/Users/Julie Zhang/Documents/1 Stanford 2020-21/Stats305C/top250-00-19.csv')
teams_from <- unique(top_transfers$Team_from)
teams_to <- unique(top_transfers$Team_to)
sum(is.na(top_transfers$Market_value))
