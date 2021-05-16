library(jsonlite)
library(tidyr)
library(reshape2)
library(data.table)
library(plyr)
library(stringi)

# this data is from 
# https://github.com/mneedham/football-transfers/blob/master/data/transfers.json
# transfers <-lapply(readLines("~/Documents/STATS305C-SOM/data/transfers.json"), fromJSON)
# transfers_df_list <- lapply(transfers, data.frame, stringsAsFactors = FALSE)
# transfers_all_df <- rbindlist(transfers_df_list)
# colnames(transfers_all_df)
# cols_want <- colnames(transfers_all_df)[c(1,3,4,5,7,9,10, 12,17,18,20,25)]
# transfers_df <- transfers_all_df[, c(1,3,4,5,7,9,10, 12,17,18,20,25)]
# transfers_df <- as.data.frame(transfers_df)
# 
# write.csv(transfers_df, 'transfers_other_source.csv')

# this data is from 
# https://www.kaggle.com/vardan95ghazaryan/top-250-football-transfers-from-2000-to-2018
top_transfers <- read.csv('~/Documents/STATS305C-SOM/data/top250-00-19.csv')
top_transfers$Season <- as.factor(substring(top_transfers$Season, 1, 4))
top_transfers <- na.omit(top_transfers)

# make market value and transfer fee in units of 1million
top_transfers$Market_value <- top_transfers$Market_value/1000000
top_transfers$Transfer_fee <- top_transfers$Transfer_fee/1000000

# make a column with difference in price
# whenever running something, only include one of price_diff and {Transfer fee, market value}
top_transfers$Price_diff <- top_transfers$Transfer_fee- top_transfers$Market_value
       
# Change some leagues names to match 
top_transfers[top_transfers == '1.Bundesliga'] <- 'Bundesliga'
top_transfers[top_transfers == 'Série A'] <- 'Serie A'
top_transfers[top_transfers == 'Série B'] <- 'Serie B'
top_transfers[top_transfers == 'Serie C - A'] <- 'Serie C'
top_transfers[top_transfers == 'Serie C - B'] <- 'Serie C'
top_transfers[top_transfers == 'Rel. Ligue 1'] <- 'Ligue 1'
top_transfers[top_transfers == 'Segunda División - Segunda Fase'] <- 'Segunda División'
top_transfers[top_transfers == ' Argentina'] <- 'Primera División'
top_transfers[top_transfers == 'Torneo Final'] <- 'Primera División'
top_transfers[top_transfers == 'Torneo Inicial'] <- 'Primera División'
top_transfers[top_transfers == ' Sweden'] <- 'Allsvenskan'
top_transfers[top_transfers == ' Croatia'] <- '1.HNL'
top_transfers[top_transfers == ' Mexico'] <- 'Liga MX Clausura'
top_transfers[top_transfers == ' Russia'] <- 'Premier Liga'
top_transfers[top_transfers == ' Qatar'] <- 'Stars League'
top_transfers[top_transfers == ' United Arab Emirates'] <- 'UAE Gulf League'
top_transfers[top_transfers == 'Liga 1 - Championship group'] <- 'Liga 1'
top_transfers[top_transfers == 'Superligaen Championship round'] <- 'Superligaen'
top_transfers[top_transfers == 'A Grupa - Championship gr.'] <- ' Bulgaria'
top_transfers[top_transfers == 'Liga MX Apertura'] <- 'Liga MX'
top_transfers[top_transfers == 'Liga MX Clausura'] <- 'Liga MX'
top_transfers[top_transfers == 'Korean FA Cup'] <- ' Korea, South'

# Fix the Super League issue, where 3 different leagues were mapped to the same name
super_league_from <- top_transfers[which(top_transfers$League_from == "Super League"),]
super_league_from <- super_league_from[, 4:5]
unique(super_league_from$Team_from)

super_league_to <- top_transfers[which(top_transfers$League_to == "Super League"),]
super_league_to <- super_league_to[, 6:7]
unique(super_league_to$Team_to)

all_teams <- unique(c(super_league_from$Team_from, super_league_to$Team_to))
all_teams

greece_SL <- c('Panathinaikos', 'Egaleo AO', 'Olympiacos', 'AEK Athens', 'PAOK Saloniki')
china_SL <- c('Liaoning FC', 'QD Jonoon', 'GZ Evergrande', 'BJ Renhe', "DL Yifang","GZ R&F",
              "HN Jianye" ,"JS Suning", "ZJ Greentown", "BJ Sinobo Guoan" ,"SD Luneng", "YB Funde",
              "SH Shenhua","SIPG","HB CFFC", "CC Yatai", "TJ Quanjian", "TJ Teda","CQ Dangdai Lif.")
swiss_SL <- c('FC Basel', 'Grasshoppers', 'FC Sion', 'FC Zürich', 'Neuchatel Xamax', 'BSC Young Boys')

top_transfers[which(top_transfers$Team_from %in% greece_SL), 5] <- 'Greece Super League'
top_transfers[which(top_transfers$Team_to %in% greece_SL), 7] <- 'Greece Super League'
top_transfers[which(top_transfers$Team_from %in% china_SL), 5] <- 'China Super League'
top_transfers[which(top_transfers$Team_to %in% china_SL), 7] <- 'China Super League'
top_transfers[which(top_transfers$Team_from %in% swiss_SL), 5] <- 'Swiss Super League'
top_transfers[which(top_transfers$Team_to %in% swiss_SL), 7] <- 'Swiss Super League'

# Fix Primier Liga Problem where two different leagues were mapped to the same league
premierliga_from <- top_transfers[which(top_transfers$League_from == "Premier Liga"),]
premierliga_from <- premierliga_from[, 4:5]
unique(premierliga_from$Team_from)

premierliga_to <- top_transfers[which(top_transfers$League_to == "Premier Liga"),]
premierliga_to <- premierliga_to[, 6:7]
unique(premierliga_to$Team_to)

all_teams <- unique(c(premierliga_from$Team_from, premierliga_to$Team_to))
all_teams

russian_teams <- c("CSKA Moscow","Torpedo Moscow", "Spartak Moscow", "Zenit S-Pb", "Dinamo Moscow",
                   "Rubin Kazan","Tom Tomsk", "Loko Moscow", "FK Moskau", "Kuban Krasnodar", "Krasnodar",
                   "Anzhi", "KS Samara", "Rostov", 'Saturn')
ukrainian_teams <- c("Metalurh D.", "Shakhtar D.", "Dynamo Kyiv", "Metalist", "Metalurh Z.", "Dnipro",
                     "Zorya Lugansk", "Akhmat Grozny")

top_transfers[which(top_transfers$Team_from %in% russian_teams), 5] <- 'Russian Premier Liga'
top_transfers[which(top_transfers$Team_to %in% russian_teams), 7] <- 'Russian Premier Liga'
top_transfers[which(top_transfers$Team_from %in% ukrainian_teams), 5] <- 'Ukranian Premier Liga'
top_transfers[which(top_transfers$Team_to %in% ukrainian_teams), 7] <- 'Ukranian Premier Liga'
# this one has team_from= Anzhi which was in lower division at that time so change it back
top_transfers[which(top_transfers$Name == 'Odil Akhmedov'), 5] <- '1.Division'

# Separate the leagues into 6 categories
# there is .txt with the name of league and country for some leagues that you can't tell
# Big5 - The top 5 European leagues 
# Big5 B - The lower tiers of the football system in the top 5 countries
# European - All the other top-flight European leagues 
# Asia Africa - All top-flight leagues in Asia or Africa
# Americas - All top-flight leagues in N and S America
# Other B - All lower tiers of non-European clubs

(all_leagues <-unique(c(top_transfers$League_from, top_transfers$League_to)))

big5_leagues <- c('Serie A', 'Premier League', 'Ligue 1', 'Bundesliga', 'LaLiga')
secondary_big5_leagues <- c('Serie B', 'Serie C', 'Ligue 2', 'Championship', 'LaLiga2', 
                            'League One', ' England', '2.Bundesliga', 'Primavera B', 
                            '3.Liga', 'Championnat National', '2a B - Grupo III', 'Ledman Liga Pro')
top_european_leagues <- c('Russian Premier Liga', 'Ukranian Premier Liga', 'Eredivisie', 'Liga NOS', 'Süper Lig', 'Premiership', 
                              'Jupiler Pro League', 'Superligaen', 'Liga 1', 'HET Liga', 'Eliteserien', 'Ekstraklasa', 'Allsvenskan', 'SuperLiga', 
                               '1.HNL', ' Bulgaria', ' Moldova', 'NB I.', 'Swiss Super League', 'Greece Super League')
secondary_leagues <- c('OBOS-ligaen', 'Superettan', '1.Division', 'Proximus League', 'Liga águila II', 'Primera B Nacional', 'Segunda División')

top_aa_leagues <- c('J1 League', 'China Super League', 'UAE Gulf League', 'Professional League','Stars League',
                       "Ligat ha'Al", 'Botola Pro', 'Ligue I Pro', ' Saudi Arabia', ' Korea, South')
top_amer_leagues <- c('Primera División', ' Brazil', ' Paraguay', ' Chile', 'Serie A Segunda Etapa', ' Uruguay',
                      'MLS', ' Canada', 'Liga MX')
setdiff(c(big5_leagues, secondary_big5_leagues, top_european_leagues, secondary_leagues, top_aa_leagues, top_amer_leagues), all_leagues)

# add column of which league type it belongs to based on the league 
get_league_type <- function(leagues) {
  n <- length(leagues) 
  league_classification <- c() 
  for (i in 1:n) {
    league<- leagues[i]
    
    if (league %in% big5_leagues) {
      league_classification[i] <- 'Big5'
    } else if (league %in% secondary_big5_leagues) {
      league_classification[i] <- 'Big5 B'
    } else if (league %in% top_european_leagues) {
      league_classification[i] <- 'Europe'
    } else if (league %in% secondary_leagues) {
      league_classification[i] <- 'Other B'
    } else if (league %in% top_amer_leagues) {
      league_classification[i] <- 'Americas'
    } else if (league %in% top_aa_leagues) {
      league_classification[i] <- 'Asia Africa'
    } 
  }
  return(league_classification)
}

league_type_to <- get_league_type(top_transfers$League_to)
league_type_from <- get_league_type(top_transfers$League_from)

top_transfers$League_type_from <- league_type_from
top_transfers$League_type_to <- league_type_to


# keep only league in league out, no club in club out 
#top_transfers_leagues <- top_transfers[,-c(1,4,6)]

# keep only club in club out, no league in league out 
#top_transfers_clubs <- top_transfers[,-c(1,5,7)]


write.csv(top_transfers, 'top_transfers_all.csv')
#write.csv(top_transfers_leagues, 'top_transfers_leagues_all.csv')
#write.csv(top_transfers_clubs, 'top_transfers_clubs_all.csv')

