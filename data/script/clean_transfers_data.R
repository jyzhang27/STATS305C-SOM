library(stringr)

#all_transfer_data <- read.csv('../all_transfer_data_no_nan.csv')
all_transfer_data <- read.csv('~/Documents/STATS305C-SOM/data/all_transfer_data_no_nan.csv')
head(all_transfer_data)

# remove English Championship data
all_transfer_data <- all_transfer_data[all_transfer_data$league_name !='Championship', ]

# rename midfield position 
all_transfer_data[all_transfer_data == 'midfield'] <- 'Midfielder'

# extract whether move was loan or permanent 
all_transfer_data$transfer_loan <- str_detect(all_transfer_data$fee, 'Loan|loan')

# remove fee and season columns 
all_transfer_data <- subset(all_transfer_data,select=-c(season, fee))

# get all the clubs within the big leagues
all_clubs <- unique(all_transfer_data$club_name)
leagues <- unique(all_transfer_data$league_name)
clubs_leagues <- unique(subset(all_transfer_data, select=c(league_name, club_name)))

# select the transfers in and out into separate datasets
transfers_in_data <- all_transfer_data[all_transfer_data$transfer_movement=='in', ]
transfers_in_data <- subset(transfers_in_data, select=-c(transfer_movement))
head(transfers_in_data)
clubs <- transfers_in_data$club_involved_name

league <- c()
for (i in 1:length(clubs)) {
  club <- clubs[i]
  if (club %in% clubs_leagues$club_name){
    league[i] <- clubs_leagues$league_name[which(clubs_leagues$club_name == club)]
  } else {
    league[i] <- 'Other'
  }
  
}

transfers_in_data$transfer_league <- league

transfers_out_data <- all_transfer_data[all_transfer_data$transfer_movement=='out', ]
transfers_out_data <- subset(transfers_out_data, select=-c(transfer_movement))

# save to directory
write.csv(all_transfer_data, file='../all_transfer_data_cleaned.csv')
write.csv(transfers_in_data, file='../transfers_in.csv')
write.csv(transfers_out_data, file='../transfers_out.csv')
