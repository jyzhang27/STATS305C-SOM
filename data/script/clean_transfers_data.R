library(stringr)

all_transfer_data <- read.csv('../all_transfer_data_no_nan.csv')
head(all_transfer_data)

# remove English Championship data
all_transfer_data <- all_transfer_data[all_transfer_data$league_name !='Championship', ]

# rename midfield position 
all_transfer_data[all_transfer_data == 'midfield'] <- 'Midfielder'

# extract whether move was loan or permanent 
all_transfer_data$transfer_loan <- str_detect(all_transfer_data$fee, 'Loan|loan')

# remove fee and season columns 
all_transfer_data <- subset(all_transfer_data,select=-c(season, fee))

# select the transfers in and out into separate datasets
transfers_in_data <- all_transfer_data[all_transfer_data$transfer_movement=='in', ]
transfers_out_data <- all_transfer_data[all_transfer_data$transfer_movement=='out', ]

# save to directory
write.csv(all_transfer_data, file='../all_transfer_data_cleaned.csv')
write.csv(transfers_in_data, file='../transfers_in.csv')
write.csv(transfers_out_data, file='../transfers_out.csv')
