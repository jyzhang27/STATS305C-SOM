library(kohonen)
library(dplyr)

#all_df <- read.csv('..//data/top_transfers_all.csv')

all_df <- read.csv('~/Documents/STATS305C-SOM/data/top_transfers_all.csv')
# drop the index from reading in 
top_transfers_leagues <- all_df[,-c(1,2,3, 5,6,7,8)]
head(top_transfers_leagues)

top_transfers_leagues$Season <- factor(top_transfers_leagues$Season)

# reorder the levels
top_transfers_leagues$League_type_to <- factor(top_transfers_leagues$League_type_to, levels=c('Big5', 'Europe', 'Americas', 'Asia Africa', 'Big5 B', 'Other B'))
top_transfers_leagues$League_type_from <- factor(top_transfers_leagues$League_type_from, levels=c('Big5', 'Europe', 'Americas', 'Asia Africa', 'Big5 B', 'Other B'))
top_transfers_leagues$Position_type <- factor(top_transfers_leagues$Position_type, levels= c('Goalkeeper', 'Defender', 'Midfielder', 'Forward'))
lapply(top_transfers_leagues, summary)

top_transfers_leagues$Transfer_fee <- as.numeric(scale(top_transfers_leagues$Transfer_fee))
top_transfers_leagues$Market_value <- as.numeric(scale(top_transfers_leagues$Market_value))
top_transfers_leagues$Age <- as.numeric(scale(top_transfers_leagues$Age))

# try with the market value and transfer fee, transfer fee is really driving it 
top_transfers_leagues1 <- subset(top_transfers_leagues, select=-c(Price_diff))
top_transfers_leagues1 <- fit_som_models(top_transfers_leagues1, xdim=20, ydim=15, 'gaussian')

top_transfers_leagues1 %>% count(cluster)

# cluster #4 has only 4 transfers! they happen to be the 4 transfers with 
# largest transfer fee and largest market value 
sum(top_transfers_leagues1$cluster == 1)
all_df[which(top_transfers_leagues1$cluster == 1), ]
sort(all_df$Transfer_fee, decreasing = T)[1:4]
sort(all_df$Market_value, decreasing = T)[1:4]

# cluster #5 has 17 transfers
sum(top_transfers_leagues1$cluster == 5)
sort(all_df[which(top_transfers_leagues1$cluster == 5), 'Market_value'], decreasing = T)
sort(all_df$Market_value, decreasing = T)[5:21]

sort(all_df[which(top_transfers_leagues1$cluster == 5), 'Transfer_fee'], decreasing = T)
sort(all_df$Transfer_fee, decreasing = T)[5:21]

# without season 
top_transfers_leagues2 <- subset(top_transfers_leagues, select=-c(Season, Price_diff))
fit_som_models(top_transfers_leagues2, xdim=20, ydim=15, 'gaussian')
som_fit <- fit_som_models(top_transfers_leagues2, xdim=20, ydim=15, 'bubble')

som_fit %>% count(cluster)
all_df[which(som_fit$cluster == 2), ]

library(cluster)
distances <- daisy(top_transfers_leagues2, metric='gower')
hierarchical_clustering <- cutree(hclust(distances, method='complete'), 5)
summary(factor(hierarchical_clustering))
plot(distances)
all_df[which(hierarchical_clustering == 5), 'Position_type']

# try with only the price difference 
top_transfers_leagues3 <- subset(top_transfers_leagues, select=-c(Market_value, Transfer_fee))
head(top_transfers_leagues3)
summary(top_transfers_leagues3)

top_transfers_leagues3 <- fit_som_models(top_transfers_leagues3, xdim=20, ydim=20, 'bubble', T)
par(mfrow=c(1,1))
hist(top_transfers_leagues3$cluster)
top_transfers_leagues3 %>% count(cluster)

# look at the 4th and 5th cluster
which(top_transfers_leagues3$cluster == 4)
all_df[which(top_transfers_leagues3$cluster >= 4), ]
sort(all_df$Price_diff, decreasing = T)[1:10]

top_transfers_leagues3 %>% count(cluster)

# just try it out 
fit_som_models <- function(df, xdim, ydim, h_function, toroidal=F, topo='hexagonal') {
  # here toroidal doesn't entirely make sense since there isn't anything cyclic about the data
  # can choose the size of the grid and neighborhood function 
  # use the batch algorithm for speed
  set.seed(1234)
  som_grid <- somgrid(xdim, ydim, topo, neighbourhood.fct = h_function, toroidal)
  
  som_model <- som(data.matrix(df), grid=som_grid, rlen=500, mode='batch')
  
  par(mfrow=c(1,1))
  # training progress to see convergence 
  plot(som_model, type='changes')
  
  # see how many samples are in each node, aim for 5-10 
  plot(som_model, type='count', main='Samples per Node')
  
  # plot each data point
  plot(som_model, type='mapping', pch=20)
  
  # U matrix for distance and then clustering!
  plot(som_model, type='dist.neighbours', main='SOM neighbor distances')
  som_hc <- cutree(hclust(object.distances(som_model, 'codes')), k=5)
  # add.cluster.boundaries(som_model, som_hc) 
  
  # plot the codebook vectors 
  plot(som_model, type='codes', zlim=c('red', 'green', 'blue'))
  som_codebook <- som_model$codes[[1]]
  print(som_codebook[1:4,])
  
  cols <- c('red', 'blue', 'green', 'yellow', 'black')
  plot(som_model, type='mapping', keepMargins = T, col=NA, 
       bg= cols[som_hc], add.cluster.boundaries(som_model, som_hc))
  
  # mapping nodes back
  df <-cbind(df, som_hc[som_model$unit.classif])
  colnames(df)[ncol(df)] <- 'cluster'
  
  # Heatmap for specific variable 
  par(mfrow=c(2,3))
  for (i in 1:ncol(som_codebook)) {
    plot(som_model, type='property', property= som_codebook[,i],
         main= colnames(som_codebook)[i], palette.name = colorRampPalette(c('blue', 'green', 'yellow','red')))
    add.cluster.boundaries(som_model, som_hc)
  }
  
  return(df)
}

