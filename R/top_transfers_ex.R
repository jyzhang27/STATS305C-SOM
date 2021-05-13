library(kohonen)

all_df <- read.csv('~/Documents/STATS305C-SOM/data/top_transfers_all.csv')
# drop the index from reading in 
top_transfers_leagues <- all_df[,-c(1,2, 5,6,7,8)]
head(top_transfers_leagues)

top_transfers_leagues[, c(1,3,7,8)] <- lapply(top_transfers_leagues[, c(1,3,7,8)], factor)
head(top_transfers_leagues)
lapply(top_transfers_leagues, summary)

# just try it out 
fit_som_models <- function(df) {
  set.seed(1234)
  som_grid <- somgrid(xdim=20, ydim=20, topo='hexagonal', neighbourhood.fct = 'gaussian', toroidal = F)
  
  som_model <- som(data.matrix(df), grid=som_grid, rlen=500)
  
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
  
  cols <- c('red', 'blue', 'green', 'yellow', 'black')
  plot(som_model, type='mapping', keepMargins = T, col=NA, 
       bg= cols[som_hc], add.cluster.boundaries(som_model, som_hc))
  
  # mapping nodes back
  df <-cbind(df, som_hc[som_model$unit.classif])
  colnames(df)[ncol(df)] <- 'cluster'
  
  # Heatmap for specific variable 
  
  par(mfrow=c(2,2))
  for (i in 1:ncol(som_codebook)) {
    plot(som_model, type='property', property= som_codebook[,i],
         main= colnames(som_codebook)[i], palette.name = colorRampPalette(c('blue', 'green', 'yellow','red')))
    add.cluster.boundaries(som_model, som_hc)
  }
  
  return(df)
}

# try with the market value and transfer fee, again transfer fee is really driving it 
top_transfers_leagues1 <- subset(top_transfers_leagues, select=-c(Price_diff))
top_transfers_leagues1 <- fit_som_models(top_transfers_leagues1)

# cluster #5 has only 5 transfers! they happen to be the 5 transfers with 
# largest transfer fee and largest market value 
sum(top_transfers_leagues1$cluster == 5)
all_df[which(top_transfers_leagues1$cluster == 5), ]
sort(all_df$Transfer_fee, decreasing = T)[1:5]
sort(all_df$Market_value, decreasing = T)[1:21]

# without season 
top_transfers_leagues2 <- subset(top_transfers_leagues, select=-c(Season, Price_diff))
fit_som_models(top_transfers_leagues2)

# try with only the price difference 
top_transfers_leagues3 <- subset(top_transfers_leagues, select=-c(Market_value, Transfer_fee))
head(top_transfers_leagues3)
summary(top_transfers_leagues3)

top_transfers_leagues3 <- fit_som_models(top_transfers_leagues3)
par(mfrow=c(1,1))
hist(top_transfers_leagues3$cluster)

# cluster #5 has only 4 transfers! they happen to be the 4 transfers with largest price gap 
which(top_transfers_leagues3$cluster == 5)
all_df[which(top_transfers_leagues3$cluster == 5), ]
sort(all_df$Price_diff, decreasing = T)[1:5]

