library(kohonen)

top_transfers_leagues <- read.csv('~/Documents/STATS305C-SOM/data/top_transfers_leagues_all.csv')
# drop the index from reading in 
top_transfers_leagues <- top_transfers_leagues[,-1]
top_transfers_leagues$Season <- as.factor(top_transfers_leagues$Season)
head(top_transfers_leagues)

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
  
  # U matrix for distance and then clustering!
  plot(som_model, type='dist.neighbours', main='SOM neighbor distances')
  som_hc <- cutree(hclust(object.distances(som_model, 'codes')), k=5)
  # add.cluster.boundaries(som_model, som_hc) 
  
  # plot the codebook vectors 
  plot(som_model, type='codes', zlim=c('red', 'green', 'blue'))
  som_codebook <- som_model$codes[[1]]
  
  cols <- c('red', 'blue', 'green', 'yellow', 'black')
  plot(som_model, type='mapping', keepMargins = F, col=NA, 
       bg= cols[som_hc], add.cluster.boundaries(som_model, som_hc))
  
  # mapping nodes back
  df <-cbind(df, som_hc[som_model$unit.classif])
  colnames(df)[ncol(df)] <- 'cluster'
  
  # Heatmap for specific variable 
  
  par(mfrow=c(3,3))
  for (i in 1:ncol(som_codebook)) {
    plot(som_model, type='property', property= som_codebook[,i],
         main= colnames(som_codebook)[i], palette.name = colorRampPalette(c('blue', 'green', 'yellow','red')))
    add.cluster.boundaries(som_model, som_hc)

  }
}

fit_som_models(subset(top_transfers_leagues, select=-c(Price_diff)))

# without season 
top_transfers_leagues2 <- subset(top_transfers_leagues, select=-c(Season, Price_diff))
fit_som_models(top_transfers_leagues2)

top_transfers_leagues3 <- subset(top_transfers_leagues, select=-c(Market_value, Transfer_fee))
fit_som_models(top_transfers_leagues3)

               