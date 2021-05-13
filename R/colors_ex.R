library(kohonen)
library(RColorBrewer)

set.seed(1234)
n <- 10000
rgb <- matrix(sample(0:255, 3*n, replace=T), nrow=n, ncol=3)
colnames(rgb) <- c('r', 'g', 'b')
rgb <- data.matrix(rgb)

rgb1 <- fit_som_models_rgb(rgb, 25, 25, 'gaussian', T)
rgb2 <- fit_som_models_rgb(rgb, 25, 25, 'bubble', T)

fit_som_models_rgb <- function(df, xdim, ydim, h_function, toroidal=F, topo='hexagonal') {
  # here toroidal makes sense since there the data is cyclic
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
  som_hc <- cutree(hclust(object.distances(som_model, 'codes')), k=10)
  # add.cluster.boundaries(som_model, som_hc) 
  
  # plot the codebook vectors 
  plot(som_model, type='codes', zlim=c('red', 'green', 'blue'))
  som_codebook <- som_model$codes[[1]]
  
  # plot the colors corresponding to the codebook vectors
  som_codebook_colors <- rgb(som_codebook[,1],som_codebook[,2],som_codebook[,3],maxColorValue=255 )
  plot(som_model, type='mapping', keepMargins = F, col=NA, main='Random Initialization of Codebook Vectors',
       bg= som_codebook_colors[sample.int(length(som_codebook_colors), size=length(som_codebook_colors))])
  plot(som_model, type='mapping', keepMargins = F, col=NA, 
       bg= som_codebook_colors)
  add.cluster.boundaries(som_model, som_hc) 
  
  cols <- brewer.pal(10, 'Spectral')
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
