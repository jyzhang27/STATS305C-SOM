library(kohonen)

# just try it out 
transfers_in_2019_data <- transfers_in_data[transfers_in_data$year == 2019, ]

set.seed(1234)

som_grid <- somgrid(xdim=20, ydim=30, topo='hexagonal', neighbourhood.fct = 'gaussian', toroidal = F)

som_model <- som(data.matrix(transfers_in_2019_data), grid=som_grid)
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
transfers_in_2019_data <-cbind(transfers_in_2019_data, som_hc[som_model$unit.classif])
colnames(transfers_in_2019_data )[11] <- 'cluster'


