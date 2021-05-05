library(kohonen)

set.seed(1234)
n <- 10000
rgb <- matrix(sample(0:255, 3*n, replace=T), nrow=n, ncol=3)
colnames(rgb) <- c('r', 'g', 'b')
rgb <- data.matrix(rgb)

# choice between two neighborhood functions, first do bubble
som_grid_b <- somgrid(xdim=25, ydim=25, topo='hexagonal', neighbourhood.fct = 'bubble', toroidal = T)
# gaussian looks better! 
som_grid_g <- somgrid(xdim=25, ydim=25, topo='hexagonal', neighbourhood.fct = 'gaussian', toroidal = T)

som_model <- som(rgb, grid=som_grid_g, rlen=500)

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
som_codebook_colors <- rgb(som_codebook[,1],som_codebook[,2],som_codebook[,3],maxColorValue=255 )

plot(som_model, type='mapping', keepMargins = F, col=NA, 
     bg= som_codebook_colors[sample.int(length(som_codebook_colors), size=length(som_codebook_colors))])
plot(som_model, type='mapping', keepMargins = F, col=NA, 
     bg= som_codebook_colors)
add.cluster.boundaries(som_model, som_hc) 

cols <- c('red', 'blue', 'green', 'yellow', 'black')
plot(som_model, type='mapping', keepMargins = F, col=NA, 
     bg= cols[som_hc], add.cluster.boundaries(som_model, som_hc))

# mapping nodes back
rgb<-cbind(rgb, som_hc[som_model$unit.classif])
colnames(rgb)[4] <- 'cluster'
