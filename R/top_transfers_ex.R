library(kohonen)
library(dplyr)
library(cluster)
library(xtable)
library(ggplot2)
library(gridExtra)

FIG.PATH <- '../figs/'

fit_som_models <- function(df, xdim, ydim, h_function, 
                           toroidal=F, topo='hexagonal', fig.path=FIG.PATH) 
{

    # toroidal doesn't entirely make sense since there isn't anything cyclic about the data

    set.seed(1234)
    som_grid <- somgrid(xdim, ydim, topo, neighbourhood.fct = h_function, toroidal)

    som_model <- som(data.matrix(df), grid=som_grid, rlen=500, mode='batch')

    par(mfrow=c(1,1))

    # training progress to see convergence 
    png(file=paste(fig.path, 'transfer_training.png', sep=''))
    plot(som_model, type='changes')
    dev.off()

    # see how many samples are in each node, aim for 5-10 
    png(file=paste(fig.path, 'transfer_node_samples.png', sep=''))
    plot(som_model, type='count', main='Samples per Node')
    dev.off()

    # plot each data point
    png(file=paste(fig.path, 'transfer_mapping.png', sep=''))
    plot(som_model, type='mapping', pch=20)
    dev.off()

    # U matrix for distance and then clustering!
    png(file=paste(fig.path, 'transfer_U.png', sep=''))
    plot(som_model, type='dist.neighbours', main='SOM neighbor distances')
    dev.off()

    # plot the codebook vectors 
    png(file=paste(fig.path, 'transfer_codebook.png', sep=''))
    plot(som_model, type='codes', zlim=c('red', 'green', 'blue'))
    dev.off()

    som_codebook <- som_model$codes[[1]]
    cols <- c('red', 'blue', 'green', 'yellow', 'black')
    som_hc <- cutree(hclust(object.distances(som_model, 'codes')), k=5)
    png(file=paste(fig.path, 'transfer_hc_boundary.png'))
    plot(som_model, type='mapping', keepMargins = T, col=NA, 
       bg= cols[som_hc], add.cluster.boundaries(som_model, som_hc))
    dev.off()

    # mapping nodes back
    df <-cbind(df, cluster=factor(som_hc[som_model$unit.classif]))

    # Heatmap for specific variable 
    png(file=paste(fig.path, 'transfer_feature_map.png'))
    par(mfrow=c(3,2))
    for (i in 1:ncol(som_codebook)) {
        plot(som_model, type='property', property=som_codebook[,i],
             main=colnames(som_codebook)[i], 
             palette.name = colorRampPalette(c('blue', 'green', 'yellow','red')))
        add.cluster.boundaries(som_model, som_hc)
    }
    dev.off()

    return(df)
}

# Plots market value, transfer fee, and player position
# histogram for each cluster defined by the cluster column.
plot.pp.mv.tf <- function(df, cluster.type)
{
    # position type bar plot per cluster
    pos.plot <-
        ggplot(df, aes(x=Position_type, colour=cluster)) +
            geom_bar()

    # market value histogram per cluster
    mv.plot <- 
        ggplot(df, aes(x=Market_value, colour=cluster)) +
            geom_histogram(bins=30)

    # transfer fee histogram per cluster
    tf.plot <- 
        ggplot(df, aes(x=Transfer_fee, colour=cluster)) +
            geom_histogram(bins=30)

    grid.arrange(pos.plot, mv.plot, tf.plot, 
                 heights=c(1, 1),
                 widths=c(1, 1),
                 layout_matrix=rbind(c(1,2),
                                     c(1,3)),
                 top=paste("Histogram of Position type, Market value, Transfer fee per",
                           cluster.type,
                           "cluster"))
}

# ====================================================================================
# Data Cleaning
# ====================================================================================

# use relative path so that code is machine independent
all_df <- read.csv('../data/top_transfers_all.csv')

# drop the index from reading in 
drop.cols <- c('X', 'Name', 'Position', 'Team_from', 'League_from',
               'Team_to', 'League_to', 'Price_diff', 'Season')
ttl <- all_df[,!(names(all_df) %in% drop.cols)]

# reorder the levels and make factors
league.types <- c('Big5', 'Europe', 'Americas', 'Asia Africa', 'Big5 B', 'Other B')
ttl$League_type_to <- factor(ttl$League_type_to, levels=league.types)
ttl$League_type_from <- factor(ttl$League_type_from, levels=league.types)

position.types <- c('Goalkeeper', 'Defender', 'Midfielder', 'Forward')
ttl$Position_type <- factor(ttl$Position_type, levels=position.types)

# standardize numeric columns
ttl$Transfer_fee <- as.numeric(scale(ttl$Transfer_fee))
ttl$Market_value <- as.numeric(scale(ttl$Market_value))
ttl$Age <- as.numeric(scale(ttl$Age))

# ====================================================================================
# Fit SOM
# ====================================================================================

ttl.sommed <- fit_som_models(ttl, xdim=20, ydim=15, 'gaussian')

# plot position, market value, transfer fee per SOM clusters
png(file=paste(FIG.PATH, 'transfer_som_ppmvtf.png'))
plot.pp.mv.tf(ttl.sommed, cluster.type='SOM')
dev.off()

# Run to get tex code to make table
# xtable(all_df[which(ttl.sommed$cluster == 5), ])

# ====================================================================================
# Fit Hierarchical Clustering
# ====================================================================================

n.clusters <- 5
ttl.dist <- daisy(ttl, metric='gower') 
ttl.dist.hc <- cutree(hclust(ttl.dist, method='complete'), n.clusters)
ttl.hced <- cbind(ttl, cluster=factor(ttl.dist.hc))

# plot position, market value, transfer fee per HC clusters
png(file=paste(FIG.PATH, 'transfer_hc_ppmvtf.png'))
plot.pp.mv.tf(ttl.hced, cluster.type='HC')
dev.off()
