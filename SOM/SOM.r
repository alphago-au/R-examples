require(kohonen)

library(RColorBrewer) #to use brewer.pal
library(fields) #to use designer.colors
library(RODBC)# Load RODBC package
library(stringdist)

capVector <-function(x, probs=c(0.02,0.98)){
  
  ranges <- quantile(x,probs=probs,na.rm=T)
  x[x<=ranges[1]] <-ranges[1]
  x[x>=ranges[2]] <-ranges[2]
  return(x)
  
}

# Create a connection to the database called "channel"
source("C:/Users/xiaobo.wang/Google Drive/works/git/R-examples/SOM/secret.r")

# Loading exsiting data from database
data_train <- sqlQuery(channel, 
                 "
select *
                 from data_mart.cotd_customer_feature_final 
                 limit 10000;

")

data_train_matrix <- as.matrix(scale(data_train))
data_train_matrix[,c(2:11)][is.na(data_train_matrix[,c(2:11)] )] <- 0;

# topologies are possible
som_grid <- somgrid(xdim = 20, ydim=20, topo="hexagonal")

# Finally, train the SOM, options for the number of iterations,
# the learning rates, and the neighbourhood are available
som_model <- som(data_train_matrix[,c(2:11)], 
                 grid=som_grid, 
                 rlen=100, 
                 alpha=c(0.05,0.01), 
                 keep.data = TRUE,
                 n.hood="circular" )

plot(som_model, type="changes")
plot(som_model, type="count")

source('C:/Users/xiaobo.wang/Google Drive/works/git/R-examples/SOM/coolBlueHotRed.r')

var <- 2 #define the variable to plot 
var_unscaled <- aggregate(as.numeric(data_train[,var]), by=list(som_model$unit.classif), FUN=mean, simplify=TRUE)[,2] 
plot(som_model, type = "property", property=var_unscaled, main=names(data_train)[var], palette.name=coolBlueHotRed)
plot(som_model, type = "property", property = som_model$codes[,8], main='test', palette.name=coolBlueHotRed)


names(som_model$data)

pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')


## use hierarchical clustering to cluster the codebook vectors
som_cluster <- cutree(hclust(dist(som_model$codes)), 6)
# plot these results:
plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters") 
add.cluster.boundaries(som_model, som_cluster)
