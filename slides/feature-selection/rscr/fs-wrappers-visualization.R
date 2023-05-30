if (!requireNamespace("mlr3verse")) {
  install.packages("mlr3verse")
}
if (!requireNamespace("igraph")) {
  install.packages("igraph")
}
if (!requireNamespace("ggraph")) {
  install.packages("ggraph")
}
library(mlr3verse)
library(mlr3learners)
library(mlr3data)
library(mlr3filters)
library(mlr3viz)
library(mlr3)
library(igraph)
library(ggplot2)
library(ggraph)
library(dplyr)


prepare_powerset_graph <- function(algorithm_path,n_var,stopping_point,drop_vertices = TRUE){

    g <- graph.empty(n = nrow(all_combinations), directed = TRUE)
    # The node names will be the variables used
    V(g)$name <- apply(algorithm_path[,1:n_var], 1, function(x) {paste(names(x[x == 1]), collapse = "\n")}) 
    a <- apply(algorithm_path[,0:n_var+1], 1, function(x) {paste("\n RMSE: ", round(x[n_var+1],2), sep = "")})
    a[a == "\n RMSE: NA"] <- ""
    a[algorithm_path[,'sum']>=stopping_point+1] <- ""
    # add it to the names
    V(g)$name <- paste(V(g)$name, a, sep = "")

    # For each pair of models...
    for (i in 1:(nrow(algorithm_path) - 1)) {
    for (j in (i + 1):nrow(algorithm_path)) {
        # If model i is a subset of model j and the difference between the number of features is 1, and we have a performance value, there is an edge
        if ((all(algorithm_path[i,1:n_var ] <= algorithm_path[j,1:n_var ])) 
            & (sum(algorithm_path[j,1:n_var ] - algorithm_path[i,1:n_var ]) == 1) 
            & (!is.na(algorithm_path[j,'regr.rmse']))
            & (!is.na(algorithm_path[i,'regr.rmse']) ) 
            & (algorithm_path[i,'regr.rmse'] == algorithm_path[i,'min_rmse'])
            & sum(algorithm_path[j,1:n_var ]) <= stopping_point  # We only want to plot edges on the first n features to show the steps
        )
            {
            # If the performance for the objective edge, we paint it red (critical path)
            if (algorithm_path[j,'regr.rmse'] == algorithm_path[j,'min_rmse']
            & algorithm_path[j,'regr.rmse'] < algorithm_path[i,'regr.rmse'])
            {
            g <- g + edge(i, j, color = "red")
            } else {
            g <- g + edge(i, j, color = "black")
            }
        }
    }
    }
    # Drop nodes with no edges
    if (drop_vertices == TRUE){
    g <- delete.vertices(g, which(degree(g) == 0))
    }
    return(g)
}

prepare_powerset_graph_backwards <- function(algorithm_path,n_var,stopping_point,drop_vertices = TRUE){
    stopping_point = n_var-stopping_point
    g <- graph.empty(n = nrow(all_combinations), directed = TRUE)
    # The node names will be the variables used
    V(g)$name <- apply(algorithm_path[,1:n_var], 1, function(x) {paste(names(x[x == 1]), collapse = "\n")}) 
    a <- apply(algorithm_path[,0:n_var+1], 1, function(x) {paste("\n RMSE: ", round(x[n_var+1],2), sep = "")})
    a[a == "\n RMSE: NA"] <- ""
    a[algorithm_path[,'sum']<=stopping_point-1] <- ""
    # add it to the names
    V(g)$name <- paste(V(g)$name, a, sep = "")

    # For each pair of models...
    for (i in nrow(algorithm_path):2) {
    for (j in (i - 1):1) {
        # If model i is a superset of model j and the difference between the number of features is 1, and we have a performance value, there is an edge
        if ((all(algorithm_path[i,1:n_var ] >= algorithm_path[j,1:n_var ])) 
            & (sum(algorithm_path[i,1:n_var ] - algorithm_path[j,1:n_var ]) == 1) 
            & (!is.na(algorithm_path[j,'regr.rmse']))
            & (!is.na(algorithm_path[i,'regr.rmse'])) 
            & (algorithm_path[i,'regr.rmse'] == algorithm_path[i,'min_rmse'])
            & sum(algorithm_path[j,1:n_var ]) >= stopping_point  # We only want to plot edges on the first n features to show the steps
        )
            {
            # If the performance for the objective edge, we paint it red (critical path)
            if (algorithm_path[j,'regr.rmse'] == algorithm_path[j,'min_rmse']
            & algorithm_path[j,'regr.rmse'] < algorithm_path[i,'regr.rmse'])
            {
            g <- g + edge(i, j, color = "red")
            } else {
            g <- g + edge(i, j, color = "black")
            }
        }
    }
    }
    # Drop nodes with no edges
    if (drop_vertices == TRUE){
    g <- delete.vertices(g, which(degree(g) == 0))
    }
    return(g)
}



# retrieve task
task = tsk("bike_sharing")
# keep only a couple of columns
variables_to_use=c("temperature", "humidity", "windspeed",'apparent_temperature')
n_var = length(variables_to_use)
task$select(variables_to_use)
task$rename("apparent_temperature","realfeel")

task$rename()

# load learner
learner = lrn("regr.rpart")
instance = fselect(
  fselector   = fs("sequential", strategy = "sfs"),
  task = task,
  learner = learner,
  resampling = rsmp("cv", folds = 10),
  measure = msr("regr.rmse")
)

# get the performance of an empty set
featurless = fselect(
  fselector   = fs("sequential"),
  task = task,
  learner = lrn("regr.featureless"),
  resampling = rsmp("cv", folds = 10),
  measure = msr("regr.rmse")
)
featureless_rmse = as.data.frame(as.data.table(featurless$archive))[1,n_var+1]

# Obtain the archive, and convert it to a data.table and then to a dataframe
df_history = as.data.frame(as.data.table(instance$archive))
algorithm_path = df_history[,0:n_var+1]*1
# Create a matrix with all possible combinations of 4 variables using zeros and 1 and order by the sum of the rows
all_combinations <- expand.grid(rep(list(0:1), n_var))
names(all_combinations) <- names(algorithm_path[,1:n_var])
#  Merge to get performances
algorithm_path <- merge(all_combinations,
                        algorithm_path,
                        by = names(all_combinations[,1:n_var]), all.x = TRUE)
algorithm_path[1,'regr.rmse'] <- featureless_rmse
# add the Sum of features used
algorithm_path$sum <- rowSums(algorithm_path[,1:n_var])
# Get minimum rmse for each sum of features used
algorithm_path <- algorithm_path %>% group_by(sum) %>% mutate(min_rmse = min(regr.rmse, na.rm = TRUE))



# Powerset diagram of depth 1
g<- prepare_powerset_graph(algorithm_path,n_var,1)

# Plot the Hasse diagram, the name of the nodes shold be the variables used
png("slides/feature-selection/figure/fs-wrappers-powerset-tree-1.png", width = 1200, height = 1200)
plot(g, 
     layout =  layout_as_tree(g), 
     vertex.shape='circle',
     vertex.label = V(g)$name,
     edge.arrow.size = 4,
     vertex.label.cex = 2,
     vertex.size = 48)
dev.off()


# Powerset diagram of depth 2
g<- prepare_powerset_graph(algorithm_path,n_var,2)

# Plot the Hasse diagram, the name of the nodes shold be the variables used
png("slides/feature-selection/figure/fs-wrappers-powerset-tree-2.png", width = 1200, height = 1200)
plot(g, 
     layout =  layout_as_tree(g), 
     vertex.shape='circle',
     vertex.label = V(g)$name,
     edge.arrow.size = 4,
     vertex.label.cex = 2,
     vertex.size = 48)
dev.off()


# Powerset diagram of depth 3
g<- prepare_powerset_graph(algorithm_path,n_var,3)

# Plot the Hasse diagram, the name of the nodes shold be the variables used
png("slides/feature-selection/figure/fs-wrappers-powerset-tree-3.png", width = 1200, height = 1200)
plot(g, 
     layout =  layout_as_tree(g), 
     vertex.shape='circle',
     vertex.label = V(g)$name,
     vertex.label.cex = 1.8,
     edge.arrow.size = 3,
     vertex.size = 38)
dev.off()


# Powerset diagram of depth 4
g<- prepare_powerset_graph(algorithm_path,n_var,4)

# Plot the Hasse diagram, the name of the nodes shold be the variables used
png("slides/feature-selection/figure/fs-wrappers-powerset-tree-4.png", width = 1000, height = 1000)
plot(g, 
     layout = layout_as_tree(g), 
     vertex.shape='circle',
     vertex.label = V(g)$name,
     edge.arrow.size = 2,
     vertex.label.cex = 1.5, 
     vertex.size = 38)
dev.off()



# Powerset diagram of depth 1
g<- prepare_powerset_graph(algorithm_path,n_var,1,drop_vertices = FALSE)

# Plot the Hasse diagram, the name of the nodes shold be the variables used
png("slides/feature-selection/figure/fs-wrappers-powerset-all-1.png", width = 1200, height = 1200)
plot(g, 
     layout = layout.reingold.tilford(graph, root = 1),
     vertex.shape='circle',
     vertex.label = V(g)$name,
     edge.arrow.size = 2,
     vertex.label.cex = 1.5,
     vertex.size = 26)
dev.off()


# Powerset diagram of depth 2
g<- prepare_powerset_graph(algorithm_path,n_var,2,drop_vertices = FALSE)

# Plot the Hasse diagram, the name of the nodes shold be the variables used
png("slides/feature-selection/figure/fs-wrappers-powerset-all-2.png", width = 1200, height = 1200)
plot(g, 
     layout = layout.reingold.tilford(graph, root = 1),
     vertex.shape='circle',
     vertex.label = V(g)$name,
     edge.arrow.size = 2,
     vertex.label.cex = 1.5,
     vertex.size = 26)
dev.off()


# Powerset diagram of depth 3
g<- prepare_powerset_graph(algorithm_path,n_var,3,drop_vertices = FALSE)

# Plot the Hasse diagram, the name of the nodes shold be the variables used
png("slides/feature-selection/figure/fs-wrappers-powerset-all-3.png", width = 1200, height = 1200)
plot(g, 
     layout = layout.reingold.tilford(graph, root = 1),
     vertex.shape='circle',
     vertex.label = V(g)$name,
     edge.arrow.size = 2,
     vertex.label.cex = 1.5,
     vertex.size = 26)
dev.off()


# Powerset diagram of depth 4
g<- prepare_powerset_graph(algorithm_path,n_var,4,drop_vertices = FALSE)

# Plot the Hasse diagram, the name of the nodes shold be the variables used
png("slides/feature-selection/figure/fs-wrappers-powerset-all-4.png", width = 1200, height = 1200)
plot(g, 
     layout = layout.reingold.tilford(graph, root = 1),
     vertex.shape='circle',
     vertex.label = V(g)$name,
     edge.arrow.size = 2,
     vertex.label.cex = 1.5,
     vertex.size = 26)
dev.off()





###### backwards #######

# load learner
learner = lrn("regr.rpart")
instance = fselect(
  fselector   = fs("sequential", strategy = "sbs"),
  task = task,
  learner = learner,
  resampling = rsmp("cv", folds = 10),
  measure = msr("regr.rmse")
)

# Obtain the archive, and convert it to a data.table and then to a dataframe
df_history = as.data.frame(as.data.table(instance$archive))
algorithm_path = df_history[,0:n_var+1]*1
# Create a matrix with all possible combinations of 4 variables using zeros and 1 and order by the sum of the rows
all_combinations <- expand.grid(rep(list(0:1), n_var))
names(all_combinations) <- names(algorithm_path[,1:n_var])
#  Merge to get performances
algorithm_path <- merge(all_combinations,
                        algorithm_path,
                        by = names(all_combinations[,1:n_var]), all.x = TRUE)
algorithm_path[1,'regr.rmse'] <- featureless_rmse
# add the Sum of features used
algorithm_path$sum <- rowSums(algorithm_path[,1:n_var])
# Get minimum rmse for each sum of features used
algorithm_path <- algorithm_path %>% group_by(sum) %>% mutate(min_rmse = min(regr.rmse, na.rm = TRUE))


# Powerset diagram of depth 4
g<- prepare_powerset_graph_backwards(algorithm_path,n_var,4)

# Plot the Hasse diagram, the name of the nodes shold be the variables used
png("slides/feature-selection/figure/fs-wrappers-backwards-powerset-tree-4.png", width = 1000, height = 1000)
plot(g, 
     layout = layout_as_tree(g), 
     vertex.shape= 'circle',
     vertex.label = V(g)$name,
     edge.arrow.size = 2,
     vertex.label.cex = 1.5, 
     vertex.size = 38)
dev.off()
