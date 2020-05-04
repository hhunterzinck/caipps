# Description: create a Bayesian Network for synthesizing fake questionnaire data.
# Author: Haley Hunter-Zinck
# Date: April 13, 2020

# packages --------------------------------

library(bnlearn)
#library(nnet)

# functions --------------------------------

softmax = function(x)
{
  return(exp(x)/sum(exp(x)))
}

filter_blanks = function(x)
{
  r_indeces = which(x =="")
  if(length(r_indeces)>0)
  {
    return(x[-r_indeces])
  }
  return(x)
}

read_node = function(file)
{
  nodes = list()
  names = c()
  lines = scan(file, sep = "\n", skip = 1, what = "character")
  splt = strsplit(split = ",", x = lines)
  for(i in 1:length(splt))
  {
    names[i] = splt[[i]][1]
    nodes[[i]] = filter_blanks(splt[[i]][2:length(splt[[i]])])
  }
  return(setNames(nodes, names))
}

read_edge = function(file)
{
  edges = as.matrix(read.csv(file, header = TRUE, stringsAsFactors = FALSE))
  return(edges)
}

construct_graph = function(nodes, edges)
{
  g = empty.graph(nodes)
  arcs(g) = edges
  return(g)
}

# Description: construct a random dataset of categorical variables.
# Arguments:
#   x: list with each element representing the values for 
#       each feature and names of elements representing feature
#       label
#   n: number of random samples to produce
# Output: data frame with n samples drawn from x setup
construct_random_dataset = function(x, n)
{
  raw = matrix(NA, nrow = n, ncol = length(x), dimnames = list(c(), names(x)))
  for(i in 1:length(x))
  {
    raw[,i] = sample(x[[i]], size = n, replace = TRUE)
  }
  return(data.frame(raw))
}

fit_graph = function(nodes, edges, n = 100)
{
  tables = list()
  g = construct_graph(names(nodes), edges)
  
  for(i in 1:length(nodes))
  {
    node_label = names(nodes)[i]
    node_value = nodes[[i]]
    
    cat(now(), ": ", node_label, "\n", sep = "")
    
    # get all parents
    indeces = which(edges[,2] == node_label)
    if(length(indeces)==0)
    {
      prob = softmax(runif(n = length(node_value), min = 0, max = 1))
      tables[[i]] = matrix(prob, nrow = 1, dimnames = list(NULL, node_value))
    } else
    {
      parent_labels = edges[indeces,1]
      parent_values = nodes[which(is.element(names(nodes), parent_labels))]
      
      # ensure conditional distributions add to 1
      sub_nodes = c(node_label, parent_labels)
      sub_edges = cbind(parent_labels, rep(node_label,length(parent_labels)))
      sub_g = construct_graph(nodes = sub_nodes, edges = sub_edges)
      sub_values = nodes[which(is.element(names(nodes), c(node_label,parent_labels)))]
      sub_data = construct_random_dataset(x = sub_values, n = n)
      sub_fit = bn.fit(x = sub_g, data = sub_data, method = "bayes")
      
      index = which(names(sub_fit) == node_label)
      if(length(index) == 0)
      {
        index = 1
      } 
      tables[[i]] = sub_fit[[index]][["prob"]] 
    }
    
    names(tables)[i] = node_label
  }
  
  # custom fit
  fit = custom.fit(g, dist = tables)
  
  return(fit)
}

now = function()
{
  return(format(Sys.time(), "%H:%M:%S"))
}
