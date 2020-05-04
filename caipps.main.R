# Description: create a Bayesian Network for synthesizing fake questionnaire data.
# Author: Haley Hunter-Zinck
# Date: April 13, 2020
# Usage: R -f caipps.main.R --args <file_node> <file_edge> <outfile_prefix>
# Dependencies: bnlearn, caipps.fxns.R

# begin -----------------------------------------

# start timer
tic = as.double(Sys.time())

# user input
args = commandArgs(trailingOnly = TRUE)
if(length(args) != 3)
{
  cat("Usage: R -f caipps.main.R --args <file_node> <file_edge> <outfile_prefix>")
}

# packages
library(bnlearn)
source("caipps.fxns.R")

# files
#file_node = "caipps_node.csv"
#file_edge = "caipps_edge.csv"
#file_synth = "caipps_synth.csv"
#file_fit = "caipps_fit.rds"
file_node = args[1]
file_edge = args[2]
out_prefix = args[3]

# outfiles
file_synth = paste0(out_prefix,".csv")
file_fit = paste0(out_prefix, ".rds")

# parameters
nsample = 200
DEBUG = TRUE

if(DEBUG)
{
  cat(now(), ": Starting caipps.main.R...\n", sep = "")
}

# graph --------------------------------

if(DEBUG)
{
  cat(now(), ": Building graph...\n", sep="")
}

nodes = read_node(file_node)
edges = read_edge(file_edge)
g = construct_graph(nodes = names(nodes), edges = edges)
fit = fit_graph(nodes, edges)
saveRDS(fit, file_fit)

# generate --------------------------------

if(DEBUG)
{
  cat(now(), ": Generating synthetic dataset...\n", sep="")
}

synth = rbn(x = fit, n = nsample)
write.csv(synth, row.names = TRUE, file = file_synth)

# close --------------------------------

if(DEBUG)
{
  toc = as.double(Sys.time())
  cat(now(), ": Synthetic dataset written to ", file_synth,"\n", sep="")
  cat("Runtime: ", toc - tic, " s\n", sep = "")
}



