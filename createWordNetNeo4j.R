#createWordNetNeo4j.R
#Functions for creating a WordNet db in Neo4j

library(RNeo4j);
source('genericGraphFunctions.R');

graph <- startGraph("http://localhost:7474/db/data/");

#Path to WordNet dict folder
dict <- "~/Downloads/WordNet-3.0/dict/"