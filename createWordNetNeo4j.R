#createWordNetNeo4j.R
#Functions for creating a WordNet db in Neo4j

library(RNeo4j);
source('genericGraphFunctions.R');

graph <- startGraph("http://localhost:7474/db/data/");

#Path to WordNet dict folder
dictPath <- "~/Downloads/WordNet-3.0/dict";

createLexNodes <- function(graph, dictPath) {
  getLexNames(dictPath);
}

getLexNames <- function(dictPath){
  lexData<-read.table(paste(dictPath,"lexnames", sep="/"), sep="\t", col.names=c("fileNumber","fileName","synCat"),stringsAsFactors=FALSE);
  lexData$synCat <- updateSynCat(lexData$synCat);
  return(lexData);
}

updateSynCat <- function(synCat){
  synCat <- gsub("1","Noun",synCat);
  synCat <- gsub("2","Verb",synCat);
  synCat <- gsub("3","Adjective",synCat);
  synCat <- gsub("4","Adverb",synCat);
  return(synCat);
}