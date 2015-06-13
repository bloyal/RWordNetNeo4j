#createWordNetNeo4j.R
#Functions for creating a WordNet db in Neo4j

#WordNet Release 3.0 This software and database is being provided to you, 
#the LICENSEE, by Princeton University under the following license. 
#By obtaining, using and/or copying this software and database, 
#you agree that you have read, understood, and will comply with 
#these terms and conditions.: Permission to use, copy, modify and 
#distribute this software and database and its documentation for 
#any purpose and without fee or royalty is hereby granted, 
#provided that you agree to comply with the following copyright notice 
#and statements, including the disclaimer, and that the same appear 
#on ALL copies of the software, database and documentation, 
#including modifications that you make for internal use or for 
#distribution. WordNet 3.0 Copyright 2006 by Princeton University. 
#All rights reserved. THIS SOFTWARE AND DATABASE IS PROVIDED "AS IS" 
#AND PRINCETON UNIVERSITY MAKES NO REPRESENTATIONS OR WARRANTIES, 
#EXPRESS OR IMPLIED. BY WAY OF EXAMPLE, BUT NOT LIMITATION, 
#PRINCETON UNIVERSITY MAKES NO REPRESENTATIONS OR WARRANTIES OF 
#MERCHANT- ABILITY OR FITNESS FOR ANY PARTICULAR PURPOSE OR THAT 
#THE USE OF THE LICENSED SOFTWARE, DATABASE OR DOCUMENTATION WILL 
#NOT INFRINGE ANY THIRD PARTY PATENTS, COPYRIGHTS, TRADEMARKS OR 
#OTHER RIGHTS. The name of Princeton University or Princeton may 
#not be used in advertising or publicity pertaining to distribution 
#of the software and/or database. Title to copyright in this software, 
#database and any associated documentation shall at all times remain 
#with Princeton University and LICENSEE agrees to preserve same.

setwd("~/GitHub/RWordNetNeo4j")

library(RNeo4j);
library(rvest);
library(R.utils);
source('genericGraphFunctions.R');

#Start empty Neo4j graph
newGraph <- function(path="http://localhost:7474/db/data/") {
  graph <- startGraph("http://localhost:7474/db/data/");
  clear(graph, input=FALSE);
  graph;
}

# Create nodes representing the 45 lexicographer files described at
# http://wordnet.princeton.edu/wordnet/man/lexnames.5WN.html
createLexNodes <- function(graph, dictPath = "~/Downloads/WordNet-3.0/dict") {
#  graph <- startGraph(graphPath);
  lexData <- getLexNames(dictPath);
  addIndex(graph,"LexName","fileNumber");
  bulkGraphUpdate(graph, lexData, createSingleLexNode);
}

readVerbDataFile <- function(path="~/Downloads/WordNet-3.0/dict/data.verb"){
  #path<-"~/Downloads/WordNet-3.0/dict/data.verb"
  verbData <- readLines(path);
  verbData <- verbData[30:length(verbData)];
  
  verbIndex <- readLines("~/Downloads/WordNet-3.0/dict/index.verb");
  verbIndex <- verbIndex[30:length(verbIndex)];
  
  lapply(verbData, function(x){ 
    raw<-strsplit(x, " ");
    wordCount<-as.integer(row[[1]][4]);
    words<-row[[1]][5:8]
    list(
        offset = raw[[1]][1],
        pos = "Verb"
      )
    });
}


#-----------Lower-Level Functions----------

getLexNames <- function(dictPath){
  lexData<-read.table(paste(dictPath,"lexnames", sep="/"), sep="\t", col.names=c("fileNumber","fileName","synCat"),stringsAsFactors=FALSE);
  lexData$synCat <- updateSynCat(lexData$synCat);
  lexData["description"] <- getLexDescriptions();
  return(lexData);
}

updateSynCat <- function(synCat){
  synCat <- gsub("1","Noun",synCat);
  synCat <- gsub("2","Verb",synCat);
  synCat <- gsub("3","Adjective",synCat);
  synCat <- gsub("4","Adverb",synCat);
  return(synCat);
}

getLexDescriptions <- function(){
  lexHTML <- getHTML("http://wordnet.princeton.edu/wordnet/man/lexnames.5WN.html");
  lexDesc <- lexHTML %>% html_nodes("table") %>% html_table(header=TRUE, fill=TRUE) %>% data.frame();
  lexDesc<-lexDesc$Contents[2:length(lexDesc$Contents)];
  lexDesc <- gsub("\n","",lexDesc);
  lexDesc <- capitalize(lexDesc);
  return(lexDesc);
}

getHTML <- function(url){
  html(url);
}

createSingleLexNode  <- function(transaction, data){
  query <- "CREATE (:LexName {
                      fileNumber:{fileNumber},
                      fileName:{fileName},
                      synCat:{synCat},
                      description:{description}
                    })";  
  appendCypher(transaction, query, 
               fileNumber = data$fileNumber,
               fileName = data$fileName,
               synCat = data$synCat,
               description = data$description
  );
}  

findSynsetData <- function(offset, data){
  data[grep(paste("^",offset, " .*",sep=""),data)]
}