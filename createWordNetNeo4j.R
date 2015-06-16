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
library(stringr);
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
#Deprecated for vectorized implementation beloww
# extractSynsetId <- function(dataRecord){
#   substr(dataRecord,1,8);
# }
# 
# extractSynsetLexFileNum <- function(dataRecord){
#   substr(dataRecord,10,11);
# }
# 
# extractSynsetType <- function(dataRecord){
#   switch(substr(dataRecord,13,13),
#          n = "Noun",
#          v = "Verb",
#          a = "Adjective",
#          s = "Adjective Sattellite",
#          r = "Adverb");
# }
# 
# extractSynsetWordCount <- function(dataRecord){
#   as.numeric(substr(dataRecord, 15,16));
# }
# 
# extractSynsetWords <- function(dataRecord){
#   wordDataCnt<-extractSynsetWordCount(dataRecord) * 2;
#   dataRecord <- substr(dataRecord, 18,nchar(dataRecord));
#   wordData <- unlist(str_extract_all(dataRecord, "\\w+"))[1:wordDataCnt];
#   wordData[c(TRUE,FALSE)];
# }

#---vectorized way to do this with regular expressions
isVerbSynset <- function(dataRecord){
  str_detect(dataRecord,"\\d{8} \\d{2} v");
}

extractSynsetPointerCount <- function(dataRecord){
  as.numeric(str_extract(dataRecord, "\\b[0-9]{3}\\b"));
}

matchVerbSysetParts <- function(dataRecord){
  str_match_all(dataRecord,"^(\\d{8}) (\\d{2}) ([nvasr]) (\\d+) (.+) (\\d{3}) (\\S+ .+ \\d{4}) (.*) \\| (.+)$");
}

matchNonVerbSysetParts <- function(dataRecord){
  str_match_all(dataRecord,"^(\\d{8}) (\\d{2}) ([nvasr]) (\\d+) (.+) (\\d{3}) (\\S+ .+ \\d{4}) \\| (.+)$");
}

processSynsetParts <- function(synsetParts){
  synsetParts <- ifelse(isVerbSynset(synsetParts), 
       matchVerbSysetParts(synsetParts), 
       matchNonVerbSysetParts(synsetParts));
  lapply(synsetParts, function(x){
    x<-x[1,2:ncol(x)];
    x;
    });
}

translateSynsetPointerSymbol <- function(symbol, pos){
  if(symbol=='\\'){
    switch(pos,
           Adjective = "Pertainym (Pertains to Noun)",
           Adverb = "Derived from Adjective")
    }
  else if(symbol=="~"){"Hyponym"}
  else if(symbol=="~i"){"Instance Hyponym"}
  else if(symbol=="-c"){"Member of this Domain - TOPIC"}
  else if(symbol=="-r"){"Member of this Domain - REGION"}
  else if(symbol=="-u"){"Member of this Domain - USAGE"}
  else if(symbol==";c"){"Domain of Synset - TOPIC"}
  else if(symbol==";r"){"Domain of Synset - REGION"}
  else if(symbol==";u"){"Domain of Synset - USAGE"}
  else if(symbol=="@"){"Hypernym"}
  else if(symbol=="@i"){"Instance Hypernym"}
  else if(symbol=="*"){"Entailment"}
  else if(symbol=="&"){"Similar To"}
  else if(symbol=="#m"){"Member Holonym"}
  else if(symbol=="#p"){"Part Holonym"}
  else if(symbol=="#s"){"Substance Holonym"}
  else if(symbol=="%m"){"Member Meronym"}
  else if(symbol=="%p"){"Part Meronym"}
  else if(symbol=="%s"){"Substance Meronym"}
  else if(symbol=="^"){"Also See"}
  else if(symbol=="+"){"Derivationally Related Form"}
  else if(symbol=="<"){"Participle of Verb"}
  else if(symbol=="="){"Attribute"}
  else if(symbol==">"){"Cause"}
  else if(symbol=="$"){"Verb Group"}
}