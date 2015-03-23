#createWordNetNeo4j.R
#Functions for creating a WordNet db in Neo4j

library(RNeo4j);
library(rvest);
library(R.utils);
source('genericGraphFunctions.R');

#Path to WordNet dict folder
#dictPath <- "~/Downloads/WordNet-3.0/dict";

# Create nodes representing the 45 lexicographer files described at
# http://wordnet.princeton.edu/wordnet/man/lexnames.5WN.html
createLexNodes <- function(dictPath = "~/Downloads/WordNet-3.0/dict", graphPath = "http://localhost:7474/db/data/") {
  graph <- startGraph(graphPath);
  lexData <- getLexNames(dictPath);
  addIndex(graph,"LexName","fileNumber");
  bulkGraphUpdate(graph, lexData, createSingleLexNode);
}

readVerbDataFile <- function(path="~/Downloads/WordNet-3.0/dict/data.verb"){
  verbData <- readLines(path);
  verbData <- verbData[30:length(verbData)];
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