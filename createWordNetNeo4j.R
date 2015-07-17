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
library(R.utils);
library(stringr);
library(plyr);
source('genericGraphFunctions.R');
source('testFunctions.R');

#runIntegrationTests();

# Create nodes representing the 45 lexicographer files described at
# http://wordnet.princeton.edu/wordnet/man/lexnames.5WN.html
createLexNodes <- function(graph, dictPath = "~/Downloads/WordNet-3.0/dict", verbose=TRUE) {
  if(verbose) {print("Creating lexicographer file nodes");}
  lexData <- getLexNames(dictPath);
  addIndex(graph,"LexName","fileNumber");
  bulkGraphUpdate(graph, lexData, createSingleLexNode);
}

#Read in POS data from dict folder
readPOSdata <- function(folderPath="~/Downloads/WordNet-3.0/dict/", verbose=TRUE){
  if(verbose){print("Reading POS data");}
  advData<-readAdvData(folderPath, verbose);
  verbData<-readVerbData(folderPath, verbose);
  adjData<-readAdjData(folderPath, verbose);
  nounData<-readNounData(folderPath, verbose);
  #rbind(advData, verbData, adjData, nounData);
  list(adv = advData, verb = verbData, adj = adjData, noun = nounData);
}

createSynsetNodes <- function(graph,posList, verbose=TRUE){
  if(verbose){print("Creating Synset Nodes");}
  addIndex(graph, "Synset","synsetOffset");
  invisible(lapply(posList, createPOSSpecificSynsetNodes, graph, verbose));
}

createWordNodes <- function(graph, posList, verbose=TRUE){
  if(verbose){print("Creating Word Nodes");}
  addIndex(graph, "Word", "name");
  invisible(lapply(posList, createPOSSpecificWordNodes, graph, verbose));
}

createSynsetPointers <- function(graph, posList, verbose=TRUE){
  if(verbose){print("Creating Synset-Synset Pointers");}
  invisible(lapply(posList, createSynsetSynsetPointers, graph, verbose));
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

getLexDescriptions <- function(path="lexFileLookup.csv"){
  lexDesc <-read.csv(path);
  lexDesc<-lexDesc$Contents#[2:length(lexDesc$Contents)];
  capitalize(lexDesc);
}

# getLexDescriptions <- function(){
#   lexHTML <- getHTML("http://wordnet.princeton.edu/wordnet/man/lexnames.5WN.html");
#   lexDesc <- lexHTML %>% html_nodes("table") %>% html_table(header=TRUE, fill=TRUE) %>% data.frame();
#   lexDesc<-lexDesc$Contents[2:length(lexDesc$Contents)];
#   lexDesc <- gsub("\n","",lexDesc);
#   lexDesc <- capitalize(lexDesc);
#   return(lexDesc);
# }
#
# getHTML <- function(url){
#   html(url);
# }

createSingleLexNode  <- function(transaction, data){
  query <- "MERGE (:LexName {
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

readPosDataFile <- function(path="~/Downloads/WordNet-3.0/dict/data.verb"){
  posData <- readLines(path);
  posData <- posData[30:length(posData)];
  processSynsetData(posData);
}

processSynsetData <- function(synsetData){
  synsetData <- matchSynsetParts(synsetData);
  convertSynsetPartsToDf(synsetData);
}

matchSynsetParts <- function(dataRecord){
  str_match_all(dataRecord,"^(\\d{8}) (\\d{2}) ([nvasr]) (\\w{2}) (.+) \\| (.+)$");
}

convertSynsetPartsToDf <- function(synsetParts){
  ldply(synsetParts, function(x) data.frame(synsetOffset = x[2], 
                                            lexFilenum = x[3], 
                                            lexFileName = translateLexFilenum(x[3]), 
                                            pos = x[4],
                                            posName = translatePOS(x[4]),
                                            wCnt = strtoi(x[5],16),
                                            words = str_match(x[6],"(^.+ \\d{1}) (\\d{3})")[2],
                                            pCnt = as.integer(str_match(x[6],"(^.+ \\d{1}) (\\d{3})")[3]),
                                            pointers = str_match(x[6],"\\d{3} (.+$)")[2],
                                            frames = str_match(x[6],"\\W(\\d{2} \\+ \\d{2} .+)$")[2],
                                            gloss = x[7],
                                            stringsAsFactors = FALSE));
}

translatePOS <-function(posSymbol){
  switch(posSymbol,
         n = "Noun",
         v = "Verb",
         a = "Adjective",
         s = "Adjective Sattellite",
         r = "Adverb",
         other = NA
         );
}

translateLexFilenum <- function(lexFilenum){
  switch(lexFilenum,
         "00"="all adjective clusters",
         "01"="relational adjectives (pertainyms)",
         "02"="all adverbs",
         "03"="unique beginner for nouns",
         "04"="nouns denoting acts or actions",
         "05"="nouns denoting animals",
         "06"="nouns denoting man-made objects",
         "07"="nouns denoting attributes of people and objects",
         "08"="nouns denoting body parts",
         "09"="nouns denoting cognitive processes and contents",
         "10"="nouns denoting communicative processes and contents",
         "11"="nouns denoting natural events",
         "12"="nouns denoting feelings and emotions",
         "13"="nouns denoting foods and drinks",
         "14"="nouns denoting groupings of people or objects",
         "15"="nouns denoting spatial position",
         "16"="nouns denoting goals",
         "17"="nouns denoting natural objects (not man-made)",
         "18"="nouns denoting people",
         "19"="nouns denoting natural phenomena",
         "20"="nouns denoting plants",
         "21"="nouns denoting possession and transfer of possession",
         "22"="nouns denoting natural processes",
         "23"="nouns denoting quantities and units of measure",
         "24"="nouns denoting relations between people or things or ideas",
         "25"="nouns denoting two and three dimensional shapes",
         "26"="nouns denoting stable states of affairs",
         "27"="nouns denoting substances",
         "28"="nouns denoting time and temporal relations",
         "29"="verbs of grooming, dressing and bodily care",
         "30"="verbs of size, temperature change, intensifying, etc.",
         "31"="verbs of thinking, judging, analyzing, doubting",
         "32"="verbs of telling, asking, ordering, singing",
         "33"="verbs of fighting, athletic activities",
         "34"="verbs of eating and drinking",
         "35"="verbs of touching, hitting, tying, digging",
         "36"="verbs of sewing, baking, painting, performing",
         "37"="verbs of feeling",
         "38"="verbs of walking, flying, swimming",
         "39"="verbs of seeing, hearing, feeling",
         "40"="verbs of buying, selling, owning",
         "41"="verbs of political and social activities and events",
         "42"="verbs of being, having, spatial relations",
         "43"="verbs of raining, snowing, thawing, thundering",
         "44"="participial adjectives");
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

readVerbData <- function(path="~/Downloads/WordNet-3.0/dict/", verbose=TRUE){
  if(verbose){print("Reading verb data");}
  path<-paste(path,"data.verb",sep="")
  verbData<-readPosDataFile(path);
  #For verbs, need special step to remove frame from end of pointers
  #Also need to remove space from front of frames!
  #verbData$frames <- 
  removeFramesFromPointers(verbData);
}

removeFramesFromPointers<-function(verbData){
  verbData$pointers<-str_replace(verbData$pointers," \\d{2} \\+ \\d{2} \\d{2}.*$","");
  return(verbData);
}

readAdvData <- function(path="~/Downloads/WordNet-3.0/dict/", verbose=TRUE){
  if(verbose){print("Reading adverb data");}
  path<-paste(path,"data.adv",sep="")
  readPosDataFile(path);
}

readNounData <- function(path="~/Downloads/WordNet-3.0/dict/", verbose=TRUE){
  if(verbose){print("Reading noun data");}
  path<-paste(path,"data.noun",sep="")
  readPosDataFile(path);
}

readAdjData <- function(path="~/Downloads/WordNet-3.0/dict/", verbose=TRUE){
  if(verbose){print("Reading adjective data");}
  path<-paste(path,"data.adj",sep="")
  readPosDataFile(path);
}

createPOSSpecificSynsetNodes <- function(synsetData, graph, verbose=TRUE){
  if(verbose){print(paste("Creating ",synsetData[1,5]," synsets",sep=""))}
  bulkGraphUpdate(graph, synsetData, createSingleSynsetNode);
}

createSingleSynsetNode  <- function(transaction, data){
  #print(data[c("synsetOffset","lexFilenum","lexFileName","pos","posName","wCnt","pCnt","gloss", "words")]);
  query <- "MERGE (:Synset {
                      synsetOffset:{synsetOffset},
                      lexFilenum:{lexFilenum},
                      lexFileName:{lexFileName},
                      pos:{pos},
                      posName:{posName},
                      wCnt:{wCnt},
                      pCnt:{pCnt},
                      gloss:{gloss},
                      words:{words}
                    })";  
  appendCypher(transaction, query, 
               synsetOffset = data$synsetOffset,
               lexFilenum = data$lexFilenum,
               lexFileName = data$lexFileName,
               pos = data$pos,
               posName = data$posName,
               wCnt = data$wCnt,
               pCnt = data$pCnt,
               gloss = data$gloss,
               words = data$words
  );
}

createPOSSpecificWordNodes <- function(synsetData, graph, verbose=TRUE){
  if(verbose){print(paste("Creating ",synsetData[1,5]," words",sep=""))}
  #Make a data frame to map relationships between synset offsets and words
  wordFrame <- getWordFrame(synsetData);
  #print(wordFrame);
  bulkGraphUpdate(graph, wordFrame, createSingleWordNode);
  bulkGraphUpdate(graph, wordFrame, createSingleSynsetWordRelationship);
}

#Create word frame
getWordFrame <- function(synsetData){
  z<-apply(synsetData, 1, transformSynsetDataToWordMap)
  ldply(z)
}

#process single line of synset data (inside apply) to create a narrow data frame with the offset and words
transformSynsetDataToWordMap <- function(synsetLine){
  #print(synsetLine["words"]);
  offset<-synsetLine["synsetOffset"];
  pos<-synsetLine["pos"];
  words<-str_replace_all(str_to_lower(str_match_all(synsetLine["words"], "(\\S+) \\d")[[1]][,2]),"_"," ");
  #print(words);
  df<-data.frame(synsetOffset=offset, pos=pos, name=words, stringsAsFactors=FALSE, row.names=NULL);
  cbind(df,wordNum=as.numeric(rownames(df)));
}

createSingleWordNode <- function(transaction, data){
  #print(data$name);
  query <- "MERGE (:Word {name:{name}})";  
  appendCypher(transaction, query, name = data$name);
}

createSingleSynsetWordRelationship <- function(transaction, data){
  #print(data$pos);
  query <- "MATCH (a:Synset {synsetOffset:{synsetOffset}, pos:{pos}}), (b:Word {name:{name}})
            MERGE (a)-[:has_word {wordNum:{wordNum}}]->(b)";  
  appendCypher(transaction, query, synsetOffset = data$synsetOffset, pos = data$pos, name = data$name, wordNum = data$wordNum);
}

createSynsetSynsetPointers <- function(synsetData, graph, verbose=TRUE){
  #print("Creating SS rels");
  synsetPointerFrame <- getSynsetPointerFrame(synsetData);
  bulkGraphUpdate(graph, synsetPointerFrame, createSingleSynsetSynsetPointer);
}

getSynsetPointerFrame <- function(synsetData){
  #convert Synset data into a data frame with x columns: 
  #start SynID, Start POS, Rel type, End Syn ID, End POS, Start Word, End Word
  z<-apply(synsetData, 1, transformSynsetDataToSynPointerMap)
  ldply(z)
}

#process single line of synset data (inside apply) to create a narrow data frame with x columns: 
#start SynID, Start POS, Rel type, End Syn ID, End POS, Start Word, End Word
transformSynsetDataToSynPointerMap <- function(synsetLine){
  startOffset<-synsetLine["synsetOffset"];
  startPOS<-synsetLine["pos"];
  pointers<-str_match_all(synsetLine["pointers"], "(\\S) (\\d{8}) ([nvasr]) (\\d{2})(\\d{2})")[[1]];
  data.frame(startOffset=startOffset, startPOS=startPOS, 
             relTypeCode=pointers[,2], endOffset=pointers[,3], endPOS=pointers[,4],
             startWordNum=pointers[,5], endWordNum=pointers[,6],
             stringsAsFactors=FALSE, row.names=NULL);
}

createSingleSynsetSynsetPointer <- function(transaction, data){
  #print(data);
  query <- "MATCH (a:Synset {synsetOffset:{startOffset}, pos:{startPOS}}), (b:Synset {synsetOffset:{endOffset}, pos:{endPOS}})
            MERGE (a)-[:has_synset_pointer {typeCode:{typeCode}}]->(b)";  
  appendCypher(transaction, query, startOffset = data$startOffset, startPOS = data$startPOS,
               endOffset = data$endOffset, endPOS = data$startPOS,
               typeCode = data$relTypeCode);
}