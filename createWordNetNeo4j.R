#createWordNetNeo4j.R
#Functions for creating a WordNet db in Neo4j
setwd("~/GitHub/RWordNetNeo4j")

library(RNeo4j);
library(R.utils);
library(stringr);
library(plyr);
source('genericGraphFunctions.R');
source('testFunctions.R');
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------

#runIntegrationTests();

createWordNetGraph <- function(dictPath = "~/Downloads/WordNet-3.0/dict", verbose=TRUE){
  if(verbose) {print(paste(Sys.time(),"Starting graph creation", sep=": "))};
  if(verbose) {print(paste(Sys.time(),"Clearing Neo4j data", sep=": "))};
  #Initialize graph
  graph<-newGraph(username="neo4j", password="graph");
  
  #Create nodes representing the 45 lexicographer file names
  createLexNodes(graph, dictPath, verbose=verbose);
  
  #Create nodes representing the 35 verb frames
  createFrameNodes(graph, verbose=verbose);
  
  #Create synsets
  wordNetData <- readPOSdata(dictPath, verbose);
  createSynsetNodes(graph, wordNetData, verbose=verbose);
  
  #Create words
  wordFrame <- ldply(lapply(wordNetData, getWordFrame));
  createWordNodes(graph, wordFrame, verbose=verbose);
  
  #Create semantic pointers
  pointerFrame <- ldply(lapply(wordNetData, getSynsetPointerFrame));
  createSemanticPointers(graph, pointerFrame[pointerFrame$startWordNum==0,], verbose=verbose);
  
  #Create lexical pointers
  pointerFrame <- getLexicalPointerWords(pointerFrame[pointerFrame$startWordNum!=0,], wordFrame);
  createLexicalPointers(graph, pointerFrame, verbose=verbose);
  
  #Create verb frame relationships
  verbFrameFrame<- ldply(apply(wordNetData$verb,1,transformSynsetDataToFrameMap));
  createVerbFrameRelationships(graph, verbFrameFrame, verbose=verbose);
  
  if(verbose) {print(paste(Sys.time(),"Graph creation complete", sep=": "))};
}

#--------------------------------------------------------------------------------------
# Functions for creating lex nodes
#--------------------------------------------------------------------------------------

# Create nodes representing the 45 lexicographer files described at
# http://wordnet.princeton.edu/wordnet/man/lexnames.5WN.html
createLexNodes <- function(graph, dictPath = "~/Downloads/WordNet-3.0/dict", verbose=TRUE) {
  if(verbose) {print(paste(Sys.time(),"Creating lexicographer file nodes", sep=": "))};
  lexData <- getLexNames(dictPath, verbose);
  addIndex(graph,"LexName","fileNumber");
  bulkGraphUpdate(graph, lexData, createSingleLexNode);
}

getLexNames <- function(dictPath, verbose){
  if(verbose) {print(paste(Sys.time(),"Retrieving lex file names", sep=": "))};
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

#--------------------------------------------------------------------------------------
# Functions for creating verb frame nodes
#--------------------------------------------------------------------------------------
createFrameNodes <- function(graph, verbose=TRUE){
  if(verbose) {print(paste(Sys.time(),"Creating verb sentance frame nodes", sep=": "))};
  frameData<- read.csv("verbFrameLookup.csv", stringsAsFactors = FALSE);
  addIndex(graph,"VerbFrame","number");
  bulkGraphUpdate(graph, frameData, createSingleVerbFrame);
}

createSingleVerbFrame  <- function(transaction, data){
  query <- "MERGE (:VerbFrame {
                      number:{number},
                      sentenceFrame:{sentenceFrame}
                    })";  
  appendCypher(transaction, query, number = data$number,sentenceFrame = data$sentenceFrame
  );
}  

#--------------------------------------------------------------------------------------
# Functions for creating synset nodes
#--------------------------------------------------------------------------------------

#Read in POS data from dict folder
readPOSdata <- function(dictPath="~/Downloads/WordNet-3.0/dict", verbose=TRUE){
  if(verbose) {print(paste(Sys.time(),"Reading POS data", sep=": "))}; 
  advData<-readAdvData(dictPath, verbose);
  verbData<-readVerbData(dictPath, verbose);
  adjData<-readAdjData(dictPath, verbose);
  nounData<-readNounData(dictPath, verbose);
  list(adv = advData, verb = verbData, adj = adjData, noun = nounData);
}

readVerbData <- function(path="~/Downloads/WordNet-3.0/dict", verbose=TRUE){
  if(verbose) {print(paste(Sys.time(),"Reading verb data", sep=": "))};
  path<-paste(path,"data.verb",sep="/")
  verbData<-readPosDataFile(path);
  #For verbs, need special step to remove frame from end of pointers and remove space from front of frames!
  removeFramesFromPointers(verbData);
}

removeFramesFromPointers<-function(verbData){
  verbData$pointers<-str_replace(verbData$pointers," \\d{2} \\+ \\d{2} \\d{2}.*$","");
  return(verbData);
}

readAdvData <- function(path="~/Downloads/WordNet-3.0/dict", verbose=TRUE){
  if(verbose) {print(paste(Sys.time(),"Reading adverb data", sep=": "))};
  path<-paste(path,"data.adv",sep="/")
  readPosDataFile(path);
}

readNounData <- function(path="~/Downloads/WordNet-3.0/dict", verbose=TRUE){
  if(verbose) {print(paste(Sys.time(),"Reading noun data", sep=": "))};
  path<-paste(path,"data.noun",sep="/")
  readPosDataFile(path);
}

readAdjData <- function(path="~/Downloads/WordNet-3.0/dict", verbose=TRUE){
  if(verbose) {print(paste(Sys.time(),"Reading adjective data", sep=": "))};
  path<-paste(path,"data.adj",sep="/")
  readPosDataFile(path);
}

readPosDataFile <- function(path="~/Downloads/WordNet-3.0/dict/data.verb"){
  posData <- readLines(path);
  posData <- posData[30:length(posData)];
  processSynsetData(posData);
}

processSynsetData <- function(synsetData){
  synsetData <- matchSynsetParts(synsetData);
  #print(synsetData);
  x<-convertSynsetPartsToDf(synsetData);
  #print(x);
  x;
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
                                            #words = str_match(x[6],"(^.+ \\d{1}) (\\d{3})")[2],
                                            #pCnt = as.integer(str_match(x[6],"(^.+ \\d{1}) (\\d{3})")[3]),
                                            #pointers = str_match(x[6],"\\d{3} (.+$)")[2],
                                            words = str_match(x[6],"(^.+ [0-9a-f]{1}) (\\d{3})(\\s|$)")[2],
                                            pCnt = as.integer(str_match(x[6],"(^.+ [0-9a-f]{1}) (\\d{3})(\\s|$)")[3]),
                                            pointers = str_match(x[6],"\\d{3} (.+$)")[2],
                                            frames = str_match(x[6],"\\W(\\d{2} \\+ \\d{2} .+)$")[2],
                                            gloss = x[7],
                                            stringsAsFactors = FALSE));
}

#Createsynset nodes and lex relationships

createSynsetNodes <- function(graph,posList, verbose=TRUE){
  if(verbose) {print(paste(Sys.time(),"Creating synset nodes and lex file relationships", sep=": "))};
  addIndex(graph, "Synset","synsetOffset");
  invisible(lapply(posList, createPOSSpecificSynsetNodes, graph, verbose));
}

createPOSSpecificSynsetNodes <- function(synsetData, graph, verbose=TRUE){
  if(verbose){print(paste(Sys.time(), ": Creating ",synsetData[1,5]," synsets",sep=""))}
  bulkGraphUpdate(graph, synsetData, createSingleSynsetNode);
  if(verbose){print(paste(Sys.time(), ": Creating ",synsetData[1,5]," synset-lex relationships",sep=""))}
  bulkGraphUpdate(graph, synsetData, createSingleSynsetLexRelationship);
}

createSingleSynsetNode  <- function(transaction, data){
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
               synsetOffset = data$synsetOffset,lexFilenum = data$lexFilenum,
               lexFileName = data$lexFileName,pos = data$pos,
               posName = data$posName,wCnt = data$wCnt,
               pCnt = data$pCnt,gloss = data$gloss,words = data$words
               );
  }

createSingleSynsetLexRelationship <- function(transaction, data){
  query <- "MATCH (a:Synset {synsetOffset:{synsetOffset}, pos:{pos}}), (b:LexName {fileNumber:{fileNumber}})
  MERGE (a)-[:has_lexicographer_file]->(b)";  
  appendCypher(transaction, query, 
               synsetOffset = data$synsetOffset, pos = data$pos, fileNumber = as.numeric(data$lexFilenum));
}

#--------------------------------------------------------------------------------------
# Functions for creating word nodes
#--------------------------------------------------------------------------------------

createWordNodes <- function(graph, wordFrame, verbose=TRUE){
  addIndex(graph, "Word", "name");
  #print(typeof(wordFrame));
  if(verbose) {print(paste(Sys.time(),"Creating word nodes", sep=": "))};
  bulkGraphUpdate(graph, wordFrame, createSingleWordNode);
  if(verbose) {print(paste(Sys.time(),"Creating synset-word relationships", sep=": "))};
  bulkGraphUpdate(graph, wordFrame, createSingleSynsetWordRelationship);
}

#Create word frame
getWordFrame <- function(synsetData){
  z<-apply(synsetData, 1, transformSynsetDataToWordMap)
  ldply(z)
}

#process single line of synset data (inside apply) to create a narrow data frame with the offset and words
transformSynsetDataToWordMap <- function(synsetLine){
  offset<-synsetLine["synsetOffset"];
  pos<-synsetLine["pos"];
  words<-str_replace_all(str_to_lower(str_match_all(synsetLine["words"], "(\\S+) [0-9a-f]")[[1]][,2]),"_"," ");
  #words<-str_to_lower(str_match_all(synsetLine["words"], "(\\S+) [0-9a-f]")[[1]][,2]);
  df<-data.frame(synsetOffset=offset, pos=pos, name=words, stringsAsFactors=FALSE, row.names=NULL);
  cbind(df,wordNum=as.numeric(rownames(df)));
}

createSingleWordNode <- function(transaction, data){
  query <- "MERGE (:Word {name:{name}})";  
  appendCypher(transaction, query, name = data$name);
}

createSingleSynsetWordRelationship <- function(transaction, data){
  query <- "MATCH (a:Synset {synsetOffset:{synsetOffset}, pos:{pos}}), (b:Word {name:{name}})
  MERGE (a)-[:has_word {wordNum:{wordNum}}]->(b)";  
  appendCypher(transaction, query, synsetOffset = data$synsetOffset, pos = data$pos, name = data$name, wordNum = data$wordNum);
}

#--------------------------------------------------------------------------------------
# Functions for creating semantic pointers
#--------------------------------------------------------------------------------------

getSynsetPointerFrame <- function(synsetData){
  print(synsetData[1,3]);
  #convert Synset data into a data frame with x columns: 
  #start SynID, Start POS, Rel type, End Syn ID, End POS, Start Word, End Word
  z<-apply(synsetData[!is.na(synsetData$pointers),], 1, transformSynsetDataToSynPointerMap)
  z<-ldply(z);
  cbind(z, pointerType = translateMultiPointerSymbols(z$pointerSymbol, z$startPOS), stringsAsFactors = FALSE);
}

#process single line of synset data (inside apply) to create a narrow data frame with x columns: 
#start SynID, Start POS, Rel type, End Syn ID, End POS, Start Word, End Word
transformSynsetDataToSynPointerMap <- function(synsetLine){
  startOffset<-synsetLine["synsetOffset"];
  startPOS<-synsetLine["pos"];
  print(startOffset);
  pointers<-str_match_all(synsetLine["pointers"], "(\\S{1,2}) (\\d{8}) ([nvasr]) ([0-9a-f]{2})([0-9a-f]{2})")[[1]];
  print(pointers)
  data.frame(startOffset=startOffset, startPOS=startPOS, 
             pointerSymbol=pointers[,2], endOffset=pointers[,3], endPOS=pointers[,4],
             startWordNum=strtoi(pointers[,5],16), endWordNum=strtoi(pointers[,6],16),
             stringsAsFactors=FALSE, row.names=NULL);
}

createSemanticPointers <- function(graph, synsetPointerFrame, verbose=TRUE){
  if(verbose) {print(paste(Sys.time(),"Creating semantic synset pointers", sep=": "))};
  #print(nrow(synsetPointerFrame));
  bulkGraphUpdate(graph, synsetPointerFrame, createSingleSemanticPointer);
}

createSingleSemanticPointer <- function(transaction, data){
  query <- "MATCH (a:Synset {synsetOffset:{startOffset}, pos:{startPOS}}), 
              (b:Synset {synsetOffset:{endOffset}, pos:{endPOS}})
            MERGE (a)-[:has_pointer {relation:'Semantic', pointerSymbol:{pointerSymbol}, pointerType:{pointerType}}]->(b)";  
  appendCypher(transaction, query, startOffset = data$startOffset, startPOS = data$startPOS,
               endOffset = data$endOffset, endPOS = data$startPOS,
               pointerSymbol = data$pointerSymbol, pointerType = data$pointerType);
}

#--------------------------------------------------------------------------------------
# Functions for creating lexical pointers
#--------------------------------------------------------------------------------------

getLexicalPointerWords <- function(pointerFrame, wordFrame){
  #Add empty columns for start and end words
  pointerFrame<-cbind(pointerFrame, startWord = rep("",nrow(pointerFrame)), endWord = rep("",nrow(pointerFrame)), stringsAsFactors=FALSE);
  
  #iterate through lexical pointers, pull out start/end words and add to data frame
  for (i in 1:nrow(pointerFrame)){
    words <- searchForLexicalWords(pointerFrame[i,], wordFrame);
    if(length(words$endWord) != 0L){
      pointerFrame[i,"startWord"] <- words$startWord
      pointerFrame[i,"endWord"] <- words$endWord
    }
  }
  return(pointerFrame);
}

searchForLexicalWords <- function(pointerLine, wordFrame){
  
  list(
    startWord=wordFrame[wordFrame$synsetOffset==pointerLine$startOffset & 
                          wordFrame$pos==pointerLine$startPOS & 
                          #wordFrame$wordNum==as.numeric(pointerLine$startWordNum),
                          wordFrame$wordNum==pointerLine$startWordNum,
                        "name"],
    endWord=wordFrame[wordFrame$synsetOffset==pointerLine$endOffset & 
                        wordFrame$pos==pointerLine$endPOS & 
                        #wordFrame$wordNum==as.numeric(pointerLine$endWordNum),
                        wordFrame$wordNum==pointerLine$endWordNum,
                      "name"]
  );
  
}

createLexicalPointers <- function(graph, lexPointerFrame, verbose=TRUE){
  if(verbose) {print(paste(Sys.time(),"Creating lexical synset pointers", sep=": "))};
  bulkGraphUpdate(graph, lexPointerFrame, createSingleLexicalPointer);
}

createSingleLexicalPointer <- function(transaction, data){
  query <- "MATCH (a:Word {name:{startWord}}), (b:Word {name:{endWord}})
            MERGE (a)-[:has_pointer {relation:'Lexical', pointerSymbol:{pointerSymbol}, pointerType:{pointerType}}]->(b)";  
  appendCypher(transaction, query, startWord = data$startWord, endWord = data$endWord,
               pointerSymbol = data$pointerSymbol, pointerType = data$pointerType);
}

#--------------------------------------------------------------------------------------
# Functions for creating verb frame relationships pointers
#--------------------------------------------------------------------------------------

createVerbFrameRelationships <- function (graph, verbFrameFrame, verbose){
  #Create synset-frame relationships
  if(verbose) {print(paste(Sys.time(),"Creating synset-verb frame relationships", sep=": "))};
  bulkGraphUpdate(graph, verbFrameFrame[verbFrameFrame$wordNum==0,], createSingleSynsetFrameRelationship);  
  
  #Create word-frame relationships
  if(verbose) {print(paste(Sys.time(),"Creating word-verb frame relationships", sep=": "))};
  bulkGraphUpdate(graph, verbFrameFrame[verbFrameFrame$wordNum!=0,], createSingleWordFrameRelationship);
}

#process single line of synset data (inside apply) to create a narrow data frame
transformSynsetDataToFrameMap <- function(synsetLine){
  startOffset<-synsetLine["synsetOffset"];
  startPOS<-synsetLine["pos"];
  frames<-ldply(strsplit(str_match_all(synsetLine["frames"], "(\\d{2} \\d{2})")[[1]][,1]," "));
  frameFrame<-data.frame(startOffset=startOffset, startPOS=startPOS, 
             frameNumber=as.numeric(frames$V1), wordNum=as.numeric(frames$V2),
             stringsAsFactors=FALSE, row.names=NULL);
  
  word<-apply(frameFrame, 1, function(x){
    if(x["wordNum"] == 0){""}
    else{
      getWordByNumber(synsetLine[["words"]], x[["wordNum"]])
    }
  })
  cbind(frameFrame, word);
}

createSingleSynsetFrameRelationship <- function(transaction, data){
  query <- "MATCH (a:Synset {synsetOffset:{startOffset}, pos:{startPOS}}), (b:VerbFrame {number:{frameNumber}})
            MERGE (a)-[:has_sentence_frame {synsetOffset:{startOffset}, synsetPOS:{startPOS}}]->(b)";  
  appendCypher(transaction, query, startOffset = data$startOffset, startPOS = data$startPOS,
               frameNumber = data$frameNumber, word = data$word)
}

createSingleWordFrameRelationship <- function(transaction, data){
  query <- "MATCH (a:Word {name:{name}}), (b:VerbFrame {number:{frameNumber}})
            MERGE (a)-[:has_sentence_frame {synsetOffset:{synsetOffset}, synsetPOS:{synsetPOS}}]->(b)";  
  appendCypher(transaction, query, 
               name = data$word, frameNumber = data$frameNumber,
               synsetOffset = data$startOffset, synsetPOS = data$startPOS);
}

#--------------------------------------------------------------------------------------
# Miscellaneous functions
#--------------------------------------------------------------------------------------

findSynsetData <- function(offset, data){
  data[grep(paste("^",offset, " .*",sep=""),data)]
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


translateMultiPointerSymbols <- function(symbolVector, posVector){
  input <- data.frame(symbol = symbolVector, pos = posVector, stringsAsFactors = FALSE);
  unlist(apply(input, 1, translateSynsetPointerSymbol));
}

translateSynsetPointerSymbol <- function(input){
  if(input["symbol"]=='\\'){
    switch(input["pos"],
           a = "Pertainym (Pertains to Noun)",
           s = "Pertainym (Pertains to Noun)",
           r = "Derived from Adjective")
  }
  else if(input["symbol"]=="!"){"Antonym"}
  else if(input["symbol"]=="~"){"Hyponym"}
  else if(input["symbol"]=="~i"){"Instance Hyponym"}
  else if(input["symbol"]=="-c"){"Member of this Domain - TOPIC"}
  else if(input["symbol"]=="-r"){"Member of this Domain - REGION"}
  else if(input["symbol"]=="-u"){"Member of this Domain - USAGE"}
  else if(input["symbol"]==";c"){"Domain of Synset - TOPIC"}
  else if(input["symbol"]==";r"){"Domain of Synset - REGION"}
  else if(input["symbol"]==";u"){"Domain of Synset - USAGE"}
  else if(input["symbol"]=="@"){"Hypernym"}
  else if(input["symbol"]=="@i"){"Instance Hypernym"}
  else if(input["symbol"]=="*"){"Entailment"}
  else if(input["symbol"]=="&"){"Similar To"}
  else if(input["symbol"]=="#m"){"Member Holonym"}
  else if(input["symbol"]=="#p"){"Part Holonym"}
  else if(input["symbol"]=="#s"){"Substance Holonym"}
  else if(input["symbol"]=="%m"){"Member Meronym"}
  else if(input["symbol"]=="%p"){"Part Meronym"}
  else if(input["symbol"]=="%s"){"Substance Meronym"}
  else if(input["symbol"]=="^"){"Also See"}
  else if(input["symbol"]=="+"){"Derivationally Related Form"}
  else if(input["symbol"]=="<"){"Participle of Verb"}
  else if(input["symbol"]=="="){"Attribute"}
  else if(input["symbol"]==">"){"Cause"}
  else if(input["symbol"]=="$"){"Verb Group"}
}

getWordByNumber <- function(synsetWords, wordNum){
  words<-strsplit(synsetWords,"\\s")[[1]];
  convertedWordNum <- ceiling(as.numeric(wordNum) / 2);
  words[convertedWordNum];
}