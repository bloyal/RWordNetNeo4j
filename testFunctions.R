#createWordNetNeo4j.R Test Functions
#source('createWordNetNeo4j.R');

#--------------
runIntegrationTests <-function(dictPath="./testData", verbose=FALSE){
  
  #Initiate graph
  graph<-newGraph(username="neo4j", password="graph");
  
  #Create lex nodes
  createLexNodes(graph, dictPath, verbose=verbose);
  unitTest("Lexicographer node count", countNodesbyLabel(graph, "LexName"),45);
  
  #Create frame nodes
  createFrameNodes(graph, verbose=verbose);
  unitTest("Verb frame node count", countNodesbyLabel(graph, "VerbFrame"),35);
  
  #Load in POS test data
  testData<-readPOSdata(dictPath, verbose);
  unitTest("Noun data count", nrow(testData$noun),27);
  unitTest("Verb data count", nrow(testData$verb),26);
  unitTest("Adjective data count", nrow(testData$adj),50);
  unitTest("Adverb data count", nrow(testData$adv),25);
  
  #Create synset nodes
  createSynsetNodes(graph, testData, verbose=verbose);
  unitTest("Synset node count", countNodesbyLabel(graph, "Synset"),128);
  unitTest("Synset-lex file relationship count",countRelationshipsByLabel(graph,"has_lexicographer_file"),128);
  
  #Create word nodes
  wordFrame <- ldply(lapply(testData, getWordFrame));
  unitTest("Word data count", nrow(wordFrame),220);
  createWordNodes(graph, wordFrame, verbose=verbose);
  unitTest("Word node count", countNodesbyLabel(graph, "Word"),206);
  unitTest("Synset-word relationship count",countRelationshipsByLabel(graph,"has_word"),220);
    
  #Create semantic pointers
  pointerFrame <- ldply(lapply(testData, getSynsetPointerFrame));
  createSemanticPointers(graph, pointerFrame[pointerFrame$startWordNum=="00",], verbose=verbose);
  unitTest("Semantic pointer count",countRelationshipsByLabel(graph,"has_pointer"),97);
  
  #Create lexical pointers
  pointerFrame <- getLexicalPointerWordsMem(pointerFrame[pointerFrame$startWordNum!="00",], wordFrame);
  createLexicalPointers(graph, pointerFrame, verbose=verbose);
  unitTest("Semantic + lexical pointer count",countRelationshipsByLabel(graph,"has_pointer"),131);
  
  #Create verb frame relationships
  verbFrameFrame<- ldply(apply(testData$verb,1,transformSynsetDataToFrameMap));
  createVerbFrameRelationships(graph, verbFrameFrame, verbose=verbose);
  unitTest("Synset-verb frame relationship count",countRelationshipsByLabel(graph,"has_sentence_frame"),33);
}

unitTest <- function(testName, actualValue, expectedValue){
  if(actualValue==expectedValue) {
    return(TRUE);
  }
  else {
    print(paste("Unit test failed: ", testName, ". Actual Value: ", actualValue, 
                ". Expected Value: ", expectedValue, sep=""));
    return(FALSE);
  }
}

getDataFileHeader <- function(){
  examplePath <- "~/Downloads/WordNet-3.0/dict/data.adv";
  exampleData <- readLines(examplePath);
  exampleData[1:29];
}

createTestDataFile <- function(testData, fileName){
  header <- getDataFileHeader();
  body <- testData[,c("synsetOffset","lexFilenum","pos","wCnt","words","pCnt","pointers","frames","gloss")];
  body$wCnt <- str_pad(body$wCnt,2,"left", "0");
  body$pCnt <- str_pad(body$pCnt,3,"left", "0");
  body$gloss <- paste("| ", body$gloss, sep="");
  #body$frames <- paste(body$frames, "|", sep="");
  write.table(header, file=fileName, col.names = FALSE, row.names = FALSE, quote=FALSE);
  write.table(body, append=TRUE, na = "", file=fileName, col.names = FALSE, row.names = FALSE, quote=FALSE);
}

benchmark <- function(){
  microbenchmark(
    createWordnetGraph(dictPath = "./testData", verbose=FALSE),
    createWordnetGraphMem(dictPath = "./testData", verbose=FALSE),
    times=25L
  )
}
