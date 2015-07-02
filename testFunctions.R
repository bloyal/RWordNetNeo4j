#createWordNetNeo4j.R Test Functions
#source('createWordNetNeo4j.R');

#--------------
runIntegrationTests <-function(dataPath="./testData/"){
  #Load in POS test data
  testData<-readPOSdata(dataPath, verbose=FALSE);
  unitTest("Noun data count", nrow(testData$noun),26);
  unitTest("Verb data count", nrow(testData$verb),25);
  unitTest("Adjective data count", nrow(testData$adj),50);
  unitTest("Adverb data count", nrow(testData$adv),25);
  
  #Initiate graph
  graph<-newGraph(username="neo4j", password="graph");
  
  #Create lex nodes
  createLexNodes(graph, verbose=FALSE);
  unitTest("Lexicographer node count", countNodesbyLabel(graph, "LexName"),45);
  
  #Create synset nodes
  createSynsetNodes(graph, testData, verbose=FALSE);
  unitTest("Synset node count", countNodesbyLabel(graph, "Synset"),126);
  
  #Create word nodes
  createWordNodes(graph, testData, verbose=FALSE);
  unitTest("Word node count", countNodesbyLabel(graph, "Word"),202);
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
