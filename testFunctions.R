# createWordNetNeo4j.R Test Functions source('createWordNetNeo4j.R');
library(testthat)

# --------------
runIntegrationTests <- function(dictPath = "./newTestData", verbose = FALSE) {
  
  graph <- newGraph(username = "neo4j", password = "graph")
  
  context("Lex Nodes")
  test_that("All 45 lexicographer nodes have been created", {
    createLexNodes(graph, dictPath, verbose = verbose)
    expect_equal(countNodesbyLabel(graph, "LexName"), 45)
    # expect_that(countNodesbyLabel(graph, 'LexName'), equals(45))
  })
  
  context("Verb Frames")
  test_that("All 35 verb frame nodes have been created", {
    createFrameNodes(graph, verbose = verbose)
    expect_equal(countNodesbyLabel(graph, "VerbFrame"), 35)
    # expect_that(countNodesbyLabel(graph, 'VerbFrame'), equals(35))
  })
  
  wordNetData <- readPOSdata(dictPath, verbose)
  context("Read POS Data")
  test_that("Synset nodes are created correctly", {
    # VerbFrame
    expect_equal(nrow(wordNetData$noun), 3)
    expect_equal(nrow(wordNetData$verb), 3)
    expect_equal(nrow(wordNetData$adj), 3)
    expect_equal(nrow(wordNetData$adv), 2)
  })
  
  createSynsetNodes(graph, wordNetData, verbose = verbose)
  context("Synset Nodes")
  test_that("Synset nodes are created correctly", {
    expect_equal(countNodesbyLabel(graph, "Synset"), 11)
    expect_equal(countRelationshipsByLabel(graph, "has_lexicographer_file"), 
                 11)
  })
  
  wordFrame <- readPOSWordIndex(dictPath, verbose = verbose)
  createWordNodes(graph, wordFrame, verbose = verbose)
  context("Word Nodes")
  test_that("Word nodes are created correctly", {
    expect_equal(nrow(wordFrame), 59)
    expect_equal(countNodesbyLabel(graph, "Word"), 16)
    expect_equal(countRelationshipsByLabel(graph, "has_synset"), 14)
  })
  
  pointerFrame <- ldply(lapply(wordNetData, getSynsetPointerFrame))
  createSemanticPointers(graph, pointerFrame[pointerFrame$startWordNum == 
                                               0, ], verbose = verbose)
  context("Semantic Pointers")
  test_that("Semantic pointers are created correctly", {
    expect_equal(countRelationshipsByLabel(graph, "has_pointer"), 6)
  })
  
  wordFrame <- ldply(lapply(wordNetData, getWordFrame))
  pointerFrame <- getLexicalPointerWords(pointerFrame[pointerFrame$startWordNum != 
                                                        0, ], wordFrame)
  createLexicalPointers(graph, pointerFrame, verbose = verbose)
  test_that("Lexical pointers are created correctly", {
    expect_equal(countRelationshipsByLabel(graph, "has_pointer"), 9)
  })
  
  verbFrameFrame <- ldply(apply(wordNetData$verb, 1, transformSynsetDataToFrameMap))
  createVerbFrameRelationships(graph, verbFrameFrame, verbose = verbose)
  test_that("Verb frame nodes  are created correctly", {
    expect_equal(countRelationshipsByLabel(graph, "has_sentence_frame"), 
                 5)
  })
  return(TRUE)
}

unitTest <- function(testName, actualValue, expectedValue) {
  if (actualValue == expectedValue) {
    return(TRUE)
  } else {
    print(paste("Unit test failed: ", testName, ". Actual Value: ", 
                actualValue, ". Expected Value: ", expectedValue, sep = ""))
    return(FALSE)
  }
}

getDataFileHeader <- function() {
  examplePath <- "~/Downloads/WordNet-3.0/dict/data.adv"
  exampleData <- readLines(examplePath)
  exampleData[1:29]
}

createTestDataFile <- function(testData, fileName) {
  header <- getDataFileHeader()
  body <- testData[, c("synsetOffset", "lexFilenum", "pos", "wCnt", "words", 
                       "pCnt", "pointers", "frames", "gloss")]
  body$wCnt <- str_pad(body$wCnt, 2, "left", "0")
  body$pCnt <- str_pad(body$pCnt, 3, "left", "0")
  body$gloss <- paste("| ", body$gloss, sep = "")
  # body$frames <- paste(body$frames, '|', sep='');
  write.table(header, file = fileName, col.names = FALSE, row.names = FALSE, 
              quote = FALSE)
  write.table(body, append = TRUE, na = "", file = fileName, col.names = FALSE, 
              row.names = FALSE, quote = FALSE)
}

benchmark <- function() {
  microbenchmark(createWordnetGraph(dictPath = "./newTestData", verbose = FALSE), 
                 createWordnetGraphMem(dictPath = "./newTestData", verbose = FALSE), 
                 times = 25L)
}
