# getWordInfo.R
library(reshape)
getWordInfo <- function(graph, word) {
  # get semantic pointer info
  query <- "match (a:Word {name:{word}})-[r:has_synset]->(b:Synset),\n          (b)-[r2:has_pointer]->(c)<-[r3:has_synset]-(d:Word)\n          return a.name as word, b.posName as pos, r.wordNum as sense, b.gloss as gloss, b.lexFileName as lexFile, r2.pointerType as pointer, r2.relation as pointerType, d.name as pointerWord\n          order by a.name, b.posName, r.wordNum, r2.pointerType, r3.wordNum"
  semanticResults <- unique(cypher(graph, query, word = word))
  semanticResults <- merge(semanticResults, ddply(semanticResults, .(pos, 
                                                                     sense, pointer), summarise, pointerWords = paste0(pointerWord, 
                                                                                                                       collapse = ", ")), by = c("pos", "sense", "pointer"))
  results <- unique(semanticResults[, c("word", "pos", "sense", "gloss", 
                                        "lexFile", "pointer", "pointerType", "pointerWords")])
  
  # get lexical pointer info
  query <- "match (a:Word {name:{word}})-[r:has_synset]->(b:Synset),\n          (a)-[r2:has_pointer]->(c:Word)\n            return a.name as word, b.posName as pos,r.wordNum as sense, b.gloss as gloss, b.lexFileName as lexFile, r2.pointerType as pointer, r2.relation as pointerType, c.name as pointerWord\n            order by a.name, b.posName,r.wordNum, r2.pointerType"
  lexResults <- unique(cypher(graph, query, word = word))
  if (!is.null(lexResults)) {
    lexResults <- merge(lexResults, ddply(lexResults, .(pos, sense, 
                                                        pointer), summarise, pointerWords = paste0(pointerWord, collapse = ", ")), 
                        by = c("pos", "sense", "pointer"))
    lexResults <- unique(lexResults[, c("word", "pos", "sense", "gloss", 
                                        "lexFile", "pointer", "pointerType", "pointerWords")])
    results <- rbind(results, lexResults)
  }
  return(results[order(results$word, results$pos, results$sense, results$pointerType, 
                       results$pointer), ])
}