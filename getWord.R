#getWord

getWord <- function(graph, word){
  query<-"MATCH (a:Word {name:{word}}) RETURN a.name";
  print(query);
  cypher(graph, query, word=word);
}