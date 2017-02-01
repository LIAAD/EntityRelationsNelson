

# Check if named entities occur in the sentence
debugEntities<-function(pair, text)
{
  entity1<-grepl(pair[1], text)
  entity2<-grepl(pair[2], text)

  if (!entity1 & !entity2)
    stop("Both entities not found on the sentence","\n")
  else if (!entity1 | !entity2)
    stop("One of the entities not found on the sentence","\n")
}
