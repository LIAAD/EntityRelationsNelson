
#Verify if reference occurs on
checkEntity<-function(named.entity, text)
{
  grepl(named.entity, text)
}



#Check if entities are on the sentence
debugEntities<-function(pair, text)
{
  entity1<-grepl(pair[1], text)#checkEntity(pair[1], text)
  entity2<-grepl(pair[2], text)#checkEntity(pair[2], text)

  if (!entity1 & !entity2)
    stop("Both entities not found on the sentence","\n")
  else if (!entity1 | !entity2)
    stop("One of the entities not found on the sentence","\n")
}
