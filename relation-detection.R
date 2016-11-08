
#Confirm if two entities are on the same piece of text
sameSentence<-function(entity1, entity2)
{
  same.file <- entity1[,"File"]==entity2[,"File"]
  same.paragraph <- entity1[,"Paragraph"]==entity2[,"Paragraph"]
  same.sentence <- entity1[,"Sentence"]==entity2[,"Sentence"]

  if(same.file & same.paragraph & same.sentence)
    TRUE
  else
    FALSE
}



#Confirm if two entities are not the same
distinctEntities<-function(entity1, entity2)
{
  disambiguation1<-entity1[,"Entity_desamb"]
  disambiguation2<-entity2[,"Entity_desamb"]
  
  if(disambiguation1!=disambiguation2)
    TRUE
  else
    FALSE
}



# TODO - Know what the i-th named entity with same string to extract the right intermediate context 
buildRelation<-function(first.entity, second.entity)
{
  #Get the text file name and the position of the sentence
  text.name<-first.entity[,"File"]
  paragraph<-as.integer(first.entity[,"Paragraph"])
  sentence<-as.integer(first.entity[,"Sentence"])
  
  #What the entities
  entity1.name<-first.entity[,"Entity"]
  entity2.name<-second.entity[,"Entity"]
  
  relationLog(first.entity[,"Entity_desamb"], second.entity[,"Entity_desamb"])
  
  #Entities and its positions in a text
  position<-c(paragraph, sentence)
  entity.pair<-c(entity1.name, entity2.name)
  
  #Get entity pairs and respective contexts 
  context<-extractContext(text.name, position, entity.pair)
  
  relationship<-c(entity1.name, entity2.name, context)

  contextLog(text.name, position, entity.pair, context)
  
  relationship
}



#Print info and review pairs
treatRelations<-function(relations, lexicographical.order, file.name)
{
  total<-paste("Number of detected relations:", nrow(relations), "\n")
  cat(total)
  
  if(lexicographical.order) #Exchange pair names if it is required
  {
    relations[relations$entity1.name>relations$entity2.name,
              c(1,2)] <- relations[relations$entity1.name>relations$entity2.name, c(2,1)]
  }
  if(file.name!="")
  {
    write.table(relations, paste(relation.file,".csv",sep=''),
                sep=", ", row.names=FALSE, quote=FALSE)
  }
  relations
}



scanIteratively<-function(entities, relation.file="", order.significance=FALSE)
{
  relations<-data.frame(entity1.name=character(), entity2.name=character(),
                        context=character(), stringsAsFactors=FALSE)
  i<-1
  #Verify if current object is on the same sentence than the previous one
  while (i<nrow(entities))
  {
    current.entity<-entities[i,]
    next.entity<-entities[i+1,]
    
    same.sentence <- sameSentence(current.entity, next.entity)
    valid.relation <- distinctEntities(current.entity, next.entity)
    
    if (same.sentence & valid.relation)
    {
      current.relation<-buildRelation(current.entity, next.entity)
      
      relations[nrow(relations)+1,]<-current.relation
    }
    if (!only.consecutive & same.sentence) {
      # Iterate over every pairs on same sentence
      j <- 2
      while (same.sentence & (i+j <= nrow(entities)))
      {
        current.entity <- entities[i,]
        next.entity <- entities[i+j,]
        
        same.sentence <- sameSentence(current.entity, next.entity)
        valid.relation <- distinctEntities(current.entity, next.entity)
        
        if (same.sentence & valid.relation)
        {
          current.relation <- buildRelation(current.entity, next.entity)
          
          relations[nrow(relations)+1,] <- current.relation
        }
        j <- j + 1
      }
    }
    i<-i+1
  }
  relations <- na.omit(relations) #Remove pairs without context
  relations <- treatRelations(relations, !order.significance, relation.file)

  relations
}



#Verify possible relations between a entity and next entities on same sentence
checkRelation<-function(current.entity, another.ones)
{
  do.call("rbind",
          lapply(1:nrow(another.ones),
                 function (z, current=current.entity,
                           others=another.ones) {
                   
                   valid.relation<-distinctEntities(current, others[z,])

                   if(valid.relation)
                     r<-buildRelation(current, others[z,])
                   else
                     r<-c("","",NA)
                   r}))
}



# Try to find relations between a entity and the following one(s)
searchRelations<-function(times, next.neighbours){ 

  do.call("rbind", lapply(times, function(y, neighbours=next.neighbours) {
    
    if(only.consecutive)
      new.relations <- checkRelation(neighbours[y,], neighbours[y+1,])
    else
    {
      n<-nrow(neighbours)
      new.relations <- checkRelation(neighbours[y,], neighbours[(y+1):n,])
    }
    #Add relations from a sentence to relations already discovered
    new.relations}))
}



#Carry over sentences with entity references
exploreSentences<-function(entities)
{
  sentences<-unique(entities[,c(1,2,3)])
  
  #Check each sentence with named entities
  do.call(rbind,
          lapply(1:nrow(sentences),
                 function(i, e=entities, s=sentences) {
                   
                   common.file <- is.element(e[,1], s[i,1])
                   common.paragraph <- is.element(e[,2], s[i,2])
                   common.sentence <- is.element(e[,3], s[i,3])
                   
                   neighbour.entities<-e[common.file
                                         & common.paragraph
                                         & common.sentence,]
                   
                   if(nrow(neighbour.entities)>1)
                   {
                    entity.time<-1:(nrow(neighbour.entities)-1)
                   
                    r<-searchRelations(entity.time, neighbour.entities)
                   }
                   else
                     r<-c()
                   r <- na.exclude(r) #Remove pairs without context
          r})) 
}



scanEfficiently<-function(entity.positions, relation.file="", order.significance=FALSE)
{
  relations<-data.frame(entity1.name=character(), entity2.name=character(),
                        context=character(), stringsAsFactors=FALSE)
  
  extension <- exploreSentences(entity.positions)
  if(nrow(extension)!=0)
    relations[c(1:nrow(extension)),] <- extension
  relations <- treatRelations(relations, !order.significance, relation.file)
  
  relations
}
