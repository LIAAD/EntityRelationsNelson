
# Confirm if two entities are named in the same piece of text
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



# Confirm if two entities are distinct concepts
distinctEntities<-function(entity1, entity2)
{
  disambiguation1<-entity1[,"Entity_desamb"]
  disambiguation2<-entity2[,"Entity_desamb"]
  
  if(disambiguation1!=disambiguation2)
    TRUE
  else
    FALSE
}



#How much times the same entity is named
countName<-function(segment, name)
{
  l <- nrow(segment)
  
  # Compare names
  f<-function(x,s=segment,n=name)
  {
    if(grepl(n,s[x,1]))#s[x,1]==n
      1
    else
      0
  }
  if(l!=0)
    do.call("sum",lapply(1:l,f))
  else
    0
}



# Add a pair with the right context 
buildRelation<-function(previous.entities=data.frame(), first.entity,
                        posterior.entities=data.frame(), second.entity)
{
  #Get the text file and the sentence position
  text.name<-first.entity[,"File"]
  paragraph<-first.entity[,"Paragraph"]
  sentence<-first.entity[,"Sentence"]
  
  #Get the entities
  entity1.name<-first.entity[,"Entity"]#as.character()
  entity2.name<-second.entity[,"Entity"]#as.character()
  
  relationLog(first.entity[,"Entity_desamb"], second.entity[,"Entity_desamb"])
  
  #An entity pair and its positions in a text
  position<-c(paragraph, sentence)
  entity.pair<-c(entity1.name, entity2.name)
  
  #How many times elements of a pair appear
  entity.occurrences<-c(countName(previous.entities, entity1.name)+1,
                        countName(posterior.entities, entity2.name)+1)
  
  context<-extractContext(text.name, position, entity.pair, entity.occurrences)
  
  relationship<-c(entity1.name, entity2.name, context)

  contextLog(text.name, position, entity.pair, context)
  
  relationship
}



# Print info and review pairs
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
    title<-paste(file.name,".rds",sep='')
    saveRDS(relations, file=title)
  }
  relations
}



scanIteratively<-function(entities, relation.file="", order.significance=FALSE)
{
  relations<-data.frame(entity1.name=character(), entity2.name=character(),
                        context=character(), stringsAsFactors=FALSE)
  n<-nrow(entities)
  
  if(only.consecutive)
    limit <- 1
  else
    limit <- .Machine$integer.max
  
  i<-1
  #Verify if current object is in the same sentence than the next one(s)
  while (i<n)
  {
    current.entity<-entities[i,]
    
    #Pick entities with sentence in common
    f <- current.entity[,"File"]
    p <- current.entity[,"Paragraph"]
    s <- current.entity[,"Sentence"]

    #Verify entity names before a pair in the same sentence
    left.names <- subset(entities[-(i:n),],
                         File==f & Paragraph==p & Sentence==s, 4)
    
    j <- 1
    # Iterate over every pairs on same sentence
    while (j<=limit & (i + j <= n))
    {
      next.entity <- entities[i+j,]
      
      # Avoid unnecessary relationship processing
      same.sentence <- sameSentence(current.entity, next.entity)
      
      if(!same.sentence)
        break
      
      valid.relation <- distinctEntities(current.entity, next.entity)
      
      if (valid.relation)
      {
        #Verify entity names between a pair in the same sentence
        right.names <- subset(entities[-c(1:i, ((i + j):n)), ],
                              File == f & Paragraph == p & Sentence == s, 4)
        
        current.relation <- buildRelation(left.names,
                                          current.entity,
                                          right.names,
                                          next.entity)
        
        relations[nrow(relations) + 1, ] <- current.relation
      }
      j <- j + 1
    }
    i<-i+1
  }
  relations <- na.omit(relations) #Remove pairs without context
  relations <- treatRelations(relations, !order.significance, relation.file)

  relations
}



# Verify possible relations between an entity and each of next ones
checkRelation<-function(left.names=data.frame(), current.entity, another.ones)
{
  do.call("rbind",
          lapply(1:nrow(another.ones),
                 function (z, current=current.entity,
                           others=another.ones) {
                   
                   valid.relation<-distinctEntities(current, others[z,])

                   if(valid.relation)
                   {
                     if(z>1) #Get entity names between the pair if it exist
                     {
                       right.names<-subset(others[1:(z-1),],select=4)
                       r<-buildRelation(left.names, current,
                                        right.names, others[z,])
                     }
                     else
                     {
                       r<-buildRelation(previous.entities=left.names,
                                        first.entity=current,
                                        second.entity=others[z,])
                     }
                   }
                   else
                     r<-c("","",NA)
                   r}))
}



# Find relations between an entity and the following one(s)
searchRelations<-function(times, next.neighbours)
{
  do.call("rbind", lapply(times, function(y, neighbours=next.neighbours) {
    
    # Select only the successor or every next ones
    if(only.consecutive)
      followers <- neighbours[y+1,]
    else
      followers <- neighbours[(y+1):nrow(neighbours),]
    
    #If there are entities before the pair, get its names
    if(y>1)
    {
      previous.entities<-subset(neighbours[1:(y-1),],select=4)       
      new.relations<-checkRelation(previous.entities,
                                   neighbours[y,], followers)
    }
    else
    {
      new.relations<-checkRelation(current.entity=neighbours[y,],
                                   another.ones=followers)
    }
    #Add pairs from a sentence to already-discovered relations
    new.relations}))
}



# Lookup sentences with entities
exploreSentences<-function(entities)
{
  sentences<-unique(entities[,c(1,2,3)])
  
  #Check each sentence
  do.call(rbind,
          lapply(1:nrow(sentences),
                 function(i, e=entities, s=sentences) {
                   
                   #Select position features
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
