library(PAMPO)

# Proceed to a recognition of named entities
extractEntities<-function(source, to="extracted_entities")
{
  if(idiom=="pt")
  {
    start.extraction <- Sys.time()
    if (data.type == "folder")
      entity.positions <- PAMPO_pt(source, 0)
    else if (data.type == "text")
      entity.positions <- PAMPO_pt(source, 2)
    else
      stop("Invalid data type")
    end.extraction <- Sys.time()
  }
  else
  {
    stop("NER is unavailable for this language","\n")
  }
  extraction.time <- round(difftime(end.extraction, start.extraction, units = "mins"), 3)
  
  #Add the File column, avoiding code changes to deal with text option
  if(data.type=="text")
  {
    File<-"?"
    entity.positions<-cbind(File, entity.positions)
  }

  info1<-paste("Number of entity locations:", nrow(entity.positions))
  info2<-paste("Number of analyzed files:", length(unique(entity.positions$File)))
  info3<-paste("Number of distinct entities:", length(unique(entity.positions$Entity_desamb)))
  info4<-paste("Extraction time:", extraction.time, "minutes")

  cat(info1,"\n\n")
  cat(info2,"\n\n")
  cat(info3,"\n\n")
  cat(info4,"\n\n\n")
  
  entity.positions[c(2,3)] <- lapply(entity.positions[c(2,3)], as.integer)

  if(to!="") #Positions of named entities are written on a file
  {
    output.file<-paste(to, ".csv", sep = '')
    write.table(entity.positions, output.file, sep=", ", row.names=FALSE,
                col.names=TRUE, quote = FALSE, append = FALSE)
  }
  entity.positions
}



# Get positions of named entities from a former recognition
fileExtraction<-function(file, file.header=TRUE, file.separator=", ")
{
  # Data frame imported from a file with information about
  #named entities and their positions through a given corpus
  extracted.entities<-read.csv(file, header=file.header, sep=file.separator)
  extracted.entities[c(4,5)] <- lapply(extracted.entities[c(4,5)],
                                       trimws, which="left")
  extracted.entities
}


# Preprocessing of named entities based on elements not found
#in respective sentences and on uninteresting elements
cleanEntities<-function(entities, special.cases=c())
{
  entities[c(1,4,5)] <- lapply(entities[c(1,4,5)], as.character)
  entities[c(4,5)] <- lapply(entities[c(4,5)], trimws, which="left")
  
  if(length(special.cases)>0)
    entities<-entities[-special.cases,]
  
  entities<-entities[!is.element(entities$Entity, exceptions),]

  entities
}



#Set the frequency of named entities in each sentence
countEntities<-function(entity.locations)
{
  occurrences<-rep(1, nrow(entity.locations))
  entity.locations<-cbind(entity.locations, occurrences)
  
  entity.occurrences<-aggregate(occurrences~File+Paragraph+Sentence,
                                  data=entity.locations[,c(1,2,3)], FUN=sum)
  
  entity.occurrences[order(entity.occurrences$File, entity.occurrences$Paragraph,
                           entity.occurrences$Sentence),]
}



#Verify if counting of named entities is completely correct
confirmOccurrences<-function(entity.locations, entity.counter)
{
  right<-TRUE

  correct<-sapply(1:nrow(entity.counter), function(i) {
    
    f<-entity.counter[i,"File"]
    p<-entity.counter[i,"Paragraph"]
    s<-entity.counter[i,"Sentence"]
    
    #Select elements corresponding to the same sentence
    location<-entity.locations[entity.locations$File==f &
                                 entity.locations$Paragraph==p &
                                 entity.locations$Sentence==s,]
    
    if(!nrow(location)==entity.counter[i,"occurrences"])
    {
      wrong<-paste("There's a wrong number of occurrences at line",
                   i, "on data frame.\n")
      cat(wrong)
      right<-FALSE
    }
    right})
  
  Reduce("&", correct)
}



#Preserve sentences with certain frequencies of named entities
filterSentences<-function(entity.locations, sentence.entities, entity.number=c(2))
{
  #To maintain ordering
  entity.locations$ID  <- 1:nrow(entity.locations)
  #Choose the exact sentences
  sentence.entities<-sentence.entities[is.element(sentence.entities$occurrences,
                                                   entity.number),]

  entity.locations<-merge(entity.locations, sentence.entities,
                          by=c("File", "Paragraph", "Sentence"))
  
  entity.locations[order(entity.locations$ID),-c(6,7)]
}
