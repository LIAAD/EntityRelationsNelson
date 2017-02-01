library(tm)

# Where named entities are in a sentence
filterContext<-function(entity.pair, sentence, filter.type)
{
  #Log from analyzed sentence 
  cat("Candidate sentence: \"", sentence, "\"\n\n", sep='')
  
  #Decompose named entities
  first.term<-unlist(strsplit(entity.pair[1]," "))
  second.term<-unlist(strsplit(entity.pair[2]," "))

  debugEntities(entity.pair, sentence)
  
  #Filter the right context
  if (filter.type=="middle")
  {
    #Cut on entity locations
    no.left.entity<-unlist(strsplit(sentence, entity.pair[1]))
    
    middle<-unlist(strsplit(paste(no.left.entity[-1], collapse = ''),
                              entity.pair[2]))
    context<-middle[1]
  }
  else if (filter.type=="everything")
    context<-sentence
  else
    stop("Unknown filter type")
  context
}



# Locate the right sentence based on the right paragraph
getSentence<-function(position, sentence.set)
{
  sentences<-strsplit(sentence.set, "\\.|!|\\?")
  
  #Paragraph and then sentence
  sentences[[position[1]]][position[2]]
}



# Context extraction from a text or a file
extractContext<-function(text.file, sentence.position, context.entities)
{
  #Verify what is the context type
  context<-match(what.context,c('between','all'))
  
  if (is.na(context))
    stop("Context type is not correctly defined")

  if (text.file=="?")
  {
    text<-VCorpus(VectorSource(textual.content),
                  readerControl=list(language=idiom,id="id1"))
    document<-1
  }
  else
  {
    text<-VCorpus(DirSource(directory=textual.content,pattern=text.file),
                  readerControl=list(language=idiom,id="id1"))
    document<-text.file
  }
  my.corpus<-tm_map(text, removeNumbers)
  
  text.sentences<-(as.character(my.corpus[[document]])) 
  
  sentence<-getSentence(sentence.position, text.sentences)
  
  #Get every words
  if (context==1){
    #Exceptio words before left reference and after right reference
    context<-filterContext(context.entities, sentence, "middle")
  }
  else if(context==2)
    context<-filterContext(context.entities, sentence, "everything")
  
  context
}
