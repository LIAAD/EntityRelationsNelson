library(tm)

# Where entities are in a sentence
filterContext<-function(entity.pair, sentence, filter.type, frequencies)
{
  #Log from analyzed sentence 
  cat("Candidate sentence: \"", sentence, "\"\n\n", sep='')

  center<-debugEntities(entity.pair, sentence,
                        frequencies[1], frequencies[2])
  
  #Obtain the right context
  if (filter.type=="middle")
    context<-center
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
extractContext<-function(text.file, sentence.position, context.entities, entity.frequency)
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
  if (context==1)
  {
    #Exceptio for words before left entity and after right entity
    context<-filterContext(context.entities, sentence,
                           "middle", entity.frequency)
  }
  else if(context==2)
  {
    context<-filterContext(context.entities, sentence,
                           "everything", entity.frequency)
  }
  context
}
