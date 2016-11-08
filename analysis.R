
# Return sentence from the right position
lookContext<-function(document, paragraph.number, sentence.number, match, option)
{
  #Verify if it is not a real file
  if (document==1)
  {
    my.corpus<-VCorpus(VectorSource(textual.content),
                       readerControl=list(language=idiom,id="id1"))
  }
  else
  {
    my.corpus<-VCorpus(DirSource(directory=textual.content,pattern=document),
                       readerControl=list(language=idiom,id="id1"))
  }
  #Find the right ones between everyone
  text.sentences<-(as.character(my.corpus[[document]])) 
  
  sentence<-getSentence(c(paragraph.number,sentence.number),
                        text.sentences)
  
  if(option=="fixed")
  {
    matched <- Reduce("|",
                      sapply(1:length(match),
                             function(x) grepl(match[x],
                                               sentence,
                                               fixed = TRUE)))
  }
  else if(option=="case sensitive")
  {
    matched <- Reduce("|",
                      sapply(1:length(match),
                             function(x) grepl(match[x],
                                               sentence)))
  }
  else
  {
    matched <- Reduce("|",
                      sapply(1:length(match),
                             function(x) grepl(match[x],
                                               sentence,
                                               ignore.case = TRUE)))
  }
  if(matched)
    sentence
  else
    NA
}



# Explore sentences from texts
inspectContexts<-function(sentence.positions, certain.string=c(""), exact.string=TRUE,
                          case.sensitive=FALSE)
{
  explored.contexts <- data.frame(position=character(),
                                  sentence=character(),
                                  stringsAsFactors=FALSE)
  #Set sentence locations to explore
  contexts <- unique(sentence.positions[,c("File","Paragraph","Sentence")])
  contexts[1] <- lapply(contexts[1], as.character)

  files.number<-length(unique(contexts$File))
  
  #Specify pattern
  if(exact.string)
    string.condition<-"fixed"
  else
  {
    if(case.sensitive)
      string.condition<-"case sensitive"
    else
      string.condition<-""
  }
  if(files.number==1 & contexts[1,"File"]=="?")
  {
    inspection <- do.call("rbind", lapply(1:nrow(contexts), function(s) {
      
      paragraph<- contexts[s,2]
      sentence<- contexts[s,3]
      
      context<-lookContext(1,paragraph,sentence,
                           certain.string,string.condition)
      if(!is.na(context))
      {
        sentenceLog(c(paragraph,sentence), context)
        
        from<-paste("(",
                    paste(contexts[s,1], contexts[s,2], sep = ", "),
                    ")", sep='')
        
        instance <- c(from, context)
      }
      else
        instance <- c(NA,NA)
      instance
    }))
  }
  else
  {
    inspection <- do.call("rbind", lapply(1:nrow(contexts), function(s) {
      
      file<- contexts[s,1]
      paragraph<- contexts[s,2]
      sentence<- contexts[s,3]
      
      context<-lookContext(file,paragraph,sentence,
                           certain.string,string.condition)
      if(!is.na(context))
      {
        sentenceLog(c(paragraph,sentence,file), context)
        
        from<-paste("(",
                    paste(contexts[s,1], contexts[s,2], contexts[s,3],
                          sep = ", "),
                    ")", sep='')
        
        instance <- c(from, context)
      }
      else
        instance <- c(NA,NA)
      instance
    }))
  }
  # Fill the sentence info
  inspection <- na.exclude(inspection)
  if(nrow(inspection)!=0)
    explored.contexts[c(1:nrow(inspection)),] <- inspection
  explored.contexts
}



createPairs<-function(pairs)
{
  do.call("rbind", lapply(1:nrow(pairs), function(i) {
    
    pair<-paste(pairs[i,1], "-", pairs[i,2])
    
    cat("Created pair:", pair, "\n")
    
    pair}))
}



lookPairs<-function(relation.pairs)
{
  #Select only entity references
  pairs<-relation.pairs[,c(1,2)]

  freq<-createPairs(pairs)

  barplot(sort(table(freq[,1]), decreasing=TRUE), xlab="pairs",
          ylab = "number of occurrences")
  freq
}
