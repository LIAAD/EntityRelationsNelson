
# Verify if an entity mention finishes a sentence
findName<-function(t, name)
{
  x <- paste0(t, name)
  
  y <- unlist(strsplit(x, name))
  
  last <- length(y)
  
  limit <- nchar(y[last])

  if(limit==0)
    TRUE
  else
    FALSE
}



# Check if names occur in the sentence
debugEntities<-function(pair, text, i=1, j=1)
{
  #Verify i-th name to left entity
  v1 <- unlist(strsplit(text, pair[1]))

  left.exists <- length(v1)>i & nchar(text)>sum(nchar(v1))
  
  if(!left.exists)
    stop("Left entity not found in the sentence","\n")
  
  #Know if the sentence end was shortened due to left entity
  extreme <- findName(text, pair[1])
  
  if(extreme)
    remainder <- c(v1[(i+1):length(v1)],pair[1])
  else
    remainder <- v1[(i+1):length(v1)]
  
  remainder <- paste0(remainder, collapse=pair[1])
  
  #Verify j-th name to right entity
  v2 <- unlist(strsplit(remainder, pair[2]))
  
  right.exists <- length(v2)>=j & nchar(remainder)>sum(nchar(v2))
  
  if (!right.exists)
    stop("Right entity not found in the sentence","\n")
  
  taken.context <- paste0(v2[1:j], collapse=pair[2])

  taken.context
}
