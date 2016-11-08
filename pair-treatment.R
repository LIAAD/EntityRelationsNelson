
# Take or delete pairs based on certain named entities
selectPairs<-function(pairs, special.pairs, removing)
{
  left.side <- sapply(1:(length(special.pairs)/2), function(i) special.pairs[i*2-1])
  right.side <- sapply(1:(length(special.pairs)/2), function(i) special.pairs[i*2])
  
  p<-do.call("rbind",
             sapply(1:NROW(pairs), function(x) {
               
               left.occurs <- which(left.side %in% pairs[x,1])
               right.occurs <- which(right.side %in% pairs[x,2])
               
               common.values <- intersect(left.occurs, right.occurs)
               
               if(length(common.values)==1 & removing)
                 c()
               else if(length(common.values)==1 & !removing)
                 pairs[x,]
               else if(length(common.values)!=1 & !removing)
                 c()
               else if(length(common.values)!=1 & removing)
                 pairs[x,]}))
  
  rownames(p) <- seq_len(nrow(p))
  p
}



# Analyze contexts from certain named entities forming pairs
checkContexts<-function(contexted.pairs, named.entities)
{
  invisible(sapply(1:(length(named.entities)/2), function(i) {
    
    pair<-contexted.pairs[contexted.pairs$entity1.name==named.entities[i*2-1]
                          & contexted.pairs$entity2.name==named.entities[i*2],]
    
    cat("First entity:", pair[,1], "\n", "Second entity:", pair[,2], "\n\n")
    cat(pair[,3][[1]],"\n\n\n")
  }))
}



# Discard pairs with contexts where the length is below a value
discriminateContexts<-function(pairs, threshold=9)
{
  some.pairs<-do.call("rbind", lapply(1:nrow(pairs), function(x) {
    
    size1 <- nchar(pairs[x,1])
    size2 <- nchar(pairs[x,2])
    
    do.call("rbind", lapply(1:length(pairs[x,3][[1]]), function(doc) {

      if(nchar(pairs[x,3][[1]][doc]) < ((size1+size2)+threshold))
        c(pairs[x,1],pairs[x,2])
      else
        c()
    }))
  }))
  some.pairs<-as.vector(t(unique(some.pairs)))
}



# Get or remove entity pairs from the existing ones
choosePairs<-function(total.pairs, partial.pairs, removing=FALSE, double.pairs=FALSE)
{
  if(removing)
    current.pairs<-total.pairs[-partial.pairs,]
  else
    current.pairs<-total.pairs[partial.pairs,]

  if(double.pairs)
  {
    pair.info<-""
    if(nrow(current.pairs)%%2!=0)
      stop("The pairs number is not correct to form pairs of pairs.")
    else
    {
      for(i in 1:(nrow(current.pairs)/2))
      {
        pair.info <- append(pair.info,
                            paste('c(c("',
                                  current.pairs[i*2-1,1],
                                  '","',
                                  current.pairs[i*2-1,2],
                                  '"), c("',
                                  current.pairs[i*2,1],
                                  '","',
                                  current.pairs[i*2,2],
                                  '"))', sep = ''))
      }
    }
    pair.info<-pair.info[-1]
  }
  else
  {
    pair.info <- apply(current.pairs, 1,
                       function(x) paste('c("',x[1],'","',x[2],'")', sep = ''))
  }
  cat(paste('c(', paste(pair.info,collapse = ',  '), ')', sep = ''))
}