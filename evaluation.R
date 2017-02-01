
# Obtain number of must-link or cannot-link pairs with shared cluster
countAssociations<-function(cluster.assigns, elements, info="", logging=FALSE)
{
  if(logging)
    clusteringLog(cluster.assigns, elements, info)
  
  matching<-sapply(1:(length(elements)/4), function(e) {
    
    left <- cluster.assigns$entity1.name==elements[e*4-3]
    right <- cluster.assigns$entity2.name==elements[e*4-2]
    selected1 <- left & right
      
    left <- cluster.assigns$entity1.name==elements[e*4-1]
    right <- cluster.assigns$entity2.name==elements[e*4]
    selected2 <- left & right
    
    c1<-cluster.assigns[selected1,3]
    c2<-cluster.assigns[selected2,3]
    
    if(c1==c2)
      1
    else
      0
  })
  sum(matching)
}



# Calculate total of pairs correctly predicted in same cluster
#over total of pairs predicted in same cluster 
getPrecision<-function(clustering, must.link, cannot.link, log)
{
  MSC <- countAssociations(clustering, must.link, "Must-Link Pairs", log)
  XSC <- countAssociations(clustering, cannot.link, "Cannot-Link Pairs", log)
  PSC <- XSC+MSC
  CSC <- MSC
  
  CSC/PSC
}



# Calculate total of pairs correctly predicted in same cluster
#over total of pairs actually in same cluster 
getRecall<-function(clustering, must.link, log)
{
  MSC <- countAssociations(clustering, must.link, logging = FALSE)
  ASC <- length(must.link)/2
  CSC <- MSC

  CSC/ASC  
}



# Calculate F-measure
getF1<-function(precision, recall)
{
  if(precision==0 | recall==0)
    0
  else
    (2*precision*recall)/(precision+recall)
}



# Do evaluation and present traditional measures of information retrieval
evaluateClustering<-function(clusters, must.link, cannot.link, logging=FALSE)
{
  p <- getPrecision(clusters, must.link, cannot.link, logging)
  r <- getRecall(clusters, must.link, logging)
  f <- getF1(p,r)
  
  if(logging)
    evaluationLog(c(p,r,f))
  
  round(f,3)
}



# Represent scores with different cluster quantities to various distance measures
composeGraphic<-function(cluster.quantities, scores, distance.measures,
                         color.names=c("red","blue","orange","green","brown","gray"),
                         weight, algorithm, experience)
{
  n1 <- length(distance.measures)
  n2 <- length(color.names)

  if(n1!=n2)
    stop("Number of measures is different than number of available colors")
  
  plot(1, main = paste("Clustering evaluation with", weight, "and", algorithm),
               type = "n", bty='L', xlim = c(0, max(cluster.quantities)),
               ylim = c(0, 100), xlab = "Number of Clusters", ylab = "F-score")
  
  for(i in 1:length(color.names))
  {
    lines(cluster.quantities[i,], scores[i,], lty=1,
          col = color.names[i], type = "b")
  }
  legend(0.1, 105, distance.measures, lty=1, cex=0.65, ncol = 3, bty="n",
         col = color.names, title = "Distance measures")
  
  #Exceptions to file name
  if(weight=="TF/IDF") 
    weight<-"TFIDF"
  if(algorithm=="Hartigan-Wong")
    algorithm<-"HartiganAndWong"
  
  plot.name <- paste(experience, weight, algorithm, sep = "-")
  dev.copy(pdf, paste(plot.name, ".pdf", sep = ''))
  
  invisible(dev.off())
}



# Try different clustering processes with a predefined unique number of clusters
initialEvaluation<-function(contextualized.pairs, measure="euclidean", percentage=0.1,
                            weights, kmeans.types, max.iterations=30, no.stopwords=TRUE,
                            labels.number=10, must.pairs, cannot.pairs, max.attempt=6,
                            evaluation.log=FALSE)
{
  nw <- length(weights)
  nk <- length(kmeans.types)
  
  # Structure to support main data
  f.scores <- matrix(0, nw, nk+1)
  rownames(f.scores) <- weights
  best.f1 <- 0
  set <- ""
  valid.occurrences <- matrix(max.attempt, nw, nk)
  
  # Verify exact number of clusters
  pairs.number<-nrow(contextualized.pairs)
  n<-round(pairs.number*percentage)
  cat("Quantity of desirable clusters:", n, "\n")
  
  for(a in 1:max.attempt)
  {
    for(w in 1:nw)
    {
      for(k in 1:nk)
      {
        clustering <- defineClustering(contextualized.pairs, weight=weights[w], measure,
                                       kmeans.args = list(algorithm=kmeans.types[k],
                                                          iter.max=max.iterations),
                                       cluster.percentage = percentage,
                                       remove.stopwords = no.stopwords,
                                       words.number = labels.number)
        
        current.score <- f.scores[w,k]
        next.score <- evaluateClustering(clustering[,-4],
                                         must.pairs, cannot.pairs, evaluation.log)
        if(next.score==0)
          valid.occurrences[w,k] <- valid.occurrences[w,k]-1
        f.scores[w,k] <- current.score + next.score
        
        if(current.score>best.f1)
        {
          best.f1<-current.score
          analyzed.clustering <- clustering
          set <- paste(weights[w],"-",kmeans.types[k])
        }
      }
    }
  }
  #Get score averages
  f.scores[,-(nk+1)] <- (f.scores[,-(nk+1)]/valid.occurrences)*100
  f.scores[!is.finite(f.scores)]<-0
  f.scores[,nk+1] <- apply(f.scores[,1:nk], 1, mean)

  list("clusters" = analyzed.clustering, "scores" = f.scores, "parameters" = set)
}



# Different clustering processes with various predefined numbers of clusters
multipleEvaluation<-function(contextualized.pairs, measures, percentages, domain,
                            weighting, kmeans.type, max.iterations=30, no.stopwords=TRUE,
                            must.pairs, cannot.pairs, max.attempt=3, evaluation.log=FALSE)
{
  nd <- length(measures)
  nc <- length(percentages)
  
  # Structures to support main data
  cluster.numbers <- matrix(0, nd, nc)
  f.scores <- matrix(0, nd, nc)
  valid.occurrences <- matrix(max.attempt, nd, nc)
    
  for(a in 1:max.attempt)
  {
    for(d in 1:nd)
    {
      for(c in 1:nc)
      {
        clustering <- defineClustering(contextualized.pairs, weight=weighting, measures[d],
                                       kmeans.args = list(algorithm=kmeans.type,
                                                          iter.max = max.iterations),
                                       cluster.percentage = percentages[c],
                                       remove.stopwords = no.stopwords)
        
        cluster.numbers[d,c] <- length(unique(clustering[clustering$cluster.key!=0,3]))
          
        current.score <- f.scores[d,c]
        next.score <- evaluateClustering(clustering[,-4],
                                         must.pairs, cannot.pairs, evaluation.log)
        if(next.score==0)
          valid.occurrences[d,c] <- valid.occurrences[d,c]-1
        f.scores[d,c] <- current.score + next.score
      }
    }
  }
  #Get score averages
  f.scores <- (f.scores/valid.occurrences)*100
  f.scores[!is.finite(f.scores)]<-0
  #Get result representations
  composeGraphic(cluster.numbers, f.scores, distance.measures = measures,
                 weight = weighting, algorithm = kmeans.type, experience = domain)
}
