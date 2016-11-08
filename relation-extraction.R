
#Join contexts with same pair in common
aggregateContexts<-function(relations)
{
  aggregate(context~entity1.name+entity2.name, data=relations, function(x,contexts=c()){
    append(as.character(x), contexts)
    c(as.character(x),contexts)
  })
}



#Assign a corpus with contexts from entity pairs
getCorpus<-function(pair.contexts, removal)
{
  corpus<-VCorpus(VectorSource(pair.contexts$context))
  # Clean corpus
  pair.elements <- unique(as.vector(t(pair.contexts[,c(1,2)])))
  useless<-c(exceptions, pair.elements, if(removal) stopwords(idiom))

  corpus <- tm_map(corpus, removeWords, useless)
  corpus <- tm_map(corpus, content_transformer(removePunctuation),
                   preserve_intra_word_dashes = TRUE)
  if(use.stemming)
    corpus <- tm_map(corpus, stemDocument, language = idiom)
  corpus
}



#Pair profiling
weightingChoice<-function(corpus, type)
{
  if(type=="TF")
    w<-weightTf
  else if(type=="TF/IDF")
    w<-weightTfIdf
  else if(type=="Binary")
    w<-weightBin
  else if(type=="SMART")
  w<-weightSMART
  else
    stop("Invalid weighting","\n",sep='')
  DocumentTermMatrix(corpus, control = c(weighting = w, tolower = TRUE))
}



# Set weighting, removing empty contexts
weightTerms<-function(corpus, weighting.mode)
{
  matrix<-weightingChoice(corpus, weighting.mode)
  
  row.totals<-apply(matrix, 1, sum) #Find the sum of words in each document
  empty.rows<-matrix[row.totals==0, ]$dimnames[1][[1]]
  if(length(empty.rows)>0)
    corpus<-corpus[-as.numeric(empty.rows)]
  weightingChoice(corpus, weighting.mode)
}



# Set pair characteristics
assignLabels<-function(cluster.numbers, labels, pair.numbers)
{
  cluster.key<-rep(0, pair.numbers)
  label.words<-rep(NA, pair.numbers)
  
  # Assign cluster key and respective label
  for(i in 1:length(cluster.numbers))
  {
    pair.position<-as.numeric(names(cluster.numbers[i]))
    cluster.key[pair.position]<-as.integer(cluster.numbers[i][[1]])
    label.words[pair.position]<-paste(labels[cluster.numbers[i],], collapse = " / ")
  }
  cluster.identifiers<-data.frame(cluster.key, label.words, stringsAsFactors=FALSE)

  cluster.identifiers
}



defineClustering<-function(pair.contexts, weight.type="TF/IDF",
                           distance.measure="euclidean", dist.args=list(),
                           algorithm.type="k-means", hclust.args=list(method="ward.D"),
                           kmeans.args=list(algorithm="Lloyd"), cluster.percentage=0.1,
                           remove.stopwords=TRUE, words.number=1)
{
  corpus<-getCorpus(pair.contexts, remove.stopwords)

  # Get a DTM with weights
  weighted.matrix<-weightTerms(corpus, weight.type)
  
  # Distance matrix computation
  distance.matrix<-dist(weighted.matrix, method = distance.measure, dist.args)

  # Verify what contexts belong to each appropiate cluster
  pairs.number<-nrow(pair.contexts)
  n<-round(pairs.number*cluster.percentage)
  
  if (algorithm.type=="hierarchical")
  {
    tree<-hclust(distance.matrix, hclust.args)
    cluster.key<-cutree(tree, n)
  }
  else if (algorithm.type=="k-means")
  {
    clusters<-do.call(kmeans, c(list(distance.matrix, n), kmeans.args))
    cluster.key<-clusters$cluster
  }
  else
  {
    stop("Invalid clustering algorithm type.")
  }
  semantic.label<-matrix(nrow = n, ncol = words.number)
  
  for(i in 1:n)
  {
    c<-weighted.matrix[cluster.key==i,]
    word.sum<-apply(c,2,sum)
    # Get most frequent word in each cluster such way to label it
    semantic.label[i,]<-names(word.sum[order(word.sum,decreasing=T)[1:words.number]])
  }
  clusters<-assignLabels(cluster.key, semantic.label, nrow(pair.contexts))
  pair.contexts<-cbind(pair.contexts[,c(-3)],clusters)
  pair.contexts
}


