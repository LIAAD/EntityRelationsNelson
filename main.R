setwd("C:/Users/Nelson/MEOCloud/Dissertation-work")
source("entity-processing.R")
source("logger.R")
source("debugger.R")
source("context-processing.R")
source("relation-detection.R")
source("EDA.R")
source("relation-extraction.R")
source("pair-treatment.R")
source("evaluation.R")


# TODO - Complete this.
operateRelationship<-function(entity.info, operation.option=1,
                              data.type="folder", content, idiom="pt", what.context="between",
                              only.consecutive=TRUE, dateline.removing="", exceptions=c(), use.stemming=TRUE)
{
  start.time <- Sys.time()
  
  #NE Pre-processing
  every.entities<-extractEntities(source=textual.content, to="nothing")#
  #Alternative
  every.entities<-fileExtraction("extracted_entities.csv", TRUE, ",")
                              
  useful.entities<-cleanEntities(every.entities, special.cases = c(1643,2188))
  
  counter<-countEntities(useful.entities)
  confirmOccurrences(useful.entities, counter)
  essential.entities<-filterSentences(useful.entities, counter)
  
  #(...)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
}


binary.relations <- scanEfficiently(essential.entities)
relation.contexts<-aggregateContexts(binary.relations)

some.relations <- scanEfficiently(useful.entities)
frequency.pairs<-pairsDistribution(some.relations)


more.relations <- scanEfficiently(useful.entities)

clusters<-defineClustering(relation.contexts, words.number = 10)

new.clusters<-defineClustering(relation.contexts, dist.args = list(p=1.5),
                               kmeans.args=list(algorithm="Hartigan-Wong", trace=TRUE, iter.max=300))
#,dist.args=list(diag=TRUE,upper=TRUE)

#,kmeans.args=list(algorithm="Hartigan-Wong" ,trace=TRUE)
new.clusters2<-defineClustering(relation.contexts, kmeans.args=list(algorithm="Lloyd",iter.max=200))


