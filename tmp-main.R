setwd("C:/Users/Nelson/MEOCloud/Dissertation-work")
source("entity-processing.R")
source("logger.R")
source("debugger.R")
source("context-processing.R")
source("relation-detection.R")
source("analysis.R")
source("relation-extraction.R")
source("pair-treatment.R")
source("evaluation.R")

# start.time <- Sys.time()

data.type<<-"folder" #"text"
textual.content<<-"C:/Users/Nelson/MEOCloud/Dissertation-work/textos/"
idiom<<-"pt"
what.context<<-"between" #"all"
only.consecutive<<-TRUE
exceptions<<-c("Lusa", "Fim", "LUSA", "FIM", "dez", "Dez")#
use.stemming<<-TRUE

every.entities<-extractEntities(source=textual.content, to="nothing")#
#Alternative
every.entities<-fileExtraction("extracted_entities.csv", TRUE, ",")
                            
# "Diário (As) Beiras" / "Tagmé (Na) Waié"
useful.entities<-cleanEntities(every.entities, special.cases = c(1643,2188))
useful.entities2<-cleanEntities(every.entities, special.cases = c(1643,2188))


counter<-countEntities(useful.entities)
counter2<-countEntities(useful.entities2)
confirmOccurrences(useful.entities, counter)
essential.entities<-filterSentences(useful.entities, counter)
essential.entities2<-filterSentences(useful.entities2, counter2)

binary.relations <- scanEfficiently(essential.entities)
between.binary.relations <- scanEfficiently(essential.entities2, order.significance = TRUE)
relation.contexts<-aggregateContexts(binary.relations)
relation.contexts2<-aggregateContexts(between.binary.relations)

some.relations <- scanEfficiently(useful.entities)
frequency.pairs<-pairsDistribution(some.relations)


more.relations <- scanEfficiently(useful.entities)

clusters<-defineClustering(relation.contexts, words.number = 10)

new.clusters<-defineClustering(relation.contexts, dist.args = list(p=1.5),
                               kmeans.args=list(algorithm="Hartigan-Wong", trace=TRUE, iter.max=300))
#,dist.args=list(diag=TRUE,upper=TRUE)

#,kmeans.args=list(algorithm="Hartigan-Wong" ,trace=TRUE)
new.clusters2<-defineClustering(relation.contexts, kmeans.args=list(algorithm="Lloyd",iter.max=200))


# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken
