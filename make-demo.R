source("entity-processing.R")
source("logger.R")
source("debugger.R")
source("context-processing.R")
source("relation-detection.R")
source("analysis.R")
source("relation-extraction.R")
source("pair-treatment.R")
source("evaluation.R")

# Main parameters useful for pair clustering
idiom<<-"pt"
exceptions<<-c("Lusa", "Fim", "LUSA", "FIM", "dez", "Dez") #Common words from headers/footers 
use.stemming<<-TRUE

######### Clustering with intermediate contexts #########

df01 <- readRDS("distinct-pairs-with-intermediate-contexts.rds")

clustering01<-defineClustering(df01, weight.type = "SMART", distance.measure = "maximum",
                               kmeans.args=list(algorithm="Forgy", iter.max=35),
                               cluster.percentage=0.05, words.number = 6)

######### Clustering with complete contexts #########

df02 <- readRDS("distinct-pairs-with-complete-contexts.rds")

clustering02<-defineClustering(df02, weight.type = "TF", distance.measure = "manhattan",
                               kmeans.args=list(algorithm="Lloyd", iter.max=30),
                               cluster.percentage=0.075, words.number = 10)

