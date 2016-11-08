
#Observe pieces of text
sentenceLog<-function(sentence.location, sentence)
{
  info<-paste("\tSentence ", sentence.location[1],
              " of paragraph ", sentence.location[2],
              sep = '')
  
  if(!is.na(sentence.location[3]))
    info<-paste(info, " from file \"", sentence.location[3], "\":", sep = '')
  
  cat(info,"\n\n")
  cat(sentence,"\n\n\n\n")
}



#Facts about a context
contextLog<-function(file.name, sentence.position, context.entities, context)
{
  file.info <- paste("File:", file.name, "\n")
  paragraph.info <- paste("Paragraph:", sentence.position[1], "\t")
  sentence.info <- paste("Sentence:", sentence.position[2], "\n")
  entities.info <- paste("Entities:", context.entities[1],
                         "/", context.entities[2], "\n")
  context.info <- paste("Context: \"", context, "\"\n\n\n", sep = '')
  
  #Everything to show about a relationship
  cat(file.info)
  cat(paragraph.info,sentence.info)
  cat(entities.info)
  cat(context.info)
}



#Alert to a relation
relationLog<-function(some.entity, another.entity)
{
  alert<-paste("There is a relation between \"", some.entity, "\" and \"",
           another.entity, "\"!", "\n\n", sep = '')
  cat(alert)
}



#Show must-link pairs and respective descriptions
presentMSC<-function(relation.pairs, m, reasons, labels)
{
  cat("Must-Link Pairs\n\n")
  invisible(sapply(1:length(reasons), function(i) {
    
    cat("Reason to association: \"", reason[i], "\"\n\n", sep = '')
    
    left <- relation.pairs$entity1.name==m[i*4-3]
    right <- relation.pairs$entity2.name==m[i*4-2]
    first.element <- left & right
    cat(relation.pairs[first.element,1],"\t")
    cat(relation.pairs[first.element,2],"\n")
     
    left <- relation.pairs$entity1.name==m[i*4-1]
    right <- relation.pairs$entity2.name==m[i*4]
    second.element <- left & right
    cat(relation.pairs[second.element,1],"\t")
    cat(relation.pairs[second.element,2],"\n")

    cat("\nAssigned keywords: \"", labels[i], "\"\n\n\n", sep = '')
  }))
}



#Show cannot-link pairs and respective descriptions
presentXSC<-function(relation.pairs, x, reasons)
{
  cat("Cannot-Link Pairs\n\n")
  invisible(sapply(1:(length(x)/4), function(i) {
    
    left.element <- relation.pairs$entity1.name==x[i*4-3]
    right.element <- relation.pairs$entity2.name==x[i*4-2]
    pair.selection <- left.element & right.element
    
    cat("Relation reason: \"",reasons[i*2-1],"\"\n", sep = '')
    cat("\t",relation.pairs[pair.selection,1],"\t")
    cat("\t",relation.pairs[pair.selection,2],"\n")
    
    left.element <- relation.pairs$entity1.name==x[i*4-1]
    right.element <- relation.pairs$entity2.name==x[i*4]
    pair.selection <- left.element & right.element
    
    cat("Relation reason: \"",reasons[i*2],"\"\n", sep = '')
    cat("\t",relation.pairs[pair.selection,1],"\t")
    cat("\t",relation.pairs[pair.selection,2],"\n\n")
  }))
}



# The clusters where pairs stay
clusteringLog<-function(cluster, pairs, header)
{
  cat(header, "\n\n")
  sapply(1:(length(pairs)/4), function(p){
    
    #Entity references and respective numbers 
    left <- cluster$entity1.name==pairs[p*4-3]
    right <- cluster$entity2.name==pairs[p*4-2]
    first.element <- cluster[left & right,]
    
    cat(first.element[,1], "\t")
    cat(first.element[,2], "\t")
    c1 <- first.element[,3]
    cat("c:",c1,"\n")
    
    left <- cluster$entity1.name==pairs[p*4-1]
    right <- cluster$entity2.name==pairs[p*4]
    second.element <- cluster[left & right,]
    
    cat(second.element[,1], "\t")
    cat(second.element[,2], "\t")
    c2 <- second.element[,3]
    
    if(c1==c2)
    {
      cat("c:",c2,"\n\n")
      cat("There is a cluster matching!\n\n\n")
    }
    else
      cat("c:",c2,"\n\n\n")
  })
}



#Present the measures
evaluationLog<-function(measure)
{
  cat("\nPrecision: ", round(measure[1]*100, 2),"%", sep = '')
  cat("\nRecall: ", round(measure[2]*100, 2),"%", sep = '')
  cat("\nF-Score: ", round(measure[3]*100, 2),"%\n\n\n", sep = '')
}



#Presented table with portuguese language content in LaTeX notation
showTable<-function(data.content, content.type="data_frame_header")
{
  line <- "\t\t\t\t\\hline\n"
  double.line <- "\t\t\t\t\\hline\\hline\n"
  # Start table printing
  cat("\t\\begin{table}[h]\n") 
  cat("\t\t\\begin{center}\n") 
  cat("\t\t\t\\begin{tabular}{}\n")
  cat(line)
  
  # Show content inside cells 
  if(content.type=="simple_pairs") # Named entity pairs
  {
    cat("\t\t\t\t\\multicolumn{1}{|c|}{\\textbf{$1º$ elemento}} &")
    cat(" \\multicolumn{1}{|c|}{\\textbf{$2º$ elemento}} \\\\","\n", sep = '')
    cat(double.line)
    for(i in 1:(length(data.content)/4))
    {
      table.row <- paste(data.content[i*2-1], data.content[i*2], sep = ' & ')
      cat("\t\t\t\t", table.row," \\\\","\n", sep = '')
      cat(line)
    }
  }
  else if(content.type=="double_pairs") # Pairs of pairs
  {
    cat("\t\t\t\t\\multicolumn{2}{|c|}{\\textbf{$1º$ elemento}} &")
    cat(" \\multicolumn{2}{|c|}{\\textbf{$2º$ elemento}} \\\\","\n", sep = '')
    cat(double.line)
    cat("\t\t\t\t\\multicolumn{1}{|c|}{\\textbf{$1º$ elemento}} &")
    cat(" \\multicolumn{1}{|c|}{\\textbf{$2º$ elemento}} &")
    cat(" \\multicolumn{1}{|c|}{\\textbf{$1º$ elemento}} &")
    cat(" \\multicolumn{1}{|c|}{\\textbf{$2º$ elemento}} \\\\","\n", sep = '')
    cat(double.line)
    for(i in 1:(length(data.content)/4))
    {
      table.row <- paste(data.content[(i*4-3):(i*4)], collapse = ' & ')
      cat("\t\t\t\t", table.row," \\\\","\n", sep = '')
      cat(line)
    }
  }
  else if(content.type=="data_frame_header") # Column names 
  {
    col1<-"\\multicolumn{1}{|c|}{\\textbf{Atributo}} & "
    col2<-"\\multicolumn{1}{|c|}{\\textbf{Descrição}} \\\\"
    cat("\t\t\t\t", col1, col2, "\n", double.line, sep = '') # table header
    between <- paste(' & ... \\\\\n', line,'\t\t\t\t', sep = '')
    variables <- paste(names(data.content), collapse = between)
    cat("\t\t\t\t", variables, " & ... \\\\\n", line, sep = '')
  }
  else if(content.type=="data_frame") # Row values
  {
    variable.names <- paste(names(data.content),
                            collapse = '}} & \\multicolumn{1}{|c|}{\\textbf{')
    cat("\t\t\t\t", "\\multicolumn{1}{|c|}{\\textbf{", variable.names, "}} \\\\",
        "\n", sep = '') # table header
    cat(double.line)
    apply(data.content, 1,
          function(x) {
            instance<-paste(x, collapse = ' & ')
            cat("\t\t\t\t", instance, " \\\\", "\n", sep = '')
            cat(line)
            })
  }
  else
    stop("Invalid content.")

  cat("\t\t\t\\end{tabular}\n")
  cat("\t\t\t\\caption{... \\label{tab:}}\n")
  cat("\t\t\\end{center}\n")
  cat("\t\\end{table}\n")
}
