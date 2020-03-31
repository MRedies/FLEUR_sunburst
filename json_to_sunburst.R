library("rjson")
library("sunburstR")


sumOfSubtimers <- function(node){
  time <- 0.0
  if(length(node["subtimers"][[1]]) > 0){
    for (i in 1:length(node["subtimers"][[1]])){
      time <- time  +  node[["subtimers"]][[i]]$totaltime
    }
  }
  return(time)
}


parseTree <- function(node, parentString, outlist){
  timername <- gsub("-", "", node["timername"])
  if(nchar(parentString) > 0){
    myString <- paste(parentString, paste(timername,node$totaltime,"s"), sep="-")
  }else{
    myString <- timername
  }
  selfTime <- node$totaltime - sumOfSubtimers(node)
    
  newRow <- data.frame(x=character(), y=numeric(), stringsAsFactors = FALSE)
  newRow[1,] <- list(toString(myString), selfTime)
  names(newRow) <- names(outlist)
  outlist <- rbind(outlist, newRow)
  
  if(length(node["subtimers"][[1]]) > 0){
    for (i in 1:length(node["subtimers"][[1]])){
      outlist <- parseTree(node[["subtimers"]][[i]], myString, outlist)
    }
  }

  return(outlist)
}

for (fol in Sys.glob("/Users/redies/calculations/benchmark/first_booster_run/*/k*/m*/juDFT_times*.json")){
    htmlname <-  paste(substring(fol, 1, nchar(fol)-5), ".html", sep="")
  if(!file.exists((htmlname))){
    print("create:")
    print(htmlname)
    data <- fromJSON(file=fol)
    outlist <- data.frame(V1=character(), V2=integer())
    outlist <- parseTree(data, "", outlist)
    
    sunburst(outlist)
    plot <- sunburst(outlist)
    htmltools::save_html(plot, file=htmlname)
  # }else{
  #   print("exists:")
  #   print(htmlname)
  }
}