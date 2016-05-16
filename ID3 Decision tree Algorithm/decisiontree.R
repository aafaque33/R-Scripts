#-----------------------------------------------------------------
# Author: Aafaque Aafaque
# Email: aafaqueabdullah@yahoo.com
#
# This is a Custom Code written to implement ID3 Decision Tree Algorithm
#
# Main Function to call " builddecisiontree"
# 
# command ot run "builddecisiontree('treedata1.csv')"
#
#-----------------------------------------------------------------

getattributes <- function()
{
  a <- names(treedata)
  for(i in 1:ncol(treedata)) { 
    
    b <- summary(treedata[[a[i]]])
    print(b)
  }
}

readdata <- function(filepath)
{
  data <- read.csv(file=filepath,head=TRUE,sep=",")
  return(data)
}

getclassifier <- function(treedata)
{
  name <- names(treedata)
  allent <- c()
  for(j in 1:length(name))
  {
    a <- table(treedata[[name[j]]])
    b <- sum(table(treedata[[name[j]]]))
    sum = calculateentropy(a,b)
    allent <- rbind(allent, c(name[j],sum))
  }
  suppressWarnings(index <- which.min(allent[,2]))
  list(treedata[[index]],index,name[[index]])
}

getotherattributes <- function (treedata,index)
{
  otherdata <- treedata[,-index]
  return(otherdata)
}

splitdataset <- function (classifier,otherdata)
{
  split <- c()
  name <- names(otherdata)
  for(i in 1:ncol(otherdata))
  {
    split[[name[i]]] <- as.matrix(table(otherdata[[i]],classifier))
    split[[name[i]]] <- cbind(split[[name[i]]],0)
    split[[name[i]]] <- rbind(split[[name[i]]],0)
    cols = ncol(split[[name[i]]])
    rows =  nrow(split[[name[i]]])
    for(j in 1:rows)
    {
      split[[name[i]]][j,cols]  <- sum(split[[name[i]]] [j,])
    }
    for(k in 1:cols)
    {
      split[[name[i]]][rows,k]  <- sum(split[[name[i]]][,k])
    }
  }
  return(split)
}

calculateentropy <- function(set,total)
{
  sum = 0 
  
  for(i in 1:length(set))
  {
    sum <- sum + (-((set[i]/total) * (log2(set[i]/total))))
  }
  return(replace(sum,is.nan(sum),0))
}

attrentropy <- function(attr,classifier)
{
  sum = 0 
  rows <- nrow(attr)
  cols <- ncol(attr)
  for(i in 1:(rows-1))
  {
    sum <- sum + ((attr[i,cols] / attr[rows,cols]) * calculateentropy(attr[i,-cols],attr[i,cols]))
  }
  return(sum)
}

getinfogain <- function(splitdata,classifier)
{
  gains <- c();
  for( i in 1:length(splitdata))
  {
    gain <- calculateentropy(table(classifier),sum(table(classifier))) - attrentropy(splitdata[[i]],classifier)
    gains <- c(gains,gain)
  }
  return(gains)
}

maxinfogain <- function(splitdata,classifier)
{
  gains <- c();
  for( i in 1:length(splitdata))
  {
    gain <- calculateentropy(table(classifier),sum(table(classifier))) - attrentropy(splitdata[[i]],classifier)
    gains <- c(gains,gain)
  }
  return(names(splitdata[which.max(gains)]))
}

getfirstnode <- function(treedata)
{
  n <- getclassifier(treedata)
  otherdata <- getotherattributes(treedata,n[[2]])
  splitteddata <- splitdataset(n[[1]],otherdata)
  node <- maxinfogain(splitteddata,n[[1]])
  return(node)
}

getallnodes <- function(node,treedata,classifier,count,treeseparator)
{
  if(count == 0)
    cat(node,"\n\n")
  nodeindex <- which(names(treedata) == node)
  childs <- levels(treedata[[node]])
  updatedtree <- subset(treedata, treedata[[nodeindex]] == childs[1], -nodeindex) 
  # print(childs)
  for(i in 1:length(childs))
  {
    remainingdata <- subset(treedata, treedata[[nodeindex]] == childs[i], -nodeindex) 
    index <- which(names(remainingdata) == classifier)
    classifier1 <- remainingdata[[index]]
    if(length(unique(classifier1)) == 1 )
    {
      node1 <- as.character(unique(classifier1))
      cat(paste(treeseparator,childs[i],"\n\n"))
      cat(paste(treeseparator,"=>",node1,"\n\n"))
      if(count > 0)
      {
        return
      }
    }
    else
    {
      #print(classifier1)
      otherdata1 <- getotherattributes(remainingdata,index)
      #print("")
      splitteddata1 <- splitdataset(classifier1,otherdata1)
      node1 <- maxinfogain(splitteddata1,classifier1)
      cat(paste(treeseparator,childs[i],"\n\n"))
      cat(paste(treeseparator,"=>",node1,"\n\n"))
      #print("")
      getallnodes(node1,remainingdata,classifier,count+1,paste(treeseparator,"=> =>"))
    }
  }
  return
}

builddecisiontree <- function (filepath)
{
  treedata <- readdata(filepath)
  cat("Original Tree Data \n")
  cat("----------------------------------------------------------")
  cat("\n")
  print(treedata)
  cat("\n")
  cat("===============================================================")
  cat("\n")
  n <- getclassifier(treedata)
  cat("Data to Classify \n")
  cat("----------------------------------------------------------")
  cat("\n")
  cat(paste(n[[3]],"\n"))
  print(n[[1]])
  cat("\n")
  cat("===============================================================")
  cat("\n")
  otherdata <- getotherattributes(treedata,n[[2]])
  cat("Classifiers \n")
  cat("----------------------------------------------------------")
  cat("\n")
  print(otherdata)
  cat("\n")
  cat("===============================================================")
  cat("\n")
  splitteddata <- splitdataset(n[[1]],otherdata)
  cat("Splitted Data \n")
  cat("----------------------------------------------------------")
  cat("\n")
  print(splitteddata)
  cat("\n")
  cat("===============================================================")
  cat("\n")
  cat("Decision Tree for the Data \n")
  cat("----------------------------------------------------------")
  cat("\n")
  node <- maxinfogain(splitteddata,n[[1]])
  getallnodes(node,treedata,n[[3]],0,"=>")
}
