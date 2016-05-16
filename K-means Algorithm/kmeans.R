#-----------------------------------------------------------------
# Author: Aafaque Aafaque
# Email: aafaqueabdullah@yahoo.com
#
# This is a Custom Code written to implement K-means Algorithm
#
# Main Function to call " createclusters "
# 
# command ot run "createclusters('kmeansdata1.csv',clustersize)"
#
#-----------------------------------------------------------------

readclusterdata <- function(filepath)
{
  data <- read.csv(file=filepath,head=TRUE,sep=",")
  return(data)
}

choosevectors <- function(clusterdata,number)
{
  size = nrow(clusterdata) * ncol(clusterdata)
  vectors <- as.matrix(clusterdata)
  centers <- c()
  for(i in 1:number)
  {
    j = sample(1: nrow(clusterdata),1,replace=FALSE)
    centers <- rbind(centers,vectors[j,])
  }
  return(centers)
}

calculatedistance <- function(point,center)
{
  distance <- 0
  for(i in 1:length(center))
  {
    distance <- distance + abs(point[i] - center[i])
  }
  return(distance)
}

calculatecluster <- function(point,center)
{
  rows <- c()
  for(i in 1:nrow(point))
  {
    columns <- c()
    for(j in 1:nrow(center))
    {
      distance <- calculatedistance(point[i,],center[j,])
      columns <- c(columns, as.numeric(distance))
    }
    columns <- c(columns,which.min(columns))
    columns <- c(columns,i)
    rows <- rbind(rows, columns)
  }
  rownames(rows) <- NULL
  return(rows)
}

calculatemean <- function(result)
{
  newcenter <- c()
  for(i in 1:ncol(result))
  {
    sum <- 0
    for(j in 1:nrow(result))
    {
      sum <- sum + result[j,i]
    }
    mean <- sum / nrow(result)
    newcenter <- c(newcenter,mean)
  }
  return(newcenter)
}

getnewcentroid <- function(clusterdata,clusterresult,noofcluster) 
{
  newcentroid <- c()
  for(i in 1:noofcluster)
  {
    subsets <- subset(clusterdata, clusterresult[,noofcluster+1] == i)
    newcentroid <- rbind(newcentroid,calculatemean(subsets))
  }
  return(round(newcentroid, 1))
}

getfinalclusters <- function(point,newcenters,oldcenters,noofcluster)
{
  if(all(newcenters == oldcenters))
  {
    rows <- calculatecluster(point,newcenters)
    return(rows)
  }
  else
  {
  rows <- calculatecluster(point,newcenters)
  test <- getnewcentroid(point,rows,noofcluster)
  #print(test)
  final <- getfinalclusters(point,test,newcenters,noofcluster)
  }
  return(final)
}

createclusters <- function(filepath,clustersize)
{
  clusterdata <- readclusterdata(filepath)
  cat("Create Cluster for following Data \n")
  cat("----------------------------------------------------------")
  cat("\n")
  print(clusterdata)
  cat("\n")
  cat("===============================================================")
  cat("\n")
  centers <- choosevectors(clusterdata,clustersize)
  cat("Random Initial Centers \n")
  cat("----------------------------------------------------------")
  cat("\n")
  print(centers)
  cat("\n")
  cat("===============================================================")
  cat("\n")
  rows <- calculatecluster(clusterdata,centers)
  cat("Initial Clusters \n")
  cat("----------------------------------------------------------")
  cat("\n")
  print(rows)
  cat("\n")
  cat("===============================================================")
  cat("\n")
  newcenters <- getnewcentroid(clusterdata,rows,clustersize)
  finalclusters <- getfinalclusters(clusterdata,newcenters,centers,clustersize)
  cat("Final Clusters after recursive mean calculation \n")
  cat("----------------------------------------------------------")
  cat("\n")
  print(finalclusters)
  cat("\n")
  cat("===============================================================")
  cat("\n")
  cat("Cluster Wise Data \n")
  cat("----------------------------------------------------------")
  cat("\n")
  for(i in 1:clustersize)
  {
    subsets <- subset(clusterdata, finalclusters[,clustersize+1] == i)
    cat("\n")
    cat(paste("Cluster",i,"\n"))
    print(subsets)
    cat("\n")
  }
  cat("\n")
  cat("===============================================================")
  cat("\n")
}