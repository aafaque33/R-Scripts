#-----------------------------------------------------------------
# Author: Aafaque Aafaque
# Email: aafaqueabdullah@yahoo.com
#
# This is a Custom Code written to implement Apriori Data mining Algorithm
#
# Main Function to call "myAprioriAlgo "
# 
# command ot run "myAprioriAlgo("TestARules2.csv",minsupport,minconf)"
#
#-----------------------------------------------------------------

#This will take Filepath and name as input and return the unique items present in basket and also the basket in form of list
#itemsandbasket = basketitems("TestARules2.csv")
#basket = itemsandbasket[[2]]
#items <- itemsandbasket[[1]]

basketitems <- function(filepath) {
  
  a <- read.csv(file=filepath,head=FALSE,sep=",")
  items <- c()
  
  for(i in 1:ncol(a))
  {
    v = paste("V",i,sep = "")
     items <- c(items,levels(a[[v]]))
  }
  
  uniqueitems = unique(items)
  uniqueitems[uniqueitems==""] <- NA
  itemsets <- uniqueitems[complete.cases(uniqueitems)]
  a <- as.matrix(a)
  list(itemsets,a)
  
}

# This function will get unique Itemset in a basket and return all the subsets of these itemsets

getallsubsets <- function(items) {
  
  library('plyr')
  n <- length(items)
  bin <- expand.grid(rlply(n, c(F, T)))
  itemset <- mlply(bin, function(...) { items[c(...)] })
  possiblesets = c()
    for(i in 2:2^n)
    { 
        possiblesets <- rbind(possiblesets,itemset[[i]])
    }
  return(possiblesets)
}

# This function is same as getallsubsets but will get unique Itemset in a basket  and K value and return all the k-length Subsets

getksubsets <- function(items,k) {
  library('plyr')
  n <- length(items)
  bin <- expand.grid(rlply(n, c(F, T)))
  itemset <- mlply(bin, function(...) { items[c(...)] })
  possiblesets = c()
  for(i in 2:2^n)
  { 
    if(length(itemset[[i]]) == k)
    {
      possiblesets <- rbind(possiblesets,itemset[[i]])
      
    }
  }
  return(possiblesets)
}

# This function will return the Support for a given itemset or subset in a basket

supItemSet <- function(Itemset,basket)
{
  count = 0 ;
  for(i in 1:nrow(basket))
  {
    if((length(grep("TRUE", is.element(Itemset,basket[i,]))) >= length(Itemset)) == TRUE)
    {
      count = count + 1
    }
  }
  return(count)
}

#This Function will return item Sets of length n that have support greater than or equal to min support

getnFrequentSets <- function(basket,sets,minsupp,n)
{
  freqset = c(); 
  poss <- getksubsets(sets,n)
  #print(poss)
  if(length(poss) != 0)
  {
    for(i in 1:nrow(poss))
    {
      s <- supItemSet(poss[i,],basket)
      if(s >= minsupp)
      {
        freqset <- c(freqset,poss[i,])
      }
    }
  }
  return(unique(freqset))
}

#This Function will return all Freq Item Sets of that have support greater than or equal to minsupport

getallFrequentSets <- function(basket,items,minsupp)
{
  onefreqitemset = items
  freqitemsets <- c()
  for(i in 1:length(items))
  {
    onefreqitemset <- getnFrequentSets(basket,onefreqitemset,minsupp,i)
    if(length(onefreqitemset) == 0 )
      break
    freqitemsets <- getksubsets(onefreqitemset,i)
  }
  return(freqitemsets)
}

# This function is used to generate all assocation rules, from frequent itemsets.

getassocrules <- function(items,basket)
{
  rule3 = c();
  for(i in 1:nrow(items))
  {
    for(j in 1:(length(items[i,])-1))
    {
      subsets <- getksubsets(items[i,],j)
      for(k in 1:nrow(subsets))
      {
        remain <- setdiff(items[i,], subsets[k,])
        rule <- paste("{",paste(as.vector(subsets[k,]),collapse=","),"}","=>","{",paste(as.vector(remain),collapse=","),"}")
        c <- confAR(subsets[k,],remain,basket)
        s <- suppAR(c(subsets[k,],remain),basket)
        l <- liftAR(subsets[k,],remain,basket)
        rule3 = rbind(rule3,c(rule,format(round(s, 2), nsmall = 2),format(round(c, 2), nsmall = 2),format(round(l, 2), nsmall = 2)))
      }
    }
  }
  colnames(rule3) <- c("rules","support","confidence","lift")
  return(rule3)
}

# Calculate Confidence of a given association Rules

confAR <- function(X,Y,basket)
{
  num <- supItemSet(c(X,Y),basket)
  din <- supItemSet(X,basket)
  conf <- num / din
  return(conf)
}

# Calculate Support of a given union of association rule

suppAR <- function(X,basket)
{
  num <- supItemSet(X,basket)
  din <- nrow(basket)
  supp <- num / din
  return(supp)
}

# Calculate lift of a given association Rules

liftAR <- function(X,Y,basket)
{
  Pxuy <- suppAR(c(X,Y),basket)
  Px <- suppAR(X,basket)
  Py <- suppAR(Y,basket)
  lift <- Pxuy / (Px * Py )
  return(lift)
}

# This function is used to get all association rlules that have confidence greater than or euals to mininum confidence out of all the generated association rules

getbestassocrules <- function(allassocrules,minconf)
{
  bestassocrules = c()
  for(i in 1:nrow(allassocrules))
  {
    if(allassocrules[i,3] >= minconf)
    {
      bestassocrules <- rbind(bestassocrules,allassocrules[i,])
    }
  }
  return(bestassocrules)
}

# this function is used to get all associaton rules that has best and maximum lift out of all the generated association rules

getbestliftarules <- function(allassocrules)
{
  bestliftarules = c()
  maxlift <- max(allassocrules[,4])
  for(i in 1:nrow(allassocrules))
  {
    if(allassocrules[i,4] == maxlift)
    {
      bestliftarules <- rbind(bestliftarules,allassocrules[i,])
    }
  }
  return(bestliftarules)
}


# This is main function for apriori algorithm that is using all other functions it just take 
# File name and path, minimum support (in percentage 0-1 or frequency) and Min confidence in percentage

myAprioriAlgo <- function(file,minsupp,minconf)
{
  itemsandbasket = basketitems(file)
  basket = itemsandbasket[[2]]
  print("----------------------------------------------------------")
  print("Basket Data")
  print(" ")
  print(basket)
  print(" ")
  print("===============================================================")
  print(" ")
  
  items <- itemsandbasket[[1]]
  
  print("Items in basket")
  print("----------------------------------------------------------")
  print(" ")
  print(items)
  print(" ")
  print("===============================================================")
  print(" ")
  
  if(minsupp < 1 )
  {
    x <- minsupp * nrow(basket)
    minsupp <- round(x, digits = 0)
  }
  
  #=====================================================================
  # If you want to get last frequent items sets generated use this code
  #=====================================================================
  
  #freqitems = getallFrequentSets(basket,items,minsupp)
  #print("Most Frequent Items")
  #print(freqitems)
  #print(" ")
  #print("===============================================================")
  #print(" ")
  #r <- getassocrules(freqitems,basket)
  
  #=====================================================================
  # If you want to get all frequent items sets generated use this code
  #=====================================================================
  
  onefreqitemset = items
  freqitems <- c()
  r <- c()
  print("Most Frequent Items")
  print("----------------------------------------------------------")
  print(" ")
  for(l in 1:length(items))
  {
    freqitemsets <- c()
    onefreqitemset <- getnFrequentSets(basket,onefreqitemset,minsupp,l)
    if(length(onefreqitemset) == 0 )
      break
    freqitems <- getksubsets(onefreqitemset,l)
    for(i in 1:nrow(freqitems))
    {
      s <- supItemSet(freqitems[i,],basket)
      if(s >= minsupp)
      {
        freqitemsets <- rbind(freqitemsets,freqitems[i,])
      }
    }
    print(freqitemsets)
    if(length(freqitemsets[1,]) > 1 )
    r <- rbind(r,getassocrules(freqitemsets,basket))
  }
  print(" ")
  print("===============================================================")
  print(" ")
  
  print("All the generated Association Rules")
  print("----------------------------------------------------------")
  print(" ")
  print(r)
  print(" ")
  print("===============================================================")
  print(" ")
  
  rconf <- getbestassocrules(r,0.5)
  
  print("All the generated Association Rules that has Confidence greater than or Equals min Confidence")
  print("----------------------------------------------------------")
  print(" ")
  print(rconf)
  print(" ")
  print("===============================================================")
  print(" ")
  
  
  rlift <- getbestliftarules(rconf)
  
  print("Best generated Association Rules that has maximun Lift")
  print("----------------------------------------------------------")
  print(" ")
  print(rlift)
  print(" ")
  print("===============================================================")
  print(" ")
  
}