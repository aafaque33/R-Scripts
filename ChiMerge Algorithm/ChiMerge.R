#-----------------------------------------------------------------
# Author: Aafaque Aafaque
# Email: aafaqueabdullah@yahoo.com
#
# This is a Custom Code written to implement Chi-merrge Data mining Algorithm For iris R default data
#
# We can use Data from CSV also instead of default data
#
#-----------------------------------------------------------------

library(plyr)
library(discretization)

chithreshold = 5.99
maxintervals = 6
df = 2
freqTab =table(iris$Sepal.Length,iris$Species)
print("____________________________________")
print(" ")
print("Frequency Table Level 1")
print("------------------------------------")
print(" ")
print(freqTab)
print(" ")
s <- rownames(freqTab)
intLevel1 <- as.numeric(s[1:length(s)-1])
matrix1 <- freqTab[1:2,]
chisq1 <- chiSq(matrix1)

Chi2Level1 <- chisq1

print("____________________________________")
print(" ")
print("Interval list Level 2")
print("------------------------------------")
print(" ")
for (i in 2:length(intLevel1))
{
  j = i+1;
  matrix1 <- freqTab[i:j,]
  print(matrix1)
  Chi2Level1 <- c(Chi2Level1,chiSq(matrix1))
}
print(" ")

m <- format(round(Chi2Level1,2),nsmall=2)
print("____________________________________")
print(" ")
print("ChiSquare list Level2")
print("------------------------------------")
print(" ")
print(m)
print(" ")
m1 = as.numeric(m)
m2 = as.numeric(min(m))
minChiLevel1 <- as.numeric(format(round(m2,4),nsmall=2))

i <- 1
while (i <= length(intLevel1))
{
  if ( m1[i] <= minChiLevel1 )
  {
    intLevel2  <- intLevel1[i+1]
    freqtabLevel2 <- freqTab[i,] + freqTab[i+1,]
    k <- i + 1 
    
    #check if other levels to merge too
    
    while ( (m1[k] <= minChiLevel1) && (k != length(intLevel1)) )
    {
    #  print(k)
      intLevel2  <- intLevel1[k+1]
      freqtabLevel2 <- freqTab[k+1,] + freqtabLevel2
      if ( k == length(intLevel1) )
      {
        break 
      }
      k <- k + 1
    }
   # print("In first if")
   # print(c(intLevel2,freqtabLevel2))
    if( i == 1 )
    {
      TableLvl1 = rbind(c(intLevel2,freqtabLevel2))
    }
    else
    {
      TableLvl1 = rbind(TableLvl1,c(intLevel2,freqtabLevel2))
    }
    
    #print(i)
    i <- k
   # print(freqtabLevel2)
  }else{
    intLevel2 <- intLevel1[i]
    freqtabLevel2 <- freqTab[i,]
   # print("In first else")
  #  print(c(intLevel2,freqtabLevel2))
    if( i == 1 )
    {
      TableLvl1 = rbind(c(intLevel2,freqtabLevel2))
    }
    else
    {
      TableLvl1 = rbind(TableLvl1,c(intLevel2,freqtabLevel2))
    }
   # print(i)
  }
  
  i <- i + 1
}

print("____________________________________")
print(" ")
print("Frequency Table Level 2")
print("------------------------------------")
print(" ")
print(TableLvl1)
print(" ")

##Level 2 Starts here

s <- TableLvl1[,1]
intLevel2 <- as.numeric(s[1:length(s)])
matrix2 <- TableLvl1[1:2,]
chisq2 <- chiSq(matrix1)

Chi2Level2 <- chisq2

print("____________________________________")
print(" ")
print("Interval list Level 2")
print("------------------------------------")
print(" ")

for (i in 2:length(intLevel2)-1)
{
  j = i+1;
  matrix2 <- TableLvl1[i:j,]
  print(matrix2)
  Chi2Level2 <- c(Chi2Level2,chiSq(matrix2))
}
print(" ")

m <- format(round(Chi2Level2,2),nsmall=2)
print("____________________________________")
print(" ")
print("ChiSquare list Level3")
print("------------------------------------")
print(" ")
print(m)
print(" ")
m1 = as.numeric(m)
m2 = as.numeric(min(m))
minChiLevel2 <- as.numeric(format(round(m2,4),nsmall=2))

i <- 1
while (i <= length(intLevel2))
{
  if ( m1[i] <= minChiLevel2 )
  {
    intLevel3  <- intLevel2[i+1]
    freqtabLevel3 <- TableLvl1[i,2:4] + TableLvl1[i+1,2:4]
    k <- i + 1 
    while ( (m1[k] <= minChiLevel2) && (k != length(intLevel1)) )
    {
     # print(k)
      intLevel3  <- intLevel2[k+1]
      freqtabLevel3 <- TableLvl1[k+1,2:4] + freqtabLevel3
      if ( k == length(intLevel2) )
      {
        break 
      }
      k <- k + 1
    }
   # print("In first if")
    #print(c(intLevel3,freqtabLevel3))
    if( i == 1 )
    {
      TableLvl2 = rbind(c(intLevel3,freqtabLevel3))
    }
    else
    {
      TableLvl2 = rbind(TableLvl2,c(intLevel3,freqtabLevel3))
    }
    
   # print(i)
    i <- k
    # print(freqtabLevel2)
  }else{
    intLevel3 <- intLevel2[i]
    freqtabLevel3 <- TableLvl1[i,2:4]
    # print("In first else")
   # print(c(intLevel3,freqtabLevel3))
    if( i == 1 )
    {
      TableLvl2 = rbind(c(intLevel3,freqtabLevel3))
    }
    else
    {
      TableLvl2 = rbind(TableLvl2,c(intLevel3,freqtabLevel3))
    }
   # print(i)
  }
  
  i <- i + 1
}
print("____________________________________")
print(" ")
print("Frequency Table Level 3")
print("------------------------------------")
print(" ")
print(TableLvl2)
print(" ")
