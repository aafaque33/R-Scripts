Some Important Commands To run

source('Apriori.R')
myAprioriAlgo("filename.csv",minsupport,minconf)


# Extra Commands 

# as basketitems return list of basket and items so we have to split them after return
itemsandbasket = basketitems("TestARules2.csv")
basket = itemsandbasket[[2]]
items <- itemsandbasket[[1]]

freqitems = getallFrequentSets(basket,items,2)
freqitems
getksubsets(freqitems[1,],4)

r <- getassocrules(freqitems)
r <- getassocrules(freqitems,basket)
rconf <- getbestassocrules(r,0.5)
getbestliftarules(rconf)