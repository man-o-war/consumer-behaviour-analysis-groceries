 # Load the libraries
library (arules)
library (arulesViz)
library (datasets)
data (Groceries) # Load the data set
transDat <- Groceries
transDat <- as (myDataFrame, "transactions") # convert to 'transactions' class
inspect (transDat) # view the observations
length (transDat) # get number of observations
size (transDat) # number of items in each observation
LIST(transDat) # convert 'transactions' to a list, note the LIST in CAPS
frequentItems <- eclat (Groceries, parameter = list(supp = 0.07, maxlen = 15)) # calculates support for frequent items
itemFrequencyPlot (Groceries,topN=10,type="absolute") # plot frequent items
rules <- apriori (Groceries, parameter = list(supp = 0.001, conf = 0.5)) # Min Support as 0.001, confidence as 0.8.
quality(rules) # show the support, lift and confidence for all rules
# Show the top 5 rules, but only 2 digits
options (digits=2)
inspect (rules[1:5])
rules <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
rules <- apriori (Groceries, parameter = list (supp = 0.001, conf = 0.5, maxlen=3)) # maxlen = 3 limits the elements in a rule to 3
redundant <- which (colSums (is.subset (rules, rules)) > 1) # get redundant rules in vector
rules <- rules[-redundant] # remove redundant rules
rules <- apriori (data=Groceries, parameter=list (supp=0.001,conf = 0.08), 
	appearance = list (default="lhs",rhs="whole milk"), control = list (verbose=F)) # get rules that lead to buying 'whole milk'
rules <- apriori (data=Groceries, parameter=list (supp=0.001,conf = 0.15,minlen=2), 
	appearance = list (default="rhs",lhs="whole milk"), control = list (verbose=F)) # those who bought 'milk' also bought..
rules <- sort (rules, decreasing=TRUE,by="confidence")
redundant <- which (colSums(is.subset(rules, rules)) > 1) # get redundant rules in vector
rules <- rules[-redundant] # remove redundant rules inspect (rules[1:7])
discretize (x, method="cluster", categories=3) # method can make cuts in equal "intervals", "frequency", "cluster", "fixed"
# Interactive Plot
plot (rules[1:25],method="graph",interactive=TRUE,shading="confidence") # feel free to expand and move around the objects in this plot
plot (rules, measure=c("support", "lift"), shading="confidence")
affinity(transDat) # Calculates affinity - the 'nxn' Jaccard Index affinity matrix
transDat_c <- addComplement(transDat, "Item 1") # Adds "Item 1" to all transactions in transDat
duplicated(rules) # find out if any rule is duplicated