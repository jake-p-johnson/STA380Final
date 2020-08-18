###########################
# Association rule mining #
# Groceries               #
###########################
rm(list=ls())

library(arules)
library(arulesViz)

## read in & summary
groceries = read.transactions("~/MSBAsummer20/groceries.txt", rm.duplicates=TRUE, format="basket", sep=",")
summary(groceries)
itemFrequencyPlot(groceries, support=0.1, cex.names=0.8)

#### 
grocery_rules = apriori(groceries, parameter=list(support=.05, confidence=.3, maxlen=8))
plot(grocery_rules, interactive=TRUE)

#top 3 rules sorted by confidence
inspect(head(sort(grocery_rules, by = "support"), 10))

# shows 
inspect(subset(grocrules, subset=support > .05))

# parallel coordinates
subrules2 <- head(grocery_rules, n = 10, by = "lift")
plot(subrules2, method = "paracoord")

############ subset talk about alcohol buying tendencies

# generates rules that lead to buying wine
redwine_rules <- apriori(data=groceries, parameter=list (supp=0.001,conf = 0.08), appearance = list (rhs="red/blush wine"))
plot(redwine_rules)

# generates rules that lead to buying wine
liquor_rules <- apriori(data=groceries, parameter=list (supp=0.0001,conf = 0.08), appearance = list (rhs="liquor"))
plot(liquor_rules)
inspect(head(sort(liquor_rules, by = "support"), 10))
subrulesliquor <- head(liquor_rules, n = 10, by = "confidence")
plot(subrulesliquor, method = "paracoord")

############ subset talk about whole milk buying tendencies

# generates rules that lead to buying whole milk
milk_rules <- apriori(data=groceries, parameter=list (supp=0.001,conf = 0.08), appearance = list (rhs="whole milk"))
plot(milk_rules)
inspect(head(sort(milk_rules, by = "support"), 10))
inspect(head(sort(milk_rules, by = "confidence"), 10))
subrulesmilk <- head(milk_rules, n = 10, by = "lift")
plot(subrulesmilk, method = "paracoord")

###### small v large
rulesSmallSize <- subset(rules, subset = size(rules) <=2 )
rulesLargeSize <- subset(rules, subset = size(rules) >= 5 )