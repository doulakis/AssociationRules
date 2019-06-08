## DBTechNet, UMA HoL (D.A. Dervos, May 10, 2019): Association rules, discovery of
rm(list=ls())
library("arules")
library("datasets")
library("arulesViz")

#
# set the working directory to the current directory
#
# read the data into the groceries transactions sparse matrix
Groceries <- read.transactions("groceries.csv", sep=",")

#############
# execute the "apriori" function with default parameters
rules <- apriori(Groceries)
# Inspecting the apriori() output:
# 0 rules produced
# default "apriori()" parameters: support>=10%, confidence>=80%, minlen=1, maxlen=10)
# "8 items done": 8 items have support >= 0.1% 

#############
# Planning:
# "Groceries" registers one month's transactions data
# Interested in items purchased at least twice in each one day 
# => support >= 60/9385 = 0.0064 = 0.64%
# minimum confidence: depends on what we are looking for. This time: 0.25
# minlen=2 (to avoid trivial rules of the {}=>{A} type)
rules <- apriori(Groceries, parameter=list(supp=0.005, conf=0.25, minlen=2))
rules            ### 662 rules

############
# Model (quality) inspection and improvement
summary(rules)

inspect(sort(rules, by="lift")[1:10]) # rules in head-expanded form

# rule subsets
selected <- subset(rules, items %in% "whole milk")
inspect(selected)
selected <- subset(rules, items %pin% "frozen")
inspect(selected)
selected <- subset(rules, rhs %in% "whole milk")
inspect(selected)
selected <- subset(rules, lhs %in% "other vegetables" & rhs %in% "whole milk" )
inspect(selected)

#########
# From: http://www.rdatamining.com/examples/association-rules
# spot the redundancies (e.g.):

# {onions} => {other vegetables}
# {root vegetables,onions} => {other vegetables}
# {root vegetables} => {other vegetables}	
#########
# remove the redundant rules
rules.sorted <- sort(rules, decreasing=TRUE, by="lift")
write(rules.sorted,file="output/all_rules.csv",quote=TRUE,sep=",",col.names=NA)
subset.matrix <- is.subset(rules.sorted,rules.sorted, sparse=FALSE)
subset.matrix[lower.tri(subset.matrix, diag=T)] <-NA
redundant <- colSums(subset.matrix, na.rm=T) >=1
str(redundant)
redundant
which(redundant)
View(redundant)
rules.redundant<-rules.sorted[redundant]
summary(rules.redundant)
write(rules.redundant,file="output/redundant_rules.csv",quote=TRUE,sep=",",col.names=NA)
rules.pruned <- rules.sorted[!redundant]
rules<-rules.pruned
summary(rules)
inspect(rules[1:5])
write(rules,file="output/pruned_rules.csv",quote=TRUE,sep=",",col.names=NA)

selected <- subset(rules, lhs %in% "other vegetables" & rhs %in% "whole milk" )
inspect(selected)
rules  # number of rules: reduced to 417

selected <- subset(rules, items %pin% "frozen")
inspect(selected)

# plot(selected, method="graph", interactive=TRUE, shading=NA)
plot(selected, method="graph", engine="interactive", shading=NA)
plot(selected, method="graph", control=list(type="items"), shading=NA)
plot(selected)
plot(selected, method="grouped")
plot(selected, method="paracoord", control=list(reorder=TRUE))


##################
# rules into a data frame
# ordered by decreasing support, lift, etc. 
##################
rules_df = data.frame(lhs=labels(lhs(rules)),
                      rhs=labels(rhs(rules)),
                      rules@quality)
head(rules_df)
rules_df
rules_df[order(-rules_df$support),]
rules_df[order(-rules_df$confidence),]

