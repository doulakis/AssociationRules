# Author:Stelios Doulakis
# Developed for MSc Web Intelligence of International University of Greece

# Import Libraries
library(DBI)
library("arules")
library("datasets")
library("arulesViz")

# initialize the connection and find the details from hidden file
# ".my_cnf" under the project directory
con <- dbConnect(RMySQL::MySQL(), group = "doulakis-db")


# Fetching all the data from the table groceries
res <- dbSendQuery(con, "SELECT * FROM groceries")

# Put the data into a dataframe
data <-dbFetch(res, n=-1)

# Splitting the data 
data <- strsplit(as.character(data$transaction),',',fixed=T) # split by comma

# Create the transaction Data Structure
trans <- as(data, "transactions")

# Run the apriori with default settings (resulting zero rules)
rules <- apriori(trans)

# Run the apriori with supp=0.005, conf=0.25 and minlen=2
# found 662 rules
rules <- apriori(trans, parameter=list(supp=0.005, conf=0.25, minlen=2))


# Print the first 10 rules
inspect(rules[1:10])

# Create a new dataframe from arules
df = data.frame(
  lhs = labels(lhs(rules)),
  rhs = labels(rhs(rules)), 
  rules@quality)

# Write the dataframe into MySQL Table "df" (autocreation)

dbWriteTable(con, "df", df, overwrite=TRUE)



#######################
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

# Create a new dataframe from redundant rules
df_reduntant = data.frame(
  lhs = labels(lhs(rules.redundant)),
  rhs = labels(rhs(rules.redundant)), 
  rules.redundant@quality)

# Write Redundant rules to MySQL Table
dbWriteTable(con, "df_reduntant", df_reduntant, overwrite=TRUE)



summary(rules.redundant)
write(rules.redundant,file="output/redundant_rules.csv",quote=TRUE,sep=",",col.names=NA)
rules.pruned <- rules.sorted[!redundant]
rules<-rules.pruned
summary(rules)
inspect(rules[1:5])


# Create a new dataframe from redundant rules
df_pruned = data.frame(
  lhs = labels(lhs(rules.pruned)),
  rhs = labels(rhs(rules.pruned)), 
  rules.pruned@quality)

# Write Redundant rules to MySQL Table
dbWriteTable(con, "df_pruned", df_pruned, overwrite=TRUE)



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

final_rules <- rules_df

dbWriteTable(con, "final_rules", final_rules, overwrite=TRUE)


head(rules_df)
rules_df
rules_df[order(-rules_df$support),]
rules_df[order(-rules_df$confidence),]

