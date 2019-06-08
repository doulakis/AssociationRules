# DBTechNet, HoL4a (D.A. Dervos, Malaga, May 10, 2019): The "transaction" data structure

# clear the working memory
rm(list=ls())

# libraries
library("readxl")
library("arules")
library("Matrix")

# set the working directory
# setwd("/Volumes/MTR1TB/My_Office/TEI_Semesters/TEI_18b/classes/UMA/Ref/HoL_4_Part_A")

#########################################################
# read the "groceries" data into a matrix-like data frame
g_matrix<-read.csv("groceries.csv", stringsAsFactors = FALSE)
is.data.frame(g_matrix)  ## g_matrix: 532672 bytes

# View the first five rows
# Xaction number 1 has (erroneously) been taken top register column labels...
g_matrix[1:5,]
View(g_matrix)   # same item may appear in different column(s) and thus be treated differently for
                 # the corresponding xactions

# Output the "g_matrix" to "g_matrix.csv" 
write.csv(g_matrix, file="output/g_matrix.csv")
# Notice that all xactions are of MAX(lenght)=4
# Comparing "g_matrix.csv" to "groceries.csv", one notes that beginning with the first 
# xaction of length>4 (xaction number 6 in "groceries.csv"), xaction contents are "messed up"
# Apparently, this is not the way to go...


#############################################
# How about reading the "groceries" data into a matrix-like data frame w. factors?
# ...same thing...
gg_matrix<-read.csv("groceries.csv", stringsAsFactors = TRUE)
is.data.frame(gg_matrix)  ## gg_matrix: 294744 bytes
str(gg_matrix)
View(gg_matrix)

####################################################
# This time, read the "groceries.csv" content into a (arules) "transaction" 
# data structure: "groceries"
# 9835 xactions
# 169 items
# 9835 * 169 = 1662115 cells
groceries <- read.transactions("groceries.csv", sep=",")
str(groceries)   ## groceries: 228560 bytes

# Items in xactions are sorted alphabetically
inspect(groceries[5,])
str(groceries[5,])  # formal class "transactions"
inspect(groceries[1:20,])
str(groceries[1:20,]) # formal class "transactions"

# Columns: items, sorted alphabetically
# frequencies of item numbers 1:5
itemFrequency(groceries[,1:5])

# Xaction #12 items may be extracted in the form of a list of one vector
alfa <- as(groceries,"list")[12]
alfa
str(alfa)
alfa[1]
alfa[[1]]
alfa[[1]][1]
alfa[[1]][4]


##################################
# Decide to continue with the "transactions" (sparse matrix) data structure
rm(g_matrix)
rm(gg_matrix)
rm(alfa)


############################
#### Descriptive Statistics
summary(groceries)  
itemFrequency(groceries[,1:6]) # items are placed in (numbered) columns in alphabetical order

# items appearing in >= 10% of the xactions
itemFrequencyPlot(groceries, support=0.1)
# topN (frequency-wise) items
itemFrequencyPlot(groceries, topN=10)
# Establish a view to the sparse matrix content:
image(groceries[1:20])
# For large xaction databases: pick a (random) sample of their content
image(sample(groceries,150))


