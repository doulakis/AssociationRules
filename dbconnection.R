library(DBI)
library("arules")
library("datasets")
library("arulesViz")
# Connect to my-db as defined in ~/.my.cnf
con <- dbConnect(RMySQL::MySQL(), group = "doulakis-db")

#dbListTables(con)

#dbListFields(con, "groceries")
#dbReadTable(con, "groceries")

#dbClearResult(res)
# You can fetch all results:
res <- dbSendQuery(con, "SELECT * FROM groceries")
data <-dbFetch(res, n=-1)
#new2 <- as(data,"transactions")
#names(data)[1] <- "tid"
#names(data)

data <- strsplit(as.character(data$transaction),',',fixed=T) # split by comma    
trans <- as(data, "transactions")
#inspect(trans)


#Groceries <- trans
rules <- apriori(trans)
rules

#write(data, file = "Data/temp_trans");

#trans = read.transactions("Data/temp_trans", sep=",");

#inspect(trans);

rules <- apriori(trans, parameter=list(supp=0.005, conf=0.25, minlen=2))
rules

ruledf = data.frame(
        lhs = labels(lhs(rules))$elements,
        rhs = labels(rhs(rules))$elements, 
        rules@quality)

df = data.frame(
  lhs = labels(lhs(rules)),
  rhs = labels(rhs(rules)), 
  rules@quality)

dbWriteTable(con, "df", df)


inspect(head(trans))
#dbClearResult(res)

# Or a chunk at a time
#res <- dbSendQuery(con, "SELECT * FROM groceries")
#while(!dbHasCompleted(res)){
#  chunk <- dbFetch(res, n = 5)
#  print(nrow(chunk))
#}
# Clear the result
#dbClearResult(res)

# Disconnect from the database
dbDisconnect(con)

