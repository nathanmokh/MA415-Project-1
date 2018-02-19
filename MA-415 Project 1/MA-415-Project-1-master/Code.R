# Call in data sets
A2010 <- read.csv("bp appre 2010.csv", header = TRUE, stringsAsFactors = FALSE)
A2017 <- read.csv("bp appre 2017.csv", header = TRUE, stringsAsFactors = FALSE)

# get rownames of data tables
rownames(A2010) <- A2010[,1]
A2010[,1]
rownames(A2017) <- A2017[,1]

# add rows and columns to get total of each sector and month
getTotals <- function(data) {
  data <-  subset(data, select= -c(Sector))
  rownames(data)
  
  data <- rbind(data, colSums(data))
  rownames(data)
  
  -length(rownames(data))  
  
  rownames(data) <- c(rownames(data)[-length(rownames(data))], "Total")
  
  data <- cbind(data,rowSums(data))
  
  colnames(data) <- c(colnames(data)[-length(colnames(data))], "Total")
  return(data)
  
}

# Append totals for data
A2010 <- getTotals(A2010)
A2017 <- getTotals(A2017)

# view new data table with totals
View(A2010)
View(A2017)

# get side by side barplots comparing 2010 and 2017 apprehensions by month and sector
compare <- function(){
  
  for (x in 1:9) {
    
    title <- rownames(A2010)[x]
    
    both <- rbind(A2010[x,1:12], A2017[x,1:12])
    
    row.names(both) <- c("2010", "2017")
    
    barplot(as.matrix(both), beside = TRUE, col = c("red", "blue"), bty="n",xlab = 'Month', ylab = 'Number of Apprehensions' )
    
    legend("topright", c("2010","2017"), pch=15,  col=c("red","blue"),  bty="n", title = title)
    
  }
}
compare()

# T Test for maximum apprehensions of sector
customTest <- function(earlyData, newData) {
  #Assumes getTotals function has been run on data
  #Data for old data input
  maximumOld <- max(earlyData[1:9,13]) # find max of total
  maxRowIndexOld <- which.max(earlyData[1:9,13]) # find index of max sector
  earlyDataMean <- rowMeans(earlyData[maxRowIndexOld,1:12])

  
  #Data for new data input
  maximumNew <- max(newData[1:9,13])
  maxRowIndexNew <- which.max(newData[1:9,13])
  newDataMean <- rowMeans(newData[maxRowIndexNew,1:12])

  
  t.test(earlyData[maxRowIndexOld,1:12], newData[maxRowIndexNew,1:12])
  
  
}
customTest(A2010,A2017)

# Get top three months by apprehensions
topMonths <- function(data){
  copy <- data
  copy <- sort(copy[10,1:12], decreasing = TRUE)[1:3] # get months in order from highest to lowest apprehensions
  copy
}

# t test for max apprehensions of 3 month periods
compareThreeMonthPeriod <- function(earlyData, newData) {
  earlyDataThree <- topMonths(earlyData) # call in previous function to get max 3 months
  newDataThree <- topMonths(newData)
  earlyColName1 <- grep(colnames(earlyDataThree[1]), colnames(earlyData)) # get index of the max 3 months in original data set 
  earlyColName2 <- grep(colnames(earlyDataThree[2]), colnames(earlyData))
  earlyColName3 <- grep(colnames(earlyDataThree[3]), colnames(earlyData))
  earlyIndex <- min(earlyColName1,earlyColName2,earlyColName3) # get min index to get the first month of 3 month period
  
  newColName1 <- grep(colnames(newDataThree[1]), colnames(newData))
  newColName2 <- grep(colnames(newDataThree[2]), colnames(newData))
  newColName3 <- grep(colnames(newDataThree[3]), colnames(newData))
  newIndex <- min(newColName1,newColName2,newColName3)

  
  earlyMean <- mean(c(earlyData[1:9,earlyIndex],earlyData[1:9,earlyIndex+1],earlyData[1:9,earlyIndex+2]))
  newMean <- mean(c(newData[1:9,newIndex],newData[1:9,newIndex+1],newData[1:9,newIndex+2]))
  
  
  
  t.test(c(earlyData[1:9,earlyIndex],earlyData[1:9,earlyIndex+1],earlyData[1:9,earlyIndex+2]),c(newData[1:9,newIndex],newData[1:9,newIndex+1],newData[1:9,newIndex+2]))
  
  
}

compareThreeMonthPeriod(A2010,A2017)


# t test for data sets as a whole
t.test(A2010,A2017)


# time series graph
ts <- readClipboard(raw = FALSE) # copy data from monthly summaries

ts1 <- read.table("clipboard", sep = "\t")
ts1

ts2 <- rev(as.vector(t(ts1))) # reverse data to get years in chronological order

ts3 <- ts(ts2, start = c(2000,10), frequency=12)

ts.plot(ts3, gpars=list(xlab="year", ylab="Apprehensions", lty=c(1:3)), col = 'blue')

