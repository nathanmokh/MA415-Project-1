#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)

B2010 <- read.csv("bp appre 2010.csv")

B2017 <- read.csv("bp appre 2017.csv")

A2010 <- read.csv( "bp appre 2010.csv" , header = TRUE, stringsAsFactors = FALSE, sep = ",", dec = ".")

A2017 <- read.csv("bp appre 2017.csv", header = TRUE, stringsAsFactors = FALSE)

A2000.2017 <- read.csv("PB monthly summaries.csv", header = TRUE, stringsAsFactors = FALSE)

#### organiza data with Rownames, Column Totals, and Row Totals

## Use strings in Col 1 as row names
rownames(A2010) <- A2010[,1]

## Drop column 1
A2010 <-  subset(A2010, select= -c(Sector))

## rbind ColSums to dataframe
A2010 <- rbind(A2010, colSums(A2010))

## rbind assigns a rowname -- drop this name
-length(rownames(A2010))  

## rename the row with column totals "Total"
rownames(A2010) <- c(rownames(A2010)[-length(rownames(A2010))], "Total")

## cbind rowSums to dataframd
A2010 <- cbind(A2010,rowSums(A2010))

## rename last column "Totals
colnames(A2010) <- c(colnames(A2010)[-length(colnames(A2010))], "Total")



####################################################################################

## Use strings in Col 1 as row names
rownames(A2017) <- A2017[,1]

## Drop column 1
A2017 <-  subset(A2017, select= -c(Sector))

## rbind ColSums to dataframe
A2017 <- rbind(A2017, colSums(A2017))

## rbind assigns a rowname -- drop this name
-length(rownames(A2017))  

## rename the row with column totals "Total"
rownames(A2017) <- c(rownames(A2017)[-length(rownames(A2017))], "Total")

## cbind rowSums to dataframd
A2017 <- cbind(A2017,rowSums(A2017))

## rename last column "Totals
colnames(A2017) <- c(colnames(A2017)[-length(colnames(A2017))], "Total")

#####################################################################
# Define UI for application that draws a barplot
ui <- fluidPage(
   
   # Application title
   titlePanel("Border Patrol Apprehensions Charts"),
   
   #Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
        selectInput("slot_mode",
                    "Mode:",
                    choices = list("Apprehensions by Sector", "Time Series",
                                   "Total Apprehensions by Year")),
        conditionalPanel(condition = "input.slot_mode == 'Apprehensions by Sector'",
                         selectInput("slot_sector",
                                     "Sector:",
                                     choices = B2010$Sector)),
        conditionalPanel(condition = "input.slot_compare == false
                         & input.slot_mode == 'Apprehensions by Sector'",
                         selectInput("slot_year",
                                     "Year:",
                                     choices = list("2010", "2017"))),
        conditionalPanel(condition = "input.slot_mode == 'Total Apprehensions by Year'",
                         selectInput("slot_year2",
                                     "Year:",
                                     choices = list("2010", "2017"))),
        conditionalPanel(condition = "input.slot_mode == 'Apprehensions by Sector'",
                         checkboxInput(inputId = "slot_compare",
                                       label = strong("Year to year comparison"),
                                       value = FALSE))
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)



# Define server logic required to draw a barplot
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     
     if (input$slot_mode == "Apprehensions by Sector") {
     
       if (!input$slot_compare) {
       
           if (input$slot_year == "2010") {
           
             barplot(height = as.matrix(B2010[B2010$Sector == input$slot_sector, 2:13]), 
                     main = input$slot_sector,
                     ylab = "Number of Apprehensions",
                     xlab = "Month",
                     col = "red")
           }
           
           else {
             
             barplot(height = as.matrix(B2017[B2017$Sector == input$slot_sector, 2:13]), 
                     main = input$slot_sector,
                     ylab = "Number of Apprehensions",
                     xlab = "Month",
                     col = "blue")
           }
       }
       
       else {
         
         title <- input$slot_sector
         
         both <- rbind(B2010[B2010$Sector == input$slot_sector, 2:13],
                       B2017[B2017$Sector == input$slot_sector, 2:13])
         
         row.names(both) <- c("2010", "2017")
         
         barplot(as.matrix(both), beside = TRUE, col = c("red", "blue"), bty="n", xlab = "Month", ylab = "Number of Apprehensions" )
         
         legend("topright", c("2010","2017"), pch=15,  col=c("red","blue"),  bty="n", title = title)
         
       }
       
     } else if (input$slot_mode == "Total Apprehensions by Year") {
       
         if (input$slot_year2 == "2010") {
           
           barplot(A2010[1:9,13], 
                   names.arg = rownames(A2010)[1:9], 
                   las=2,
                   axisnames=TRUE,
                   main="2010 Border Patrol Apprehensions by Sector",
                   border="blue",
                   col="green",
                   ylim = c(0,250000))
           
         } else {
           
           barplot(A2017[1:9,13], 
                   names.arg = rownames(A2017)[1:9], 
                   las=2,
                   axisnames=TRUE,
                   main="2017 Border Patrol Apprehensions by Sector",
                   border="blue",
                   col="yellow",
                   ylim = c(0,250000))
         }
     } else {
       
       ts2 <- rev(as.vector(t(A2000.2017[,2:13])))
       
      ts3 <- ts(ts2, start = c(2000,10), frequency=12)
          
      ts.plot(ts3, gpars=list(xlab="year", ylab="Apprehensions", lty=c(1:3)), col = 'blue')
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

