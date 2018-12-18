## Tracking Shiny

if(!require("shiny")) {install.packages("shiny"); require("shiny")}
if(!require("tidyverse")) {install.packages("tidyverse"); require("tidyverse")}
if(!require("stringr")) {install.packages("stringr"); require("stringr")}
if(!require("tibble")) {install.packages("tibble"); require("tibble")}

## UI
ui <- fluidPage(
  fluidPage(
    titlePanel("Upload Raw Data From Qualtrics, then Download the Reformatted File"),
    sidebarLayout(
      sidebarPanel(
        fileInput('file1', 'Choose CSV File',
                  accept=c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')),
        downloadButton('downloadData', 'Download')
      ),
      mainPanel(
      )
    )
  )
)
# Define server logic ----

server <- function(input, output) {
  
  getData <- reactive({
    
    inFile <- input$file1
    
    if (is.null(input$file1))
      return(NULL)
    isolate({
   
      my_data <- read.csv(inFile$datapath)
      ## End list with delimiter 
      totalPages = max(str_count(my_data$perPagePT, "PAGE BREAK"))
      originalColumnNames <- colnames(my_data)
      ## separate arguments = 1. column to split 2. labels of column (finds total columns based on number of pages in survey) 3. Delimiter
      df <- my_data %>% separate(perPagePT, sprintf("Page_%d", 1:(totalPages)),  sep = "PAGE BREAK", fill = "right")
      
      ## Drop all other columns, only one column (with array of times) per page remains
      timingArrays <- df[ , !(names(df) %in% originalColumnNames)] 
      
      arrayColumnNames <- colnames(timingArrays)
      ## Loop through the column for each page and extract additional columns of data (clickAways, timeOffPage, timeOnPage )
      for(i in 1:totalPages) {
        
        columnName <- names(timingArrays)[i]
        ## Separate so one there is one time per cell
        boundaryCrossings <- timingArrays %>% separate_(columnName, sprintf("Crossing_%d", 0:max(str_count(timingArrays[,i], "\\."),na.rm = T)),sep = ",", fill = "right")
        ## Drop all other columns
        boundaryCrossings <- boundaryCrossings[ , !(names(boundaryCrossings) %in% arrayColumnNames)] 
        ## Drop Crossing_0 column (since each vector starts with a ",")
        boundaryCrossings <- boundaryCrossings[,-which(names(boundaryCrossings)=="Crossing_0")] 
        ## Convert to numeric
        boundaryCrossings <- as.data.frame(sapply(boundaryCrossings, as.numeric))
        
        
        ## Calculate total time on the page
        timeOnPage <- apply(boundaryCrossings, 1, function(x) if(all(is.na(x))) {""} else{sum(x[x>0],na.rm = T)})
        df <- add_column(df, timeOnPage,.after = paste("Page_", i, sep = "")) 
        ## Rename newly added column
        names(df)[which(colnames(df)== paste("Page_", i, sep = ""))+1] <-  paste(paste("Page_",i,sep = ""),"_TimeOnPage", sep = "")
        
        ## Calculate total time off the page
        timeOffPage <- apply(boundaryCrossings, 1, function(x) if(all(is.na(x))) {""} else{abs(sum(x[x<0],na.rm = T))})
        df <- add_column(df, timeOffPage,.after = paste("Page_", i, sep = ""))
        ## Rename newly added column
        names(df)[which(colnames(df)== paste("Page_", i, sep = ""))+1] <-  paste(paste("Page_",i,sep = ""),"_TimeOffPage", sep = "")
        
        ## Calculate click aways (one pair = one click away)
        clickAways <-apply(boundaryCrossings, 1,function(x) if(all(is.na(x))) {""} else{ ceiling(sum(!is.na(x))/2)})
        df <- add_column(df, clickAways,.after = paste("Page_", i, sep = ""))
        ## Rename newly added column
        names(df)[which(colnames(df)== paste("Page_", i, sep = ""))+1] <-  paste(paste("Page_",i,sep = ""),"_ClickAways", sep = "")
      }
      
      ## Replace NAs with blanks
      df <- sapply(df, as.character)
      df[is.na(df)] <- " "
      as.data.frame(df)
      
      my_data <- df
      
    })
   
    my_data
    
  })
  
  
  output$contents <- renderTable(
    
    getData()
    
  )
  
  output$downloadData <- downloadHandler(
    
    filename = function() { 
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      
      write.csv(getData(), file)
      
    })
  
}

# See above for the definitions of ui and server


shinyApp(ui = ui, server = server)




