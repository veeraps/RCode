
library("shiny")

ui = fluidPage(
  
  titlePanel(title = h1("Defects Inflow Vs Outflow Analysis", align="center")) ,
  mainPanel(
      selectInput("typeofChart","Defects Analysis By",choices = c("Month","Date"),selected = "Month"),
      plotOutput("DefectChart", height=400,width=900),
      plotOutput("StateChart",height=300,width=700)
    )
  )

server = function(input, output) {
  library ("ggplot2")
  library(reshape2)
  
  setwd("/srv/connect/apps/RShiny")
  #setwd("C:/RMS Laptop/RMS/Actuos Global/Status Reports")
  ProdDefects <- read.csv("SourceProdDefects.csv")
  
  #Remove spaces in the column names
  colnames(ProdDefects) <- gsub(" ","",colnames(ProdDefects))

  #Remove records created by US team        
  ProdDefects <- ProdDefects[!ProdDefects$Created.By=='Holly Justice',]
  ProdDefects <- ProdDefects[!ProdDefects$Created.By=='Josh Houghton',]
  ProdDefects <- ProdDefects[!ProdDefects$Created.By=='sean.leadum',]
  ProdDefects <- ProdDefects[!ProdDefects$Created.By=='Tim Perdue',]
  ProdDefects <- ProdDefects[!ProdDefects$Created.By=='rajmohann',]
  ProdDefects <- ProdDefects[!ProdDefects$Created.By=='Nick Bowman',]
  ProdDefects <- ProdDefects[!ProdDefects$Created.By=='nair.raman',]
  ProdDefects <- ProdDefects[!ProdDefects$Created.By=='Balaji Ramamoorthy',]
  ProdDefects <- ProdDefects[!ProdDefects$Created.By=='Kyle DeVaney',]
  ProdDefects <- ProdDefects[!ProdDefects$Created.By=='reddy.vatti',]
  ProdDefects <- ProdDefects[!ProdDefects$Created.By=='Ryan Kirkland',]
  #write.csv(ProdDefects,"GlobalProdDefects.csv")
  StatusTable <-table(ProdDefects$State)
  Status = data.frame(StatusTable)
  
  output$StateChart = renderPlot({
    return ( ggplot(Status, aes(x=Var1, y=Freq)) + geom_bar(stat='identity', fill = c("light green","orange","sky blue","tomato")) +
      geom_text(aes(label=Freq), size=4,position=position_stack(vjust=0.5))+
      theme(axis.text.x = element_text(angle = 90,size=13),axis.title.x = element_text(face="bold", colour="#990000", size=15),axis.title.y = element_text(face="bold", colour="#990000", size=15))+
      labs(x = "State",y="Defects")+
      ggtitle("Defects By State") )
  })
  
  
  
  ProdDefects$LoggedMonth <- format(as.Date(strptime(ProdDefects$Created.Date,"%m/%d/%Y")), "%y-%m")
  ProdDefects$ClosedMonth <- format(as.Date(strptime(ProdDefects$Closed.Date,"%m/%d/%Y")), "%y-%m")
  
  ProdDefects$LoggedDate <- format(as.Date(strptime(ProdDefects$Created.Date,"%m/%d/%Y")), "%y-%m-%d")
  ProdDefects$ClosedDate <- format(as.Date(strptime(ProdDefects$Closed.Date,"%m/%d/%Y")), "%y-%m-%d")
  
  ProdDefects$LoggedYear <- format(as.Date(strptime(ProdDefects$Created.Date,"%m/%d/%Y")), "%y")
  
  data = reactive({input$typeofChart})

  output$DefectChart = renderPlot({
    if (data() == "Month"){
      LoggedMonthTable <- table(ProdDefects$LoggedMonth)
      LoggedMonth <- as.data.frame(LoggedMonthTable)
      
      ClosedMonthTable <- table(ProdDefects$ClosedMonth)
      ClosedMonth <- as.data.frame(ClosedMonthTable)
      
      DefectTable <- merge(LoggedMonth, ClosedMonth, by.x="Var1", by.y="Var1", all = T)
      colnames(DefectTable) <- c("Month", "LoggedCount", "ClosedCount")
      
      write.csv(DefectTable, file="defectstable.csv")
      
      DefectFlow = read.csv("defectstable.csv")
      myvars = c("LoggedCount","ClosedCount","Month")
      DefectFlow = DefectFlow[myvars]
      
      melteddata = melt(DefectFlow, id.vars = 'Month')
      return (ggplot(melteddata, aes(x=Month, y=value, fill=variable)) + geom_bar(stat='identity') +
                geom_text(aes(label=value), size=4,position=position_stack(vjust=0.5))+
                theme(axis.text.x = element_text(angle = 90),axis.title.x = element_text(face="bold", colour="#990000", size=15),axis.title.y = element_text(face="bold", colour="#990000", size=15))+
                labs(x = "Month",y="Defects")+
                ggtitle("Defects Inflow Vs Outflow By Month") )
    }
    else
    {
      ProdDefects = subset ( ProdDefects, ProdDefects$LoggedMonth %in% c( "17-09", "17-10", "17-11") |  ProdDefects$ClosedMonth %in%  c("17-09","17-10","17-11"))
      
      LoggedDateTable <- table(ProdDefects$LoggedDate)
      LoggedDate <- as.data.frame(LoggedDateTable)
      
      ClosedDateTable <- table(ProdDefects$ClosedDate)
      ClosedDate <- as.data.frame(ClosedDateTable)
      
      DefectDateTable <- merge(LoggedDate, ClosedDate, by.x="Var1", by.y="Var1", all = T)
      colnames(DefectDateTable) <- c("Date", "LoggedCount", "ClosedCount")
      
      DefectDateFlow = DefectDateTable
      
      myvars = c("LoggedCount","ClosedCount","Date")
      DefectDateFlow = DefectDateFlow[myvars]
      
      melteddata = melt(DefectDateFlow, id.vars = 'Date')
      
      return(ggplot(melteddata, aes(x=reorder(Date,Date), y=value, fill=variable)) + geom_bar(stat='identity') +
               geom_text(aes(label=value), size=4,position=position_stack(vjust=0.5)) +
               theme(axis.text.x = element_text(angle = 90),axis.title.x = element_text(face="bold", colour="#990000", size=15),axis.title.y = element_text(face="bold", colour="#990000", size=15))+
               labs(x = "Date",y="Defects")+
               ggtitle("Defects Inflow Vs Outflow By Date"))
    }
  }
  )
}
shinyApp(ui = ui, server = server)
