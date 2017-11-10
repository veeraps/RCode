library("shiny")
library("ggplot2")
library("plotly")

#("C:/Veera/R/SmartParking/RShiny")
setwd("/srv/connect/apps/smart_car_parking")
#load smart parking lots data
Lots = read.csv('Smart_Parking_Lots.csv')
full = read.csv("Q1.csv")

drawplot = function(df, Title)
{
  plot_ly(df, x=~Date, y=~ParkedTime,type='scatter',mode='lines+markers') %>%
    layout(title=Title,xaxis = list(title='Date',tickangle=45), margin = list(b=160),yaxis= list(title='Occupancy Rate (mins)'))
}

drawplotWithSplit = function(df, Title, Splitby)
{
  if (Splitby=="BayType")
  {
    plot_ly(df, x=~Date, y=~ParkedTime, split = ~BayType, type='scatter',mode='lines+markers') %>%
    layout(title=Title,xaxis = list(title='Date',tickangle=45), margin = list(b=160),yaxis= list(title='Occupancy Rate (mins)'))
  }
  else if (Splitby == "StreetType")
  {
    plot_ly(df, x=~Date, y=~ParkedTime, split = ~SectorName, type='scatter',mode='lines+markers') %>%
      layout(title=Title,xaxis = list(title='Date',tickangle=45), margin = list(b=160),yaxis= list(title='Occupancy Rate (mins)'))    
  }
}

ui = fluidPage(
  #Inputs - Lot No
  navbarPage(
    title = "Smart Car Parking Data Analysis",
    tabPanel(
        "Occupancy Rate",
        sidebarPanel(
          selectInput("LotCode","Lot Codes",choices = Lots$LotCode,selected = "701"),
          selectInput("PaidValue","Parking Type",choices = c("Paid","Free"),selected = "Paid"), width = 2, height="100%"
        ),
        mainPanel(fluidRow(
                splitLayout(cellWidths = c("60%", "60%"), plotlyOutput("OverallOccRate"),plotlyOutput("PaidTimedOccRate")),
                tags$div(
                  HTML("<br><br><br>")
                ),              
                splitLayout(cellWidths = c("60%", "60%"), plotlyOutput("StreetWiseOccRate"),plotlyOutput("LotWiseOccRate"))
              )
            )
        ),
    tabPanel(
        "Over Stays",
        mainPanel((fluidRow(
          
          selectInput("Paid","Parking Type",choices = c("Paid","Free"),selected = "Paid"),
          
          splitLayout(cellWidths = c("70%", "70%"), plotlyOutput("FreeTimedOverStay",width="100%"), plotlyOutput("OverStayByBayTypeByStreet",width="100%")),
          tags$div(HTML("<br><br><br>")), 
          plotlyOutput("OverStayByBayType")          
                          )
                  ))
            )
)
)


server = function(input,output){
  library("ggplot2")
  library("plotly")

  
#1
  #Date
  #Occupancy rate = total office minutes / total parked mins
  ByDate = subset ( full , full$parkedtime > 2)
  Q1.1 = aggregate((ByDate$parkedtime/ByDate$BayCount/480) ~ByDate$ParkedDate, data=ByDate, FUN=sum)
  colnames(Q1.1) = c("Date","ParkedTime")

  output$OverallOccRate = renderPlotly( { drawplot(Q1.1, "Overall Occupancy Rate") })  
  
#2
  #Date
  #Lot
  #Occupancy Rate
  ByLot = subset ( full, full$LotNo == '701' & parkedtime > 2)
  output$LotWiseOccRate = renderPlotly( {
    ByLot = subset ( full, full$LotNo == input$LotCode & parkedtime > 2)
    #data = reactive({input$LotCode})
    Q1.2 = aggregate((ByLot$parkedtime/ByLot$BayCount/480) ~ ByLot$ParkedDate, data=ByLot, FUN=sum)
    colnames(Q1.2) = c("Date","ParkedTime")
    drawplot(Q1.2,"Occupancy Rate By Lot") 
    })  

#3
  #Date
  #Street
  #Occupancy Rate
  ByOffStreet = subset ( full, parkedtime > 2)
  Q1.3 = aggregate((ByOffStreet$parkedtime/ByOffStreet$BayCount/480) ~(ByOffStreet$ParkedDate+ByOffStreet$SectorName), data=ByLot, FUN=sum)
  colnames(Q1.3) = c("Date","SectorName","ParkedTime")
  
  output$StreetWiseOccRate = renderPlotly( { drawplotWithSplit(Q1.3, "Occupancy Rate By Street","StreetType") })  
  
#4
  #Date
  #Parking Type - On Street - Paid and Timed
  #Occupancy Rate
  ByPaidTimed = subset ( full, full$BayType.x %in% c("1P Voucher", "2P Voucher", "30m Voucher" , "4P", "8P") & parkedtime > 2)

  output$PaidTimedOccRate = renderPlotly({
      
  if ( input$PaidValue == "Paid" )
  {
    ByPaidTimed = subset ( full, full$BayType.x %in% c("1P Voucher", "2P Voucher", "30m Voucher" , "4P", "8P") & parkedtime > 2)
    myTitle = "Occupancy Rate By Paid and Timed Bays"
  }
  else
  {
    ByPaidTimed = subset ( full, full$BayType.x %in% c("5Min Free", "15m Free", "30m Free" , "1P Free", "2P Free") & parkedtime > 2)
    myTitle = "Occupancy Rate By Free and Timed Bays"
  }
  Q1.4 = aggregate((ByPaidTimed$parkedtime/ByPaidTimed$BayCount/480) ~(ByPaidTimed$ParkedDate+ByPaidTimed$BayType.x), data=ByPaidTimed, FUN=sum)
  colnames(Q1.4) = c("Date","BayType","ParkedTime")
  drawplotWithSplit (Q1.4, myTitle,"BayType") 
  })  
  
#Overstay - Q5  
  full$OverStayFlag = 0
  full[full$parkedtime - full$MaxStayPeriod  > 10, "OverStayFlag"] = 1

  Q5 = subset ( full, full$BayType.x %in% c("1P Voucher", "2P Voucher", "30m Voucher" , "4P", "8P") & full$parkedtime > 2 & full$OverStayFlag == 1)

  output$FreeTimedOverStay = renderPlotly({
    
    if ( input$Paid == "Paid" )
    {
      Q5 = subset ( full, full$BayType.x %in% c("1P Voucher", "2P Voucher", "30m Voucher" , "4P", "8P") & full$parkedtime > 2 & full$OverStayFlag == 1)
      myTitle = "Red Overstay Hotspots - Paid and Timed"
      mycolors="brown"
    }
    else
    {
      Q5 = subset ( full, full$BayType.x %in% c("5Min Free", "15m Free", "30m Free" , "1P Free", "2P Free") & full$parkedtime > 2 & full$OverStayFlag == 1 )
      myTitle = "Red Overstay Hotspots - Free and Timed"
      mycolors = "red"
    }
    Q5 = aggregate(OverStayFlag~LotName,data=Q5,FUN=length)
    Q5 = head(Q5[order(Q5$OverStayFlag, decreasing= T),], n = 5)
    Q5 = data.frame(as.character(Q5$LotName),Q5$OverStayFlag)
    colnames(Q5) = c("LotName","TotalOverStay")
    plot_ly(Q5, x=~LotName, y=~TotalOverStay,type='bar',color="", colors=mycolors) %>%
      layout(title=myTitle, xaxis = list(categoryarray = ~TotalOverStay, categoryorder = "array", tickangle=45), margin = list(b=200))
    
  })  
  
  #Overstay - Q6  
  full$OverStayFlag = 0
  full[full$parkedtime - full$MaxStayPeriod  > 10, "OverStayFlag"] = 1
  myTitle = "Overstay By Baytype"
  output$OverStayByBayType = renderPlotly({
    Q6 = subset (full, OverStayFlag == 1)
    Q6 = aggregate(OverStayFlag~(BayType.x),data=Q6,FUN=length)
    Q6 = Q6[order(Q6$OverStayFlag, decreasing= T),]
    Q6 = data.frame(as.character(Q6$BayType.x),Q6$OverStayFlag)
    colnames(Q6) = c("Baytype","TotalOverStay")
    
    plot_ly(Q6, x=~Baytype, y=~TotalOverStay,type='bar',color =c(""), colors="darkmagenta") %>%
    layout(title=myTitle, xaxis = list(categoryarray = ~TotalOverStay, categoryorder = "array", tickangle=45), margin = list(b=200))
    
  })    
  
  #Overstay - Q7  
  full$OverStayFlag = 0
  full[full$parkedtime - full$MaxStayPeriod  > 10, "OverStayFlag"] = 1
  myTitle = "Overstay By Baytype By Street"
  output$OverStayByBayTypeByStreet = renderPlotly({
    Q7 = subset (full, OverStayFlag == 1)
    Q7 = aggregate(OverStayFlag~(BayType.x+Street),data=Q7,FUN=length)
    Q7 = Q7[order(Q7$OverStayFlag, decreasing= T),]
    Q7 = data.frame(as.character(Q7$BayType.x),Q7$Street,Q7$OverStayFlag)
    colnames(Q7) = c("Baytype","Street","TotalOverStay")
    
    plot_ly(Q7, x=~Baytype, y=~TotalOverStay,type='bar',color=~Street) %>%
      layout(title=myTitle, xaxis=list(categoryarray = ~TotalOverStay, categoryorder = "array", tickangle=45), margin = list(b=200),barmode="stack")
    
  })  
}
shinyApp(ui = ui, server = server)

