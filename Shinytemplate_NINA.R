# load the required packages
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
###########################
options(encoding="UTF-8")
library(httr)
library(rjstat)
##Get data from the stats agency
# url for POST
url <- "https://data.ssb.no/api/v0/en/table/03951/"
# Query, copied from API Console
# Run by highlighting all of this function and ctrl enter/r
data <- '{  "query": [    {      "code": "Region",      "selection": {"filter": "vs:FylkerJakt3b","values": ["01","02","03","04","05","06","07","08","09","10","11","12","14","15","50","16","17","18","19","20","AAA","BBB"]}}],"response": {"format": "json-stat2"}}'
# post query
d.tmp <- POST(url , body = data, encode = "json", verbose())
# Get content from d.tmp as text, using fromJSONstat
dattable <- fromJSONstat(content(d.tmp, "text"))
#head(dattable)

#Build shinyapp
#UI
ui <- dashboardPage(
  
  dashboardHeader(
    
    title = "Norway hunters Dashboard",
    
    titleWidth = 200
    
  ),
  
  dashboardSidebar(
        sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Visit-us", icon = icon("send",lib='glyphicon'), 
                 href = "https://www.nina.no")
      )
    ),
  
  dashboardBody(
    
    tabsetPanel(
      
      id = "tabs",
      
      tabPanel(
        
        title = "Main Dashboard",
        
        value = "page1",
      fluidRow(
          valueBoxOutput("value1"),
            valueBoxOutput("value2"),
            valueBoxOutput("value3")
          ),
      fluidRow(
          valueBoxOutput("value4")
          ,valueBoxOutput("value5")
          ,valueBoxOutput("value6")
        ),
      fluidRow(
          box(
            title = "Active hunters per year"
            ,status = "primary"
            ,solidHeader = TRUE 
            ,collapsible = TRUE 
            ,plotOutput("hunterbyyr", height = "600px")
          ),
          box(
            title = "Hunters by Region"
            ,status = "primary"
            ,solidHeader = TRUE 
            ,collapsible = TRUE 
            ,plotOutput("huntersbyRegion", height = "600px")
          )
      )
      ),
      tabPanel(
        
        title = "Lynx",
        
        value = "page2",
        fluidRow(
          box(
            title = "Active hunters per year"
            ,status = "primary"
            ,solidHeader = TRUE 
            ,collapsible = TRUE 
            ,plotOutput("Lynxhunterbyyr", height = "600px")
          ),
          box(
            title = "Hunters by Region"
            ,status = "primary"
            ,solidHeader = TRUE 
            ,collapsible = TRUE 
            ,plotOutput("LynxhuntersbyRegion", height = "600px")
          )
        )
      ),
      tabPanel(
        
        title = "Grouse",
        
        value = "page3",
        fluidRow(
          box(
            title = "Active hunters per year"
            ,status = "primary"
            ,solidHeader = TRUE 
            ,collapsible = TRUE 
            ,plotOutput("Grousehunterbyyr", height = "600px")
          ),
          box(
            title = "Hunters by Region"
            ,status = "primary"
            ,solidHeader = TRUE 
            ,collapsible = TRUE 
            ,plotOutput("GrousehuntersbyRegion", height = "600px")
          )
        )
      )
      )
  )
)

# create the server functions for the dashboard  
server <- function(input, output) { 
  
#some data manipulation to derive the values of boxes (take last year in the dataset)
total.hunters<-dattable %>%
    filter(contents=="Active hunters" & `interval (year)`== max(dattable$`interval (year)`)) %>% 
    summarise(sum=sum(value))
Lynx.hunters<-dattable %>% 
  filter(contents=="Lynx hunting"& `interval (year)`== max(dattable$`interval (year)`))%>% 
  summarise(sum=sum(value))
Grouse.hunters<-dattable %>% 
  filter(contents=="Grouse hunting"& `interval (year)`== max(dattable$`interval (year)`))%>% 
  summarise(sum=sum(value))
Small.game<-dattable %>% 
  filter(contents=="Small game hunting"& `interval (year)`== max(dattable$`interval (year)`))%>% 
  summarise(sum=sum(value))
Moose.hunting<-dattable %>% 
  filter(contents=="Hunting on moose"& `interval (year)`== max(dattable$`interval (year)`))%>% 
  summarise(sum=sum(value))
Roedeer.hunting<-dattable %>% 
  filter(contents=="Roe deer hunting"& `interval (year)`== max(dattable$`interval (year)`))%>% 
  summarise(sum=sum(value))
Active.hunters<-dattable %>%
  filter(contents=="Active hunters")


#creating the valueBoxOutput content
output$value1 <- renderValueBox({
    valueBox(
      formatC(total.hunters$sum, format="d", big.mark=',')
      ,paste('Total hunters:',max(dattable$`interval (year)`))
      ,icon = icon("male",lib='font-awesome')
      ,color = "purple")})
output$value2 <- renderValueBox({
     valueBox(
      formatC(Lynx.hunters$sum, format="d", big.mark=',')
      ,paste('Total Lynx hunters:',max(dattable$`interval (year)`))
             ,icon = icon("paw",lib='font-awesome')
             ,color = "green")})
output$value3 <- renderValueBox({
     valueBox(
      formatC(Grouse.hunters$sum, format="d", big.mark=',')
      ,paste('Total Grouse hunters:',max(dattable$`interval (year)`))
             ,icon = icon("kiwi-bird",lib='font-awesome')
             ,color = "yellow") })
output$value4 <- renderValueBox({
  valueBox(
    formatC(Small.game$sum, format="d", big.mark=',')
    ,paste('Small game hunters:',max(dattable$`interval (year)`))
    ,icon = icon("bar-chart-o")
    ,color = "purple")})
output$value5 <- renderValueBox({
  valueBox(
    formatC(Moose.hunting$sum, format="d", big.mark=',')
    ,paste('Total Moose hunters:',max(dattable$`interval (year)`))
    ,icon = icon("bar-chart-o")
    ,color = "green")})
output$value6 <- renderValueBox({
  valueBox(
    formatC(Roedeer.hunting$sum, format="d", big.mark=',')
    ,paste('Total Roe deer hunters:',max(dattable$`interval (year)`))
    ,icon = icon("bar-chart-o")
    ,color = "yellow") })
  #creating the plotOutput content
  
  output$hunterbyyr <- renderPlot({
    ggplot(data = Active.hunters, 
           aes(x=`interval (year)`, y=value)) + 
             geom_bar(position = "dodge", stat = "identity", fill="#2DCCD3") +
             ylab("Number of hunters") + 
             xlab("Season") +
             theme_classic()+
             theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  
  output$huntersbyRegion <- renderPlot({
   ggplot(data=Active.hunters,
          aes(x=`interval (year)`, y=value)) + 
      geom_bar(position = "dodge", stat = "identity", fill="#2DCCD3") +
      ylab("Number of hunters") + 
      xlab("Season") +
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      facet_wrap(~region)
  })
  output$Lynxhunterbyyr <- renderPlot({
    dattable %>% 
      filter(contents=="Lynx hunting") %>% 
      ggplot(.,aes(x=`interval (year)`, y=value)) + 
      geom_bar(position = "dodge", stat = "identity", fill="#2DCCD3") +
      ylab("Number of hunters") + 
      xlab("Season") +
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  
  output$LynxhuntersbyRegion <- renderPlot({
    dattable %>% 
      filter(contents=="Lynx hunting") %>% 
      ggplot(.,aes(x=`interval (year)`, y=value)) + 
      geom_bar(position = "dodge", stat = "identity", fill="#2DCCD3") +
      ylab("Number of hunters") + 
      xlab("Season") +
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      facet_wrap(~region)
  })
  output$Grousehunterbyyr <- renderPlot({
    dattable %>% 
      filter(contents=="Grouse hunting") %>% 
      ggplot(.,aes(x=`interval (year)`, y=value)) + 
      geom_bar(position = "dodge", stat = "identity", fill="#2DCCD3") +
      ylab("Number of hunters") + 
      xlab("Season") +
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  
  output$GrousehuntersbyRegion <- renderPlot({
    dattable %>% 
      filter(contents=="Grouse hunting") %>% 
      ggplot(.,aes(x=`interval (year)`, y=value)) + 
      geom_bar(position = "dodge", stat = "identity", fill="#2DCCD3") +
      ylab("Number of hunters") + 
      xlab("Season") +
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      facet_wrap(~region)
  })
  
  
  
}

#Launch the App
shinyApp(ui, server)
