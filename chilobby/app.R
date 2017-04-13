# Data 4 Democracy Chicago Lobbying Project #
# S. Kirmer #
# April 2017 #

# Set up location
# setwd("U://Github/shinyapp/firstapp/")
# Put in the token data from shinyapps.io profile
# setAccountInfo()

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(shinythemes)
library(rsconnect)
library(ggvis)
library(plotly)

df <- read.csv("https://query.data.world/s/ebgm9pltjmv22p1w9lmkdx46e",header=T, stringsAsFactors=FALSE)
df <- dplyr::filter(df, !is.na(recipient_surname))
df2 <- unique(df[,c("Year","CLIENT_NAME", "CONTRIBUTION_DATE", "recipient_surname","AMOUNT", "CLIENT.INDUSTRY")])

server <- function(input, output) {
  #the server - literally what data is going into the plot/viz?
  
  output$table <- DT::renderDataTable(DT::datatable(rownames = FALSE,
                                                    {
                                                      data <- df2
                                                      
                                                      output$barplot <- renderPlotly({
                                                        data2 <- summarize(group_by(data, recipient_surname, Year),
                                                                           total = sum(AMOUNT, na.rm=T))
                                                        
                                                          # Render a barplot
                                                        plot1 <- ggplot(data2, aes(x=recipient_surname, y=total, fill=factor(Year)))+
                                                          theme_bw()+
                                                          theme(axis.text.x = element_text(angle = 45, vjust=.5),
                                                                legend.position = "none")+
                                                          geom_bar(stat="identity")+
                                                          labs(title = "Donations to Aldermen", x="Alderman", y="Amount of Donation") 
                                                        
                                                       
                                                        #plot1
                                                        gp <- ggplotly(plot1)
                                                        #gp
                                                        gp %>% layout(margin = list(l=90, r=60, t=60, b=90))
                                                        
                                                      })

                                                      if(input$Industry != "All"){
                                                        data <- dplyr::filter(data, CLIENT.INDUSTRY == input$Industry)
                                                      }
                                                      
                                                      if(input$Alderman != "All") {
                                                        data <- dplyr::filter(data, recipient_surname == input$Alderman) 
                                                      }
                                                      
                                                      if(input$Year != "All") {
                                                        data <- dplyr::filter(data, Year == input$Year) 
                                                      }
                                                      
                                                      if(input$Funder != "All") {
                                                        data <- dplyr::filter(data, CLIENT_NAME == input$Funder) 
                                                      }
                                                      
                                                      data}))
}




ui <- fluidPage(
  theme = shinytheme('lumen'),
  titlePanel("Connections Between Aldermen and Funders Through Lobbying"),
  
  "This page is part of the Data4Democracy Chicago Lobbying project. This data comes from the City of Chicago Data Portal. Begin typing in the Funder box to browse the funders- you can select more than one.",
  br(),br(),
  
  "Join us at ", a("data.world/lilianhj/chicago-lobbyists!", href="https://data.world/lilianhj/chicago-lobbyists"),
  br(),br(),
  
  fluidRow(
    column(3,
           selectInput("Industry",
                       "Industry:",
                       c("All",  sort(trimws(unique(as.character(df$CLIENT.INDUSTRY))))))),
    column(3,
           selectInput("Alderman",
                       "Alderman:",
                       c("All", sort(trimws(unique(as.character(df$recipient_surname))))))),
    column(2,
           selectInput("Year",
                       "Year:",
                       c("All", sort(trimws(unique(as.character(df$Year))))))),
    
    column(3,
           selectizeInput("Funder",
                       "Funder:",
                       c(Choose='', "All", sort(trimws(unique(as.character(df$CLIENT_NAME))))), 
           multiple=TRUE, selected = "All"))
  )
  ,
  fluidRow(
    plotlyOutput("barplot", height = 500)  
  )
  ,
  fluidRow(
    DT::dataTableOutput("table")
  )

)

shinyApp(ui = ui, server = server)