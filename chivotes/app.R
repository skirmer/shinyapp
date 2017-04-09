# Data 4 Democracy Chicago Lobbying Project #
# App: Votes by Aldermen #
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
library(lubridate)

df <- read.csv("https://query.data.world/s/bqziq63nd9vdt5e7ph3htop6c",header=T, stringsAsFactors=FALSE)

# 
# for(i in 1:nrow(df)){
#   for(n in 1:length(oldald)){
#     ifelse(grepl(oldald[n], df[i, "Alderman"]),
#            df[i, "alderman2"] <- as.character(oldald[n]), NA)
#   }
# }

#Bit of cleanup#
df$Vote <- ifelse(df$Vote == "AO’Connor", "A", df$Vote)
df$Vote <- ifelse(df$Vote %in% c("ErvinY","PawarY","SmithY"), "Y", df$Vote)
df <-dplyr::filter(df, Vote %in% c("Y", "N", "A", "E", "NV", "V"))

df$Date <- as.Date(df$Date, format = '%Y-%m-%d')
df$Year <- year(df$Date)



df2 <- unique(df[,c("Alderman","Ward", "Vote", "Date","Year","Record", "Title")])
df2$Ward <- as.numeric(df2$Ward)

server <- function(input, output) {
  #the server - literally what data is going into the plot/viz?
  
  output$table <- DT::renderDataTable(DT::datatable(rownames = FALSE,
                                                    {
                                                      data <- df2
                                                      
                                                      if(input$Alderman != "All"){
                                                        data <- dplyr::filter(data, Alderman == input$Alderman)
                                                      }
                                                      
                                                      if(input$Ward != "All") {
                                                        data <- dplyr::filter(data, Ward == input$Ward) 
                                                      }
                                                      
                                                      if(input$Record != "All") {
                                                        data <- dplyr::filter(data, Record == input$Record) 
                                                      }
                                                      
                                                      if(input$Vote != "All") {
                                                        data <- dplyr::filter(data, Vote == input$Vote) 
                                                      }
                                                      
                                                      if(input$Year != "All") {
                                                        data <- dplyr::filter(data, Year == input$Year) 
                                                      }
                                                      
                                                      data}))
}




ui <- fluidPage(
  theme = shinytheme('lumen'),
  titlePanel("Aldermanic Voting Records"),
  
  "This page is part of the Data4Democracy Chicago Lobbying project. This data comes from the City of Chicago Data Portal.",
  br(),br(),
  "Key to votes: Y=yes | N=no | A=absent | NV=not voting | V=Vacant | E= Excused",
  br(),br(),
  "Join us at ", a("data.world/lilianhj/chicago-lobbyists!", href="https://data.world/lilianhj/chicago-lobbyists"),
  br(),br(),
  
  fluidRow(
    column(3,
           selectInput("Alderman",
                       "Alderman:",
                       c("All",  sort(trimws(unique(as.character(df$Alderman))))))),
    column(2,
           selectInput("Ward",
                       "Ward:",
                       c("All", sort(trimws(unique(as.character(df$Ward))))))),
    column(2,
           selectInput("Record",
                       "Record:",
                       c("All", sort(trimws(unique(as.character(df$Record))))))),
    column(2,
           selectInput("Year",
                       "Year:",
                       c("All", sort(trimws(unique(as.character(df$Year))))))),
    
    column(2,
           selectInput("Vote",
                       "Vote:",
                       c("All", sort(trimws(unique(as.character(df$Vote)))))))
  )
  ,
  fluidRow(
    DT::dataTableOutput("table")
  )

)

shinyApp(ui = ui, server = server)