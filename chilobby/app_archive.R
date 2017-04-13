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
colnames(df2) <- c("Year", "Funder", "Contribution Date", "Alderman", "Amount", "Funder Industry")

server <- function(input, output) {
  #the server - literally what data is going into the plot/viz?
  
  output$table <- DT::renderDataTable(DT::datatable(rownames = FALSE,
                                                    {
                                                      data <- df2
                                                      
                                                      output$barplot <- renderPlotly({
                                                        data2 <- summarize(group_by(data, Alderman, Year),
                                                                           aggregate = sum(Amount, na.rm=T),
                                                                           annual_amt = sum(Amount, na.rm=T))
                                                        
                                                          # Render a barplot
                                                        plot1 <- ggplot(data2, aes(x=Alderman, y=aggregate, fill=factor(Year), label=annual_amt))+
                                                          theme_bw()+
                                                          theme(axis.text.x = element_text(angle = 45, vjust=.5),
                                                                legend.position = "none")+
                                                          geom_bar(stat="identity")+
                                                          labs(title = "Donations to Aldermen", x="Alderman", y="Amount of Donations") 
                                                        
                                                       
                                                        #plot1
                                                        gp <- ggplotly(plot1, tooltip = c("Alderman","factor(Year)", "annual_amt"))
                                                        #gp
                                                        gp %>% layout(margin = list(l=90, r=60, t=60, b=90))
                                                        
                                                      })

                                                      if(input$Industry != "All"){
                                                        data <- dplyr::filter(data, `Funder Industry` == input$Industry)
                                                      }
                                                      
                                                      if(input$Alderman != "All") {
                                                        data <- dplyr::filter(data, Alderman == input$Alderman) 
                                                      }
                                                      
                                                      if(input$Year != "All") {
                                                        data <- dplyr::filter(data, Year == input$Year) 
                                                      }
                                                      
                                                      if(input$Funder != "All") {
                                                        data <- dplyr::filter(data, Funder == input$Funder) 
                                                      }
                                                      
                                                      data}))
}




ui <- fluidPage(
  theme = shinytheme('lumen'),
  titlePanel("Connections Between Aldermen and Funders Through Lobbying"),
  
  "This page is part of the Data4Democracy Chicago Lobbying project, and is a collaborative work in progress. (Not all funders have industry classifications yet.) All data comes from the City of Chicago Data Portal. Begin typing in the Funder box to browse the funders- you can select more than one.",
  br(),br(),
  
  "If you'd like to learn more, join us at ", a("data.world/lilianhj/chicago-lobbyists!", 
                   href="https://data.world/lilianhj/chicago-lobbyists"),
  "If you like this app, you might also enjoy the sister apps ", a("Aldermanic Voting Records",
                                                                  href="https://skirmer.shinyapps.io/chivotes/"),
  "and ", a("Lobbyist Connections Network.",
            href="https://nathanielwroblewski.github.io/data-for-democracy-2017/"),
  br(),br(),
  "Code for this app is available on ", a("GitHub.", 
                                          href= "https://github.com/skirmer/shinyapp/tree/master/chilobby"), br(),br(),
  
  fluidRow(

    column(3,
           selectizeInput("Funder",
                          "Funder:",
                          c(Choose='', "All", sort(trimws(unique(as.character(df2$Funder))))), 
                          multiple=TRUE, selected = "All")),
    column(3,
           selectInput("Industry",
                       "Funder Industry:",
                       c("All",  sort(trimws(unique(as.character(df2$`Funder Industry`))))))),
    column(3,
           selectInput("Alderman",
                       "Alderman:",
                       c("All", sort(trimws(unique(as.character(df2$Alderman))))))),
    column(2,
           selectInput("Year",
                       "Year:",
                       c("All", sort(trimws(unique(as.character(df2$Year)))))))
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