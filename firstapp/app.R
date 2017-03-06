# Data 4 Democracy Drug Spending Project #
# S. Kirmer #
# March 2017 #

# Set up location
# setwd("U://Github/shinyapp/firstapp/")
# Put in the token data from shinyapps.io profile
# setAccountInfo()

library(shiny)
library(ggplot2)
library(DT)
library(shinythemes)
library(rsconnect)
library(ggvis)
library(plotly)

df <- read.csv("https://query.data.world/s/4sxwyfyj6zg9y0wo6ix6n6zsm",header=T,stringsAsFactors=FALSE)
df$DRUG.NAME <- iconv(df$DRUG.NAME, to = "UTF-8", sub="")

df2 <- unique(df[,c(4,2,3,5)])

server <- function(input, output) {
  #the server - literally what data is going into the plot/viz?
  
  output$table <- DT::renderDataTable(DT::datatable(rownames = FALSE,
                                                    colnames = c("Manufacturer", "HCPCS Code", "NDC Code", "Drug Name"),
                                                      {
  data <- df2
  
  output$barplot <- renderPlotly({
    
    if(nrow(data) < 51){
      # Render a barplot
      plot1 <- ggplot(data, aes(x=LABELER.NAME, fill=DRUG.NAME))+
        theme_bw()+
        theme(axis.text.x = element_text(angle = 45, vjust=.5),
              legend.position = "none")+
        geom_bar()+
        labs(title = "Who Makes What?", x="Manufacturer", y="Number of Products") 
    }
    else{    # Render a barplot
      plot1 <- ggplot(data[c(1:50),], aes(x=LABELER.NAME, fill=DRUG.NAME))+
        theme_bw()+
        theme(axis.text.x = element_text(angle = 45, vjust=.5),
              legend.position = "none")+
        geom_bar()+
        labs(title = "Who Makes What?", x="Manufacturer", y="Number of Products")}
    
    #plot1
    gp <- ggplotly(plot1)
    #gp
    gp %>% layout(margin = list(l=60, r=60, t=60, b=200))
 
  })
  
  
  if(input$Manufacturer != "All"){
    data <- dplyr::filter(data, LABELER.NAME == input$Manufacturer)
  }
  
  if(input$Drug != "All") {
    data <- dplyr::filter(data, DRUG.NAME == input$Drug) 
  }
  
  if(input$HCPCS != "") {
    data <- dplyr::filter(data, HCPCS.CODE == input$HCPCS)
  }
  
  data
}))
}




ui <- fluidPage(
  theme = shinytheme('lumen'),
  titlePanel("Manufacturers and the Drugs They Make"),

  "This page is part of the Data4Democracy Medicare drug spending project. This specific file comes from the Centers for Medicare and Medicaid services. 
  Once you find a HCPCS code that interests you, filter and you will see a plot of all the manufacturers produce the same or similar products. For a sample, try 90746.",
  br(),br(),
  
  "Join us at ", a("data.world/data4democracy/drug-spending!", href="https://data.world/data4democracy/drug-spending"),
  br(),br(),
  
  fluidRow(
    column(4,
           selectInput("Manufacturer",
                       "Manufacturer:",
                       c("All",  sort(trimws(unique(as.character(df$LABELER.NAME))))))),
  column(4,
         selectInput("Drug",
                     "Drug:",
                     c("All", sort(trimws(unique(as.character(df$DRUG.NAME))))))),
  column(4,
         textInput("HCPCS",
                     "HCPCS Code:",
                     ""))
  )
  ,
  fluidRow(
    DT::dataTableOutput("table")
  )
  ,
  fluidRow(
    plotlyOutput("barplot", height = 600)  
  )
)

shinyApp(ui = ui, server = server)