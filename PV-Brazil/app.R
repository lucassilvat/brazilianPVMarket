#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(leaflet)
library(shiny)
library(sf)
library(plotly)
library(dplyr)
library(assertthat)

aneel <- read.csv("ANEEL2.csv",stringsAsFactors=F,fileEncoding="UTF-8")
aneel$municipio <- tolower(aneel$municipio)
aneel$data <- as.Date(aneel$data,"%Y-%m-%d")

states <- st_read("states.shp")
mun <- st_read("mun.shp")

mun$name_mn <- tolower(mun$name_mn)

max_city <- aneel %>% 
        group_by(municipio) %>% 
        summarize(pot = sum(potencia), qtde = n()) %>%
        select(pot,qtde)%>%
        sapply(max)

max_state <- aneel %>% 
        group_by(uf) %>% 
        summarize(pot = sum(potencia), qtde = n()) %>% 
        select(pot,qtde)%>%
        sapply(max)

#browser()
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Distributed photovoltaic summary for brazilian market"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("ufs",
                        "Choose UF",
                        choices = c("Whole country",unique(aneel$uf)),
                        multiple=F),
            uiOutput("timeSlider"),
            uiOutput("animate"),
            radioButtons("potqt", label = h3("Summarize by:"),
                         choices = list("Power" = "pot", "Count" = "qtde"), 
                         selected = "pot"),
            helpText("The main goal of this APP is to provide a map and a bar plot summarizing
            the brazilian photovoltaic market.",br(),"You can choose between brazilian states (UFs) and
            also select a time period."),
            helpText("You can summarize by total power or total amount of pv systems"),
            helpText("There is also a possibility to show animation for the barplot"),
            helpText("Please send questions and commentaries to lucassilvat@gmail.com")
        ),

        # Show a plot of the generated distribution
        mainPanel(
                tabsetPanel(
                        id="tabs",
                        tabPanel(
                                "Brazilian map",
                                value = "1",
                                leafletOutput("plot",height = "600px")
                        ),
                        tabPanel(
                                "Plots",
                                value = "2",
                                plotlyOutput("barplot",height = "900px")
                        )
                        )
                        
                )
    )
)
           
        


# Define server logic required to draw a histogram
server <- function(input, output) {

        
        aneel.out <- reactive({
                
                if(is.null(input$animatebox)){
                        aneel %>% filter(data >= input$time[1] & data <= input$time[2])
                } else if(!input$animatebox){
                        aneel %>% filter(data >= input$time[1] & data <= input$time[2])
                } else {
                        aneel %>% filter(data <= input$time[1])
                }
                
        })
        
        output$animate <- renderUI({
                
                if (input$tabs == "2") checkboxInput("animatebox","Do you wanna animate?", value = F) else return()
        })
        
        output$timeSlider <- renderUI({
                
                if(!is.null(input$animatebox)){
                        if(input$animatebox){
                                sliderInput("time",
                                            "Choose period",
                                            min = min(aneel$data),
                                            max = max(aneel$data),
                                            value = as.Date("01/01/2012","%d/%m/%Y"),
                                            step = 10,
                                            timeFormat = "%d/%m/%Y",
                                            animate=animationOptions(interval=500)
                                )
                        }else{
                                sliderInput("time",
                                            "Choose period",
                                            min = min(aneel$data),
                                            max = max(aneel$data),
                                            value = c(min(aneel$data),max(aneel$data)),
                                            step = 1,
                                            timeFormat = "%d/%m/%Y")
                        }
                }else{
                        sliderInput("time",
                                    "Choose period",
                                    min = min(aneel$data),
                                    max = max(aneel$data),
                                    value = c(min(aneel$data),max(aneel$data)),
                                    step = 1,
                                    timeFormat = "%d/%m/%Y")
                }
                
        })
        
        summary <- reactive({
                
                aneel.out <- if(is.error(t <- try(aneel.out()))) aneel else t
                if (input$ufs == "Whole country") {
                        aneel.out <- group_by(aneel.out,uf)
                        db <- states
                        merge.by <- c("abbrv_s","uf")
                        ind <- rep(T,length(aneel.out$uf))
                } else {
                        aneel.out <- group_by(aneel.out,municipio)
                        db <- mun[mun$abbrv_s == input$ufs,]
                        merge.by <- c("name_mn","municipio")
                        ind <- aneel.out$uf == input$ufs
                }
                
                aneel.out %>%
                        .[ind,]%>%
                        summarize(potencia = sum(potencia),qtde = n()) %>%
                        ungroup%>%
                        merge(db,.,by.x=merge.by[1],by.y=merge.by[2])
                
        })
        
        popup <- reactive({
                
                switch(input$potqt,    
                       "pot"=  {if (input$ufs == "Whole country"){
                               paste0("<strong>UF: </strong>",
                                      summary()$abbrv_s,
                                      "<br><strong>Power: </strong>",
                                      paste(format(round(summary()$potencia/1000,digits=0),big.mark=","),"MWp"))
                       }else{
                               paste0("<strong>City: </strong>",
                                      summary()$name_mn,
                                      "<br><strong>Power: </strong>",
                                      paste(format(round(summary()$potencia,digits=2),big.mark=","),"kWp"))
                               
                       }},
                       "qtde" = {if (input$ufs == "Whole country"){
                               paste0("<strong>UF: </strong>",
                                      summary()$abbrv_s,
                                      "<br><strong>Count: </strong>",
                                      format(round(summary()$qtde,digits=0),big.mark=","))
                       }else{
                               paste0("<strong>City: </strong>",
                                      summary()$name_mn,
                                      "<br><strong>Count: </strong>",
                                      format(round(summary()$qtde,digits=2),big.mark=","))
                               
                       }})
        })
        
        state_contour <- reactive({
                if(input$ufs == "Whole country") states else states[states$abbrv_s == input$ufs,]
        })
        
        
        bins <- reactive({
                switch(input$potqt,
                       "pot"= {if (input$ufs == "Whole country"){
                               bins <- c(0, 1000, 5000, 20000, 50000, 10000, 250000, 500000, Inf)
                       } else {
                               bins <- c(0, 100, 500, 1000, 2500, 5000, 10000,15000,20000, Inf)
                       }},
                       "qtde"={if (input$ufs == "Whole country"){
                               bins <- c(0, 100, 500, 2000, 5000, 1000, 25000, 50000, Inf)
                       } else {
                               bins <- c(0, 10, 50, 100, 250, 500, 1000,1500,2000, Inf)
                       }})
                
                
        })
        
        
        output$plot <- renderLeaflet({
                
                #browser()
                summary <- summary()
                popup <- popup()
                
                pal <- colorBin("YlOrRd",
                                domain=summary[[switch(input$potqt,"pot"="potencia","qtde"="qtde")]],
                                bins = bins())
                map <- summary %>% 
                        leaflet() %>%
                        addProviderTiles("CartoDB.Positron")%>%
                        addPolygons(fillColor = ~pal(summary[[switch(input$potqt,"pot"="potencia","qtde"="qtde")]]),
                                    fillOpacity = 0.8,
                                    color = "#BDBDC3",
                                    weight = 1,
                                    popup = popup,
                                    highlightOptions = highlightOptions(color = "black", weight = 2,
                                                                        bringToFront = TRUE)
                        )%>%
                        addPolylines(data = state_contour(),
                                     color="black", 
                                     opacity = 1,
                                     smoothFactor=1,
                                     weight = 1)%>%
                        addLegend("bottomright",pal=pal,opacity=1,values=~summary[[switch(input$potqt,"pot"="potencia","qtde"="qtde")]],
                                  title=switch(input$potqt,"pot"="Power (kWp)","qtde"="Count"))
                
        })
        
        output$barplot <- renderPlotly({
                
                #browser()
                
                summary <- summary()
                #browser()
                summary <- summary %>%
                        data.frame %>%
                        select(-geometry)%>%
                        .[order(.[[switch(input$potqt,"pot"="potencia","qtde"="qtde")]],decreasing = T),]
                
                #browser()
                
                
                if(input$ufs == "Whole country"){
                        
                        summary$abbrv_s <- reorder(summary$abbrv_s,summary[[switch(input$potqt,"pot"="potencia","qtde"="qtde")]])
                        
                        plot_ly(summary,x=~summary[[switch(input$potqt,"pot"="potencia","qtde"="qtde")]],y=~abbrv_s,type="bar",
                                text=~summary[[switch(input$potqt,"pot"="potencia","qtde"="qtde")]],textposition="auto")%>%
                                layout(
                                        title = switch(input$potqt,"pot"="Total PV Power by UF","qtde"="Total amount of PV systems by UF"),
                                        xaxis = list(title = switch(input$potqt,"pot"="Power (kWp)","qtde"="Count")
                                                     #,range = c(0,max_state[[1]])
                                        ),
                                        yaxis = list(title = "UF"))
                } else {
                        summary$name_mn <- reorder(summary$name_mn,summary[[switch(input$potqt,"pot"="potencia","qtde"="qtde")]])
                        summary <- summary[1:30,]
                        
                        plot_ly(summary,x=~summary[[switch(input$potqt,"pot"="potencia","qtde"="qtde")]],y=~name_mn,type="bar",
                                text=~summary[[switch(input$potqt,"pot"="potencia","qtde"="qtde")]],textposition="auto")%>%
                                layout(
                                        title = switch(input$potqt,"pot"="Total PV Power by City","qtde"="Total amount of PV systems by City"),
                                        xaxis = list(title = switch(input$potqt,"pot"="Power (kWp)","qtde"="Count")
                                                     #,range = c(0,max_city[[1]])
                                        ),
                                        yaxis = list(title = "City"))
                }
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
