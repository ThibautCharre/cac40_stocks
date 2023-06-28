# Infos from API
library(jsonlite)
library(httr)

# Data treatment
library(data.table)
library(DT)
library(lubridate)
library(magrittr)

# Graph
library(plotly)
library(ggplot2)

# Stats
library(tseries)
library(forecast)

# Shiny
library(shiny)
library(shinydashboard)

source("Shiny/Functions/functions.R")

# List of all CAC40 companies
companies = c("LVMH", "L'Oréal", "Hermès", "TotalEnergies", "Sanofi", "Airbus", "Schneider Electric", "Air Liquide", 
              "EssilorLuxottica", "BNP Paribas", "Kering", "AXA", "Vince", "Safran", "Dassault Systèmes", 
              "Pernod Ricard", "Stellantis", "STMicroelectronics", "Danone", "ENGIE", "Crédit Agricole", "Capgemini", 
              "Thales", "Compagnie de Saint-Gobain", "Orange", "Legrand", "ArcelorMittal", "Veolia", "Michelin", "Société Générale", 
              "Publicis Groupe", "Carrefour", "Eurofins Scientific", "Bouygues", "Alstom", "Renault", "Worldline", 
              "Teleperformance", "Vivendi", "Unibail-Rodamco-Westfield", "CAC40")

tickers = c("MC.PA", "OR.PA", "RMS.PA", "TTE", "SNY", "AIR.PA", "SU.PA", "AI.PA", 
            "EL.PA", "BNP.PA", "KER.PA", "CS.PA", "SAF.PA", "DG.PA", "DSY.PA", 
            "RI.PA", "STLA", "STM", "BN.PA", "ENGI.PA", "ACA.PA", "CAP.PA", 
            "HO.PA", "SGO.PA", "ORAN", "LR.PA", "MT", "VIE.PA", "ML.PA", "GLE.PA", 
            "PUB.PA", "CA.PA", "ERF.PA", "EN.PA", "ALO.PA", "RNO.PA", "WLN.PA",
            "TEP.PA", "VIV.PA", "URW.AS", "^FCHI") %>% sort()

corresp_table <- data.table(Companies = companies, Tickers = tickers)

# We gather stocks values into a single list
listStocks <- lapply(X = tickers, FUN = function(x) {get_hist(symbol = x)})
names(listStocks) <- tickers



#### UI part
ui <- 
  dashboardPage(
    dashboardHeader(title = "STocks-STats"), 
    dashboardSidebar(
      sidebarMenu(
        menuItem(text = "Historical Datas", tabName =  "histo_datas", icon = icon("history")),
        menuItem(text = "Stocks Comparisons", tabName =  "compare", icon = icon("database")),
        menuItem(text = "ARIMA Predictions", tabName = "stock_pred", icon = icon("desktop"))
      )
    ), 
    dashboardBody(
      tabItems(
        tabItem(tabName = "histo_datas", 
                fluidRow(
                  column(selectInput(inputId = "stockSelected", label = "Stock", choices = tickers, selected = tickers[1]), width = 4, offset = 2), 
                  column(dateRangeInput(inputId = "dateRange", label = "Calendar", start = Sys.Date() - 2, end = Sys.Date() - 1, min = Sys.Date() - 2, max = Sys.Date() - 1), width = 4, offset = 2)
                ), 
                fluidRow(
                  column(plotlyOutput(outputId = "graphMean"), width = 8),
                  column(DT::dataTableOutput(outputId = "stockDT"), width = 4)
                ),
                fluidRow(
                  column(plotlyOutput(outputId = "histoStock"), width = 7),
                  column(DT::dataTableOutput(outputId = "riskDT"), width = 5)
                )
                
        )
      )
    )
  )


#### Server part
server <- function(input, output) {
  
  #### TAB 1
  
  # Definition of the ticker
  symbol <- reactive({input$stockSelected})
  
  # Update of dates range
  observe({
    updateDateRangeInput(inputId = "dateRange", start = min(listStocks[[symbol()]]$date), end = max(listStocks[[symbol()]]$date), min =  min(listStocks[[symbol()]]$date), max = max(listStocks[[symbol()]]$date))
  })
  
  # Start and End Date
  startDate <- reactive({
    input$dateRange[1]
  })
  
  # End Date
  endDate <- reactive({
    input$dateRange[2]
  })
  
  # Stock Graph + Mean Average
  output$graphMean <- renderPlotly({
    get_value_graph(list_stocks = listStocks, symbol = symbol(), start_date = startDate(), end_date = endDate())
  })
  
  # DT output
  output$stockDT <- DT::renderDataTable({
    listStocks[[symbol()]] %>% 
      as.data.table() %>%
      .[date >= startDate() & date <= endDate()]
  })
  
  # list reactive
  listReturns <- reactive({
    get_return_distr(list_stocks = listStocks, symbol = symbol(), start_date = startDate(), end_date = endDate())
  })
  
  # Histogram returns
  output$histoStock <- renderPlotly({
    listReturns()[["Graph"]]
  })
  
  # Risks Indic DT
  output$riskDT <- DT::renderDataTable({
    listReturns()[["statDT"]]
  })
  
  #### TAB 2
  
}

shinyApp(ui = ui, server = server)


