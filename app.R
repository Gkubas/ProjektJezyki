## app.R ##
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(jsonlite)
library(wordcloud)
library(tibble)

ui <- dashboardPage(
    dashboardHeader(title = "Jezyki Projekt"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Wykresy", tabName = "openFile", icon = icon("folder-open")),
            menuItem("Porownanie", tabName = "compare", icon = icon("not-equal"))
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "openFile",
                    sidebarPanel(
                      
                      # Input: Select a file ----
                      fileInput("jsonFile", "Wybierz plik JSON",
                                multiple = FALSE,
                                accept = c("text/json",
                                           "text/comma-separated-values,text/plain",
                                           ".json")),
                      sliderInput(inputId = "minFreq",
                                  label = "Minimlna czestotliwość:",
                                  min = 0.1,
                                  max = 10,
                                  value = 0.1,
                                  step = 0.1),
                      selectInput("ngram", "N-gram:",
                                  c("Litery" = "letters",
                                    "Bigramy" = "bigrams",
                                    "Trigramy" = "trigrams"), selected = 'letters'),
                      #checkboxInput("table", "Wyswietl Zawartosc pliku", value = FALSE, width = NULL),
                      plotOutput(outputId = "wordCloud")
                    ),
                    
                    # Main panel for displaying outputs ----
                    mainPanel(
                      tabsetPanel(
                        tabPanel("Plot",plotOutput(outputId = "plotTest")),
                        tabPanel("Plot 2", plotOutput(outputId = "plotv2")),
                        tabPanel("Tabela", DT::dataTableOutput("table"))
                      ),
                      
                      
                      # Output: Data file ----
                      textOutput("contents")
                      #plotOutput(outputId = "plotTest"),
                      #DT::dataTableOutput("table")
                      
                    )
            ),
            tabItem(tabName = "compare",
                    sidebarPanel(
                      fileInput("jsonFile2", "Wybierz plik JSON do porownania z plikiem z wykresow",
                                multiple = FALSE,
                                accept = c("text/json",
                                           "text/comma-separated-values,text/plain",
                                           ".json")),
                      sliderInput(inputId = "minFreq2",
                                  label = "Minimlna czestotliwość:",
                                  min = 0.1,
                                  max = 10,
                                  value = 0.1,
                                  step = 0.1),
                      selectInput("ngram2", "N-gram:",
                                  c("Litery" = "letters",
                                    "Bigramy" = "bigrams",
                                    "Trigramy" = "trigrams"), selected = 'letters')
                    ),
                    mainPanel(
                      plotOutput(outputId = "plotTestN"),
                      
                      # Output: Data file ----
                      textOutput("compare")
                      
                    )


                    )
        )
    )
)

server <- function(input, output) {
  
  output$contents <- renderText({
    #czytanie jsona
    req(input$jsonFile)
    tryCatch(
      {
        loadedDf <- fromJSON(input$jsonFile$datapath, flatten=TRUE)
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    #Wybranie odpowiedniej opcji
    if (input$ngram == 'letters')
      loadedList <- loadedDf$letters
    if (input$ngram == 'bigrams')
        loadedList <- loadedDf$digrams
    if (input$ngram == 'trigrams')
      loadedList <- loadedDf$trigrams
    #transformacja listy do odpowiednich dataframe'ow
    mydf <- as.data.frame.list(loadedList)
    tmp <- as.data.frame(t(mydf[,-1]))
    colnames(tmp) <- mydf$A
    tmp
    colnames(tmp) <- c('value')
    loadedDf1 <- rownames_to_column(tmp, 'id')
    loadedDf1$value <- (round(loadedDf1$value*100,2))
    #wyświetlenie tabeli z zawartoscia pliku
    #if (input$table == TRUE)
    output$table <- DT::renderDataTable(DT::datatable(df0, options = list(order = list(list(2,'desc')))))

    #okrojenie zbioru o minimalną częstotliwość z slidera
    df0 <<- subset(loadedDf1, loadedDf1$value>input$minFreq)
    #generownie plotly
    output$plotv2 <- renderPlot(ggplot(df0, mapping = aes(x = df0$id, y = df0$value, group = 1)) +
                                  geom_line()+
                                  labs(x="Litery", y="Procenty", tittle="Wizualizacja Danych"))
    #generowanie wordclouda
    output$wordCloud <- renderPlot(wordcloud(words = df0$id, freq = df0$value, max.words = 100, colors = c("grey80", "cadetblue1", "#3c8dbc")))
    #generowanie histogramu
    output$plotTest <- renderPlot(ggplot(df0, aes(df0$id, df0$value))  +
              geom_bar(stat="identity", fill="#3c8dbc", colour="darkgreen") + labs(x="Litery", y="Procenty", tittle="Wizualizacja Danych")+
               theme(axis.text.x=element_text(angle=45, hjust=1)))
    return(" ")
    
  })
  output$compare <- renderText({
    req(input$jsonFile2)
    tryCatch(
      {
        loadedDf2 <- fromJSON(input$jsonFile2$datapath, flatten=TRUE)
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    
    #Wybranie odpowiedniej opcji
    if (input$ngram2 == 'letters')
      loadedList2 <- loadedDf2$letters
    if (input$ngram2 == 'bigrams')
      loadedList2 <- loadedDf2$digrams
    if (input$ngram2 == 'trigrams')
      loadedList2 <- loadedDf2$trigrams
    
    #transformacja listy do odpowiednich dataframe'ow
    mydf <- as.data.frame.list(loadedList2)
    tmp <- as.data.frame(t(mydf[,-1]))
    colnames(tmp) <- mydf$A
    tmp
    colnames(tmp) <- c('value')
    loadedDf2 <- rownames_to_column(tmp, 'id')
    loadedDf2$value <- (round(loadedDf2$value*100,2))
    
    #okrojenie zbioru o minimalną częstotliwość z slidera
    df2 <- subset(loadedDf2, loadedDf2$value>input$minFreq2)
    
    dfn<-rbind(df0, df2)
    # output$plotTestN<- renderPlot(ggplot(dfn, aes(dfn$id, dfn$value))  +
    #                                 geom_bar(stat="identity", position = "dodge", fill="#3c8dbc", colour="red") +
    #                                 labs(x="Litery", y="Procenty", tittle="Wizualizacja Danych"))
    output$plotTestN<- renderPlot(ggplot(dfn, aes(dfn$id, dfn$value))  +
                                    geom_bar(stat="identity", position = "dodge", fill="#3c8dbc", colour="white") +
                                    coord_flip() +
                                    theme_minimal() +
                                    labs(x="Litery", y="Procenty", tittle="Wizualizacja Danych"))
    
    return(" ")
    
  })
  
}
shinyApp(ui, server)
