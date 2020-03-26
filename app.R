
library(RCurl)
library(tidyverse)
library(glue)
library(DT)
library(plyr)
library(dplyr)
library(plotly)
library(janitor)
library(RColorBrewer)

# devtools::install_github("exploratory-io/exploratory_func")
# p_load(janitor, tidyr, dplyr)

library(readxl)
covid_confirmed_usafacts <- read_excel("covid_confirmed_usafacts.xlsx", col_types = c("numeric", "text", "text", "text", 
                                                                                      rep("numeric", 59)))
covid_confirmed_usafacts %>% 
  gather(date, confirmed, `43852`:`43910`, convert = TRUE)
covid_confirmed_usafacts  <- tibble::rowid_to_column(covid_confirmed_usafacts, "ID")

library(readxl)
covid_confirmed_usafacts <- read_excel("covid_confirmed_usafacts.xlsx", col_types = c("numeric", "text", "text", "text", 
                                                                                      rep("numeric", 59)))
covid_confirmed_usafacts <- covid_confirmed_usafacts %>% 
  gather(date, confirmed, `43852`:`43910`, convert = TRUE) %>% mutate(date = excel_numeric_to_date(date))
covid_confirmed_usafacts  <- tibble::rowid_to_column(covid_confirmed_usafacts, "ID")


covid_deaths_usafacts <- read_excel("covid_deaths_usafacts.xlsx", col_types = c("numeric", "text", "text", "text", 
                                                                                rep("numeric", 59)))
covid_deaths_usafacts <- covid_deaths_usafacts %>% 
  gather(date, deaths, `43852`:`43910`, convert = TRUE) %>% mutate(date = excel_numeric_to_date(date))

covid_deaths_usafacts  <- tibble::rowid_to_column(covid_deaths_usafacts, "ID")

simplr_covid_deaths <- covid_deaths_usafacts %>% select(date, State, countyFIPS, deaths)

simplst_covid <- left_join(covid_confirmed_usafacts, simplr_covid_deaths, by = c("countyFIPS", "State", "date"))


simplst_by_state <- simplst_covid %>% 
  select(-c(ID, countyFIPS, `County Name`, stateFIPS)) %>%
  group_by(State, date) %>%
  summarize_each(list(~sum(., na.rm = T))) %>%
  ungroup()


ui <- fluidPage(
  tags$style("* { font-family: Arial; }"),
  
  titlePanel("COVID-19 Outbreak Plot from USA facts Data"),
  
  sidebarLayout(
    sidebarPanel(
      p("This plot shows the cumulative number of confirmed COVID-19 cases by selected states."),
      p("Source data: ", a("USA Facts Covid-19 page", target="_blank", href="https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/")),
      p("Modified from: ", a("Max Salvatore's source code (Github)", target="_blank", href="https://github.com/maxsal/covid_19")),
      p("Source code for this app: ", a("Github", target="_blank", href="https://github.com/DattaHub/covid_usafacts")),
      
      dateRangeInput("dates",  h4("Date Range"), start = min(simplst_by_state$date), end = max(simplst_by_state$date)),
      checkboxGroupInput(inputId = "line",
                         label   = h4("Which states would you like to plot?"),
                         choices = unique(simplst_by_state$State))
    ),
  
 
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Confirmed", plotOutput("plot"),
                           downloadButton(outputId = "downloadPlot", label = "Download Plot")),
                  tabPanel("Deaths", plotOutput("plot_2"),
                           downloadButton(outputId = "downloadPlot_2", label = "Download Plot")),
                  tabPanel("Table", DT::dataTableOutput("table"),
                           downloadButton("downloadTable", "Download Table")))
      # 
      # plotOutput("plot_1")
    )
  )
)

server <- function(input, output) {
  
  plot_input <- reactive({
    validate(need(!is.null(input$line), "Please tick a box to show a plot."))
  
  data <- simplst_by_state %>% 
      filter(State %in% input$line) %>%
      filter(date >= input$dates[1] & date <= input$dates[2])
    
  ggplot(data = data, aes(x = date, y = confirmed, group = State)) +
    geom_line(aes(color = State), size = 1) +
    labs(
      title   = "Confirmed Covid-19 cases by State",
      x       = "Date",
      y       = "Count",
      color   = "State",
      caption = "USA Facts: https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/"
    ) +
    scale_color_brewer(palette="Dark2") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          axis.text=element_text(size=10),
          axis.title=element_text(size=12))
})
  
  plot_2_input <- reactive({
    validate(need(!is.null(input$line), "Please tick a box to show a plot."))
    
    data <- simplst_by_state %>% 
      filter(State %in% input$line) %>%
      filter(date >= input$dates[1] & date <= input$dates[2])
    
    ggplot(data = data, aes(x = date, y = deaths, group = State)) +
      geom_line(aes(color = State), size = 1) +
      labs(
        title   = "Covid-19 deaths by State",
        x       = "Date",
        y       = "Count",
        color   = "Country",
        caption = "USA Facts: https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/"
      ) +
      scale_color_brewer(palette="Dark2") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            axis.text=element_text(size=10),
            axis.title=element_text(size=12))
  })
  
  output$plot <- renderPlot({ plot_input() })
  # output$plot_1 <- renderPlot({ plot_1_input() })
  output$plot_2 <- renderPlot({ plot_2_input() })
  
  # output$rate_plot <- renderPlot({ rate_plot_input() })
  # output$rate_trend_plot <- renderPlot({ rate_trend_plot_input() })

  
  d <- reactive({
    simplst_by_state %>% 
      filter(State %in% input$line) %>%
      filter(date >= input$dates[1] & date <= input$dates[2]) #%>%
      #select(-pop)
  })
  
  output$table <- DT::renderDataTable({
    d()
  })
  
  output$downloadTable <- downloadHandler(
    filename = function() {
      glue("covid19_data_{input$dates[1]}_to_{input$dates[2]}.txt")
    },
    content = function(file) {
      write_tsv(d(), file)
    }
  )
  
  output$downloadPlot <- downloadHandler(
    filename = glue("covid19_caseplot_{input$dates[1]}_to_{input$dates[2]}.pdf"),
    content  = function(file) {
      pdf(file, width = 7, height = 5)
      print(plot_input())
      dev.off()
    }
  )
  
  output$downloadPlot_2 <- downloadHandler(
    filename = glue("covid19_deathplot_{input$dates[1]}_to_{input$dates[2]}.pdf"),
    content  = function(file) {
      pdf(file, width = 7, height = 5)
      print(plot_2_input())
      dev.off()
    }
  )
  
}

shinyApp(ui, server)