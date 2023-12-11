library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)

source("final_data.R")

column_mapping <- c(
  "High Blood Pressure" = "Deaths.that.are.from.all.causes.attributed.to.high.systolic.blood.pressure..in.both.sexes.aged.all.ages", 
  "High Cholesterol" = "Deaths.that.are.from.all.causes.attributed.to.high.ldl.cholesterol..in.both.sexes.aged.all.ages", 
  "High BMI" = "Deaths.that.are.from.all.causes.attributed.to.high.body.mass.index..in.both.sexes.aged.all.ages", 
  "Vitamin Deficiency" = "Deaths.that.are.from.all.causes.attributed.to.vitamin.a.deficiency..in.both.sexes.aged.all.ages", 
  "Calcium Deficiency" = "Deaths.that.are.from.all.causes.attributed.to.low.bone.mineral.density..in.both.sexes.aged.all.ages", 
  "Iron Deficiency" = "Deaths.that.are.from.all.causes.attributed.to.iron.deficiency..in.both.sexes.aged.all.ages", 
  "Low Phyisical Activities" = "Deaths.that.are.from.all.causes.attributed.to.low.physical.activity..in.both.sexes.aged.all.ages"
)

home_page <- fluidPage(
  titlePanel("Introduction"),
  HTML("<p> The global proliferation of fast-food industries has been noted in tandem with a rise in obesity rates. 
      In the contemporary landscape, the costs of essential fresh foods like vegetables, dairy, fruits, and meat within 
      supermarkets have experienced a discernible upswing over the past 12 years. This surge in prices has prompted numerous 
      apprehensions regarding consumer choices, as the affordability of fast-food options juxtaposed with the relatively higher 
      costs of fresh produce has left a substantial portion of the global population opting for less expensive yet less healthful 
      alternatives, often favoring choices like McDonald’s Big Mac Burgers.</p>"),
  HTML("<p>The three main questions we aim to answer are : </p>"),
  HTML("<li>How does the death rate due to food disparities relate in correlation to the average inflation rates? </li>"),
  HTML("<li>How does the death rate of personal health problems relate in correlation to the average inflation rates?</li>"),
  HTML("<li>How do the inflation rates change over the course from 2007 to 2019?</li>"),
  
  br(),
  HTML("<p>We are using data sets from sources such as The World Bank and Our World in Data which are credible sources that 
 help us see how different inflation rates around the world increase in correlations to the death tolls from diet and health conditions.</p>"),
  
  HTML('<img src="https://www.oneaccounting.cpa/wp-content/uploads/inflation-Canada.jpg">')
)

inflation_trend_page <- fluidPage(
  titlePanel("Average Inflation Rate over the Years"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "avg_country",
        label = "Select Country:",
        choices = unique(df$Entity),
        selected = unique(df$Entity)[1]
      ),
    ),
    mainPanel(
      plotlyOutput(outputId = "avgline")
    ),
  ),
  titlePanel("Summary"),
  HTML("<p>Let's go look into how a real world problem such as the food inflation rates impact our health. 35% of countries have a 13% increase in food inflation rates since 2007. 
    How would sustainable eating occur when prices of healthy foods like vegetables, meat and fruits are too expensive for majority of the population to buy?</p>")
)

death_page <- fluidPage(
  titlePanel("Personal Health Deaths vs Food Inflation Prices"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "death_country",
        label = "Select Country:",
        choices = unique(df$Entity),
        selected = unique(df$Entity)[1]
      ),
      radioButtons(
        inputId = "radio",
        label = "Deaths From: ",
        choices = names(column_mapping),
        selected = names(column_mapping)[1]
      ),
    ),
    mainPanel(
      plotlyOutput(outputId = "radioavgline"),
      plotlyOutput(outputId = "radioline")
    ),
  ),
  titlePanel("Summary"),
  HTML("<p>This trend in graph aims to answer the question of 'How does the death rate of personal health
       problems relate in correlation to the average inflation rates?' After anayzing the numbers and looking
       at the graphs we can see that not all personal health problems have a correlation between inflation rates.
       **Note** that not all country has avg inflation avaliable but they all are working.</p>")
)

food_disparity_deaths <- fluidPage(
  titlePanel("Food Disparity Deaths in Relations to Inflation"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "food_disparity_country",
        label = "Select Country:",
        choices = unique(df$Entity),
        selected = unique(df$Entity)[1]
      ),
      checkboxGroupInput(
        inputId = "food_disparity_checkboxes",
        label = "Deaths From:",
        selected = "",
        choiceNames = c("Low Fruit Diet","Low Vegetable Diet", "Low Whole Grains Diet", "Low Nuts and Seeds Diet",
                        "High Sodium Diet", "Unsafe Drinking Water"),
        choiceValues = c("Deaths.that.are.from.all.causes.attributed.to.diet.low.in.fruits..in.both.sexes.aged.all.ages", 
                         "Deaths.that.are.from.all.causes.attributed.to.diet.low.in.vegetables..in.both.sexes.aged.all.ages",
                         "Deaths.that.are.from.all.causes.attributed.to.diet.low.in.whole.grains..in.both.sexes.aged.all.ages",
                         "Deaths.that.are.from.all.causes.attributed.to.diet.low.in.nuts.and.seeds..in.both.sexes.aged.all.ages",
                         "Deaths.that.are.from.all.causes.attributed.to.diet.high.in.sodium..in.both.sexes.aged.all.ages",
                         "Deaths.that.are.from.all.causes.attributed.to.unsafe.water.source..in.both.sexes.aged.all.ages"),
      )
    ),
    mainPanel(
      plotlyOutput(outputId = "foodavgline"),
      plotlyOutput(outputId = "fooddeathline")
    ),
  ),
  titlePanel("Summary"),
  HTML("<p>This trend in graph aims to answer the question of 'How does the death rate due to food disparities
       relate in correlation to the average inflation rates?' After anayzing the numbers and looking at the 
       graphs we can see that over time with inflation rates increasing the deaths due to food disarities are increasing
       which has a correlation between inflation rates. But we also see that as time goes on deaths from unsanitary water decreases.
       **Note** that not all country has avg inflation avaliable but they all are working.</p>")
)

ui<- navbarPage(
  "Food Inflation Trends",
  tabPanel("Introduction", home_page),
  tabPanel("Average Inflation Rate", inflation_trend_page),
  tabPanel("Deaths from Personal Health", death_page),
  tabPanel("Deaths from Food Disparity", food_disparity_deaths)
)

server <- function(input, output) {
  output$radioavgline <- renderPlotly({
    df_avg <- df %>%
      filter(Entity == input$death_country) %>%
      select(Year, avg_inflation)
    
    p <- ggplot(df_avg, aes(x = Year , y = avg_inflation, color = input$death_country)) + 
      geom_line() + 
      labs(title = "Inflation Rates Change Over the Years",
           x = "Years",
           y = "Inflation Rate Change (%)",
           color = "Inflation for Country") +
      theme_minimal()
    
    p <- ggplotly(p, tooltip = "text")
    return(p)
  })
  
  output$radioline <- renderPlotly({
    selected_country <- input$death_country
    selected_column <- column_mapping[input$radio]
    
    df_filtered <- df %>%
      filter(Entity == selected_country) %>%
      select(Year, !!as.name(selected_column))
  
    p <- ggplot(df_filtered, aes(x = Year, y = !!as.name(selected_column), color = input$radio)) +
      geom_line() +
      labs(title = paste("Health Related Deaths in", selected_country, "over the Years"),
           x = "Years", y = "Number of Deaths", color = "Personal Health Problem") +
      theme_minimal()
    
    p <- ggplotly(p, tooltip = "text")
    return(p)
  })
  
  output$avgline <- renderPlotly({
    df_avg <- df %>%
      filter(Entity == input$avg_country) %>%
      select(Year, avg_inflation)
    
    p <- ggplot(df_avg, aes(x = Year , y = avg_inflation, color = input$avg_country)) + 
      geom_line() + 
      labs(title = "Inflation Rates Change Over the Years",
           x = "Years",
           y = "Inflation Rate Change (%)",
           color = "Inflation for Country") +
      theme_minimal()
    
    p <- ggplotly(p, tooltip = "text")
    return(p)
  })
  
  output$foodavgline <- renderPlotly({
    df_avg <- df %>%
      filter(Entity == input$food_disparity_country) %>%
      select(Year, avg_inflation)
    
    p <- ggplot(df_avg, aes(x = Year , y = avg_inflation, color = input$food_disparity_country)) + 
      geom_line() + 
      labs(title = "Inflation Rates Change Over the Years",
           x = "Years",
           y = "Inflation Rate Change (%)",
           color = "Inflation for Country") +
      theme_minimal()
    
    p <- ggplotly(p, tooltip = "text")
    return(p)
  })
  
  filteredFoodData <- reactive({
    if (length(input$food_disparity_checkboxes) > 0) {
      df %>%
        filter(Entity == input$food_disparity_country) %>%
        select(Year, input$food_disparity_checkboxes)
    } else {
      data.frame(Year = numeric(), stringsAsFactors = FALSE)
    }
  })
  
  output$fooddeathline <- renderPlotly({
    df_foodline <- filteredFoodData()
    
    p <- ggplot() +
      labs(title = paste("Food Disparity Deaths in", input$food_disparity_country, "over the Years"),
           x = "Years", y = "Number of Deaths") +
      theme_minimal()
    
    if (nrow(df_foodline) > 0) {
      for (col in input$food_disparity_checkboxes) {
        p <- p + geom_line(data = df_foodline, aes(x = Year, y = !!sym(col)))
      }
    }
    
    p <- ggplotly(p, tooltip = "text")
    return(p)
  })
}

shinyApp(ui = ui, server = server)