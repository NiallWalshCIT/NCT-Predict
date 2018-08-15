#Niall Walsh
#R00169183
#install and load neseccary packages
#devtools::install_github("jcheng5/googleCharts")
library(tidyverse)
library(readr)
library(shiny)
library(RColorBrewer)
library(colorRamps)
library(shinythemes)
library(plotly)
library(callr)
library(googleCharts)
library(shinysense)

# load the three seperate years of NCT data and bind them together to form the full the dataset
full_nct <- rbind(read.csv(url("http://www.rsa.ie/Documents/NCT/2015%20Make%20Model%20Year%20Failures.csv"), 
                           skip = 6, header = TRUE, stringsAsFactors = FALSE),
                  read.csv(url("http://www.rsa.ie/Documents/NCT/Make%20Model%20Data%202016.csv"),
                           skip = 5, header = TRUE, stringsAsFactors = FALSE ),
                  read.csv(url("http://www.rsa.ie/Documents/NCT/Make%20Model%20Data%202017.csv"),
                           skip = 5, header = TRUE, stringsAsFactors = FALSE ))


# A variable used for some basic statistics that will change each year I add annual NCT tests
numyears = as.numeric(format(Sys.Date(), format = "%Y")) - 2015

# axis for google chart
xlim <- list(
  min = -5,
  max = 30
)
ylim <- list(
  min = min(full_nct$PASS..) - 10,
  max = max(full_nct$PASS..) + 3
)


ui <- navbarPage("NCTPredict",
                 tabPanel(strong("Home"),
                          fluidPage(
                            shinyjs::useShinyjs(),
                            div(id = "profile", class = "card",
                                h2(strong("Welcome to NCTPredict"), align = "center"),
                                hr(),
                                h4(p("This application aims to help inform people about the latest trends in National Car safety tests in Ireland. Data used in this application can be found",a(href = "http://www.rsa.ie/en/RSA/Your-Vehicle/Your-Vehicle-/NCT/", "here."))),
                                hr(),
                                h4(strong("Overview of Data:"), "Basic statistics about the NCT data including number of NCTs in the dataset, NCTs per year of car registration, Car Makes and Car models"),
                                hr(),
                                h4(strong("Test Your Car:"), "Analyse your car's chances of passing the NCT test on its first attempt and the most common areas it fails on. Predictions are made based on NCT tests carried out since 2015."),
                                hr(),
                                h4(strong("Top Car Makes:"), "Identify the most reliable Car make on Irish Roads by visualising the total number of cars tests in comparision with overall NCT pass rate."),
                                hr(),
                                h4(strong("Test Car Makes:"), "Analyse Car Makes by testing area to see where their strengths and weaknesses are. Visualise a Car Makes Pass Rate from the first year it completed an NCT"),
                                hr(),
                                h4(strong("Google Charts:"), "This is an Interactive Animation of 6 Variables (Car Model, Car Make, Car Year, Pass Rate, Number of cars tested and % that failed on Emmissions) which allows us to easily identify trends and patterns in the NCT data."),
                                hr(),
                                # Average amount of NCT's per year
                                h3("Mean NCT per year:", strong(round(sum(full_nct$Total)/numyears)), align ="Center"),  
                                hr(),
                                # Averge pass rate 
                                h3("Mean Pass Rate:", strong(paste(round(((sum(full_nct$PASS) / sum(full_nct$Total)) * 100))), "%"), align = "center"),
                                hr(),
                                # Calculate the mean cost of retests per year based on the mean amount that fail
                                h3("Annual Mean Cost of Re-tests (in euro):", strong(paste(round((sum(full_nct$Total)/numyears) * (sum(full_nct$FAIL) / sum(full_nct$Total)) * 28))), align = "center")
                            ) #end div
                          ) #end fluidpage
                 ),# end tappanel
                 
                 tabPanel(strong("Overview of Data"),
                          sidebarPanel(
                            # text out put of the total number of NCTs, number of car makes, number of car models in the dataset
                            h3("Total Number of NCT's:", strong(paste(sum(full_nct$Total))), align = "center"),
                            hr(),
                            h3("Total Car Makes:", strong(paste(nlevels(unique(as.factor(full_nct$VehicleMake))))), align = "center"),
                            hr(),
                            h3("Total Car Models:", strong(paste(nlevels(unique(as.factor(full_nct$VehicleModel))))), align = "center"),
                            hr(),
                            sliderInput("numncts", "Total Number of NCT's",
                                        min = 0, max = 2000, value = 1, step = 100),
                            hr(),
                            sliderInput("nummodels", "Total Number of Models",
                                        min = 0, max = 120, value = 30)
                            
                          ), # close sidebar
                          
                          mainPanel(
                            tabsetPanel(
                              tabPanel(p(icon("line-chart"), "Overview of Data"),
                                       # out put bar graph of the number of NCTs per year of car of registration
                                       h4('"Total Number of NCTs per year of car registration",', align = "center"),
                                       h5('This graph highlights number of NCTS for each year of car registration. Please hover over each bar to see Result',
                                          align ="center"),
                                       plotlyOutput(outputId = "plot8") ,
                                       
                                       hr(),
                                       # out put graph of the number of car models 
                                       h4('"Total number of models by Car Make",', align = "center"),
                                       h5('This graph highlights number of models for each car model. Please hover over each bar to see the result.',
                                          align ="center"),
                                       plotlyOutput(outputId = "plot9")
                                       
                              ) # close tab panel
                              
                            ) # close tabset panel
                            
                          )# close mainpanel
                          
                 ), # close tabpanel
                 
                 tabPanel(strong("Test Your Car"),
                          sidebarPanel(
                            # selecting car make, model and year
                            selectInput(inputId = "carmake",
                                        label = "Choose a car make",
                                        choices = unique(sort(full_nct$VehicleMake), decreasing = FALSE), 
                                        selected = full_nct$VehicleMake[6874]),
                            selectInput(inputId = "carmodel",
                                        label = "Choose a car model",
                                        ""),
                            selectInput(inputId = "year",
                                        label = "Choose the year of your car",
                                        ""),
                            
                            hr(),
                            # visual output of pass or fail text
                            h1(strong(textOutput("pass"), align = "center", style = "color:green")),
                            h1(strong(textOutput("fail"), align = "center", style = "color:red")),
                            hr(),
                            # text output of the probability of pass
                            h3(textOutput("prob"), align = "center"),
                            hr(),
                            # text output of the number of training samples
                            h3(textOutput("numtrain"), align = "center")
                            
                            
                          ), # close sidebarpanel
   
                          mainPanel(
                            tabsetPanel( 
                              tabPanel(p(icon("line-chart"), "Visualize the Data"),
                                       h4('"% that fail by test area",', align = "center"),
                                       h5('This graph highlights the most common testing area the selected fails on. Please hover over each bar to see the result.', 
                                          align ="center"),
                                       plotlyOutput(outputId = "plot"),
                                       
                                       hr(),
                                       
                                       h4('"Pass Rate by year",', align = "center"),
                                       h5('This graph highlights when the selected car has a higher probabity of failing the NCT. Please hover over each line to see Result', 
                                          align ="center"),
                                       plotlyOutput(outputId = "plot2"),
                                       
                                       hr(),
                                       
                                       h4('"Total Number of Car Model Tested",', align = "center"),
                                       h5('This graph highlights the amount of NCTs per year of registration of a selected car. Please hover over maker to see Result', 
                                          align ="center"),
                                       plotlyOutput(outputId = "plot3")
                                       
                                    ) # close tabpanel
                                 ) # close tabsetpanel
                              ) # close mainpanel
                          ), # close tabpanel
                 
                 
                 
                 tabPanel(strong("Top Car Makes"),
                          mainPanel(     
                            h4('"Car Makes with over 50000 NCT Tests"', align = "center"),
                            h5('This graph shows the % pass rate along with total amount of a car make tested for the most common cars i.e those with over 50,000 NCTs. Please hover over line to see Result', 
                               align ="center"),
                            plotlyOutput(outputId = "plot4"))
                          
                 ), # close tabpanel
                 # new tab panel for exploreing by car make
                 tabPanel(strong("Explore Car Makes"),
                          sidebarPanel(
                            selectInput(inputId = "carmake2",
                                        label = "Choose a car make",
                                        choices = unique(sort(full_nct$VehicleMake), decreasing = FALSE), 
                                        selected = full_nct$VehicleMake[6874])),
                          # outputting the three graphs
                          mainPanel(
                            tabsetPanel(
                              tabPanel(p(icon("line-chart"), "Visualize the Data"),
                                       h4('"Average Fail Rates Vs Selected Car Make Fail Rate By Test Area",', align = "center"),
                                       h5('The graph highlights the most problematic testing areas for individual car makes.Please hover over each line to see Result',
                                          align ="center"),
                                       plotlyOutput(outputId = "plot5") ,
                                       
                                       hr(),
                                       
                                       h4('"Average Pass Rate Vs Selected Car Pass Rate",', align = "center"),
                                       h5('Please hover over each bar to see the result.',
                                          align ="center"),
                                       plotlyOutput(outputId = "plot6"),
                                       
                                       hr(),
                                       
                                       h4('"Total number of NCTs",', align = "center"),
                                       h5('Please hover over each line to see Result',
                                          align ="center"),
                                       plotlyOutput(outputId = "plot7")
                                       
                              ) # tabpanel
                           ) # close tabsetpanel
                     ) # close mainpanel
                 ), # close tabpanel
                 

                 
                 tabPanel(strong("Google Charts"),
                          fluidPage(
                            googleChartsInit(),
                            
                            # This changes the font to sans pro however for some reason when deployed online it changes
                            tags$link(
                              href=paste0("http://fonts.googleapis.com/css?",
                                          "family=Source+Sans+Pro:300,600,300italic"),
                              rel="stylesheet", type="text/css"),
                            tags$style(type="text/css",
                                       "body {font-family: 'Source Sans Pro'}"
                            ),
                            
                            h2("NCT Pass Rate Vs % of fails on emmissions"),
                            
                            googleBubbleChart("chart",
                                              width="100%", height = "475px",
                                          
                                              options = list(
                                                fontName = "Source Sans Pro",
                                                fontSize = 13,
                                                # Set axis labels and ranges
                                                hAxis = list(
                                                  title = "% Fail rate on Emmissions",
                                                  viewWindow = xlim
                                                ),
                                                vAxis = list(
                                                  title = "% Pass Rate",
                                                  viewWindow = ylim
                                                ),
                                                # The default padding is a little too spaced out
                                                chartArea = list(
                                                  top = 50, left = 75,
                                                  height = "75%", width = "80%"
                                                ),
                                                # Allow pan/zoom
                                                explorer = list(),
                                                # Set bubble visual props
                                                bubble = list(
                                                  opacity = 0.8, stroke = "none",
                                                  # Hide bubble label
                                                  textStyle = list(
                                                    color = "none"
                                                  )
                                                ),
                                                # Set fonts
                                                titleTextStyle = list(
                                                  fontSize = 16
                                                ),
                                                tooltip = list(
                                                  textStyle = list(
                                                    fontSize = 12
                                                  )
                                                )
                                              )
                            ), # close googlebubble
                            # slider inputs for the google bubble chart
                            fluidRow(
                              shiny::column(4, offset = 2,
                                            sliderInput("year2", "Year",
                                                        min = 1987, max = max(full_nct$YearOfBirth),
                                                        value = 1987, animate = TRUE, step = 1)),
                              
                              shiny::column(width = 4,
                                            sliderInput("numtested", "Total Number of Cars Tested",
                                                        min = min(full_nct$Total), max = max(full_nct$Total), value = 500))
                            ) #closing fluid row
 
                          )#closing fluid page
                          
                 ) # closing tabpanel
                 
)# closing bracket for navbar


server <- function(input, output, session) {
  # filters the data set on vehicle make
  by_make <- reactive({  
    full_nct %>%
      filter(VehicleMake == as.character(input$carmake))
  })
  # an observe function to change when a car model is selected
  observe({
    updateSelectInput(session, "carmodel", selected = full_nct$VehicleModel[6874], choices = unique(sort(by_make()$VehicleModel), decreasing = FALSE))
    
  })
  #  filters the data set on vehicle model
  by_make_model <- reactive({
    by_make () %>% 
      filter(VehicleModel == as.character(input$carmodel))
  })
  # an observe function to change when a car year is selected 
  observe({
    updateSelectInput(session, "year", selected = full_nct$YearOfBirth[6874],choices = unique(sort(by_make_model()$YearOfBirth, decreasing = FALSE)))
    
  })
  #  filters the data set on vehicle year of birth
  by_make_model_year <- reactive({
    by_make_model() %>% 
      filter(YearOfBirth == as.integer(input$year)) 
  })
  
  Car_summary <- reactive({
    by_make_model_year() %>% 
      summarise( Vehicle.Safety.Equipment = ((sum(Vehicle.and.Safety.Equipment) / sum(Total)) * 100),
                 Lighting.and.Electrical = ((sum(Lighting.and.Electrical) / sum(Total)) * 100),
                 Steering.and.Suspension = ((sum(Steering.and.Suspension) / sum(Total)) * 100),
                 Braking.Equipment = ((sum(Braking.Equipment) / sum(Total)) * 100),
                 Engine.Noise.and.Exhaust = ((sum(Engine..Noise.and.Exhaust) / sum(Total)) * 100),
                 Chassis.and.Body = ((sum(Chassis.and.Body) / sum(Total)) * 100),
                 Side.Slip.Test = ((sum(Side.Slip.Test) / sum(Total)) * 100),
                 Suspension.Test. = ((sum(Suspension.Test) / sum(Total)) * 100),
                 Light.Test = ((sum(Light.test) / sum(Total)) * 100),
                 Brake.Test = ((sum(Brake.Test) / sum(Total)) * 100),
                 Emmissions.Test = ((sum(Emmissions) / sum(Total)) * 100))
             
      
    
  })
  # reactive function to store the pass rate of a specific car
  passrate <- reactive({
    k <- ((sum(by_make_model_year()$PASS) / sum(by_make_model_year()$Total)) * 100)
    return(k)
  })
  # reactive function to store the number of training samples of a specific car
  num_train <- reactive({
    num <- sum(by_make_model_year()$Total)
    return(num)
  })
  
  # reactive function to gather the car summary
  Car_summary_gathered <- reactive({
    gather(Car_summary(), Area_Tested, Average_Result)
  })
  
  # Two reactive functions to out put prediction based on 0.5 threshold
  output$pass  <- renderText({
    ifelse(passrate() >= 50, paste("PASS"), paste(""))
  })
  output$fail <- renderText({
    ifelse(passrate() >= 50, paste(""), paste("FAIL"))
  })
  
  # outputs the probability of a pass
  output$prob <- renderText({
    paste("Probability of Passing:" ,round(passrate(),2),"%")
  })
  
  # outputs the number training examples
  output$numtrain <- renderText({
    paste("Number of Training examples:", as.character(num_train())) 
    
  })
  
  # bar plot of the most common areas for fail
  output$plot <- renderPlotly({ 
    
    f <- plot_ly(Car_summary_gathered(),x = ~round(Car_summary_gathered()$Average_Result,2), y = ~reorder(Car_summary_gathered()$Area_Tested, Car_summary_gathered()$Average_Result), name = 'Test',
                 type = 'bar', orientation = 'h',
                 marker = list(color = 'rgba(29, 70, 237, 0.72)',
                               line = list(color = 'rgba(15, 48, 65, 0.3)', width = 2))) %>%
      layout(yaxis = list(showgrid = TRUE, showline = FALSE, showticklabels = TRUE, domain= c(0, 1), title = ""),
             xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE, title = "% of Fail Tests"),
             margin = list(l = 200, r = 50, b = 50, t = 50, pad = 10),
             title = paste(by_make_model_year()$VehicleMake[1], by_make_model_year()$VehicleModel[1], by_make_model_year()$YearOfBirth[1]))
    return(f)
  })
  
  # genetrates the rates of fail and pass per year and total number testes for car type
  Rates_by_year <- reactive({
    by_make_model() %>%            
      group_by(YearOfBirth) %>% 
      summarise( Pass = (sum(PASS) /sum(Total)) * 100,
                 Fail = (sum(FAIL) /sum(Total)) * 100,
                 Total_Tested = sum(Total))
  })
  
  # line plots the fail/pass rates
  output$plot2 <- renderPlotly({
    j <- plot_ly(data = Rates_by_year()) %>%
      add_lines(x = ~YearOfBirth, y = ~round(Pass, 2), name = "Pass",line = list(color = 'rgb(0, 204, 0)', width = 4)) %>%
      add_lines(x = ~YearOfBirth, y = ~round(Fail,2), name = "Fail",line = list(color = 'rgb(255, 0, 0)', width = 4)) %>% 
      layout( xaxis = list(title = "Year of Registration"), yaxis = list(title = "% Fail/Pass Rate"),title =  paste(by_make_model()$VehicleMake[1], by_make_model()$VehicleModel[1]),showlegend = TRUE, hovermode = 'compare') %>% 
      config(displayModeBar = F, showLink = F)
    return(j)
    
  })
  
  # scatter plots the total tested, scatter plot becasue for some years there will be mssing values
  output$plot3 <- renderPlotly({
    
    plotCheck1 = plot_ly(Rates_by_year(),x = ~YearOfBirth, y = ~Total_Tested, name = "Total Tested",type = 'scatter', mode = 'markers',
                         marker = list(size = 10,
                                       color = 'rgba(31, 17, 228, 0.53)',
                                       line = list(color = 'rgba(31, 17, 228, 0.87)',
                                                   width = 2))) %>% 
      layout(xaxis = list(title = "Year of Registration"), yaxis = list(title = "Total number tested"),title = paste(Rates_by_year()$VehicleMake[1],Rates_by_year()$VehicleModel[1]),showlegend = TRUE,  hovermode = 'compare') %>% 
      config(displayModeBar = F, showLink = F)
   # rgba(255, 69, 26, 0.95)
    
    return(plotCheck1)
  })
  
  # genetares the top cars, over 50000 NCTS and the pass rate based on this
  Top_Cars <- reactive({
    one_model <- full_nct %>% 
      group_by(VehicleMake) %>% 
      summarise(Total_Cars = sum(Total),
                Pass_rate = (sum(PASS) /sum(Total)) * 100) %>% 
      filter(Total_Cars > 50000)
    
  })
  
  # generates two plots, total tested and pass rate, uses the vehicale make as Y axis and then has seperate x axis
  # this is very good for easily comparing the two variables
  output$plot4 <- renderPlotly({
    p1 <- plot_ly(Top_Cars(), x = ~Total_Cars, y = ~reorder(VehicleMake, Pass_rate), name = 'Total number of cars tested',
                  type = 'bar', orientation = 'h',
                  marker = list(color = 'rgba(0,0,255,0.3)',
                                line = list(color = 'rgba(0,0,255,0.3)', width = 1))) %>%
      layout(yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85)),
             xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE)) 
    
    
    p2 <- plot_ly(Top_Cars(),x = ~round(Pass_rate,2), y = ~reorder(VehicleMake, Pass_rate), name = 'Pass Rate',
                  type = 'scatter', mode = 'markers') %>% 
      layout(yaxis = list(showgrid = FALSE, showline = TRUE, showticklabels = FALSE,
                          linecolor = 'rgba(102, 102, 102, 0.8)', linewidth = 2,
                          domain = c(0, 0.85)),
             xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE,
                          side = 'top', dtick = 5)) 
    
    p <- subplot(p1, p2) %>% 
      layout(title = 'Total Number of Cars tested Vs Pass Rate', align = "center",
             legend = list(orientation = 'h'),
             width = 1200,
             height = 600,
             margin = list(l = 120, r = 20, t = 70, b = 70),
             paper_bgcolor = 'rgb(248, 248, 255)',
             plot_bgcolor = 'rgb(248, 248, 255)') 
    
    return(p)
  })
  
  # this function generates a mean nct testing results for all cars and also one for a selected car make
  # this data will be used for a grouped bar chart 
  failarea_carmake <- reactive({
    
    Selected_car_info <- full_nct %>%
      filter(VehicleMake == input$carmake2) %>% 
      summarise( Vehicle.Safety.Equipment = ((sum(Vehicle.and.Safety.Equipment) / sum(Total)) * 100),
                 Lighting.and.Electrical = ((sum(Lighting.and.Electrical) / sum(Total)) * 100),
                 Steering.and.Suspension = ((sum(Steering.and.Suspension) / sum(Total)) * 100),
                 Braking.Equipment = ((sum(Braking.Equipment) / sum(Total)) * 100),
                 Engine.Noise.and.Exhaust = ((sum(Engine..Noise.and.Exhaust) / sum(Total)) * 100),
                 Chassis.and.Body = ((sum(Chassis.and.Body) / sum(Total)) * 100),
                 Side.Slip.Test = ((sum(Side.Slip.Test) / sum(Total)) * 100),
                 Suspension.Test. = ((sum(Suspension.Test) / sum(Total)) * 100),
                 Light.Test = ((sum(Light.test) / sum(Total)) * 100),
                 Brake.Test = ((sum(Brake.Test) / sum(Total)) * 100),
                 Emmissions.Test = ((sum(Emmissions) / sum(Total)) * 100))
                
    
    Selected_car_info <- gather(Selected_car_info, Test.Area , Average.Result)
    
    nct_mean_info <- full_nct %>%
      summarise(
        Vehicle.Safety.Equipment = ((sum(Vehicle.and.Safety.Equipment) / sum(Total)) * 100),
        Lighting.and.Electrical = ((sum(Lighting.and.Electrical) / sum(Total)) * 100),
        Steering.and.Suspension = ((sum(Steering.and.Suspension) / sum(Total)) * 100),
        Braking.Equipment = ((sum(Braking.Equipment) / sum(Total)) * 100),
        Engine.Noise.and.Exhaust = ((sum(Engine..Noise.and.Exhaust) / sum(Total)) * 100),
        Chassis.and.Body = ((sum(Chassis.and.Body) / sum(Total)) * 100),
        Side.Slip.Test = ((sum(Side.Slip.Test) / sum(Total)) * 100),
        Suspension.Test. = ((sum(Suspension.Test) / sum(Total)) * 100),
        Light.Test = ((sum(Light.test) / sum(Total)) * 100),
        Brake.Test = ((sum(Brake.Test) / sum(Total)) * 100),
        Emmissions.Test = ((sum(Emmissions) / sum(Total)) * 100))
        
    
    nct_mean_info <- gather(nct_mean_info, Test.Area , Average.Result)
    
    joineddata <- data.frame(nct_mean_info$Test.Area, Selected_car_info$Average.Result, nct_mean_info$Average.Result)
    colnames(joineddata) <-  c("Test_Area", "Selected", "Overall")
    return(joineddata)
    
  })
  
  # OUTPUT A BAR CHART FROM THE selected data
  output$plot5 <- renderPlotly({
    p <- plot_ly(data = failarea_carmake(), x = ~round(Overall, 2), y = ~Test_Area, title = "", type = "bar",orientation = 'h', marker = list(color = 'rgb(255, 102, 0)'), name = "NCT Mean") %>% 
      add_trace(x = ~round(Selected, 2), name = as.character(input$carmake2), marker = list(color = 'rgb(51, 102, 255)')) %>% 
      layout( yaxis = list(title = ""), xaxis = list(title = '% of Fail Tests'),barmode = 'group', margin = list(l = 200, r = 50, b = 50, t = 50, pad = 10))
    return(p)
    
  })
  
  # the function creates two data frames the first is a data frame filtered to generate the average nct pass rate
  # for the minimum year the selected car make has a NCT test registered, the second creates a dataframe for the selected car for
  # pass rate and the total amount of tests the car make has for each year
  # the data frames are them merged so they can be plotted
  PassRate_CarMake <- reactive({
    one_model <- full_nct %>% 
      group_by(YearOfBirth)  %>% 
      filter( YearOfBirth > min(full_nct$YearOfBirth[full_nct$VehicleMake == input$carmake2])) %>% 
      summarise( Pass_Rate = (sum(PASS)/ sum(Total)) * 100 ) %>% 
      arrange(desc(YearOfBirth))
    
    test_model2 <- full_nct %>% 
      group_by(YearOfBirth)  %>% 
      filter( YearOfBirth > min(full_nct$YearOfBirth[full_nct$VehicleMake == input$carmake2]), VehicleMake == input$carmake2) %>% 
      summarise( Pass_Rate_Selected_make = (sum(PASS)/ sum(Total)) * 100,
                 Total_Tested = sum(Total)) %>% 
      arrange(desc(YearOfBirth))
    
    new <- merge(one_model,test_model2, by="YearOfBirth")
    colnames(new) <- c("YearofRegistration", "Pass_Rate" , "Selected", "Total_Tested")
    return(new)
  })
  # generates a line plot so the data can be compared 
  output$plot6 <- renderPlotly({
    
    plotCheck = plot_ly(data = PassRate_CarMake()) %>%
      add_lines(x = ~YearofRegistration, y = ~round(Pass_Rate, 2), name = "NCT Mean",line = list(color = 'rgb(255, 102, 0)', width = 4)) %>%
      add_lines(x = ~YearofRegistration, y = ~round(Selected, 2), name = as.character(input$carmake2), line = list(color = 'rgb(51, 102, 255)', width = 4)) %>% 
      layout(title =  "Pass Rate by year" , yaxis = list(title = "Pass Rate %"), xaxis = list(title = "Year of Registration"), showlegend = TRUE, hovermode = 'compare') %>% 
      config(displayModeBar = F, showLink = F)
    
    return(plotCheck)
  })
  # generates a maker/scatter plot to show how many NCT a selected car make has for each year of it vehicles
  output$plot7 <- renderPlotly({
    
    plotCheck1 = plot_ly(PassRate_CarMake(),x = ~YearofRegistration, y = ~Total_Tested, name = "Total Tested",type = 'scatter', mode = 'markers') %>% 
      layout(title = "Number of NCT's per year for Selected Car Make", xaxis = list(title = "Year of Registration"), yaxis = list(title = "Total Number Tested"), showlegend = TRUE,  hovermode = 'compare') %>% 
      config(displayModeBar = F, showLink = F)
    return(plotCheck1)
  })
  # OVERVIEW OF DATA
  #generates dataframe of the number of NCTs per year of vehicle registration
  numberncts <- reactive({
    NCT_per_year <- full_nct %>% 
      group_by(YearOfBirth) %>% 
      summarise( total.per.year = sum(Total)) %>% 
      filter(total.per.year > input$numncts)
    return(NCT_per_year)
  })
  # OVERVIEW OF DATA
  # bar plot of the number of NCTs per year of vehicle registration
  output$plot8 <- renderPlotly({
    p <- plot_ly(numberncts(), x = ~YearOfBirth, y = ~total.per.year, name = 'Test',type = 'bar') %>% 
      layout(yaxis = list(showgrid = TRUE, showline = FALSE, showticklabels = TRUE, domain= c(0, 1), title = "Number of NCTs"),
             xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE, title = "Year of Car Registration"))
    return(p)
  })
  
  # OVERVIEW OF DATA
  # generates dataframe of the number of car models per car make 
  numbermodels <- reactive({
    models_per_car <- full_nct %>% 
      group_by(VehicleMake) %>% 
      summarise( num_models = n_distinct(VehicleModel)) %>% 
      filter(num_models > input$nummodels) %>% 
      arrange(desc(num_models)) 
    return(models_per_car)
    
  })
  # OVERVIEW OF DATA
  # bar plot of the number of car models per car make 
  output$plot9 <- renderPlotly({
    p <- plot_ly(numbermodels(), x = ~num_models, y = ~reorder(VehicleMake, num_models),type = 'bar', orientation = 'h',
                 marker = list(color = 'rgba(0, 194, 0, 0.58)',
                               line = list(color = 'rgba(15, 48, 65, 0.3)', width = 2))) %>%
      layout(yaxis = list(showgrid = TRUE, showline = FALSE, showticklabels = TRUE, domain= c(0, 1), title = ""),
             xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE, title = "Number of Models"),
             margin = list(l = 200, r = 50, b = 50, t = 50, pad = 10))
            
                 
                 
    return(p)
    
  })
  
  # the following code is for the google chart
  # This provides explicit colors for car makes, so they don't get recoded when the
  # different series happen to be ordered differently from year to year.
  #  I still have not cracked how to get the colour to stay the same each year
  defaultColors <- colorRampPalette(c("red","blue", "orange", "yellow","green", "magenta", "Cyan"))
  
  # this is used to colour the bubbles for each car make and seems to 
  # works as a key value pair
  series <- structure(
    lapply(defaultColors, function(color) { list(color=color) }),
    names = levels(unique((full_nct$VehicleMake))))
  # reactive fucnction for google chart
  yearData <- reactive({
    # Filters to the desired year, and put the columns
    # in the order that Google's Bubble Chart expects
    # them i.e selected(name, x, y, color, size).
    # Also sorts them by make and model and generates the pass rate, total tested, and the percentage that failed 
    # on emmissions
    # the data set can also be filtered on the total number tested to reduce the cluttered chart
    test_model <- full_nct %>% 
      filter(YearOfBirth == input$year2) %>% 
      select(VehicleModel,Emmissions, PASS, VehicleMake, Total) %>% 
      group_by(VehicleMake, VehicleModel) %>% 
      summarise(Percentage.Failed.on.Emmissions = round((sum(Emmissions)/ sum(Total)) * 100, 2),
                Total.Tested = sum(Total),
                Pass.Rate = round((sum(PASS)/ Total.Tested) * 100, 2)) %>% 
      filter(Total.Tested > input$numtested) %>% 
      arrange(desc(Percentage.Failed.on.Emmissions))
    # rearranges the data frame into the correct order
    test_model <- test_model[c(2,3,5,1,4)]
    return(test_model)
    
  })
  # outputs the google chart
  output$chart <- reactive({
    # Return the data and options
    list(
      data = googleDataTable(yearData()),
      options = list(
        title = sprintf(
          "Pass Rate vs. Percenatge of Fails due to Emmissions for Cars Registered in, %s",
          input$year2),
        series = series
      )
    )
  }) 
  
} # Closing UI 

#Creating the shiny app object
shinyApp(ui = ui, server = server)
