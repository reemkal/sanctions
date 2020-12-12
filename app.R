#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#loading the libraries

library(shiny)
library(tidyverse)
library(readxl)
library(janitor)
library(gapminder)
library(gganimate)
library(gifski)
library(rstanarm)
library(shinythemes)

#reading in necessary data and models from raw_data file

final <- read_csv("final.csv")

country_vector <- unique(final$countryname)

final_model <- read_csv("final_model.csv")

fit_1 <- stan_glm(data = final_model, 
                  formula = infant_mortality ~ us_length + Year, refresh = 0)

fit_2 <- stan_glm(data = final_model, 
                  formula = adult_literacy ~ us_length + Year, refresh = 0)

fit_3 <- stan_glm(data = final_model, 
                  formula = imports ~ us_length + Year, refresh = 0)

fit_4 <- stan_glm(data = final_model, 
                  formula = exports ~ us_length + Year, refresh = 0)

predictions <- readRDS("predictions.RDS")

predictions_2 <- readRDS("predictions_2.RDS")

predictions_3 <- readRDS("predictions_3.RDS")

predictions_4 <- readRDS("predictions_4.RDS")


# Define UI 
ui <- navbarPage(
    "Failed Punishment: How US Sanctions Have Impacted Development Worldwide",
    theme = shinytheme("sandstone"),
    tabPanel("Introduction", 
             titlePanel(strong("Introduction")),
             fixedRow(
                column(4,
                    p("Over the past few years sanctions have come under extreme 
               criticism by the international community. The claim made by 
               critics of this popular foreign policy cite that sanctions 
               take a toll on innocent civilians rather than corrupt and 
               dangerous governments, only further harming innocent people. 
               This argument has stemmed from developing and developed nations 
               alike, and rather than rely on pure theory, this project
               focuses on the impact of US Sanctions between 1980 and 2015
               on the following development indicators: public health, 
               education, imports, and exports."),
             h3("Trend in US Sanctions Worldwide"),
             p("Though it is expected that in a  more globalized and connected 
               world, development occur worldwide, the reality for the past few 
               decades has been a step up by the United States in what it views
               as exercises of its moral authority. Wielding the baton of 
               sanctions, the United States has acted as the world's police 
               force, justifying that sanctions will help countries to comply
               with international standards in a globalized world. The following
               graph illustrates this trend of increase in sanctions. Each
               blue dot on the graph represents a new country that the US has 
               imposed sanctions on. As clearly illustrated, over time,
               the US's reliance on the use of sanctions has increased, with a 
               sharp increase in the 1990s after the fall of the Berlin Wall and
               again in the 2000s, presumably with the 'War on Terror'.")),
             column(5, imageOutput("sanctions")))), 

    #public health data and graphs - analysis of infant mortality rates
    
    tabPanel("Public Health",
             titlePanel(strong("Impact of Sanctions on Public Health")),
             fixedRow(
                column(4,
                p("For years, public health has been an important indicator of 
               development. If sanctions are truly meant to make the world a
               better place by forcing countries to develop, then in theory, 
               infant mortality rates, an important indicator of health, should 
               not be negatively impacted and rise despite length of sanctions.
               Of course, it is important to contextualize here that naturally,
               over the course of years and with the development of modern 
               medicine, infant mortality rates are projected to decrease.
               The question is whether sanctions either increase those rates or
               slow the potential development of the public health sector in
               affected countries."), 
             h3("Infant Mortality Rate"), 
             p("According to the Center for Disease Control and Prevention, 
             'infant mortality is the death of an infant before his or her first 
             birthday. The infant mortality rate is the number of infant deaths 
             for every 1,000 live births.' The dynamic graph illustrates the 
             trend in infant mortality rates over the years, showing a general
             declining trend over the years, no doubt due to the developments of
             modern medicine. A look at the regression table, however, indicates
             that while time reduces infant mortality rate, each year of 
             sanctions increases the infant mortality rate by 0.23, which though
             minimal, accumulates over time to a pretty significant impact on
             the public health infrastructure of a developing nation. To further
             evaluate the infant mortality rate for each individual country 
             while accounting for US sanctions, use the drop down menu below."),
             br()),
             column(5,
                    imageOutput("health_filtered"))), 
             br(),
             h3("Evaluate the Data: Infant Mortality Rate Progression by Country"),
             sidebarLayout(
                 sidebarPanel(
                     selectInput("select_country",
                                 "Select Country",
                                 choices = country_vector)
                 ),
                 mainPanel(
                     plotOutput("healthplots")
                 )
             )
    ),
     
    #education data and graphs - analysis of adult literacy rates
            
    tabPanel("Education",
             titlePanel(strong("Impact of Sanctions on Education")),
             fixedRow(
                 column(4,
             p("For countries to appropriately develop, the foundation is often
               a string education program, with competitive public and private
               schools. With a strong basis for education, countries arguably
               develop a more skilled workforce competitive on the global 
               market and world stage. One of the best measures of education,
               and consequently, of development, happens to be adult literacy 
               rates. The higher the rate, the more educated the popualtion. In
               the context of sanctions as a punitive measure, an argument in
               favor of sanctions would say that to prove success, adult 
               literacy rates are not negatively impacted."), 
             h3("Adult Literacy Rates"), 
             p("Adult Literacy Rates, as defined by the World Bank, are 
               measured as a percentage of people aged 15 and above that are 
               literate. Unfortunately, as seen in the dynamic graph, adult 
               literacy rate does not appear to be recorded until approximately 
               the year 2000. For the most part, adult literacy rate appears to 
               be increasing over time for nations in which the statistic is 
               available, though there are a few nations with a longer period of 
               sanctions implemented where adult literacy rate remains stagnant. 
               Furthermore, a look at the regression table indicates that for 
               every year that sanctions are in place, there is a 0.13% 
               reduction in adult literacy rate, regardless of the positive 
               impact of time. To further evaluate the adult literacy rate for 
               each individual country while accounting for US sanctions, use 
               the drop down menu below.")),
             br(),
             column(5,
                    imageOutput("sanc_health_dem_edu"))), 
                br(),
                h3("Evaluate the Data: Adult Literacy Rate Progression by Country"),
                sidebarLayout(
                    sidebarPanel(
                        selectInput("select_country",
                                    "Select Country",
                                    choices = country_vector)
                    ),
                    mainPanel(
                        plotOutput("educationplots")
                    )
                )
            ),
    
    #imports data and graphs - analysis of imports in current USD data
    
     tabPanel("Imports",
              fixedRow(
                  column(4,
                         titlePanel(strong("Impact of Sanctions on Imports")),
             p("Imports are quite crucial to development as they help to grow
               local economies by bringing in new products and businesses,
               potentially helping to build products locally. Sanctions,
               which tend to be economic in nature, and often limit imports,
               have significantly impacted nations in which they have been
               implemented, and by extension, limit economic development."), 
             h3("Imports in USD"), 
             p("Imports, as defined by the World Bank, are the imports of goods
               and services measured in current US dollars. The dynamic graph 
               clearly illustrates that for nations that have faced longer
               periods of sanctions have significantly lower import records, 
               regardless of time. This, of course, can be explained by the fact
               that sanctions are largely economic in nature, impacting the 
               ability of nations to grow and develop their economies, and as
               such, the standard of living for most citizens. A look at the
               regression table will tell a similar story, with the data
               indicating that for each year the US imposes sanctions on a 
               nation, it will lose out on approximately $2.2 billion in
               imports.To further evaluate the imports data for each individual 
               country while accounting for US sanctions, use the drop down menu 
               below.")),
             br(),
             br(),
             column(5,
                    imageOutput("sanc_health_dem_edu_imp"))),
                br(),
                h3("Evaluate the Data: Imports of Goods and Services (USD)"),
                sidebarLayout(
                    sidebarPanel(
                        selectInput("select_country",
                                    "Select Country",
                                    choices = country_vector)
                    ),
                    mainPanel(
                        plotOutput("importplots")
                    )
                )
            ),
     
     #Exports data and graphs - analysis of exports in current USD data
            
     tabPanel("Exports",
              titlePanel(strong("Impact of Sanctions on Exports")),
              fixedRow(
                  column(4,
             p("Exports are quite important to a nation's development, helping
               to boost the local economy while simultaneously helping local
               businesses to grow and increase their revenue. Sanctions,
               which tend to be economic in nature, and often limit exports,
               have significantly impacted nations in which they have been
               implemented, and by extension, limit economic development."), 
             h3("Exports in USD"), 
             p("Exports, as defined by the World Bank, are the exports of goods
               and services measured in current US dollars. A look at the 
               dynamic graph clearly indicates that for nations that have faced 
               longer periods of sanctions implemented by the United States have 
               significantly lower export records, regardless of time. As 
               explained with regards to imports as well, this, can be explained 
               by the fact that sanctions are largely economic in nature, 
               impacting the ability of nations to grow and develop their 
               economies, and as such, the standard of living for most citizens. 
               A look at the regression table will tell a similar story, with 
               the data indicating that for each year the US imposes sanctions on a 
               nation, it will lose out on approximately $2.4 billion in
               exports.To further evaluate the exports data for each individual 
               country while accounting for US sanctions, use the drop down menu 
               below.")),
             br(),
             column(5,
                    imageOutput("final"))),
             br(),
             br(),
             h3("Evaluate the Data: Exports of Goods and Services (USD)"),
             sidebarLayout(
                 sidebarPanel(
                     selectInput("select_country",
                                 "Select Country",
                                 choices = country_vector)
                 ),
                 mainPanel(
                     plotOutput("exportplots")
                 )
             )
     ),
    
    #using the predictive models generated to create posterior distributions
    #for each of the development indicators
    
    tabPanel("Predicting Development Based on Sanction Length",
             sidebarLayout(
                 sidebarPanel(
                     h3("Predicting Development Based on Sanction Length"),
                     h5("The following interactive model can be used to predict
                        four main development indicators: public health,
                        education, imports, and exports. Set each of the sliders
                        to a situation you desire. For instance, a sanction 
                        length slider set at 10 indicates 10 years of US-imposed 
                        sanctions. As for the Year slider, if set at 15, that
                        indicates a point 15 years from 1980 and would set the 
                        prediction for 2005."),
                     sliderInput("us_length", "Set Sanction Length",
                                 max = 25, min = 1, value = 10),
                     sliderInput("Year", 
                                 "Set the Year Starting in 1980 on a 1-30 Scale",
                                 max = 25, min = 1, value = 20),
                     actionButton("run", "Run Model")),
                 mainPanel(
                     tabsetPanel(id = "tabs",
                                 tabPanel("Predicted Infant Mortality Rate",
                                          plotOutput("infant")),
                                 tabPanel("Predicted Adult Literacy Rate",
                                          plotOutput("adult")),
                                 tabPanel("Predicted Imports (USD)",
                                          plotOutput("imports")),
                                 tabPanel("Predicted Exports (USD)",
                                          plotOutput("exports")))))),
    
    
    #panel introducing me and the data sources
    
    tabPanel("About",
             titlePanel(strong("About")),
             h3("About Me"),
             p("Hello! My name is Reem Ali and I am a sophomore at Harvard
             College studying Government on the Data Science track and Computer 
             Science. I'm interested in and passionate about international 
             development and human rights. 
             
             Being Sudanese-American, my motivation for this project lay in 
             seeing how sanctions ate away at any chance of development for 
             Sudan. Though sanctions claim to put pressure on governments to 
             democratize and comply with international standards by threatening 
             crucial relationships and resources, the reality is anything but. 
             Sanctions helped corrupt governments to continue to plunder while 
             significantly lowering the standard of living for the most 
             vulnerable in society, cutting off much access to the outside world 
             and severely inhibiting development.
             
             I can be reached at my email, reemali@college.harvard.edu.
               You can also access my GitHub repo",
               tags$a(href = "https://github.com/reemkal/final-project", 
                      "here.")),
             h3("About the Data"),
             p("I referenced a number of datasets to obtain my data. 
               The sanctions data was obtained from the ", 
               tags$a(href = "https://www.polver.uni-konstanz.de/gschneider/research/archive/eusanct/",
                      "EUSANCT dataset."), "The remainder of the data was 
               obtained from World Bank datasets, with the infant mortality rate
               data accessible",
               tags$a(href = "https://data.worldbank.org/indicator/SP.DYN.IMRT.IN", 
                      "here,"), "the adult literacy rate data accessible",
               tags$a(href = "https://data.worldbank.org/indicator/SE.ADT.LITR.ZS", 
                      "here,"), "the imports data accessible",
               tags$a(href = "https://data.worldbank.org/indicator/NE.IMP.GNFS.CD", 
                      "here,"), "and the exports data accessible",
               tags$a(href = "https://data.worldbank.org/indicator/NE.EXP.GNFS.CD", 
                      "here."))))
 
# Define server logic 
server <- function(input, output) {
  
    #creating the infant mortality rate plots for each of the countries
  
  output$healthplots <- renderPlot({
        health_filtered %>%
            filter(countryname == input$select_country) %>%
            ggplot(aes(x = Year, y = infant_mortality, color = us_length)) +
            geom_point() +
            geom_smooth(method = "lm") +
            labs(title = "Infant Mortality Rate Progression",
                 x = "Year", y = "Infant Mortality Rate", 
                 color = "Length of US Sanctions") +
            theme_bw()
    })
    
  #creating the adult literacy rate plots for each of the countries
  
    output$educationplots <- renderPlot({
        sanc_health_dem_edu %>%
            filter(countryname == input$select_country) %>%
            ggplot(aes(x = Year, y = adult_literacy, color = us_length)) +
            geom_point() +
            geom_smooth(method = "lm") +
            labs(title = "Adult Literacy Rate Progression",
                 x = "Year", y = "Adult Literacy Rate", 
                 color = "Length of US Sanctions") +
            theme_bw()
    })
    
    #creating the imports plots for each of the countries
    
    output$importplots <- renderPlot({
        sanc_health_dem_edu_imp %>%
            filter(countryname == input$select_country) %>%
            ggplot(aes(x = Year, y = imports, color = us_length)) +
            geom_point() +
            geom_smooth(method = "lm") +
            labs(title = "Progression of Imports (USD)",
                 x = "Year", y = "Imports of Goods and Services (USD)", 
                 color = "Length of US Sanctions") +
            scale_x_continuous(labels=scales::dollar_format()) +
            theme_bw()
    })
    
    #creating the exports  plots for each of the countries 
    
    output$exportplots <- renderPlot({
        final %>%
            filter(countryname == input$select_country) %>%
            ggplot(aes(x = Year, y = exports, color = us_length)) +
            geom_point() +
            geom_smooth(method = "lm") +
            labs(title = "Progression of Exports (USD)",
                 x = "Year", y = "Exports of Goods and Services (USD)", 
                 color = "Length of US Sanctions") +
            scale_x_continuous(labels=scales::dollar_format()) +
            theme_bw()
    })
    
    #reading in the cleaned sanctions dataset and creating the dynamic trends
    #US sanctions graph
    
    sanctions <- read_csv("sanctions.csv")
    
    output$sanctions <- renderImage({
        
        outfile_3 <- tempfile(fileext='.gif')
        
        sanctions_plot <- ggplot(sanctions, aes(x = Year, y = us_length)) +
            geom_point(alpha = 0.5, color = "dodgerblue1") +
            transition_reveal(Year) +
            labs(title = "Trend in US Sanctions Worldwide (1980-2015)", 
                 x = "Year", y = "Length of US Sanctions") +
            theme_bw()
        
        animate(sanctions_plot, nframes = 75, 
                render = gifski_renderer("outfile_3.gif"))
        list(src  = "outfile_3.gif", contentType = "image/gif")
    })
    
    #reading in the cleaned infant mortality rate dataset and 
    #creating the dynamic trends US sanctions and health graph
    
    health_filtered <- read_csv("health_filtered.csv")
    
    output$health_filtered <- renderImage({
        
        outfile_1 <- tempfile(fileext='.gif')
        
         health_plot <- ggplot(health_filtered, aes(x = Year, 
                                                    y = infant_mortality, 
                                                    size = us_length)) +
            geom_point(alpha = 0.5, color = "dodgerblue1") +
            transition_reveal(Year) +
            scale_size(range = c(2, 12)) +
            labs(title = "Infant Mortality Rates Over the Years (1980-2015)", 
                 x = "Year", 
                 y = "Infant Mortality Rate per 1000 Babies Born", 
                 size = "Length of US Sanctions") +
            theme_bw()
        
         animate(health_plot, nframes = 75, 
                 render = gifski_renderer("outfile_1.gif"))
         list(src  = "outfile_1.gif", contentType = "image/gif")
    })
    
    #reading in the cleaned adult literacy rate dataset and 
    #creating the dynamic trends US sanctions and education graph
    
    sanc_health_dem_edu <- read_csv("sanc_health_dem_edu.csv")
    
    output$sanc_health_dem_edu <- renderImage({
        
        outfile_2 <- tempfile(fileext='.gif')
        
        education_plot <-  ggplot(sanc_health_dem_edu, aes(x = Year, 
                                                           y = adult_literacy, 
                                                           size = us_length)) +
        geom_point(alpha = 0.5, color = "dodgerblue1") +
        transition_reveal(Year) +
        scale_size(range = c(2, 12)) +
        labs(title = "Adult Literacy Rates Over the Years (1980-2015)", 
             x = "Year", 
             y = "Adult Literacy Rates", 
             size = "Length of US Sanctions") +
        theme_bw()
        
    animate(education_plot, nframes = 75, 
            render = gifski_renderer("outfile_2.gif"))
    list(src  = "outfile_2.gif", contentType = "image/gif")
    
    })
    
    #reading in the cleaned imports dataset and 
    #creating the dynamic trends US sanctions and imports graph
    
    sanc_health_dem_edu_imp <- read_csv("sanc_health_dem_edu_imp.csv")
    
    output$sanc_health_dem_edu_imp <- renderImage({
        
        outfile_4 <- tempfile(fileext='.gif')
        
        imports_plot <-  ggplot(sanc_health_dem_edu_imp, aes(x = Year, 
                                                             y = imports, 
                                                             size = us_length)) +
            geom_point(alpha = 0.5, color = "dodgerblue1") +
            transition_reveal(Year) +
            scale_size(range = c(2, 12)) +
            labs(title = "Imports Over the Years (1980-2015)", x = "Year", 
                 y = "Imports", 
                 size = "Length of US Sanctions") +
          scale_y_continuous(labels=scales::dollar_format()) +
            theme_bw()
        
        animate(imports_plot, nframes = 75, 
                render = gifski_renderer("outfile_4.gif"))
        list(src  = "outfile_4.gif", contentType = "image/gif")
        
    })
    
    #reading in the cleaned exports dataset and 
    #creating the dynamic trends US sanctions and exports graph
    
    final <- read_csv("final.csv")
    
    output$final <- renderImage({
        
        outfile_5 <- tempfile(fileext='.gif')
        
        exports_plot <-  ggplot(final, aes(x = Year, y = exports, 
                                           size = us_length)) +
            geom_point(alpha = 0.5, color = "dodgerblue1") +
            transition_reveal(Year) +
            scale_size(range = c(2, 12)) +
            labs(title = "Exports Over the Years (1980-2015)", x = "Year", 
                 y = "Exports", 
                 size = "Length of US Sanctions") +
          scale_y_continuous(labels=scales::dollar_format()) +
            theme_bw()
        
        animate(exports_plot, nframes = 75, 
                render = gifski_renderer("outfile_5.gif"))
        list(src  = "outfile_5.gif", contentType = "image/gif")
        
    })
    
    observe({
        
        # Matching user input values to their corresponding labels as used 
        # in the model.
        
        us_length <- as.numeric(input$us_length)
        Year <- as.numeric(input$Year)
        
        # Binding those input values into a data frame.
        
        temp <- cbind(us_length, Year)
        bound <- as_data_frame(temp)
        
        # Here I am returning a reactive expression in response to a user
        # clicking the 'run model' button.
        
        observeEvent(input$run, {
            
          pp <- predictions %>%
            filter(us_length == input$us_length, Year == input$Year) %>%
            t() %>%
            as_tibble() %>%
            slice(1:4000) %>%
            ggplot(aes(x = V1)) +
            geom_histogram(bins = 100, color = "white", fill = "dodgerblue1", 
                           alpha = 0.5) +
            labs(title = "Predicted Infant Mortality Rate Distribution", 
                 x = "Predicted Infant Mortality Rate Per 1000 Births", 
                 y = "Count") +
            theme_bw()
          
            
            output$infant <- renderPlot({pp})
            # End of observeEvent.
            
        }) 
        
        #setting the model to run for education indicator
        
        observeEvent(input$run, {
          
          pp_2 <- predictions_2 %>%
            filter(us_length == input$us_length, Year == input$Year) %>%
            t() %>%
            as_tibble() %>%
            slice(1:4000) %>%
            ggplot(aes(x = V1)) +
            geom_histogram(aes(y = after_stat(count/sum(count))),
                           bins = 100, color = "white", fill = "dodgerblue1", 
                           alpha = 0.5) +
            labs(title = "Predicted Adult Literacy Rate Distribution", 
                 x = "Predicted Adult Literacy Rate", 
                 y = "Count") +
            theme_bw()
          
          
          output$adult <- renderPlot({pp_2})
          # End of observeEvent.
          
        }) 
        
        #setting the model to run for imports indicator
        
        observeEvent(input$run, {
          
          pp_3 <- predictions_3 %>%
            filter(us_length == input$us_length, Year == input$Year) %>%
            t() %>%
            as_tibble() %>%
            slice(1:4000) %>%
            ggplot(aes(x = V1)) +
            geom_histogram(aes(y = after_stat(count/sum(count))),
                           bins = 100, color = "white", fill = "dodgerblue1", 
                           alpha = 0.5) +
            labs(title = "Predicted Imports (USD) Distribution", 
                 x = "Predicted Imports (USD)", 
                 y = "Count") +
            scale_x_continuous(labels=scales::dollar_format()) +
            theme_bw()
          
          
          output$imports <- renderPlot({pp_3})
          # End of observeEvent.
          
        }) 
        
        #setting the model to run for exports indicator
        
        observeEvent(input$run, {
          
          pp_4 <- predictions_4 %>%
            filter(us_length == input$us_length, Year == input$Year) %>%
            t() %>%
            as_tibble() %>%
            slice(1:4000) %>%
            ggplot(aes(x = V1)) +
            geom_histogram(aes(y = after_stat(count/sum(count))),
                           bins = 100, color = "white", fill = "dodgerblue1", 
                           alpha = 0.5) +
            labs(title = "Predicted Exports (USD) Distribution", 
                 x = "Predicted Exports (USD)", 
                 y = "Count") +
            scale_x_continuous(labels=scales::dollar_format()) +
            theme_bw()
          
          
          output$exports <- renderPlot({pp_4})
          # End of observeEvent.
          
        }) 
        
    })
    
        
}

# Run the application
shinyApp(ui = ui, server = server)    
    
    
    
    
    
