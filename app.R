#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readxl)
library(janitor)
library(gapminder)
library(gganimate)
library(gifski)
library(shinythemes)

country_vector <- unique(final$countryname)

#final_model <- read_csv("final_model.csv")

#fit_1 <- stan_glm(data = final_model, 
                  #formula = infant_mortality ~ us_length + Year, refresh = 0)

# Define UI 
ui <- navbarPage(
    "Failed Punishment: How US Sanctions Have Impacted Development Worldwide",
    theme = shinytheme("sandstone"),
    tabPanel("Introduction", 
             fixedRow(
                column(4,
                       titlePanel(strong("Introduction")),
                    p("Over the past few years sanctions have come under extreme 
               criticism by the international community. The claim made by 
               critics of this popular foreign policy cite that sanctions 
               take a toll on innocent civilians rather than corrupt and 
               dangerous governments, only further harming innocent people. 
               This argument has stemmed from developing and developed nations 
               alike, and rather than rely on pure theory, this project
               focuses on the impact of US Sanctions between 1980 and 2015
               on the following development indicators: public health, democracy
               and autocracy scores, education, imports, and exports."),
             h3("Trend in US Sanctions Worldwide"),
             p("Though it is expected that in a  more globalized and connected 
               world, developemt occur worldwide, the reality for the past few 
               decades has been a step up by the United States in what it views
               as exercises of its moral authority. Wielding the baton of 
               sanctions, the United States has acted as the world's police 
               force, justifying that sanctions will help countries to comply
               with international standards in a globalized world. The following
               graph illustrates this trend of increase in sanctions.")),
             column(5, imageOutput("sanctions")))), 

    tabPanel("Public Health",
             fixedRow(
                column(4,
                    titlePanel(strong("Impact of Sanctions on Public Health")),
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
             for every 1,000 live births.' To evaluate the infant mortality rate 
             for each individual country while accounting for US sanctions, use 
             the drop down menu below.")),
             column(5,
                    imageOutput("health_filtered"))), 
             br(),
             br(),
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
             
    tabPanel("Education",
             fixedRow(
                 column(4,
             titlePanel(strong("Impact of Sanctions on Education")),
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
             p("Adult Literacy Rates, as defined by the World Bank, are defined
               as a percentage of people aged 15 and above.")),
             br(),
             column(5,
                    imageOutput("sanc_health_dem_edu"))), 
                br(),
                br(),
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
               and services measured in current US dollars.")),
             br(),
             column(5,
                    imageOutput("sanc_health_dem_edu_imp"))),
                br(),
                br(),
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
             
     tabPanel("Exports",
              fixedRow(
                  column(4,
             titlePanel(strong("Impact of Sanctions on Exports")),
             p("Exports are quite important to a nation's development, helping
               to boost the local economy while simultaneously helping local
               businesses to grow and increase their revenue. Sanctions,
               which tend to be economic in nature, and often limit exports,
               have significantly impacted nations in which they have been
               implemented, and by extension, limit economic development."), 
             h3("Exports in USD"), 
             p("Exports, as defined by the World Bank, are the exports of goods
               and services measured in current US dollars.")),
             br(),
             column(5,
                    imageOutput("final"))),
             br(),
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
    
    tabPanel("Predicting Development Based on Sanction Length",
             h5("Predict how developed your country will be:"),
             sidebarLayout(
                 sidebarPanel(
                     sliderInput("Sanction Length", "Set Sanction Length",
                                 max = 30, min = 0, value = 10),
                     sliderInput("Year", 
                                 "Set the Year Starting in 1980 on a 1-30 Scale",
                                 max = 30, min = 1, value = 20),
                     actionButton("run", "Run Model")),
                 mainPanel(
                     tabsetPanel(id = "tabs",
                                 tabPanel("Predicted Infant Mortality Rate"),
                                 tabPanel("Predicted Adult Literacy Rate"),
                                 tabPanel("Predicted Imports (USD)"),
                                 tabPanel("Predicted Exports (USD)"))))),
    
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
                      "here."))))
 
# Define server logic 
server <- function(input, output) {
  
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
    
    output$importplots <- renderPlot({
        sanc_health_dem_edu_imp %>%
            filter(countryname == input$select_country) %>%
            ggplot(aes(x = Year, y = imports, color = us_length)) +
            geom_point() +
            geom_smooth(method = "lm") +
            labs(title = "Progression of Imports (USD)",
                 x = "Year", y = "Imports of Goods and Services (USD)", 
                 color = "Length of US Sanctions") +
            theme_bw()
    })
    
    output$exportplots <- renderPlot({
        final %>%
            filter(countryname == input$select_country) %>%
            ggplot(aes(x = Year, y = exports, color = us_length)) +
            geom_point() +
            geom_smooth(method = "lm") +
            labs(title = "Progression of Exports (USD)",
                 x = "Year", y = "Exports of Goods and Services (USD)", 
                 color = "Length of US Sanctions") +
            theme_bw()
    })
    
    sanctions <- read_csv("sanctions.csv")
    
    output$sanctions <- renderImage({
        
        outfile_3 <- tempfile(fileext='.gif')
        
        sanctions_plot <- ggplot(sanctions, aes(x = Year, y = us_length)) +
            geom_point(alpha = 0.5, color = "darkorchid1") +
            transition_reveal(Year) +
            labs(title = "Trend in US Sanctions Worldwide (1980-2015)", 
                 x = "Year", y = "Length of US Sanctions") +
            theme_bw()
        
        animate(sanctions_plot, nframes = 75, 
                render = gifski_renderer("outfile_3.gif"))
        list(src  = "outfile_3.gif", contentType = "image/gif")
    })
    
    health_filtered <- read_csv("health_filtered.csv")
    
    output$health_filtered <- renderImage({
        
        outfile_1 <- tempfile(fileext='.gif')
        
         health_plot <- ggplot(health_filtered, aes(x = Year, 
                                                    y = infant_mortality, 
                                                    size = us_length)) +
            geom_point(alpha = 0.5, color = "darkorchid1") +
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
    
    sanc_health_dem_edu <- read_csv("sanc_health_dem_edu.csv")
    
    output$sanc_health_dem_edu <- renderImage({
        
        outfile_2 <- tempfile(fileext='.gif')
        
        education_plot <-  ggplot(sanc_health_dem_edu, aes(x = Year, 
                                                           y = adult_literacy, 
                                                           size = us_length)) +
        geom_point(alpha = 0.5, color = "darkorchid1") +
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
    
    sanc_health_dem_edu_imp <- read_csv("sanc_health_dem_edu_imp.csv")
    
    output$sanc_health_dem_edu_imp <- renderImage({
        
        outfile_4 <- tempfile(fileext='.gif')
        
        imports_plot <-  ggplot(sanc_health_dem_edu_imp, aes(x = Year, 
                                                             y = imports, 
                                                             size = us_length)) +
            geom_point(alpha = 0.5, color = "darkorchid1") +
            transition_reveal(Year) +
            scale_size(range = c(2, 12)) +
            labs(title = "Imports Over the Years (1980-2015)", x = "Year", 
                 y = "Imports", 
                 size = "Length of US Sanctions") +
            theme_bw()
        
        animate(imports_plot, nframes = 75, 
                render = gifski_renderer("outfile_4.gif"))
        list(src  = "outfile_4.gif", contentType = "image/gif")
        
    })
    
    final <- read_csv("final.csv")
    
    output$final <- renderImage({
        
        outfile_5 <- tempfile(fileext='.gif')
        
        exports_plot <-  ggplot(final, aes(x = Year, y = exports, 
                                           size = us_length)) +
            geom_point(alpha = 0.5, color = "darkorchid1") +
            transition_reveal(Year) +
            scale_size(range = c(2, 12)) +
            labs(title = "Exports Over the Years (1980-2015)", x = "Year", 
                 y = "Exports", 
                 size = "Length of US Sanctions") +
            theme_bw()
        
        animate(exports_plot, nframes = 75, 
                render = gifski_renderer("outfile_5.gif"))
        list(src  = "outfile_5.gif", contentType = "image/gif")
        
    })
    
    #observe({
        
        # Matching user input values to their corresponding labels as used 
        # in the model.
        
        #us_length <- as.numeric(input$us_length)
        #Year <- as.numeric(input$Year)
        
        # Binding those input values into a data frame.
        
        #temp <- cbind(us_length, Year)
        #bound <- as_data_frame(temp)
        
        # Here I am returning a reactive expression in response to a user
        # clicking the 'run model' button.
        
        #observeEvent(input$run, {
            
            #pp <- posterior_epred(fit_1, newdata = bound) %>%
                #as_tibble() %>%
                #mutate_all(as.numeric)
            
            #output$infant <- renderTable({mean(pp)})
            # End of observeEvent.
            
        #}) 
        
    #})
        
}



# Run the application
shinyApp(ui = ui, server = server)    
    
    
    
    
    
