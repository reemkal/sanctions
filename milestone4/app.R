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

# Define UI 
ui <- navbarPage(
    "Milestone 5",
    tabPanel("About", 
             titlePanel(strong("About")),
             h3("Project Background and Introduction"),
             p("Over the past few years sanctions have come under extreme 
               criticism by the international community. The claim made by 
               critics of this popular foreign policy cite that sanctions 
               take a toll on innocent civilians rather than corrupt and 
               dangerous governments, only further harming innocent people. 
               This argument has stemmed from developing and developed nations 
               alike, and rather than pure theory, I decided to take a look at 
               the empirical data. As such, my final project tentatively focuses
               on the impact of US, UN, and EU sanctions globally on a variety 
               of development indicators including democratization, public
               health, education, trade, and the human development index.
               
               For this milestone, I've compiled the data on US, EU, and UN 
               sanctions over the years and have downloaded numerous datasets
               aligning with each of the development factors I'm evaluating.
               The cleaning of the numerous datasets is in progress though, as 
               several of the csvs are not reading into R or as csvs properly, 
               eliminating columns in the original dataset that don't have
               column names. Because I've been trying to avoid manually
               manipulating the datasets in Excel, I've thus far been looking
               for dataset alternatives, but if I'm unable to find them for some
               of the development indicator csvs, I'll try manual manipulation
               in the hopes that that will retain all of my data.
            
             You can access my GitHub repo",
               tags$a(href = "https://github.com/reemkal/final-project", "here.")),
             
             h3("About Me"),
             p("Hello, my name is Reem Ali and I am a sophomore at Harvard
             College studying Government on the Data Science track and Computer 
             Science. I'm interested in and passionate about international 
             development and human rights. 
             
             This is my Milestone 5 for my final project in the Gov50 Data 
             Science Class. You can reach me at reemali@college.harvard.edu.")),

    tabPanel("Model",
             p("Plan to have better analyzed data in here next week.")),
    
    tabPanel("Public Health",
             titlePanel(strong("Impact of Sanctions on Public Health")),
             p("For years, public health has been an indicator of development.
               If sanctions are truly as effective as the foreign policy 
               negotiationf tactic they are deemed to be and are preferable
               to other diplomatic forms of negotiation, then the longer the 
               country has sanctions imposed, in theory, 
               the better its public health system should be."), 
             h3("Infant Mortality Rate"), 
             p("According to the Center for Disease Control and Prevention, 
             'infant mortality is the death of an infant before his or her first 
             birthday. The infant mortality rate is the number of infant deaths 
             for every 1,000 live births.'"),
             br(),
             plotOutput("first_plot")), 
             
     tabPanel("Imports",
             titlePanel(strong("Impact of Sanctions on Imports")),
             p("TBD"), 
             h3("Imports in 1980 and 2015"), 
             p("TBD'"),
             br(),
             plotOutput("sanc_imp")),
             
     tabPanel("Exports",
             titlePanel(strong("Impact of Sanctions on Exports")),
             p("TBD"), 
             h3("Exports in 1980 and 2015"), 
             p("TBD'"),
             br(),
             plotOutput("sanc_exp"))
    )

# Define server logic 
server <- function(input, output) {
    
    
    first_plot <- read_csv("first_plot.csv")
    
    output$first_plot <- renderPlot({
        
        ggplot(first_plot, aes(x = countryname, y = infant_mortality_rate, 
                               fill = us_length)) +
            geom_col() +
            facet_wrap(~ Year) +
            coord_flip() +
            labs(title = "Impact of US Sanctions on Infantr Mortality Rate Internationally", 
                 x = "Country Name", y = "Infant Mortality Rate") +
            theme_bw() +
            theme(axis.text.x = element_text(size = 2, angle = 45))
        
    })
    
}

server <- function(input, output) {
    
    
    sanc_imp <- read_csv("sanc_imp.csv")
    
    output$sanc_imp <- renderPlot({
        
        ggplot(sanc_imp, aes(x = country_name, y = Imports, fill = us_length)) +
            geom_col() +
            facet_wrap(~ Year) +
            coord_flip() +
            labs(title = "Impact of US Sanctions on Imports Internationally", 
                 x = "Country Name", y = "Imports") +
            theme_bw()
        
    })
    
}

server <- function(input, output) {
    
    
    sanc_exp <- read_csv("sanc_exp.csv")
    
    output$sanc_exp <- renderPlot({
        
        ggplot(sanc_exp, aes(x = country_name, y = Exports, fill = us_length)) +
            geom_col() +
            facet_wrap(~ Year) +
            coord_flip() +
            labs(title = "Impact of US Sanctions on Exports Internationally", 
                 x = "Country Name", y = "Exports") +
            theme_bw()
        
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)    
    
    
    
    
    
