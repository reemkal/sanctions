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

# Define UI 
ui <- navbarPage(
    "Failed Punishment: How US Sanctions Have Impacted Development Worldwide",
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
             plotOutput("sanc_exp")),
    
    tabPanel("Democracy",
             titlePanel(strong("Relative Democracy and Autocracy Scores Over the 
                               Years")),
             p("This section analyzes democracy and autocracy scores over the 
               years for a number of countries and attempts to establish whether
               sanctions had any positive or effective impact."), 
             h3("Democracy and Autocracy Scores From 1980 to 2015"), 
             p("This data and plot explores the relationship between sanctions, 
               democracy, and autocracy scores. The issue with the data as 
               currently presented, however, is that combining the sanctions 
               dataset seems to result in no plot output for some reason, 
               something I noticed when trying to achieve the same result with 
               the health dataset. As such, the data as presented shows no 
               interaction with the sanctions dataset, though I plan to assign 
               that variable to the size argument for point sizes. Additionally, 
               I wasn't able to figure out a way to manipulate the limits for 
               the size argument, but now that I have a general template for my 
               animated plots, I plan to work out these issues in recitation
               and will hopefully have fully animated plots for each of my 
               indicators for the next milestone."),
             br(),
             plotOutput("democracy"))
    )

# Define server logic 
server <- function(input, output) {
    
    
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
                 render = gifski_renderer("outfile.gif"))
         list(src  = "outfile_1.gif", contentType = "image/gif")
    })
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
    
    democracy <- read_csv("democracy.csv")
    
    output$democracy <- renderImage({
        
        outfile <- tempfile(fileext='.gif')
        democracy_plot <-
            ggplot(democracy, aes(x = Year, y = Score, size = Score, 
                                  color = countryname)) +
            geom_point(alpha = 0.4) +
            ylim(0, 10) +
            transition_reveal(Year) +
            scale_size(range = c(2, 12)) +
            labs(title = "Democracy and Autocracy Scores Over the Years", 
                 x = 'Year', y = "Indicator Score") +
            facet_wrap(~ Indicator) +
            theme_bw()
        animate(democracy_plot, nframes = 75, render = gifski_renderer("outfile.gif"))
        list(src  = "outfile.gif", contentType = "image/gif")
        
    })
}



# Run the application
shinyApp(ui = ui, server = server)    
    
    
    
    
    
