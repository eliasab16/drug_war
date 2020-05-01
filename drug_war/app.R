#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(haven)
library(janitor)
library(readxl)
library(skimr)
library(rvest)
library(broom)
library(ggthemes)
library(gganimate)
library(shinythemes)
library(gifski)
library(tidyverse)

# variables for data analysis and viz
meth_purity <- read_excel("meth_purity_1981_2018_copy.xlsx")
heroin_purity <- read_excel("heroin_purity_1981_2017_copy.xlsx")
overdose <- read_excel("comprehensive_overdose_1999_2018_copy.xlsx")
spending <- read_excel("drug_control_spending_1995_2018_copy.xlsx")
thc_9 <- read_excel("cannabis_thc_9_concentration_copy.xlsx")
heroin_production <- read_excel("mexico_production_copy.xlsx")

# Define UI for application that draws a histogram
ui <- navbarPage("The War on Drugs",
                 theme = shinytheme("cerulean"),
                 
                 ##
                 ## About Page ##
                 ##
                 
                 tabPanel("About",
                          
                          # insert image
                          
                          imageOutput("drugArrest", width = "100%", height = "100%"),
                          br(),
                          
                          # title
                          
                          h2("How Effective is the War on Drugs?", align = "center"),
                          h3("Let the numbers speak.", align = "center"),
                          br(),
                          div(),
                          
                          fluidRow(column(2), column(8,
                                                     
                                                     h4(strong("Why This Project?")),          
                                                     
                                                     # brief explanation
                                                     
                                                     p("The U.S. War on Drugs has been going on for 4 decades, with each year witnessing a doubling down on
                                                       spending and law enforcement. However, all these efforts seem to be futile in eradicating the drug crisis
                                                       in the United States: the crisis is worsening. Many liberal thinkers - mainly Libertarians - 
                                                       have been advocating for putting an end to this War, claiming that it is counterproductive. In this project,
                                                       I intend to analyze how increased spending on drug control - through both supply and demand reduction efforts -
                                                       affected the drug crisis situation."),
                                                     
                                                     br(),
                                                     
                                                     #text to explain how I selected the accounts to analyze and how I coded for gender
                                                     
                                                     h4(strong("The Iron Law of Prohibition")),
                                                     
                                                     
                                                     p('The “Iron Law of Prohibition”— a term coined by cannabis activist Richard Cowan in a 1986
                                                       article titled “How the Narcs Created Crack“ — posits that “as law enforcement becomes more 
                                                       intense, the potency of prohibited substances increases.” It is based on the premise that when
                                                       drugs or alcohol are prohibited, more concentrated and powerful forms will be introduced into
                                                       the black markets, because these more potent forms offer better efficiency in the business 
                                                       model—they take up less space in storage, less weight in transportation, and they sell for 
                                                       more money. Also, since both higher- and lower-quality substances will share similar fixed
                                                       costs - in this case, social and legal costs - consumers will tend to choose the higher quality,
                                                       and thus creating higher demand for more potent drugs.'),
                                                     
                                                     br(),
                                                     
                                                     p('I will be testing this hypothesis by using data from different government agencies.'),
                                                     
                                                     span(),
                                                     
                          ))),
                          
                 ##
                 ## Singular Trends ##
                 ##
                 
                 tabPanel("Singular Trends",
                         tabsetPanel(
                             
                             
                             tabPanel("Overdose Deaths",
                                      
                                      h3("U.S. Drug Overdose Deaths 1999-2018"),
                                      
                                      br(),
                                      
                                      fluidRow(
                                        column(2), column(8, align="center",
                                               plotOutput("overdosePlot"))
                                        )
                                      ),
                             
                             tabPanel("Spending",
                                      
                                      h3("U.S. Drug Control Spending 1995-2018"),
                                      
                                      br(),
                                      
                                      sidebarPanel(
                                          helpText("Choose Category of Spending"),
                                          selectInput("spending_category", "Spending Category:",
                                                      choices = list("Total" = "total",
                                                                     "Prevention" = "supply_reduction",
                                                                     "Treatment" = "demand_reduction"),
                                                      selected = "total")),
                                      
                                      mainPanel(imageOutput("spendingPlot"))),
                             
                             tabPanel("Drug Potency",
                                      
                                      fluidRow(
                                        column(2), column(8, align="center",
                                                h2("Purity and Potency Trends of Drugs in U.S. Markets"),
                                                
                                                br(),
                                                
                                                h3("Changes in Meth Purity (Illegal U.S. Markets 1981-2018)"),
                                                h4("The Purer the drug, the more effective and deadlier"),
                                                br(),
                                                
                                                plotOutput("methPlot", width = "100%"),
                                                br(),
                                                br(),
                                                
                                                h3("Changes in Delta-9 THC Concentration in Cannabis (Illegal U.S. Markets 1995-2018)"),
                                                br(),
                                                
                                                plotOutput("thcPlot", width = "100%"),
                                                br(),
                                                br()
                                               
                                        )
                                      )))),
                             
                 
                 ##
                 ## Correlations ##
                 ##
                 
                 tabPanel("Correlation Trends",
                          tabsetPanel(
                              
                              # correlation trends of spending and overdose
                            
                              tabPanel("Spending & Overdose",
                                       
                                       h3("Trends in U.S. Drug Control Spending and Overdose Deaths (1995-2018)", 
                                          align="center"),
                                       h4("(Spending Both on Prevention and Treatment)", 
                                          align = "center"),
                                       
                                       br(),
                                       br(),
                                       
                                       fluidRow(
                                         column(2), column(8, align="center",
                                                mainPanel(imageOutput("spendingOverdosePlot", width = "100%")))
                                                )),
                              
                              # correlation between spending and drug potency
                              
                              tabPanel("Spending & Potency",
                                       
                                       fluidRow(
                                         column(2), column(8, align="center",
                                                 h3("Correlation Between Spending and Meth Purity in Illegal U.S. Markets", 
                                                    align="center"),
                                                 h4("(Spending Both on Prevention and Treatment)", 
                                                    align = "center"),
                                                 
                                                 br(),
                                                 
                                                 plotOutput("spendingMethPlot", width = "100%"),
                                                 
                                                 br(),
                                                 br(),
                                                 
                                                 h3("Correlation Between Spending and Delta-9 THC Concentration in Cannabis in Illegal U.S. Markets", 
                                                    align="center"),
                                                 h4("(Spending Both on Prevention and Treatment)", 
                                                    align = "center"),
                                                 
                                                 br(),
                                                 
                                                 plotOutput("spendingThcPlot", width = "100%"),
                                                 
                                                 br(),
                                                 br()
                                         ))),
                              
                              # trends of spending and heroin production
                              
                              tabPanel("Spending & Production",
                                       
                                       h3("Trends in U.S. Spending on Supply Reduction \n and Estimated Heroin Production in Mexcio (1999-2018)",
                                          align="center"),
                                       h4("(Mexican producers account for the majority of illicit drugs in the U.S.)", 
                                          align = "center"),
                                       
                                       br(),
                                       br(),
                                       
                                       fluidRow(
                                         column(2), column(8, align="center",
                                                plotOutput("spendingProductionPlot", width = "100%")
                                         )
                                       )))))
                 
                 # Define server logic required to draw a histogram
                 server <- function(input, output) {
                     
                     ##
                     ## About Page Picture ##
                     ##
                     
                     output$drugArrest <- renderImage({
                        list(src = "drug_arrest.jpg",
                             height = 300,
                             width = 800,
                             style = "display: block; margin-left: auto; margin-right: auto;
                             box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
                             border-radius: 4px;")},
                             deleteFile = FALSE)
                     
                     
                     ## render output for spending plots
                     
                     
                     output$spendingPlot <- renderImage({
                       
                       # temporary file
                       
                       outfile <- tempfile(fileext='.gif')
                         
                       spending_new <- spending %>% 
                           pivot_longer(cols = c("demand_reduction", "supply_reduction", "total"),
                                        names_to = "category", values_to = "total_spending") %>% 
                           mutate(total_spending = round(total_spending/1000, 2)) %>% 
                           filter(category == input$spending_category)
                         
                       p <- ggplot(spending_new, aes(x = year)) +
                         geom_col(aes(y = total_spending), fill = "#519E44",
                                  color = "black") +
                         theme_calc() + 
                         theme(
                             axis.title.x = element_text(size = 15),
                             axis.title.y = element_text(size = 15, color = "#519E44")) +
                         labs(
                             x = "Year",
                             y = "Spending in $Billions",
                             caption = "Source: ONDCP"
                         ) +
                         ylim(0, 40) +
                         transition_time(year) +
                         shadow_mark()
                       
                       anim_save("outfile.gif", animate(p))
                       
                       # Return a list containing the filename
                       
                       list(src = "outfile.gif",
                            contentType = 'image/gif',
                            width = 900,
                            height = 600
                       )}, deleteFile = TRUE)
                     
                     
                     ## plot for overdose deaths
                     
                     
                     output$overdosePlot <- renderPlot({
                         
                         overdose %>% 
                             ggplot(aes(x = year)) +
                             geom_col(aes(y = total), fill = "#A91F00", 
                                      color = "black") +
                             ylim(0, 80000) +
                             theme_calc() +
                             theme(
                                 axis.title.x = element_text(size = 15),
                                 axis.title.y = element_text(size = 15, color = "#A91F00")) +
                             labs(
                                 caption = "Source: NCHS",
                                 x = "Year",
                                 y = "Total Deaths"
                             ) +
                             geom_text(x = 2016.65, y = 75000, label = "Peak\n70.2 K",
                                       hjust = 0, color = "#A91F00", size = 4)
                     })
                     
                     
                     ## meth purity plot
                     
                     
                     output$methPlot <- renderPlot({
                         meth_purity %>% 
                             ggplot(aes(x = year, y = round(purity*100, 2))) +
                             geom_line(size = 2, color = "#F16732") +
                             theme_calc() +
                             theme(
                                 axis.title.x = element_text(size = 15),
                                 axis.title.y = element_text(size = 15, color = "#F16732")) +
                             labs(
                                 caption = "Source: DEA National Drug Threat Assesment;\nIDA The Price and Purity of IIIicit Drugs",
                                 x = "Year",
                                 y = "% Purity"
                             ) 
                         }, height = 400, width = 700)
                     
                     
                     ## thc concentration plot
                     
                     
                     output$thcPlot <- renderPlot({
                         thc_9 %>% 
                             ggplot(aes(x = year, y = thc_delta_9)) +
                             geom_line(size = 2, color = "#A233F0") +
                             theme_calc() +
                             ylim(0, 50) +
                             theme(
                                 axis.title.x = element_text(size = 15),
                                 axis.title.y = element_text(size = 15, color = "#A233F0")) +
                             labs(
                                 caption = "Source: University of Mississippi",
                                 x = "Year",
                                 y = "% Concentration"
                             )
                     }, height = 400, width = 700)
                     
                     
                     ## spending and overdose deaths trends plot
                     
                     
                     output$spendingOverdosePlot <- renderImage({
                       
                       # temp file - will be deleted later automatically
                       
                       outfile <- tempfile(fileext='.gif')
                         
                       # join the the spending and overdose datasets
                       
                       spending_overdose <- spending %>% 
                           inner_join(overdose, by = "year") %>% 
                           mutate(total_spending = total.x/1000,
                                  total_deaths = total.y) %>% 
                           select(year, total_spending, total_deaths)
                       
                       # plot with two discrete y-axes for overdose and spending
                       
                       p <- spending_overdose %>%
                           ggplot(aes(x = year)) +
                           geom_line(aes(y = total_spending), size = 1.5, color = "#5BBC4A") +
                           geom_line(aes(y = total_deaths/2600), size = 1.5, color = "#A91F00") +
                           scale_y_continuous(name = "Total Spending ($Billions)",
                                              sec.axis = sec_axis(~.*2600, name = "Total Deaths"),
                                              breaks = c(5, 10, 15, 20, 25, 30, 35, 40)) +
                           scale_x_continuous(name = "Year", breaks = c(2000, 2002, 2004, 2006, 2008,
                                                                        2010, 2012, 2014, 2016, 2018),
                                              labels = c("2000", "'02'", "'04'", "'06'", "'08'", "'10'",
                                                         "'12'", "'14'", "'16'", "'18'")) +
                           labs(
                               caption = "sources: ONDCP; NCHS") +
                           theme_calc() +
                           theme(
                               axis.title.x = element_text(size = 15),
                               axis.title.y = element_text(size = 15, color = "#519E44"),
                               axis.title.y.right = element_text(size = 15, color = "#A91F00", angle = 90)
                               
                           ) +
                           geom_text(aes(x = year - 0.7, y = total_deaths/2600 + 1,
                                         label = paste(round(total_deaths/1000, 0), "K")),
                                     hjust = 0, color = "#A91F00", size = 4) +
                           geom_text(aes(x = year - 1, y = total_spending + 1.2,
                                         label = paste("$", round(total_spending, 1), "B")),
                                     hjust = 0, color = "#5BBC4A", size = 4) +
                           transition_reveal(year)
                     
                    # save the plot as a guf
                       
                       animate(p, nframes = 75, renderer = gifski_renderer("outfile.gif"))
                     
                     # Return a list containing the filename
                     
                     list(src = "outfile.gif",
                          contentType = 'image/gif',
                          width = 800,
                          height = 600
                     )}, deleteFile = TRUE)
                     
                     
                     ## spending and heroin production plot
                     
                     
                     output$spendingProductionPlot <- renderPlot({
                         
                         # join spending and production data frames
                         
                         supply_production <- spending %>% 
                             inner_join(heroin_production, by = "year") %>% 
                             mutate(supply_reduction = supply_reduction/1000) %>% 
                             select(year, supply_reduction, mexico)
                         
                         # plot the data
                         
                         supply_production %>% 
                             ggplot(aes(x = year)) +
                             geom_line(aes(y = supply_reduction), size=1.5, color = "#5BBC4A") +
                             geom_bar(aes(y = mexico/5), stat="identity", size=.1, fill = "#EA9E36",
                                      color="black", alpha=.5) +
                             scale_y_continuous(
                                 name = "Spending on Supply Reduction ($Billions)",
                                 sec.axis = sec_axis(~.*5, name = "Heroin Production in Mexico\n(Metric Tons)")
                             ) +
                             theme_calc() +
                             theme(
                                 axis.title.x = element_text(size = 20),
                                 axis.title.y = element_text(size = 20, color = "#519E44"),
                                 axis.title.y.right = element_text(size = 20, color = "#EA9E36", angle = 90)
                                 
                             ) +
                             labs(
                                 caption = "sources: INCSR; ONDCP",
                                 x = "Year"
                             )
                     }, height = 700, width = 900)
                     
                     # spending and meth purity
                     
                     output$spendingMethPlot <- renderPlot({
                       
                       # join all dataframes together
                       
                       spending_purity <- meth_purity %>% 
                         inner_join(heroin_purity, by = "year") %>% 
                         inner_join(spending, by = "year") %>% 
                         inner_join(thc_9, by = "year") %>% 
                         mutate(meth = purity.x,
                                heroin = purity.y,
                                thc_9_concentration = thc_delta_9,
                                total = total/1000) %>% 
                         select(year, meth, heroin, total, thc_9_concentration)
                       
                       spending_purity %>% 
                         ggplot(aes(x = total, y = meth * 100)) +
                         geom_point(color = "#FFC300") +
                         geom_smooth(method = "lm", se = F, color = "#900C3F") +
                         labs(
                           x = "Spending in $Billions",
                           y = "% Purity"
                         ) +
                         ylim(0, 100) +
                         theme_calc()
                     }, heigh = 600, width = 800)
                     
                     # spending and thc concentration
                     
                     output$spendingThcPlot <- renderPlot({
                       
                       # join all dataframes together
                       
                       spending_purity <- meth_purity %>% 
                         inner_join(heroin_purity, by = "year") %>% 
                         inner_join(spending, by = "year") %>% 
                         inner_join(thc_9, by = "year") %>% 
                         mutate(meth = purity.x,
                                heroin = purity.y,
                                thc_9_concentration = thc_delta_9,
                                total = total/1000) %>% 
                         select(year, meth, heroin, total, thc_9_concentration)
                       
                       spending_purity %>% 
                         ggplot(aes(x = total, y = thc_9_concentration)) +
                         geom_point(color = "#138D75") +
                         geom_smooth(method = "lm", se = F, color = "#900C3F") +
                         labs(
                           x = "Spending in $Billions",
                           y = "% Concentration"
                         ) +
                         ylim(0, 30) +
                         theme_calc()
                     }, heigh = 600, width = 800)
                 }
                 
                 # Run the application 
                 shinyApp(ui = ui, server = server)
                 