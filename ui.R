library(shiny)

shinyUI(fluidPage(
        
       titlePanel("People who did not board the RMS Titanic at Queenstown, what if?"), 
        
        sidebarLayout(
                
                sidebarPanel(
                        
                        h5("According to 2008 article ", a('Canceled Passages Aboard Titanic', href = "http://www.encyclopedia-titanica.org/canceled-titanic-passages.html"), 
                           "by John P. Eaton, at Queenstown (now Cobh) in addition to the names of 115 adults and 5 children who boarded, there also
                            appear on the list names 18 people who did not use the third-class tickets they had purchased for passage aboard the Titanic."),
                        
                        h5("In this App we are going to explore through prediction models based on decision trees, what would happened 
                           if these people had indeed been aboard the ship."),
                        
                        h5("The data used to train the models and to obtain the perfomance indicators belong to the famous competition at 
                           Kaggle, ", a("Titanic: Machine Learning from Disaster", href = "https://www.kaggle.com/c/titanic-gettingStarted")),

                        selectInput("select", label = h6("Decision Tree Complexity"),
                                    c("Default", "Low", "High"), selected = "Default"),
                        
                        h6("App developed by ", a("Giancarlo Callaoapaza", href = "https://www.linkedin.com/in/gcallaoapaza/en"), "(2015).")
                        

                ),
                
                mainPanel(
                        
                        tabsetPanel(
                                
                                tabPanel("Decision Tree Model", plotOutput("plot")),
                        
                                tabPanel("Queenstown Data", dataTableOutput("dataQNB")),
                                
                                tabPanel("Prediction", dataTableOutput("Prediction")),
                                
                                tabPanel("Performance Indicators", verbatimTextOutput("Accuracy"))
                                
                                ))    
        )
))