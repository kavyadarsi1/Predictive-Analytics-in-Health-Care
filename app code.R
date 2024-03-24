library(tidyverse)
library(ggplot2)
library(dplyr)

health_raw <- read_csv("https://intro-datascience.s3.us-east-2.amazonaws.com/HMO_data.csv")
health=health_raw %>% filter(!(is.na(bmi)))
health=health %>% filter(!(is.na(hypertension)))
sapply(health,function(x) sum(is.na(x)))
sapply(health,function(x) sum(is.null(x)))

threshold=quantile(health$cost,probs=(.75))
health$Expensive <- ifelse(health$cost>=threshold, 1, 0)
glimpse(health)

a=health[health$Expensive==1,]
b=health[health$Expensive==0,]
sum(a$cost)
sum(b$cost)

#smoker
health$smoker<-str_replace_all(health$smoker,"no","0")
health$smoker<-str_replace_all(health$smoker,"yes","1")
#location_type
health$location_type<-str_replace_all(health$location_type,"Country","0")
health$location_type<-str_replace_all(health$location_type,"Urban","1")
#education_level
health$education_level<-str_replace_all(health$education_level,"No College Degree","0")
health$education_level<-str_replace_all(health$education_level,"Bachelor","1")
health$education_level<-str_replace_all(health$education_level,"Master","2")
health$education_level<-str_replace_all(health$education_level,"PhD","3")
#yearly_physical
health$yearly_physical<-str_replace_all(health$yearly_physical,"No","0")
health$yearly_physical<-str_replace_all(health$yearly_physical,"Yes","1")
#exercise
health$exercise<-str_replace_all(health$exercise,"Not-Active","0")
health$exercise<-str_replace_all(health$exercise,"Active","1")
#married
health$married<-str_replace_all(health$married,"Not_Married","0")
health$married<-str_replace_all(health$married,"Married","1")
#gender
# Make sure to re-code female first
health$gender<-str_replace_all(health$gender,"female","1")
health$gender<-str_replace_all(health$gender,"male","0")


library(imputeTS)
#install.packages("zoo")
library(zoo)
#delete.na <- function(DF, n) {
#  DF[rowSums(is.na(DF)) <= n,]
#}

#manipulating data again for the shiny apps
health$smoker <- as.numeric(health$smoker)
health$location_type<- as.numeric(health$location_type)
health$education_level <- as.numeric(health$education_level)
health$yearly_physical <- as.numeric(health$yearly_physical)
health$married <- as.numeric(health$married)
health$gender <- as.numeric(health$gender)
health$exercise <- as.numeric(health$exercise)
health$bmi=as.numeric(health$bmi)
health$Expensive=as.numeric(health$Expensive)
health <- health[,-14]
health <- health[,-6]



library(kernlab)
library(caret)
health$Expensive=as.factor(health$Expensive)
trainList <-  createDataPartition(y=health$Expensive,p=.25,list=FALSE)
trainset <- health[trainList,]
testset <- health[-trainList,]


#svm model to integrate in shiny apps
svm1 <- train(as.factor(Expensive) ~ ., data=trainset, method="svmRadial",preProc=c("center","scale"))
svm1
svmpred=predict(svm1,testset)
table(svmpred, testset$Expensive)
sum(diag(table(svmpred,testset$Expensive)))/sum(table(svmpred,testset$Expensive))
confusionMatrix(svmpred, testset$Expensive)

our_model <- svm1
save(our_model, file = "our_model.rda")

# Creation of Shiny App

library(shiny)
library(rsconnect)



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  fileInput("upload", label="HMO input file", accept = c(".csv")),
  #Read the actual (solution) data
  fileInput("upload_Solution", label="HMO solution file", accept = c(".csv")),
  #get a number (how much of the dataframe to show)
  numericInput("n", "Number of Rows", value = 5, min = 1, step = 1),
  #a place to output a table (i.e., a dataframe)
  tableOutput("headForDF"),
  #output the results (for now, just simple text)
  verbatimTextOutput("txt_results", placeholder = TRUE)
)

# Define server logic required
server <- function(input, output,session) {
  #require an input file, then read a CSV file
  getTestData <- reactive({
    req(input$upload)
    read_csv(input$upload$name)
  })
  #require an the actual values for the prediction (i.e. solution file)
  getSolutionData <- reactive({
    req(input$upload_Solution)
    read_csv(input$upload_Solution$name)
  })
  
  #show the output of the model
  output$txt_results <- renderPrint({
    #load the data
    dataset <- getTestData()
    dataset_solution <- getSolutionData()
    #load and use the model on the new data
    use_model_to_predict(dataset, dataset_solution)
  })
  #show a few lines of the dataframe
  output$headForDF <- renderTable({
    df <- getTestData()
    head(df, input$n)
  })
}
#these libraries are needed, will be used with predict
library(caret); library(kernlab); library(e1071)
#load a model, do prediction and compute the confusion matrix
use_model_to_predict <- function(df, df_solution){
  #load the pre-built model, we named it ‘out_model.rda’)
  load(file="our_model.rda")
  #use the model with new data
  
  #smoker
  df$smoker<-str_replace_all(df$smoker,"no","0")
  df$smoker<-str_replace_all(df$smoker,"yes","1")
  #location_type
  df$location_type<-str_replace_all(df$location_type,"Country","0")
  df$location_type<-str_replace_all(df$location_type,"Urban","1")
  #education_level
  df$education_level<-str_replace_all(df$education_level,"No College Degree","0")
  df$education_level<-str_replace_all(df$education_level,"Bachelor","1")
  df$education_level<-str_replace_all(df$education_level,"Master","2")
  df$education_level<-str_replace_all(df$education_level,"PhD","3")
  #yearly_physical
  df$yearly_physical<-str_replace_all(df$yearly_physical,"No","0")
  df$yearly_physical<-str_replace_all(df$yearly_physical,"Yes","1")
  #exercise
  df$exercise<-str_replace_all(df$exercise,"Not-Active","0")
  df$exercise<-str_replace_all(df$exercise,"Active","1")
  #married
  df$married<-str_replace_all(df$married,"Not_Married","0")
  df$married<-str_replace_all(df$married,"Married","1")
  #gender
  # Make sure to re-code female first
  df$gender<-str_replace_all(df$gender,"female","1")
  df$gender<-str_replace_all(df$gender,"male","0")
  
  
  
  library(imputeTS)
  library(zoo)
  df$smoker <- as.numeric(df$smoker)
  df$location_type<- as.numeric(df$location_type)
  df$education_level <- as.numeric(df$education_level)
  df$yearly_physical <- as.numeric(df$yearly_physical)
  df$married <- as.numeric(df$married)
  df$gender <- as.numeric(df$gender)
  df$exercise <- as.numeric(df$exercise)
  df$bmi=as.numeric(df$bmi)
  
  svmPred <- predict(our_model, df, type = "raw")
  #show how the model performed
  df_solution$expensive <-str_replace_all(df_solution$expensive,"TRUE","1")
  df_solution$expensive <-str_replace_all(df_solution$expensive,"FALSE","0")
  df_solution$expensive<- as.factor(df_solution$expensive)
  confusionMatrix(svmPred, df_solution$expensive)
  
}

# Run the application 
shinyApp(ui = ui, server = server)