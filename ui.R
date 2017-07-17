
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinythemes)
library(RColorBrewer)
library(e1071)
library(rminer)
library(party)
library(caret)
library(kernlab)
library(rpart)
library(nnet)
library(ada)
library(ROCR)
library(ggplot2)
library(reshape2)
set.seed(101)

shinyUI(fluidPage(

    theme = shinytheme("darkly"),
    titlePanel("Classification Of Suspicious and Non Suspicious Activity "),
    
    sidebarPanel(
        selectInput(inputId = "select", label = h3("Machine Learning Model"), 
                    choices = list("Naive Bayes" = 1, "SVM" = 2, "Decision Tree" = 3,"Logistic Regression" = 4 , "Boosting" = 5), 
                    selected = 1),br(),
        sliderInput(inputId = "trainingdata","Proportion of Training observations",min = 0, max = 1, value = 0.7, step = 0.1),
        actionButton(inputId = "go",label = "Launch",width = '200px'),
        actionButton(inputId="view",label="View DataSet",width = '200px',onclick ="window.open('https://drive.google.com/open?id=0B_HuRtGxHkN5a0prOWFUS2cwaGc')")
    ),
    
    mainPanel(
        br(), 
        tabsetPanel(
            tabPanel("Classification",splitLayout(cellWidths = c("50%","50%"), 
                                                  plotOutput(outputId = "map"),plotOutput(outputId = "map2")),
                     splitLayout(cellWidths = c("50%","50%"),verbatimTextOutput(outputId = "stats"),
                                 verbatimTextOutput(outputId = "statsxyz")),
                     HTML("<hr><h4><p><b><i> In classification, we have shown the number of Attacks and Normals 
                          of testing data by representing testing graph and prediction graph
                          represent the prediction made by the model. We can visualize that how much model is accurate</i></b></p></h4>")),
            tabPanel(" Accuracy ", verbatimTextOutput(outputId = "stats2"),HTML("<hr><h3><p><b><i>ACCURACY - Accuracy refers to the closeness of a measured value to a standard or known value.<br><br>PRECISION - Precision refers to the closeness of two
                                                                                or more measurements to each other.<br><br>TRUE POSITIVE - A true positive would
                                                                                be an image that has a property and that is recognized by a program as such.</i></b></p></h3>")),
            tabPanel(" Misuse ",splitLayout(cellWidths = c("50%","50%"), 
                                            plotOutput(outputId = "map1"),plotOutput(outputId = "map3")),
                     verbatimTextOutput(outputId = "stats1"),HTML("<hr><h4><p><b><i>In Misuse we have shown the different types of attcks. Testing graph represent
                                                                  the number of different attacks and predicting graph represent the predicted graph by the model.</i></b></p></h4>"
                     )),
            tabPanel(" ROC ",plotOutput(outputId = "roc"),HTML("<hr><h4><p><b><i>ROC curve represent the area under the curve. The maximum 
                                                               area represent by the model is the efficient model.</i></b></p></h4>"))
            ,
            tabPanel(" Performance Analysis ",plotOutput(outputId = "plot"),HTML("<hr><h4><p><b><i> Performance analysis represent the various performance parameters of the models which have been applied.<br>
                                                                                 Slate Grey represent the -> Accuracy.<br>Light Pink represent the -> Precision1.<br>Tomato represent the -> Precision2.<br>Berlywood represent the -> TPR1.<br>
                                                                                 Royal Blue represent the -> TPR2.</i></b></p></h4>"))
            
            
            ))
    
    
    
))
