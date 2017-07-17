
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
set.seed(101)
shinyServer(function(input, output) {

    anomaly<-read.csv("Dataset_Anomaly_AttributeSelection.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
    banomaly<-read.csv("Dataset_Misuse_AttributeSelection.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
    df=read.csv("result.csv")
    dfm=melt(df,id.vars='X')
    ResultAcc =dfm[which(dfm$variable=="ACC"),]
    ResultPre1=dfm[which(dfm$variable=="PRE1"),]
    ResultPre2=dfm[which(dfm$variable=="PRE2"),]
    ResultTpr1=dfm[which(dfm$variable=="TPR1"),]
    ResultTpr2=dfm[which(dfm$variable=="TPR2"),]
    res=cbind(ResultAcc[,3],ResultPre1[,3],ResultPre2[,3],ResultTpr1[,3],ResultTpr2[,3])
    colnames(res)=c("Accuracy","Precision1","Precision2","TPR1","TPR2")
    rownames(res)=ResultPre1$X
    aRow<-nrow(anomaly)
    aCol<-ncol(anomaly)
    aRow1<-nrow(banomaly)
    aCol1<-ncol(banomaly)
    
    
    
    clickanomaly <- eventReactive(c(input$go),{   
        
        set.seed(101)
        
        sub <- sample(nrow(anomaly), size = nrow(anomaly)*input$trainingdata)
        anomalyTrainingSet = anomaly[sub,]
        anomalyTestSet<- anomaly[-sub,]
        table(anomalyTestSet$X..AttackType.)
        
    })
    
    clickbanomaly <- eventReactive(c(input$go),{
        set.seed(101)
        
        sub1 <- sample(nrow(banomaly), size = nrow(banomaly)*input$trainingdata)
        banomalyTrainingSet<- banomaly[sub1,]
        banomalyTestSet<- banomaly[-sub1,]
        table(banomalyTestSet$X..AttackType.)
        
    })
    
    classifier <- eventReactive(c(input$go),{   
        set.seed(101)
        
        
        sub <- sample(nrow(anomaly), size = nrow(anomaly)*input$trainingdata)
        anomalyTrainingSet = anomaly[sub,]
        anomalyTestSet<- anomaly[-sub,]
        sub1 <- sample(nrow(banomaly), size = nrow(banomaly)*input$trainingdata)
        banomalyTrainingSet<- banomaly[sub1,]
        banomalyTestSet<- banomaly[-sub1,]
        
        if(input$select == 1){
            
            anomalyClassifier=naiveBayes( anomalyTrainingSet$X..AttackType. ~.,data=anomalyTrainingSet)
            anomalyPrediction=predict(anomalyClassifier,anomalyTestSet[,-aCol])
            mmetric(anomalyTestSet$X..AttackType.,anomalyPrediction,c("ACC","PRECISION","TPR",'F1'))
            table(anomalyPrediction,anomalyTestSet$X..AttackType.)
        }
        
        
        else if(input$select == 2){
            anomalyClassifier<- ksvm(anomalyTrainingSet$X..AttackType.~.,data=anomalyTrainingSet,type = 'C-svc', kernel = 'rbfdot')
            anomalyPrediction<-predict(anomalyClassifier, anomalyTestSet[,-aCol])
            mmetric(anomalyTestSet$X..AttackType.,anomalyPrediction,c("ACC","PRECISION","TPR","F1"))
            table(anomalyPrediction,anomalyTestSet$X..AttackType.)
        }
        
        else if(input$select == 3)
        {
            #anomalyClassifier<- ctree(anomalyTrainingSet$X..AttackType.~.,data = anomalyTrainingSet,controls = ctree_control(mincriterion = 0.75,minsplit = 500))
            anomalyClassifier<- rpart(anomalyTrainingSet$X..AttackType.~.,data=anomalyTrainingSet,method = "class")
            anomalyPrediction<-predict(anomalyClassifier, anomalyTestSet)
            mmetric(anomalyTestSet$X..AttackType.,anomalyPrediction,c("ACC","PRECISION","TPR","F1"))
            table(anomalyTestSet$X..AttackType.)    
            
        }
        
        else if(input$select == 4)
        {
            #anomalyClassifier<- glm(anomalyTrainingSet$X..AttackType.~.,data=anomalyTrainingSet, family = binomial("logit"))
            anomalyClassifier<- multinom(anomalyTrainingSet$X..AttackType.~.,data = anomalyTrainingSet)
            anomalyPrediction<-predict(anomalyClassifier, anomalyTestSet[,-aCol])
            mmetric(anomalyTestSet$X..AttackType.,anomalyPrediction,c("ACC","PRECISION","TPR","F1"))
            table(anomalyPrediction,anomalyTestSet$X..AttackType.)
            
        }
        
        else if(input$select == 5){
            
            anomalyClassifier<-ada(anomalyTrainingSet$X..AttackType.~., data=anomalyTrainingSet, type="discrete")
            anomalyPrediction<-predict(anomalyClassifier, anomalyTestSet[,-aCol])
            mmetric(anomalyTestSet$X..AttackType.,anomalyPrediction,c("ACC","PRECISION","TPR","F1"))
            table(anomalyPrediction,anomalyTestSet$X..AttackType.)
        }  
        
        else{
            return()
        }
    }) 
    
    
    
    roc <- eventReactive(c(input$go),{
        
        set.seed(101)
        
        sub <- sample(nrow(anomaly), size = nrow(anomaly)*input$trainingdata)
        anomalyTrainingSet = anomaly[sub,]
        anomalyTestSet<- anomaly[-sub,]
        sub1 <- sample(nrow(banomaly), size = nrow(banomaly)*input$trainingdata)
        banomalyTrainingSet<- banomaly[sub1,]
        banomalyTestSet<- banomaly[-sub1,]
        
        if(input$select == 1){
            anomalyClassifier=naiveBayes( anomalyTrainingSet$X..AttackType. ~.,data=anomalyTrainingSet)
            nbprediction = predict(anomalyClassifier, anomalyTestSet, type='raw')
            score = nbprediction[, 2]
            actual.class = anomalyTestSet$X..AttackType.
            pred = prediction(score, actual.class)
            nb.prff = performance(pred, "tpr", "fpr")
        }
        
        
        else if(input$select == 2){
            anomalyClassifier<- ksvm(anomalyTrainingSet$X..AttackType.~.,data=anomalyTrainingSet,type = 'C-svc', kernel = 'rbfdot',scale=FALSE,prob.model=TRUE)
            nbprediction = predict(anomalyClassifier, anomalyTestSet,type='prob')
            score = nbprediction[, 2]
            actual.class = anomalyTestSet$X..AttackType.
            pred = prediction(score, actual.class)
            nb.prff = performance(pred, "tpr", "fpr")
        }
        
        else if(input$select == 3){
            # anomalyClassifier<- ctree(anomalyTrainingSet$X..AttackType.~.,data = anomalyTrainingSet,controls = ctree_control(mincriterion = 0.99,minsplit = 500))
            anomalyClassifier<- rpart(anomalyTrainingSet$X..AttackType.~.,data=anomalyTrainingSet,method = "class")
            nbprediction = predict(anomalyClassifier, anomalyTestSet, type='prob')
            score = nbprediction[, 2]
            actual.class = anomalyTestSet$X..AttackType.
            pred = prediction(score, actual.class)
            nb.prff = performance(pred, "tpr", "fpr")
        }
        
        
        else if(input$select == 4)
        {
            
            anomalyClassifier<- multinom(anomalyTrainingSet$X..AttackType.~.,data=anomalyTrainingSet,scale=FALSE,prob.model=TRUE)
            nbprediction = predict(anomalyClassifier, anomalyTestSet,type = 'prob')
            score = nbprediction[, 2]
            actual.class = anomalyTestSet$X..AttackType.
            pred = prediction(score, actual.class)
            nb.prff = performance(pred, "tpr", "fpr")
        }
        
        else if(input$select == 5){
            
            anomalyClassifier<-ada(anomalyTrainingSet$X..AttackType.~., data=anomalyTrainingSet, type="discrete")
            nbprediction = predict(anomalyClassifier, anomalyTestSet, type='prob')
            score = nbprediction[, 2]
            actual.class = anomalyTestSet$X..AttackType.
            pred = prediction(score, actual.class)
            nb.prff = performance(pred, "tpr", "fpr")
        }  
        
        else{
            return()
        }
        
        
    })   
    
    
    
    
    Accuracy <- eventReactive(c(input$go),{
        
        set.seed(101)
        
        sub <- sample(nrow(anomaly), size = nrow(anomaly)*input$trainingdata)
        anomalyTrainingSet = anomaly[sub,]
        anomalyTestSet<- anomaly[-sub,]
        sub1 <- sample(nrow(banomaly), size = nrow(banomaly)*input$trainingdata)
        banomalyTrainingSet<- banomaly[sub1,]
        banomalyTestSet<- banomaly[-sub1,]
        
        if(input$select == 1){
            anomalyClassifier=naiveBayes( anomalyTrainingSet$X..AttackType. ~.,data=anomalyTrainingSet)
            anomalyPrediction=predict(anomalyClassifier,anomalyTestSet[,-aCol])
            mmetric(anomalyTestSet$X..AttackType.,anomalyPrediction,c("ACC","PRECISION","TPR"))
        }
        
        
        else if(input$select == 2){
            anomalyClassifier<- ksvm(anomalyTrainingSet$X..AttackType.~.,data=anomalyTrainingSet,type = 'C-svc', kernel = 'rbfdot')
            anomalyPrediction<-predict(anomalyClassifier, anomalyTestSet[,-aCol])
            mmetric(anomalyTestSet$X..AttackType.,anomalyPrediction,c("ACC","PRECISION","TPR"))
        }
        
        else if(input$select == 3)
        {
            #anomalyClassifier<- ctree(anomalyTrainingSet$X..AttackType.~.,data = anomalyTrainingSet,controls = ctree_control(mincriterion = 0.75,minsplit = 500))
            anomalyClassifier<- rpart(anomalyTrainingSet$X..AttackType.~.,data=anomalyTrainingSet,method = "class")
            anomalyPrediction<-predict(anomalyClassifier, anomalyTestSet[,-aCol])
            mmetric(anomalyTestSet$X..AttackType.,anomalyPrediction,c("ACC","PRECISION","TPR"))
        }
        
        else if(input$select == 4)
        {
            anomalyClassifier<- multinom(anomalyTrainingSet$X..AttackType.~.,data=anomalyTrainingSet)
            anomalyPrediction<-predict(anomalyClassifier, anomalyTestSet[,-aCol])
            mmetric(anomalyTestSet$X..AttackType.,anomalyPrediction,c("ACC","PRECISION","TPR"))
        }
        
        else if(input$select == 5){
            
            anomalyClassifier<-ada(anomalyTrainingSet$X..AttackType.~., data=anomalyTrainingSet, type="discrete")
            anomalyPrediction<-predict(anomalyClassifier, anomalyTestSet[,-aCol])
            mmetric(anomalyTestSet$X..AttackType.,anomalyPrediction,c("ACC","PRECISION","TPR"))
            
        }  
        
    })
    
    
    
    
    
    
    classifier1 <- eventReactive(c(input$go),{
        
        set.seed(101)
        
        sub <- sample(nrow(anomaly), size = nrow(anomaly)*input$trainingdata)
        anomalyTrainingSet = anomaly[sub,]
        anomalyTestSet<- anomaly[-sub,]
        sub1 <- sample(nrow(banomaly), size = nrow(banomaly)*input$trainingdata)
        banomalyTrainingSet<- banomaly[sub1,]
        banomalyTestSet<- banomaly[-sub1,]
        
        if(input$select == 1){
            banomalyClassifier<- naiveBayes(banomalyTrainingSet$X..AttackType.~.,data=banomalyTrainingSet)
            banomalyPrediction<-predict(banomalyClassifier, banomalyTestSet[,-aCol1])
            #mmetric(banomalyTestSet$X..AttackType.,banomalyPrediction,c("ACC","PRECISION","TPR","F1"))
            table(banomalyPrediction,banomalyTestSet$X..AttackType.)
        }
        
        else if(input$select == 2){
            banomalyClassifier<- ksvm(banomalyTrainingSet$X..AttackType.~.,data=banomalyTrainingSet,type = 'C-svc', kernel = 'rbfdot')
            banomalyPrediction<-predict(banomalyClassifier, banomalyTestSet[,-aCol1])
            #mmetric(banomalyTestSet$X..AttackType.,banomalyPrediction,c("ACC","PRECISION","TPR","F1")) 
            table(banomalyPrediction,banomalyTestSet$X..AttackType.)
        }
        
        else if(input$select == 3){
            #banomalyClassifier<- ctree(banomalyTrainingSet$X..AttackType.~.,data=banomalyTrainingSet,controls = ctree_control(mincriterion = 0.75,minsplit = 500))
            banomalyClassifier<- rpart(banomalyTrainingSet$X..AttackType.~.,data=banomalyTrainingSet,method = "class")
            banomalyPrediction<-predict(banomalyClassifier, banomalyTestSet[,-aCol1])
            #mmetric(banomalyTestSet$X..AttackType.,banomalyPrediction,c("ACC","PRECISION","TPR","F1"))
            table(banomalyTestSet$X..AttackType.)
        }
        else if(input$select == 4){
            banomalyClassifier<- multinom(banomalyTrainingSet$X..AttackType.~.,data=banomalyTrainingSet)
            banomalyPrediction<-predict(banomalyClassifier, banomalyTestSet[,-aCol1])
            #mmetric(banomalyTestSet$X..AttackType.,banomalyPrediction,c("ACC","PRECISION","TPR","F1"))
            table(banomalyPrediction,banomalyTestSet$X..AttackType.)
        }
        else if(input$select == 5){
            banomalyClassifier<-ada(banomalyTrainingSet$X..AttackType.~., data=banomalyTrainingSet, type="discrete")
            banomalyPrediction<-predict(banomalyClassifier, banomalyTestSet[,-aCol])
            #mmetric(banomalyTestSet$X..AttackType.,bnomalyPrediction,c("ACC","PRECISION","TPR","F1"))
            table(banomalyPrediction,banomalyTestSet$X..AttackType.)   
        } 
        
        
        
    })
    
    
    
    
    
    output$map <- renderPlot({
        
        barplot(classifier(),col=brewer.pal(8,"Set2"),main = "Predicting Graph")
    })
    
    output$map2 <- renderPlot({
        barplot(clickanomaly(),col = brewer.pal(10,"Set3"),main = "Testing Graph")
    })
    
    output$stats <- renderPrint({
        print(classifier()) 
        
    })
    
    
    output$stats2 <- renderPrint({
        print(paste("Training set: ", input$trainingdata*100, "%", sep = ""))
        print(paste("Testing set: ", (1-input$trainingdata)*100, "%", sep = ""))
        print(Accuracy()) 
    })
    
    output$map1 <- renderPlot({
        barplot(classifier1(),col=brewer.pal(10,"Set3"),main = "Predicting Graph")
    })
    
    output$stats1 <- renderPrint({
        print(classifier1()) 
        
    })
    
    output$statsxyz <- renderPrint({
        print(clickanomaly())  
        
    })
    
    output$map3 <- renderPlot({
        barplot(clickbanomaly(),col = brewer.pal(10,"Set3"),main = "Testing Graph")
        
    })
    
    output$roc <- renderPlot({
        plot(roc(),
             colorize=T,
             main = "ROC Curve",
             ylab = "Sensitivity",
             xlab = "1-Specificity")
        abline(a=0, b=1)
        
    })
    
    output$plot <- renderPlot({
        
        barplot(t(res), beside=T, ylab="%" ,legend.text = TRUE,args.legend = list("topright",x =30, y=130, bty = "n"), ylim=c(0,105), col=c("slategray","lightpink","tomato","burlywood","royalblue"),main = "Different Machine Learning Model Analysis")
        
    })
    
    
    

})
