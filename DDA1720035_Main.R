
### Reference documents for understanding the CMS Methodology of rating the hospitals
### https://u.osu.edu/korzen.1/files/2016/08/Star_Rtngs_CompMthdlgy_052016-148w094.pdf
### https://www.cms.gov/medicare/quality-initiatives-patient-assessment-instruments/hospitalqualityinits/hospitalcompare.html

rm(list=ls())
graphics.off() 
par("mar") 
par(mar=c(1,1,1,1))
while (!is.null(dev.list()))  dev.off()

### Tuning the hyper parameters of Random forest model takes a very long time
### The results of the tuning are captured as comments below the respective code.
### The tuning is disabled for now to avoid the slow down
### To check the tuning, please set doTrainRF to TRUE
doTrainRF <- TRUE

### EDA and other plots also take a little longer time
### The plotting is disabled for now
### To enable plot, please set doPlot to TRUE
doPlot <- TRUE

### The measures are saved in CSV for understanding
### Dumping of measures is disabled for now. 
### Please set doDumpMeasuresToCSV to TRUE for enabling the csv dump
doDumpMeasuresToCSV <- TRUE
set.seed(71)

########### Libraries used ##################################
## ------------------------------------------------------------------------
library(reshape2)
## ------------------------------------------------------------------------
library(devtools)
library(ggplot2)
library(stringr)

library(randomForest)
library(caTools)
library(caret)

library(PerformanceAnalytics)
library(corrplot)
library(psych)
library(GPArotation)
library(lavaan)
library(MASS)

library(fpc)

library("clValid")


############## User definded Functions ##################################
## ----fun-----------------------------------------------------------------
#Function for checking the data in each of the fields
ChkField <- function(df, FileName) {
  i = 1
  x1 <- vector(mode = 'character', length = ncol(df))
  x2 <- vector(mode = 'character', length = ncol(df))
  x3 <- vector(mode = 'character', length = ncol(df))
  x4 <- vector(mode = 'character', length = ncol(df))
  x7 <- vector(mode = 'character', length = ncol(df))
  x8 <- vector(mode = 'character', length = ncol(df))
  x9 <- vector(mode = 'character', length = ncol(df))
  
  
  for (x in colnames(df)){
    x1[i] <- sum(is.na(df[[x]]))
    x2[i] <- sum(is.null(df[[x]]))
    x6 <- sapply(df, function(x) length(which(x == ""))) # checking for blank "" values; there are none
    x8[i] <- typeof(df[[x]])
    x9[i] <- sum(na.omit(df[[x]]) == 0)
    
    x4 <- unique(df[[x]])
    x5 <- length(x4)
    x7[i] <- x5
    if (x5>10) {x5=10}
    x3[i] <- paste(x4[1:x5], sep = '\t', collapse = '  #  ') 
    i = i+1
  }
  
  DF2 <- data.frame(field = colnames(df), 
                    Field_Type = x8,
                    NA_val = x1, 
                    Null_val = x2, 
                    unique_val = x7, 
                    top_val = x3, 
                    Zero_len_string = x6, 
                    Num_Zeros = x9,
                    stringsAsFactors = FALSE)
  
  
  write.csv(DF2, paste(fieldDir, FileName))
}

### Plot factor variables
plotFactorVars = function(df, facVars, quantVar, suffix="") {
  textSize=14
  ## Size and position of labels and legends
  plotLabelInfo1 <- theme(axis.text.x = element_text(angle = 90, hjust = 1, size=textSize), 
                          axis.text.y = element_text(size=textSize), axis.title.x = element_text(size=textSize), 
                          axis.title.y = element_text(size=textSize), legend.position = "none", 
                          legend.text = element_text(size=10)) 
  plotLabelInfo2 <- theme(axis.text.x = element_text(angle = 90, hjust = 1, size=textSize), 
                          axis.text.y = element_text(size=textSize), axis.title.x = element_text(size=textSize), 
                          axis.title.y = element_text(size=textSize), legend.position = "top", 
                          legend.text = element_text(size=10))
  
  
  for (i in 1:length(facVars)) {
    name = paste(facVars[i], substr(quantVar, start = 1, stop = 5), sep="_")
    name = paste(name, suffix, sep="_")
    name = paste(name, "jpg", sep=".")
    message(facVars[i])
    plot1 <- ggplot(data=df, aes_string(x=facVars[i], fill=facVars[i])) +
      plotLabelInfo1 + 
      #labs(title = facVars[i]) + 
      geom_bar(stat="count", na.rm = TRUE, width=0.5, col="red" ) +
      geom_text(stat="count", aes(label = ..count..),colour="red", size=4)
    
    #pltTitle <- paste(facVars[i], quantVar, sep=" vs ")
    
    pltTitle <- paste("BVA vs", quantVar, sep=" ")
    
    plot2 <- ggplot(data=df, aes_string(x=facVars[i], fill=quantVar)) + 
      plotLabelInfo2 + 
      labs(title = pltTitle) + 
      geom_bar(stat="count", na.rm = TRUE, width=0.5, col="red" ) + 
      geom_text(stat="count", aes(label = ..count..),colour="red", size=5
      )
    
    #combPlot <- gridExtra::grid.arrange(plot1, plot2, ncol=2)
    message(name)
    ggsave(filename = paste(categPlotDir, name), device="jpeg", plot = plot1, width = 11.69, height = 8.27, units = "in", dpi = 600)
  }
  
}


### Plot quantitative variables
doUVAOfQuantVars = function(df, quaVars, varForBVA="", suffix="") {
  
  textSize=9
  plotLabelInfo1 <- theme(axis.text.x = element_text(angle = 90, hjust = 1, size=textSize), 
                          axis.text.y = element_text(size=textSize), axis.title.x = element_text(size=textSize), 
                          axis.title.y = element_text(size=textSize), legend.position = "none", 
                          legend.text = element_text(size=8), plot.title = element_text(size=12, hjust=0.5)) 
  
  plotLabelInfo2 <- theme(axis.text.x = element_text(angle = 90, hjust = 1, size=textSize), 
                          axis.text.y = element_text(size=textSize), axis.title.x = element_text(size=textSize), 
                          axis.title.y = element_text(size=textSize), legend.position = "top", 
                          legend.text = element_text(size=8), plot.title = element_text(size=12, hjust=0.5)) 
  
  
  ## Plot the boxplot and histogram of the integer and numeric variables.
  for (i in 1:length(quaVars)) {    
    pltName = paste(quaVars[i], substr(varForBVA, start = 1, stop = 5), sep="_")
    if (suffix != "") pltName = paste(pltName, suffix, sep="_")
    pltName = paste(pltName, "jpg", sep=".")
    
    show(pltName)
    message(quaVars[i])
    dr <- max(df[,quaVars[i]], na.rm = TRUE)-min(df[,quaVars[i]], na.rm = TRUE)
    show(paste("Data range of variable = ", dr))
    ## Dynamically select the binwidth for plotting based on the dynamic range of the data.
    bw = 1
    if (dr>10000) {
      bw = 25000
    } else if (dr>100) {
      bw = 10
    } else if (dr>50) {
      bw = 5
    } else if (dr>20) {
      bw = 5
    } else if (dr<10) {
      bw=0.1
    }
    show(paste("Selected binwidth = ", bw))  
    
    plot1 <- ggplot(data=df, aes_string(x=factor(0), y=quaVars[i])) + 
      plotLabelInfo1+ 
      labs(title = quaVars[i]) + xlab("") +
      scale_x_discrete(breaks = NULL) + coord_flip() +
      geom_boxplot(stat="boxplot", na.rm = TRUE, col = 'blue', 
                   fill="turquoise", outlier.colour = "red" )
    
    plot2 <- ggplot(data=df, aes_string(x=varForBVA, y=quaVars[i], fill=varForBVA)) + 
      plotLabelInfo2 + 
      labs(title = paste("BVA vs", varForBVA, sep=" ")) + 
      scale_x_discrete()  + coord_flip() +
      geom_boxplot(stat="boxplot", na.rm = TRUE, col = 'blue', outlier.colour = "red" )
    
    plot3 <- ggplot(data=df, aes_string(x=quaVars[i], fill=varForBVA)) + 
      plotLabelInfo2 + 
      labs(title = paste("BVA vs", varForBVA, sep=" ")) + 
      geom_histogram(stat = 'bin', position = "fill", binwidth = bw, col='red')
    
    plot4 <- ggplot(data=df, aes_string(x=quaVars[i], fill=varForBVA)) + 
      plotLabelInfo2 + 
      labs(title = paste("BVA vs", varForBVA, sep=" ")) + 
      geom_histogram(stat = 'bin', binwidth = bw, col='red')
    
    combPlot <- gridExtra::grid.arrange(plot1, plot2, plot3, plot4, ncol=2, nrow=2)
    
    ggsave(filename = paste(quaPlotDir, pltName), device="jpeg", plot = combPlot, 
           width = 20, height = 20, units = "cm", dpi = 300)
    
  }
}

### Plot quantitative variables
doBVAOfQuantVars = function(df, quaVars, varForBVA1="", 
                            varForBVA2="", hosp_scores, suffix="") {
  
  textSize=12
  plotLabelInfo1 <- theme(axis.text.x = element_text(angle = 90, hjust = 1, size=textSize), 
                          axis.text.y = element_text(size=textSize), axis.title.x = element_text(size=textSize), 
                          axis.title.y = element_text(size=textSize), legend.position = "none", 
                          legend.text = element_text(size=8), plot.title = element_text(size=12, hjust=0.5)) 
  
  plotLabelInfo2 <- theme(axis.text.x = element_text(angle = 90, hjust = 1, size=textSize), 
                          axis.text.y = element_text(size=textSize), axis.title.x = element_text(size=textSize), 
                          axis.title.y = element_text(size=textSize), legend.position = "top", 
                          legend.text = element_text(size=8), plot.title = element_text(size=12, hjust=0.5)) 
  
  
  ## Plot the Boxplots of the integer and numeric variables. 
  for (i in 1:length(quaVars)) {    
    pltName = paste(quaVars[i], substr(varForBVA1, start = 1, stop = 5), sep="_")
    if(varForBVA2 != "") pltName = paste(pltName, substr(varForBVA2, start = 1, stop = 5), sep="_")
    if(suffix != "") pltName = paste(pltName, suffix, sep="_")
    pltName = paste(pltName, "jpg", sep=".")
    
    show(pltName)
    message(quaVars[i])
    dr <- max(df[,quaVars[i]], na.rm = TRUE)-min(df[,quaVars[i]], na.rm = TRUE)
    show(paste("Data range of variable = ", dr))
    ## Dynamically select the binwidth for plotting based on the dynamic range of the data.
    bw = 1
    if (dr>10000) {
      bw = 25000
    } else if (dr>100) {
      bw = 10
    } else if (dr>50) {
      bw = 5
    } else if (dr>20) {
      bw = 5
    } else if (dr<10) {
      bw=0.1
    }
    show(paste("Selected binwidth = ", bw))  
    txt1 <- data.frame(x = 5, y = hosp_scores[i], lab = "Evanstone score")
    txt2 <- data.frame(x = 3, y = mean(df[,quaVars[i]]), lab = "National rate")
    
    plot1 <- ggplot(data=df, aes_string(y=quaVars[i], x=varForBVA1, fill=varForBVA1)) + 
      plotLabelInfo2 + 
      labs(title = paste("BVA vs", varForBVA1, sep=" ")) + 
      scale_x_discrete()  + coord_flip() +
      geom_boxplot(stat="boxplot", na.rm = TRUE, col = 'blue', outlier.colour = "red" ) +
      geom_hline(aes(yintercept=as.numeric(hosp_scores[i])), colour="plum2", linetype="dashed", lwd=1) +
      geom_hline(aes(yintercept=mean(df[,quaVars[i]])), colour="cyan1", lwd=1) + 
      annotate("text", label = "Evanstone score", 
               x = 3, y = as.numeric(hosp_scores[i]), 
               angle = 90, hjust = 1, vjust = 1, size=4) +
      annotate("text", label = "National Mean", 
               x = 4, y = mean(df[,quaVars[i]]), 
               angle = 90, hjust = 1, vjust = 1, size=4) 
    
    if (varForBVA2 != "") {
      plot2 <- ggplot(data=df, aes_string(y=quaVars[i], x=varForBVA2, fill=varForBVA2)) + 
        plotLabelInfo2 + 
        labs(title = paste("BVA vs", varForBVA2, sep=" ")) + 
        scale_x_discrete()  + coord_flip() +
        geom_boxplot(stat="boxplot", na.rm = TRUE, col = 'blue', outlier.colour = "red" ) +
        geom_hline(aes(yintercept=as.numeric(hosp_scores[i])), colour="plum2", linetype="dashed", lwd=1) +
        geom_hline(aes(yintercept=mean(df[,quaVars[i]])), colour="cyan1", lwd=1) + 
        annotate("text", label = "Evanstone score", 
                 x = 3, y = as.numeric(hosp_scores[i]), 
                 angle = 90, hjust = 1, vjust = 1, size=4) +
        annotate("text", label = "National Mean", 
                 x = 4, y = mean(df[,quaVars[i]]), 
                 angle = 90, hjust = 1, vjust = 1, size=4) 
      
      combPlot <- gridExtra::grid.arrange(plot1, plot2, ncol=2, nrow=1)
      
      ggsave(filename = paste(quaPlotDir, pltName), device="jpeg", plot = combPlot, 
             width = 20, height = 20, units = "cm", dpi = 300)
    } else {
      ggsave(filename = paste(quaPlotDir, pltName), device="jpeg", plot = plot1, 
             width = 20, height = 20, units = "cm", dpi = 300)
      
    }
  }
}

### Plot quantitative variables
doBVAHistOfQuantVars = function(df, quaVars, varForBVA1="", 
                                varForBVA2="", suffix="") {
  
  textSize=12
  plotLabelInfo1 <- theme(axis.text.x = element_text(angle = 90, hjust = 1, size=textSize), 
                          axis.text.y = element_text(size=textSize), axis.title.x = element_text(size=textSize), 
                          axis.title.y = element_text(size=textSize), legend.position = "none", 
                          legend.text = element_text(size=8), plot.title = element_text(size=12, hjust=0.5)) 
  
  plotLabelInfo2 <- theme(axis.text.x = element_text(angle = 90, hjust = 1, size=textSize), 
                          axis.text.y = element_text(size=textSize), axis.title.x = element_text(size=textSize), 
                          axis.title.y = element_text(size=textSize), legend.position = "top", 
                          legend.text = element_text(size=8), plot.title = element_text(size=12, hjust=0.5)) 
  
  
  ## Plot the histogram of the integer and numeric variables. 
  
  for (i in 1:length(quaVars)) {    
    pltName = paste(quaVars[i], substr(varForBVA1, start = 1, stop = 5), sep="_")
    if(varForBVA1 != "") pltName = paste(pltName, substr(varForBVA2, start = 1, stop = 5), sep="_")
    if(suffix != "") pltName = paste(pltName, suffix, sep="_")
    pltName = paste(pltName, "jpg", sep=".")
    
    show(pltName)
    message(quaVars[i])
    dr <- max(df[,quaVars[i]], na.rm = TRUE)-min(df[,quaVars[i]], na.rm = TRUE)
    show(paste("Data range of variable = ", dr))
    ## Dynamically select the binwidth for plotting based on the dynamic range of the data.
    bw = 1
    if (dr>10000) {
      bw = 25000
    } else if (dr>100) {
      bw = 10
    } else if (dr>50) {
      bw = 5
    } else if (dr>20) {
      bw = 5
    } else if (dr<10) {
      bw=0.1
    }
    show(paste("Selected binwidth = ", bw))  
    plot1 <- ggplot(data=df, aes_string(x=quaVars[i], fill=varForBVA1)) +
      plotLabelInfo2 +
      labs(title = paste("BVA vs", varForBVA1, sep=" ")) + 
      geom_histogram(stat = 'bin', binwidth = bw, col='red')
    
    if (varForBVA2 != "") {
      plot2 <- ggplot(data=df, aes_string(x=quaVars[i], fill=varForBVA2)) +
        plotLabelInfo2 +
        labs(title = paste("BVA vs", varForBVA2, sep=" ")) +
        geom_histogram(stat = 'bin', binwidth = bw, col='red')
      
      combPlot <- gridExtra::grid.arrange(plot1, plot2, ncol=2, nrow=1)
      
      ggsave(filename = paste(quaPlotDir, pltName), device="jpeg", plot = combPlot, 
             width = 20, height = 20, units = "cm", dpi = 300)
    } else {
      ggsave(filename = paste(quaPlotDir, pltName), device="jpeg", plot = plot1, 
             width = 20, height = 20, units = "cm", dpi = 300)
      
    }
    
  }
}

### Supervised Learning using Random forest 
### This function splits the data into training and test data
### Trains the RF model on the training data
### Prints the confusion matrix of the comparison of predicted data against the test data
createRFModel <- function(temp, comparisonColIdx) {
  Y = temp[,colnames(temp)[comparisonColIdx]]
  # Split the data into train and test
  inSamples <- sample.split(Y, SplitRatio = 7/10)
  trainData <- temp[inSamples,]
  testData <- temp[!(inSamples),]
  # Build the random forest
  trainData <- droplevels(trainData)
  data.rf <- randomForest(as.formula(paste(colnames(temp)[comparisonColIdx], ".", sep = " ~ ")), data=trainData, proximity=FALSE,
                          ntree=1000, mtry=5, do.trace=FALSE, na.action=na.omit)
  data.rf
  testPred <- predict(data.rf, newdata=testData)
  cat(paste("########## \n", colnames(temp)[comparisonColIdx], sep = " " ))
  print(confusionMatrix(testPred, testData[,colnames(temp)[comparisonColIdx]]))
  return(data.rf)
}
### Tuning the hyper parameters of Random forest 
### This function splits the data into training and test data
### Tunes the hyper parameters by training on the training data
### Prints the confusion matrix of the comparison of predicted data against the test data
### using the model with optimal hyper parameters obtained from tuning
tuneRFParams <- function(temp, comparisonColIdx) {
  Y = temp[,colnames(temp)[comparisonColIdx]]
  # Split the data into train and test
  inSamples <- sample.split(Y, SplitRatio = 7/10)
  trainData <- temp[inSamples,]
  testData <- temp[!(inSamples),]
  # Build the random forest
  trainData <- droplevels(trainData)
  # Random Search
  metric <- "Accuracy"
  control <- trainControl(method="repeatedcv", number=5, repeats=3, search="random")
  mtry <- sqrt(ncol(within(temp,rm("Hospital.overall.rating"))))
  tunegrid <- expand.grid(.mtry=c(5:sqrt(ncol(within(temp,rm("Hospital.overall.rating"))))))
  rf_random <- train(Hospital.overall.rating~., data=trainData, method="rf", 
                     metric=metric, trControl=control)
  print(rf_random)
  plot(rf_random)
  
  testPred <- predict(rf_random, newdata=testData)
  cat(paste("########## \n", colnames(temp)[comparisonColIdx], sep = " " ))
  print(confusionMatrix(testPred, testData[,colnames(temp)[comparisonColIdx]]))
  return (rf_random$finalModel)
}

### Helper functions for plotting the correlation matrix
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

### Plots the correlation matrix
plotCorr <- function(df, selVars, fileName, name) {
  library(GGally)
  ### Compute correlation and plot it
  #resCorr <- cor(df[,selVars], use="pairwise.complete.obs")
  
  #cormat <- round(cor(df[,selVars], use="pairwise.complete.obs"),2)
  cormat <- round(cor(df[,selVars], use="everything"),2)
  # Reorder the correlation matrix
  cormat <- reorder_cormat(cormat)
  
  upper_tri <- get_upper_tri(cormat)
  # Melt the correlation matrix
  melted_cormat <- melt(upper_tri, na.rm = TRUE)
  
  
  ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal()+ # minimal theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1))+
    coord_fixed() 
  corplot <- ggheatmap + 
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.6, 0.7),
      legend.direction = "horizontal")+
    labs(title = name) +
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5))
  
  
  ggsave(filename = fileName, device="jpeg", plot = corplot, 
         width = 20, height = 20, units = "cm", dpi = 300)
  
}

### Function to remove the outliers below 5% and about 95%
removeOutliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.05, .95), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- qnt[1]
  y[x > (qnt[2] + H)] <- qnt[2]
  return(y)
}

### Supervised learning using SVM
doSVMRegression <- function(temp, Y) {
  
  # Split the data into train and test
  inSamples <- sample.split(temp[,Y], SplitRatio = 7/10)
  trainData <- temp[inSamples,]
  testData <- temp[!(inSamples),]
  svmMod <- paste(colnames(temp)[Y], ".", sep = " ~ ")
  Model_linear <- ksvm(as.formula(svmMod), data = trainData, scale = TRUE, 
                       kernel = "vanilladot", type="eps-svr",
                       prob.model=TRUE)
  
  #Evaluate the model on the test data
  Eval_linear = predict(Model_linear, type = "response", 
                        newdata = testData)
  
  actuals_preds <- data.frame(cbind(actuals=testData[,Y], predicteds=Eval_linear[,1]))  # make actuals_predicteds dataframe.
  correlation_accuracy <- cor(actuals_preds)  # 82.7%
  head(actuals_preds)
  
  min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
  mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
  print(min_max_accuracy)
  print(mape)
  return(list(Model_linear))
}

############## Main Code beginning ###############
## ----Create Temp Dirs----------------------------------------------------
#set directories for saving temporary files 
mainDir <- getwd()
subDir <- "output/"
if (!dir.exists(file.path(mainDir, subDir))) dir.create(file.path(mainDir, subDir), showWarnings = TRUE)
plotDir <- "output/plots/"
if (!dir.exists(file.path(mainDir, plotDir))) dir.create(file.path(mainDir, plotDir), showWarnings = TRUE)
categPlotDir <- "output/plots/Categorical/"
if (!dir.exists(file.path(mainDir, categPlotDir))) dir.create(file.path(mainDir, categPlotDir), showWarnings = TRUE)
quaPlotDir <- "output/plots/quantitative/"
if (!dir.exists(file.path(mainDir, quaPlotDir))) dir.create(file.path(mainDir, quaPlotDir), showWarnings = TRUE)
fieldDir <- "ChkField/"
if (!dir.exists(file.path(mainDir, fieldDir))) dir.create(file.path(mainDir, fieldDir), showWarnings = TRUE)
modelDir <- "output/models/"
if (!dir.exists(file.path(mainDir, modelDir))) dir.create(file.path(mainDir, modelDir), showWarnings = TRUE)

## ------------------------------------------------------------------------
## map_df dataframe is used to extract measures from the input data files
## Has a list of measures/columns to be extracted from each data file for
## each of the group
map_df  = data.frame(matrix(ncol = 5, nrow = 9))
colnames(map_df) <-  c("group", "df_name", "FileName", "measures", 
                       "columns_in_files")

### Column indicating the group that the extracted measures belong to
map_df$group = c("Mortality",
                 "Mortality",
                 "Safety of care",
                 "Safety of care",
                 "Readmission",
                 "Patient experience",
                 "Effectiveness of care",
                 "Timeliness of care",
                 "Efficient use of medical imaging")

### Column indicating the name of the group data frame that the extracted 
### meaures will be written to
map_df$df_name<- c("df_mortality", 
                   "df_mortality", 
                   "df_safetyofcare", 
                   "df_safetyofcare", 
                   "df_readmission", 
                   "df_patientexp", 
                   "df_effectivecare", 
                   "df_timeliness", 
                   "df_imaging")


### Column indicating the input data files containing the measure
map_df$FileName <- c("Complications - Hospital.csv", 
                     "Readmissions and Deaths - Hospital.csv", 
                     "Healthcare Associated Infections - Hospital.csv", 
                     "Complications - Hospital.csv", 
                     "Readmissions and Deaths - Hospital.csv", 
                     "HCAHPS - Hospital.csv", 
                     "Timely and Effective Care - Hospital.csv", 
                     "Timely and Effective Care - Hospital.csv", 
                     "Outpatient Imaging Efficiency - Hospital.csv")

### Column indicating the list of columns that are to be extracted from the input
### data files
map_df$columns_in_files <- list(c("Measure.ID", "Score"), 
                                c("Measure.ID", "Score"),
                                c("Measure.ID", "Score"), 
                                c("Measure.ID", "Score"),
                                c("Measure.ID", "Score"),
                                c("HCAHPS.Measure.ID", "HCAHPS.Linear.Mean.Value"),
                                c("Measure.ID", "Score"),
                                c("Measure.ID", "Score"),
                                c("Measure.ID", "Score"))

### Column indicating the measures that will be extracted from each of the files
map_df$measures <- list(c("PSI_4_SURG_COMP"),
                        c("MORT_30_AMI","MORT_30_CABG","MORT_30_COPD","MORT_30_HF",
                          "MORT_30_PN","MORT_30_STK"),
                        c("HAI_1_SIR","HAI_2_SIR","HAI_3_SIR","HAI_4_SIR","HAI_5_SIR",
                          "HAI_6_SIR"),
                        c("PSI_90_SAFETY","COMP_HIP_KNEE"),
                        c("READM_30_AMI","READM_30_CABG","READM_30_COPD","READM_30_HF",
                          "READM_30_HIP_KNEE","READM_30_HOSP_WIDE","READM_30_PN",
                          "READM_30_STK"),
                        c("H_CLEAN_LINEAR_SCORE","H_COMP_1_LINEAR_SCORE",
                          "H_COMP_2_LINEAR_SCORE","H_COMP_3_LINEAR_SCORE",
                          "H_COMP_4_LINEAR_SCORE","H_COMP_5_LINEAR_SCORE",
                          "H_COMP_6_LINEAR_SCORE","H_COMP_7_LINEAR_SCORE",
                          "H_QUIET_LINEAR_SCORE","H_RECMND_LINEAR_SCORE",
                          "H_HSP_RATING_LINEAR_SCORE"),
                        c("CAC_3","IMM_2","IMM_3_OP_27_FAC_ADHPCT","OP_4","OP_22",
                          "OP_23","PC_01","STK_1","STK_4","STK_6","STK_8","VTE_1",
                          "VTE_2","VTE_3","VTE_5","VTE_6","OP_29","OP_30"),
                        c("ED_1b","ED_2b","OP_3b", "OP_5","OP_18b","OP_20","OP_21"),
                        c("OP_10", "OP_11", "OP_13", "OP_14", "OP_8"))


### Direction of the measures used in standardization of the direction of the measures
### Those Measures are assigned -ve sign - lower the values, better
### Those Measures are assigned +ve sign - Higher the values, better
measures_dir = list(c(-1, -1, -1, -1, -1, -1, -1), #Mortality
                    c(-1, -1, -1, -1, -1, -1, -1, -1), #Safety of care
                    c(-1, -1, -1, -1, -1, -1, -1, -1), #Readmission
                    c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), #Patient experience
                    c(1, 1, 1, 1, -1, #Effectiveness of care
                      1, -1, 1, 1, 1, 1, 1,
                      1, 1, 1, -1, 1, 1),
                    c(-1, -1, -1, -1, -1, -1, -1), #Timeliness of care
                    c(-1, -1, -1, -1, -1)) #Imaging

## ------------------------------------------------------------------------
### Declare empty data frames for each of the group
InputDir <- file.path(getwd(), '/Hospital_Revised_FlatFiles_20161110/')

for(i in (1:nrow(map_df))){
  ### Read the input data files
  df <- read.csv(file.path(InputDir, map_df$FileName[i]))
  
  ### Get the measures to be extracted from the files
  measureList <- map_df$measures[i]
  
  reqColumns <- c("Provider.ID", unlist(map_df$columns_in_files[i]))
  
  ### Extract the required columns from the files
  filtered_df <- subset(df, select=reqColumns)
  
  colnames(filtered_df) <- c("Provider.ID","Measure.ID","Score")
  
  ### Widen -> Convert the measures in rows to columns
  filtered_df <- dcast(filtered_df, Provider.ID ~ Measure.ID, value.var = "Score") 
  
  ### Filter the required measures 
  filtered_df <- subset(filtered_df, select = c("Provider.ID", unlist(measureList)))
  
  ### Convert the measures to numeric values - Missing values will be automatically converted to NA
  filtered_df <- as.data.frame(sapply(filtered_df, as.numeric))
  
  ### Check if the group data frame already exists
  if(exists(map_df$df_name[i])) {
    ### If the group data frame already exists, merge the extracted meaures to the group dataframe
    temp = get(map_df$df_name[i])
    filtered_df <- merge(filtered_df, temp)
  }
  
  ### Rename the temporary dataframe back to the group data frame
  assign(map_df$df_name[i], filtered_df)
  
}

### Read the Hospital general information csv file that has the overall hospital rating
### and the comparison of group scores against the National rate
hosp_gen_df <- read.csv(file.path(InputDir, "Hospital General Information.csv"))

### Remove the columns that are not needed for the analysis
colsToBeRemoved <- c("Hospital.Name", "Address", "ZIP.Code", "City", "County.Name", "Phone.Number")
hosp_gen_df <- hosp_gen_df[,!colnames(hosp_gen_df) %in% colsToBeRemoved]
hosp_gen_df <- hosp_gen_df[, -grep(colnames(hosp_gen_df),pattern = "footnote")]

####################################################################
### Plot Group score National comparison vs Hospital rating
###############################################################
temp <-  hosp_gen_df[, grep(colnames(hosp_gen_df),pattern = "comparison")]
temp$Hospital.overall.rating <- hosp_gen_df$Hospital.overall.rating

temp1 <- melt(temp, id.vars='Hospital.overall.rating')
head(temp1)
temp1$variable <- gsub(temp1$variable, pattern = ".national.comparison",
                       replacement="")
temp1$value <- ifelse(str_detect(temp1$value, "Above"), "Above",
                      ifelse(str_detect(temp1$value, "Below"), "Below",
                             ifelse(str_detect(temp1$value, "Same"), "Same", temp1$value)))
plot1 <- ggplot(data=temp1, aes(x=value, fill=variable)) + 
  geom_bar(stat="count", na.rm = TRUE, position = "dodge") +
  labs(title = "Group National Comparison vs Hospital Rating") +
  facet_wrap(~Hospital.overall.rating, scales = "free") + xlab("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=10), 
        axis.text.y = element_text(size=10), axis.title.x = element_text(size=10), 
        axis.title.y = element_text(size=10), legend.position = "bottom", 
        legend.text = element_text(size=10), plot.title = element_text(size=12, hjust=0.5)) 
ggsave(filename = paste(categPlotDir, "Group National Comparison vs Hospital Rating.jpg"), 
       device="jpeg", plot = plot1, 
       width = 10, height = 8, units = "in", dpi = 600)
############################################################################

### Extract from the Hospital general information file and
### append the hospital overall rating and the respective group score comparison to the 
### group data frame
for (i in 1:nrow(map_df)) {
  temp = get(map_df$df_name[i])
  ###  Check if the dataframe has already hospital overall rating. 
  ### If Yes, go to the next dataframe
  if ("Hospital.overall.rating" %in%colnames(temp) == 0) {
    temp <- as.data.frame(sapply(temp, as.numeric))
    ### Append Hospital overall rating and group rating to the respective group dataframe
    
    ## Get the group name
    groupName=gsub(map_df$group[i],pattern=" ", replacement = ".")
    
    ## Get the respective National comparison from the hospital general information data
    colIdx = grep(groupName, colnames(hosp_gen_df))
    
    ## Also fetch the Provider ID and Hospital rating from the hospital gen data
    reqColumns = c("Provider.ID", "Hospital.overall.rating", colnames(hosp_gen_df)[colIdx])
    
    ## Merge the extracted measures and the columns extracted from the hospital gen info
    temp <- merge(temp, subset(hosp_gen_df, select =reqColumns))
    
    ## Store the group measures along with the hospital rating and the national comparison score back to 
    ## group data frame
    assign(map_df$df_name[i], temp)
    
    if (doDumpMeasuresToCSV) {
      write.csv(file=paste(map_df$df_name[i],"csv",sep = "."), temp)
    }
  }
}

### Do the EDA for each of the measures group wise

### Get the list of group data frames
group_df_list <- unique(map_df$df_name)
group_na_count <- list()
for(i in 1:length(group_df_list)) {
  
  temp <- get(group_df_list[i])
  dir_meas <- measures_dir[[i]]
  ### For each group, add a column that contains the count of the number of measures that are not available
  temp[,paste(group_df_list[i],"numMeas",sep = "_")] <- apply(temp, 1, function(x) ncol(temp)-3-sum(is.na(x)))
  
  ### Get the Integer/Numeric columns to do the Winsorization
  #dt <- getDataTypesOfDF(temp)
  dt <- as.data.frame(sapply(temp, class)) #getDataTypesOfDF(temp)
  colnames(dt)[1] <- 'class'
  dt$measures <-  row.names(dt)
  selVars <- dt$measures[dt$class == 'numeric'|dt$class == 'integer']  
  selVars <- selVars[which(selVars!="Provider.ID")]
  selVars <- selVars[which(str_detect(selVars,"_numMeas")==0)]
  print(length(selVars))
  print(selVars)
  ### To disable plotting, set doPlot to False at the beginning of this file.
  if (doPlot) {
    
    ### Plot the univariate and bivariate plots of the measures against the 
    ## hospital rating - TBD if this is required.
    doUVAOfQuantVars(temp, selVars, varForBVA = "Hospital.overall.rating") 
    facVars = dt$measures[dt$class == 'factor']
    facVars <- c(facVars, dt$measures[which(str_detect(dt$measures,"_numMeas"))])
    plotFactorVars(df = temp, facVars, quantVar = "Hospital.overall.rating")
  }
  ### Winsorization of the measures
  ### Standardization of measures need the  national mean of measure scores
  ### National mean of measures scores can be fetched from ******-national.csv.
  ### Compared the computed mean of scores of all the hospitals against the national
  ### mean from the csv file. They are very close. 
  ### So using the computed mean instead of fetching the mean from the national.csv
  ###
  ### Missing values are imputed with the median of the measures before
  ### winsorization
  
  count_na <- vector(length=length(selVars))
  for (j in 1:length(selVars)) {
    show(selVars[j])
    scaledVec <- temp[,selVars[j]] * dir_meas[j]
    scaledVecRatedHosp <- scaledVec[which(temp$Hospital.overall.rating!="Not Available")]
  
    ### Compute the % of missing values in each measure - Used later for tuning the accuracy
    count_na[j] <- length(which(is.na(scaledVecRatedHosp)))/length(scaledVecRatedHosp)*100
    
    ## Z Score normalization
    scaledVec <- (scaledVec - mean(scaledVec, na.rm = TRUE))/sd(scaledVec, na.rm = TRUE)

    ## Do the outlier treatment 
    scaledVec[scaledVec < -3.0] = -3.0
    scaledVec[scaledVec > 3.0] = 3.0
    
    ## Impute the missing values with the median of the meaures
    scaledVec[is.na(scaledVec)] = median(scaledVec, na.rm = TRUE)
    
    temp[,selVars[j]] <- scaledVec
  }
  ### Store the % missing values of each measure in  a list
  count_na = round(count_na, 2)
  names(count_na) = selVars
  group_na_count = c(group_na_count, list(count_na))
  
  ### Write the group data frame with scaled, missing value imputed, outlier treated
  ### data frame back to the respective group data frame
  assign(group_df_list[i], temp)
  
  ChkField(temp, FileName = paste(group_df_list[i],"csv",sep = "."))
  if (doDumpMeasuresToCSV) {
    write.csv(file=paste(paste(map_df$df_name[i],"afterImputation", sep = "_"),"csv",sep = "."), temp)
  }
  
}

#### Creation of Master Data frame with all the measures, 
#### group national level comparison and overall hospital rating
master_df <- get(group_df_list[1])
for(i in 2:length(group_df_list)) {
  master_df <- merge(master_df, get(group_df_list[i]))
}

if (doDumpMeasuresToCSV) {
  write.csv(file = "master_df.csv", master_df)
}

###################################################
###Analysis of hospitals that cannot be rated
####################################################
isGroupMissing <- data.frame(matrix(ncol = 7, nrow  = nrow(master_df)))
for(i in 1:length(group_df_list)) {
  temp <- get(group_df_list[i])
  numMeasuresInGroup <- temp[, grep(colnames(temp),pattern = "_numMeas")]
  isGroupMissing[,i] <-  as.numeric(numMeasuresInGroup<3)
}
isGroupMissing$total_groups_missing <- apply(t(as.matrix(isGroupMissing)),2,sum)
isGroupMissing$outcome_groups_missing <- (isGroupMissing[1]+isGroupMissing[2]+isGroupMissing[3])
isGroupMissing$Provider.ID <- master_df$Provider.ID
isGroupMissing$Hospital.overall.rating <- master_df$Hospital.overall.rating

temp <- subset(isGroupMissing, Hospital.overall.rating == "Not Available")
naHospitalIds <- which(isGroupMissing$total_groups_missing>4 | isGroupMissing$outcome_groups_missing==3)
message("Num hospitals that are not rated as per the input data : ")
length(naHospitalIds)                          
message("Num hospitals that are not rated based on the missing measures : ")
nrow(temp)             

### 1128 out of 1170 hospitals are not rated based on the following criteria
###   Minimum number of measures per group should be less than 3
###   Atleast one of outcome groups should be present
###   Minimum number of 3 groups must be present for a hospital to be rated
###
### The remaining 42 hospitals are not rated possibly because the measures
### reported do not meet the minimum threshold for a measure criteria
### 
#######################################################

##### Supervised and Unsupervised learning of hospitals that are rated by CMS

### Filter out not rated hospitals. Use only the rated hospitals for 
### both the Supervised and Unsupervised learning
master_df <- subset(master_df, Hospital.overall.rating!="Not Available") 
df_mortality <- subset(df_mortality, Hospital.overall.rating!="Not Available") 
df_readmission <- subset(df_readmission, Hospital.overall.rating!="Not Available") 
df_safetyofcare <- subset(df_safetyofcare, Hospital.overall.rating!="Not Available") 
df_effectivecare <- subset(df_effectivecare, Hospital.overall.rating!="Not Available") 
df_timeliness <- subset(df_timeliness, Hospital.overall.rating!="Not Available") 
df_imaging <- subset(df_imaging, Hospital.overall.rating!="Not Available") 
df_patientexp <- subset(df_patientexp, Hospital.overall.rating!="Not Available") 


### Correlation plots - saved to files
### Bivariate analysis plots of measures with respect to Hospital rating (For presentation)
if (doPlot) {
  for(i in 1:length(group_df_list)) {
    temp <- get(group_df_list[i])
    temp <- temp[, -grep(colnames(temp),pattern = "Provider")]
    temp <- temp[, -grep(colnames(temp),pattern = "comparison")]
    temp1 <- temp[, -grep(colnames(temp),pattern = "_numMeas")]
    temp1 <- temp1[, -grep(colnames(temp1),pattern = "overall")]
    groupCor <- cor(temp1, use= "complete.obs")
    plotCorr(temp1, selVars = colnames(temp1), 
             paste(paste(quaPlotDir, group_df_list[i], sep = "Correlation_Plot_"), "jpg", sep="."),
             name = group_df_list[i])
    temp1 <- melt(temp, id.vars='Hospital.overall.rating')
    temp1$variable <- gsub(temp1$variable, pattern = ".national.comparison",
                           replacement="")
    temp1$value <- ifelse(str_detect(temp1$value, "Above"), "Above",
                          ifelse(str_detect(temp1$value, "Below"), "Below",
                                 ifelse(str_detect(temp1$value, "Same"), "Same", temp1$value)))
    hospital_rating <- list("Hospital Rating 1","Hospital Rating 2", "Hospital Rating 3",
                            "Hospital Rating 4", "Hospital Rating 5")
    
    plot1 <- ggplot(data=temp1, aes(y=value, x=variable)) + 
      geom_boxplot(stat="boxplot", na.rm = TRUE, col = 'blue', outlier.colour = "red" ) +
      labs(title = paste(substr(group_df_list[i],start = 4, stop = nchar(group_df_list[i])), "BVA with Hospital Rating", sep = " ")) +
      facet_wrap(~Hospital.overall.rating, scales = "free", labeller = as_labeller(hospital_rating)) + xlab("") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size=10), 
            axis.text.y = element_text(size=10), axis.title.x = element_text(size=14), 
            axis.title.y = element_text(size=10), legend.position = "bottom", 
            legend.text = element_text(size=10), plot.title = element_text(size=12, hjust=0.5, face="bold")) 
    ggsave(filename = paste(paste(quaPlotDir, group_df_list[i], sep = "Box_Plot_"), "jpg", sep="."), 
           device="jpeg", plot = plot1, 
           width = 10, height = 8, units = "in", dpi = 600)
  }
}

### Two approaches for the Random forest model to predict the hospital ratings
### based on the measures


### Approach 1:
###   Create a model for each of the group to predict the Group score comparison to the national rate
###   based on the respective group measures
###   Create a model to predict the hospital rating based on the group score comparison to 
###   national rate
###   Number of output models  - 7 + 1 -> 8 models

### Approach 2:
###   Create a model to predict the hospital rating based on the 64 measures
###   Number of models - 1 
###

### Compare the accuracy of Approach 1 and 2 and select the one that gives the best hospital rating


### Approach 1:
### Do random forest model for each of the group with the target variable
### as the respective group score comparison against the national mean

for(i in 1:length(group_df_list)) {
  
  temp <- get(group_df_list[i])
  
  ## Remove Provider.ID, Hospital.overall.rating and Number of measures 
  ## columns that are not used for this analysis
  
  temp <- temp[, -grep(colnames(temp),pattern = "Provider")]
  temp <- temp[, -grep(colnames(temp),pattern = "overall")]
  temp <- temp[, -grep(colnames(temp),pattern = "_numMeas")]
  
  comparisonColIdx = grep("national", colnames(temp))
  createRFModel(temp, comparisonColIdx)  
}
### Now we have random forest model that predicts the group score comparison
### with the respective measures as the independent variables

### Build a random forest model to predict the Hospital overall rating 
### with the group score national comparison as the independent variables 
temp <- master_df[, grep(colnames(master_df),pattern = "comparison")]
temp$Hospital.overall.rating <- master_df$Hospital.overall.rating
temp <- droplevels(temp)
comparisonColIdx = grep("Hospital.overall.rating", colnames(temp))
cat("Predict Hospital rating using the group score national comparison \n")
data.rf.app1 <- createRFModel(temp, comparisonColIdx)  


### Overall Statistics
### 
### Accuracy : 0.6837          
### 95% CI : (0.6552, 0.7112)
### No Information Rate : 0.4863          
### P-Value [Acc > NIR] : < 2.2e-16       
### 
### Kappa : 0.5172          
### Mcnemar's Test P-Value : NA              
### 
### Statistics by Class:
### 
###                      Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
### Sensitivity          0.285714   0.6439   0.7387   0.6644  0.63636
### Specificity          0.990557   0.9100   0.7491   0.8807  0.98209
### Pos Pred Value       0.500000   0.6226   0.7360   0.6667  0.52500
### Neg Pred Value       0.976723   0.9172   0.7518   0.8797  0.98861
### Prevalence           0.031993   0.1874   0.4863   0.2642  0.03016
### Detection Rate       0.009141   0.1207   0.3592   0.1755  0.01920
### Detection Prevalence 0.018282   0.1938   0.4881   0.2633  0.03656
### Balanced Accuracy    0.638136   0.7770   0.7439   0.7726  0.80923

if (doTrainRF) 
{
  cat("Tune RF (Model to predict hospital rating using the group score national comparison) params. \n")
  data.rf.app1 <- tuneRFParams(temp, comparisonColIdx)
}

###
###
### Overall Statistics
### 
### Accuracy : 0.7166          
### 95% CI : (0.6889, 0.7432)
### No Information Rate : 0.4863          
### P-Value [Acc > NIR] : < 2.2e-16       
### 
### Kappa : 0.5546          
### Mcnemar's Test P-Value : NA              
### 
### Statistics by Class:
### 
### Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
### Sensitivity          0.057143   0.7073   0.8102   0.6574  0.48485
### Specificity          1.000000   0.9213   0.7189   0.9043  0.99529
### Pos Pred Value       1.000000   0.6744   0.7317   0.7116  0.76190
### Neg Pred Value       0.969780   0.9317   0.8000   0.8803  0.98416
### Prevalence           0.031993   0.1874   0.4863   0.2642  0.03016
### Detection Rate       0.001828   0.1325   0.3940   0.1737  0.01463
### Detection Prevalence 0.001828   0.1965   0.5384   0.2441  0.01920
### Balanced Accuracy    0.528571   0.8143   0.7645   0.7809  0.74007


### Importance of groups based on the Random forest model
print(varImp(data.rf.app1))
varImpPlot(data.rf.app1, main = "Important Groups", color = 'red')
imp.rf.app1 <- varImp(data.rf.app1)
imp.rf.app1$measures <- rownames(imp.rf.app1)
imp.rf.app1 <- imp.rf.app1[order(imp.rf.app1$Overall, decreasing = TRUE),]
print(imp.rf.app1)
varImpPlot(data.rf.app1, main = "Important groups", color = 'red',type = 2, size=2)

### Groups ordered in terms of their importance as per RF model

### Overall                                             measures
### Patient.experience.national.comparison               246.3214
### Readmission.national.comparison                      199.4074
### Safety.of.care.national.comparison                   184.2578
### Mortality.national.comparison                        158.2008
### Efficient.use.of.medical.imaging.national.comparison 134.9747
### Timeliness.of.care.national.comparison               128.0280
### Effectiveness.of.care.national.comparison            124.2227
### 


### Approach 2:
### Build a random forest model to predict a hospital rating from the 64 measures

temp = master_df
temp <- droplevels(temp)

### Remove the group comparison measures,Provider IDs, count of nas in each group.
colsToBeRemoved <- grep(x = colnames(temp),pattern = "comparison")
temp <- temp[,!colnames(temp) %in% colnames(temp)[colsToBeRemoved]]
temp <- temp[, -grep(colnames(temp),pattern = "Provider")]
temp <- temp[, -grep(colnames(temp),pattern = "_numMeas")]

cat("Predict Hospital rating using the 64 measures (Excluding Hospital ratings as NAs) using the RF model \n")
comparisonColIdx = grep("Hospital.overall.rating", colnames(temp))
data.rf.app2 <- createRFModel(temp, comparisonColIdx)  

### Overall Statistics
### 
### Accuracy : 0.745          
### 95% CI : (0.718, 0.7706)
### No Information Rate : 0.4863         
### P-Value [Acc > NIR] : < 2.2e-16      
### 
### Kappa : 0.5821         
### Mcnemar's Test P-Value : NA             
### 
### Statistics by Class:
### 
### Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
### Sensitivity          0.085714   0.6098   0.9380   0.6055  0.39394
### Specificity          1.000000   0.9494   0.6512   0.9540  0.99906
### Pos Pred Value       1.000000   0.7353   0.7180   0.8255  0.92857
### Neg Pred Value       0.970669   0.9134   0.9173   0.8707  0.98148
### Prevalence           0.031993   0.1874   0.4863   0.2642  0.03016
### Detection Rate       0.002742   0.1143   0.4561   0.1600  0.01188
### Detection Prevalence 0.002742   0.1554   0.6353   0.1938  0.01280
### Balanced Accuracy    0.542857   0.7796   0.7946   0.7798  0.69650

imp.rf.app2 <- varImp(data.rf.app2)
imp.rf.app2$rf_measures <- rownames(imp.rf.app2)
colnames(imp.rf.app2) = c("RF_Importance", "RF_Measures")
imp.rf.app2 <- imp.rf.app2[order(imp.rf.app2$RF_Importance, decreasing = TRUE),]
imp.rf.app2 <- imp.rf.app2[1:20,]
rownames(imp.rf.app2) = NULL
print(imp.rf.app2)
varImpPlot(data.rf.app2, main = "Important measures", color = 'red',type = 2, size=2)


### Important measures
### Overall                  measures
### READM_30_HOSP_WIDE        113.00375        READM_30_HOSP_WIDE
### PSI_90_SAFETY              87.03596             PSI_90_SAFETY
### H_HSP_RATING_LINEAR_SCORE  57.63957 H_HSP_RATING_LINEAR_SCORE
### H_COMP_7_LINEAR_SCORE      55.23011     H_COMP_7_LINEAR_SCORE
### H_RECMND_LINEAR_SCORE      54.42203     H_RECMND_LINEAR_SCORE
### MORT_30_PN                 51.16767                MORT_30_PN
### H_COMP_1_LINEAR_SCORE      49.57018     H_COMP_1_LINEAR_SCORE
### H_COMP_3_LINEAR_SCORE      46.69436     H_COMP_3_LINEAR_SCORE
### READM_30_PN                43.04933               READM_30_PN
### H_COMP_5_LINEAR_SCORE      41.04017     H_COMP_5_LINEAR_SCORE
### READM_30_HF                40.85826               READM_30_HF
### MORT_30_HF                 38.94960                MORT_30_HF
### H_COMP_4_LINEAR_SCORE      36.75450     H_COMP_4_LINEAR_SCORE
### READM_30_COPD              35.80360             READM_30_COPD
### H_COMP_6_LINEAR_SCORE      35.75486     H_COMP_6_LINEAR_SCORE
### H_CLEAN_LINEAR_SCORE       35.05985      H_CLEAN_LINEAR_SCORE
### MORT_30_COPD               32.86478              MORT_30_COPD
### ED_1b                      31.76777                     ED_1b
### ED_2b                      29.10669                     ED_2b
### MORT_30_STK                26.28793               MORT_30_STK

### Comparing Approach 1 and 2, Approach 2 that builds a random forest to predict
### the hospital rating using the 64 measures gives a better accuracy.
### Tune the RF parameters

### As the Random Forest tuning takes a longer time, currently tuning is disabled by
### setting doTrainRF to FALSE. To enable tuning, set doTrainRF to TRUE
if (doTrainRF) {
  cat("Tune RF (Model to predict hospital rating from 64 measures) params. \n")
  rf_model_app2 <- tuneRFParams(temp, comparisonColIdx)
  print(varImp(data.rf.app2))
  varImpPlot(data.rf.app2, main = "Important measures", color = 'red')
  
  ###   Overall Statistics
  ###   
  ###   Accuracy : 0.8135          
  ###   95% CI : (0.7892, 0.8362)
  ###   No Information Rate : 0.4863          
  ###   P-Value [Acc > NIR] : < 2.2e-16       
  ###   
  ###   Kappa : 0.7067          
  ###   Mcnemar's Test P-Value : NA              
  ### 
  ### Statistics by Class:
  ### 
  ###                      Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
  ### Sensitivity           0.54286   0.7024   0.9060   0.7958  0.45455
  ### Specificity           0.99717   0.9573   0.7954   0.9429  0.99811
  ### Pos Pred Value        0.86364   0.7912   0.8074   0.8333  0.88235
  ### Neg Pred Value        0.98507   0.9331   0.8994   0.9279  0.98329
  ### Prevalence            0.03199   0.1874   0.4863   0.2642  0.03016
  ### Detection Rate        0.01737   0.1316   0.4406   0.2102  0.01371
  ### Detection Prevalence  0.02011   0.1664   0.5457   0.2523  0.01554
  ### Balanced Accuracy     0.77001   0.8298   0.8507   0.8694  0.72633  

  imp.rf.app2 <- varImp(data.rf.app2)
  imp.rf.app2$rf_measures <- rownames(imp.rf.app2)
  colnames(imp.rf.app2) = c("RF_Importance", "RF_Measures")
  imp.rf.app2 <- imp.rf.app2[order(imp.rf.app2$RF_Importance, decreasing = TRUE),]
  imp.rf.app2 <- imp.rf.app2[1:20,]
  rownames(imp.rf.app2) = NULL
  print(imp.rf.app2)
  varImpPlot(data.rf.app2, main = "Important measures", color = 'red',type = 2, size=2)
  
}

### Use SVM linear model to get the baseline accuracy
comparisonColIdx = grep("Hospital.overall.rating", colnames(temp))
Y = temp[,colnames(temp)[comparisonColIdx]]

library(kernlab)
# Split the data into train and test
inSamples <- sample.split(Y, SplitRatio = 7/10)
trainData <- temp[inSamples,]
testData <- temp[!(inSamples),]
Model_linear <- ksvm(Hospital.overall.rating~ ., data = trainData, scale = TRUE, 
                     kernel = "vanilladot", prob.model=FALSE)

#Evaluate the model on the test data
Eval_linear = predict(Model_linear, type = "response", 
                      newdata = within(testData, rm(Hospital.overall.rating)))
#confusion matrix - Linear Kernel 
confusionMatrix(Eval_linear,testData$Hospital.overall.rating)

### Overall Statistics
### 
### Accuracy : 0.9122          
### 95% CI : (0.8939, 0.9283)
### No Information Rate : 0.4863          
### P-Value [Acc > NIR] : < 2.2e-16       
### 
### Kappa : 0.8661          
### Mcnemar's Test P-Value : NA              
### 
### Statistics by Class:
### 
###                      Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
### Sensitivity           0.74286   0.9024   0.9380   0.8962  0.87879
### Specificity           0.99433   0.9764   0.9324   0.9689  0.99434
### Pos Pred Value        0.81250   0.8981   0.9292   0.9120  0.82857
### Neg Pred Value        0.99153   0.9775   0.9408   0.9630  0.99622
### Prevalence            0.03199   0.1874   0.4863   0.2642  0.03016
### Detection Rate        0.02377   0.1691   0.4561   0.2367  0.02651
### Detection Prevalence  0.02925   0.1883   0.4909   0.2596  0.03199
### Balanced Accuracy     0.86860   0.9394   0.9352   0.9326  0.93657


############################### ###################################
#################### Unsupervised Learning #######################
#################################################################
### List of outputs of Factor analysis of each group
group_fa <- list()
### List of group scores
group_scores <- list()
### List of loadings of each group
group_loadings <- list()
### List of score coefficients of each group
group_score_coeffs <- list()
### Data frame of the group scores of each hospital
df_scores <- data.frame(matrix(ncol = 7, nrow  = nrow(master_df))) 

### Measures with more than 45% are removed from the factor analysis
### The threshold of 45% is arrived empirically
allowedMissingValuePerc <- 45

for(i in 1:length(group_df_list)) {
  temp <- get(group_df_list[i])
  temp1 <- temp[, -grep(colnames(temp),pattern = "Provider")]
  temp1 <- temp1[, -grep(colnames(temp1),pattern = "overall")]
  temp1 <- temp1[, -grep(colnames(temp1),pattern = "comparison")]
  
  ### There are a high number of measures missing (and they are imputed
  ### The imputation of the missing measures badly impacts the factor
  ### analysis 
  
  ### To reduce the impact of the missing values, following
  ### are tried
  ###    * 1. Remove the measures that have a very high % of the missing values
  ###    * 2. Do an inverse weighing of each measure based on the number
  ###       of missing values in that measure
  ###    * 3. Weight the group score based on the number of measures
  ###       provided by the hospital for that group
  
  ### Use the number of measures available for each group to weight the group
  ### score. 
  numMeasuresInGroup <- temp1[, grep(colnames(temp1),pattern = "_numMeas")]
  
  ### 1. Remove the derived numMeas column for factor analysis
  temp1 <- temp1[, -grep(colnames(temp1),pattern = "_numMeas")]
  
  ### Calculate the hospital wise weightage to the group score based
  ### on the number of missing values
  ### Weight is calculated as the ratio of the number of measures 
  ### provided by the hospital to the total number of measures in the
  ### respective group
  groupWeightageBasedOnMissValues <- (numMeasuresInGroup)/ncol(temp1)
  
  ### If the number of measures is less than 3, the group is removed from rating as per CMS
  ### Using the same logic, the group score of the hospital with less than 3 measures
  ### is set to 0 by setting the group weightage to 0.
  groupWeightageBasedOnMissValues <- ifelse(numMeasuresInGroup<3, 0.0, groupWeightageBasedOnMissValues)
  
  ### Get the % of missing values in each measure in the group
  weightsPerMeasure <- group_na_count[[i]]
  
  ### 1. Allow only those measures that have missing values less than certain %
  ### The allowed percentage of missing values is tuned.
  colsWithLessMissingValues <- which(weightsPerMeasure<=allowedMissingValuePerc)
  print(paste(
    paste("Number of missing values in", group_df_list[i], sep = " "), 
    ncol(temp1)-length(colsWithLessMissingValues), sep = "  is "))
  temp1 <- temp1[,colsWithLessMissingValues]
  
  
  ### 2. For the remaining measures with certain % of missing values,
  ### create an inverse weight based on the missing % and 
  ### weight the measures
  weightsPerMeasure <- weightsPerMeasure[colsWithLessMissingValues]
  weightsPerMeasure <- (100-weightsPerMeasure)/weightsPerMeasure
  temp1 <- temp1 * weightsPerMeasure  
  
  ### Correlation matrix of the measures in the group
  groupCor <- cor(temp1, use= "complete.obs")

  ### Preliminary tests to check if the group measures are appropriate
  ### for factor analysis is an identity matrix. 
  ### If none of the variables are not correlated, the variables will not
  ### load the factors properly and factor analysis cannot be used to analyze
  ### the data.
  ### If the Bartlett's test is significant, the data meets the criteria

  ### Do the bartlett test for sphericity to determine if the correlation
  ### matrix of the data is not an identity matrix
  message(paste("Bartlett test for ", group_df_list[i], sep = " "))
  print(cortest.bartlett(groupCor, n = nrow(master_df), diag = TRUE))
  
  ### Kaiser-Meyer-Olkin Measure of Sampling Adequacy
  ### KMO compares the observed correlation coefficients with the partial
  ### correlation coefficients.
  ### Small values of KMO indicates problem with sampling adequacy.
  ### KMO > .9 - marvelous,
  ###       .80s - mertitourious
  ###       .70s - middling
  ###       .60s - medicore
  ###       .50s - miserable
  ### KMO < .5   - unacceptable.
  message(paste("KMO test for ", group_df_list[i], sep = " "))
  print(KMO(temp1))
  
  ### Scree plots to get the minimum of factors required for the factor analysis
  fa.parallel(temp1, fm = "ml", sim = FALSE, 
              main = group_df_list[i], plot = FALSE)
  VSS.scree(temp1, main = paste("VSS Scree Plot", group_df_list[i], sep = " "))
  
  ### The number of factors in the factor analysis is always set to 1 irrespective of the output 
  ### fa.parallel or scree plot as one and only one score is needed for each group
  fa1 <- fa(temp1, rotate = "oblimin", nfactors = 1, fm = "pa", 
            scores = "regression" )
  
  ### Ignore negative loadings
  fa1$loadings[,1][fa1$loadings[,1]<0.0] <- 0.0
  
  ### Reference: https://pareonline.net/pdf/v14n20.pdf
  ### One of the refined procedures suggested in the above doc
  ### is applied in the common factor extraction methods used with EFA.
  ### Refined methods aim to maximize validity by producing factor scores that are highly correlated
  ### with a given factor and to obtain unbiased estimates of
  ### the true factor scores.
  
  ### We have used Thurstone Regression method
  ### Thurstone (1935) used a least squares regression approach to 
  ### predict factor score(s). Regression factor scores predict the 
  ### location of each individual on the factor or component.
  
  ### Independent variables in the regression equation are the standardized 
  ### observed values of the items in the estimated factors or components. 
  ### These predictor variables are weighted by regression coefficients.
  ### Regression coefficients are obtained by multiplying the inverse of 
  ### the observed variable correlation matrix by the matrix of factor loadings
  ###  and, in the case of oblique factors, the factor correlation matrix
  
  ### The least squares regression is a multivariate procedure, which takes into
  ### account not only the correlation between the factors and between factors 
  ### and observed variables (via item loadings), but also the correlation among
  ### observed variables, as well as the correlation among oblique factors 
  ### (for oblique EFA solutions). The factor scores are the dependent variables
  ### in the regression equation
  
  invGroupCor <- ginv(groupCor)  ### Inverse of the correlation matrix
  score_coeff <- crossprod(invGroupCor, fa1$loadings[,1]) ### score coefficients
  score_coeff <- score_coeff/sum(score_coeff) ### Bring score coefficients between 0.0-1.0
  message(paste("score coeff",group_df_list[i], sep=" " ))
  print(score_coeff)
  
  group_loadings <- c(group_loadings, list(fa1$loadings[,1])) ### Save the loadings
  group_score_coeffs <- c(group_score_coeffs, list(score_coeff)) ### Save the score coefficients
  
  ### Use the score coefficients as the weights of the measures to get
  ### weighted sum of the measures which is the group score
  df_scores[,i] <- as.matrix(temp1) %*% score_coeff 
  
  print(paste("Fitness of factors ", group_df_list[i], sep = " : "))
  factor.fit(r=groupCor, f = fa1)
  
  fa.diagram(fa1,main = paste("Factor loadings of ",group_df_list[i]))
  group_fa <- c(group_fa, list(fa1))
  
  ### Weight the group score based on the number of measures provided by the hospital for that group
  ### Higher the number of measures, higher the weightage. 
  df_scores[,i] <- as.vector(df_scores[,i])*as.vector(groupWeightageBasedOnMissValues)
  
  latentVar <- gsub(group_df_list[i], pattern = "df_", replacement = "")
  colnames(df_scores)[i] <- latentVar
  
}

### Bartlett test and KMO test output of each group
###                               Bartlett test   KMO test
### Mortality	                        6.06E-167	  0.67
### Safety of care	                  2.20E-47	  0.53  - Inadequate sampling
### Readmission	                      0.999998	  0.51  - Not suitable for FA, Inadequate sampling 
### Patient experience	              0	          0.93
### Effectiveness of care	            1.09E-198	  0.61  - Inadequate sampling
### Timeliness of care	              0	          0.73
### Efficient use of medical imaging	7.16E-206	  0.5   - Inadequate sampling

### This variable is used for the eda plots (only for the group scores)
selVars <- colnames(df_scores)  

### Hospital summary score is the weighted sum of the group scores.
### The weights used are as per the CMS methodology.

scores_weightage = c(0.22,0.22,0.22,0.22,0.04,0.04,0.04)

hospital_summary_score <- apply(t(as.matrix(df_scores) %*% scores_weightage),2,sum)

## Winsorization of the Hospital summary score (as suggested in CMS methodology)
df_scores$hospital_summary_score <- as.vector(scale(hospital_summary_score, scale = TRUE, center = TRUE))

## Do the outlier treatment (as suggested in CMS methodology)
df_scores$hospital_summary_score <- removeOutliers(df_scores$hospital_summary_score)

## Append the Hospital Rating column to the scores data frame
df_scores$Hospital.overall.rating <- droplevels(master_df$Hospital.overall.rating)

### The Star Rating methodology utilizes k-means clustering. The k-means clustering 
### analysis is a standard method for creating categories (or clusters) so that 
### the observations (or scores) in each category are closer to their category 
### mean than to any other category mean. The number of categories is pre-specified;
### CMS specified five categories, so that k-means clustering analysis generates 
### five categories based on hospital summary scores in a way that minimizes 
### the distance between summary scores and their assigned category mean.
set.seed(1000)
clusters = kmeans(hospital_summary_score, centers = 5, iter.max = 10000, 
                  nstart = 1000, algorithm = "Lloyd")

plotcluster(hospital_summary_score, clusters$cluster)

comp_ratings <- data.frame(matrix(nrow = nrow(master_df), ncol = 2))
comp_ratings$Hospital.overall.rating <- df_scores$Hospital.overall.rating
comp_ratings$Hospital.overall.rating <- droplevels(comp_ratings$Hospital.overall.rating )

### This mapping needs to be changed according to the output of the
### clusters - Check the cluster plot and accordingly modify the below
### statement
mapped_cluster_ids <- ifelse(clusters$cluster == 3, 1,
                             ifelse(clusters$cluster == 5, 2,
                                    ifelse(clusters$cluster == 1, 3,
                                           ifelse(clusters$cluster == 2, 4, 5))))

comp_ratings$predicted.rating <- as.factor(mapped_cluster_ids)

confMatrix <- confusionMatrix(comp_ratings$predicted.rating, 
                              reference = comp_ratings$Hospital.overall.rating)
confMatrix

### Plot the Predicted ratings vs CMS ratings
qplot(comp_ratings$Hospital.overall.rating, comp_ratings$predicted.rating, 
      data=as.data.frame(hospital_summary_score),  
      colour= comp_ratings$Hospital.overall.rating, 
      geom = c("boxplot", "jitter"), 
      main = "predicted Rating vs. CMS Ratings", 
      xlab = "CMS Ratings", ylab = "Predicted Ratings")

# Compute and plot roc and auc 
library(pROC)
cmsRat <- as.numeric(levels(comp_ratings$Hospital.overall.rating))[comp_ratings$Hospital.overall.rating]
predRat <- as.numeric(levels(comp_ratings$predicted.rating))[comp_ratings$predicted.rating] 
res.roc <- multiclass.roc(cmsRat, predRat)
auc(res.roc)
rs <- res.roc[['rocs']]
plot.roc(rs[[1]], print.auc = TRUE, col = "green")
lines.roc(rs[[2]], col = "red")
lines.roc(rs[[3]], col = "blue")
lines.roc(rs[[4]], col = "yellow")
lines.roc(rs[[5]], col = "orange" )

chart.Correlation(within(df_scores, rm(Hospital.overall.rating,predicted.rating)), 
			histogram=TRUE, pch=19)

### Plot the clusters wrt the predicted ratings and ratings done by CMS
par(mfrow=c(2,1))
plot(hospital_summary_score,col=mapped_cluster_ids+1)

plot(hospital_summary_score,col=as.integer(df_scores$Hospital.overall.rating)+1)

### Append the Predicted ratings to the df_scores
df_scores$predicted.rating <- as.factor(mapped_cluster_ids)

df_scores[,1:7] <- scale(df_scores[,1:7], scale = TRUE, center = TRUE)
df_scores[,1:7] <- sapply(df_scores[,1:7], function(x) removeOutliers(x) )

### Recommendation for the hospital with Provider ID = 140010 (EVANSTON HOSPITAL) 
### The hospital's current star rating is 3 and recommendations to be provided 
### to improve it to at least 4 next year.

### Check the rating of the Evanstone hospital as per our analysis
### and compare with the CMS rating
### Create a dataframe that contains the group scores of the evanston
### hospital and the mean score for each group
### Compare each group score of evanston hospital against the national mean
### Group scores higher than the national mean would improve the rating
### and vice versa
df_eva = df_scores
df_eva$Provider.ID = master_df$Provider.ID
df_eva = subset(df_eva, Provider.ID=="140010")
df_eva_1 <- melt(df_eva, id.vars='Provider.ID')
df_eva_1 <- df_eva_1[,-1] # Remove Provider.ID
df_eva_1 <- df_eva_1[-9,] # Remove Hospital rating 
df_eva_1 <- df_eva_1[-9,] # Remove Predicted rating
colnames(df_eva_1) <- c("Groups", "Scores")
mean_vec <- sapply(df_scores[,1:8], 1, FUN=mean)
df_eva_1$Scores = as.numeric(df_eva_1$Scores)
df_eva_1$mean_scores = mean_vec # Apend group mean scores
df_eva_1$isScoreGood = ifelse(df_eva_1$Scores > df_eva_1$mean_scores,
                              "Higher", "Lower")
df_eva_1$isScoreGood
### Below csv file would contain the comparison of the group scores
### with the national mean
write.csv(df_eva_1, "Evanstone_Group_Scores.csv")

### Rating predicted as per our analysis is same as that of the CMS rating
### Current rating of the Evanstone hospital is 3.
selVars <- c(selVars, "hospital_summary_score")

### Bivariate analysis of group scores with respect to Predicted and CMS Star ratings
doBVAOfQuantVars(df_scores, selVars, "predicted.rating", "Hospital.overall.rating",  as.vector(df_eva))

### Bivariate analysis of group scores with respect to Predicted ratings (For the presentation)
doBVAOfQuantVars(df_scores, selVars, varForBVA1 = "predicted.rating",hosp_scores = as.vector(df_eva))

### Distribution of Hospital summary score and its Bivariate analysis 
### with respect to the predicted and CMS ratings
doBVAHistOfQuantVars(df_scores, c("hospital_summary_score"), 
                     "predicted.rating", "Hospital.overall.rating")

### Histogram of Predicted and CMS ratings to check if predicted rating 
### follow near normal distribution as CMS ratings
facVars <- c( "predicted.rating", "Hospital.overall.rating")
plotFactorVars(df_scores, facVars, quantVar = "hospital_summary_score")

### Analyis of the loadings and weights(score coefficients) to identity the Key Performance Indicators

### Create a data frame combining the loadings of all the measures
measures_loadings <- unlist(group_loadings)
measures_loadings <- sort(measures_loadings, decreasing = TRUE)
measures_loadings <- measures_loadings[1:20]
comp_ratings <- data.frame(matrix(nrow = nrow(master_df), ncol = 2))

meas_imp <- data.frame(matrix(ncol=3,nrow=20))
colnames(meas_imp) <- c("measures", "loadings", "idx")
meas_imp$measures=names(measures_loadings)
meas_imp$loadings=measures_loadings
meas_imp$idx=1:length(measures_loadings)

### Create a data frame combining all the score coefficients
measures_weights <- unlist(group_score_coeffs)
names(measures_weights) <- names(unlist(group_loadings))
measures_weights <- sort(measures_weights, decreasing = TRUE)
measures_weights <- measures_weights[1:20]
meas_weights <- data.frame(matrix(ncol=3, nrow=length(measures_loadings)))
colnames(meas_weights) <- c("Unsuper_Imp_Measures", "Unsuper_Weights", "idx")
meas_weights$Unsuper_Imp_Measures=names(measures_weights)
meas_weights$Unsuper_Weights=measures_weights
meas_weights$idx=1:length(measures_weights)


### Combine the supervised and unsupervised weights in a single dataframe for 
### comparison
meas_weights = cbind(meas_weights, imp.rf.app2)
meas_weights$Unsuper_Weights <- scale(meas_weights$Unsuper_Weights)
meas_weights$RF_Importance <- scale(meas_weights$RF_Importance)

### Top 20 important measures as the Supervised and Unsupervised learning
print(meas_weights)

### Below CSV file contains the top 20 important measures of Supervised
### and unsupervised learning
write.csv(meas_weights, "SuperVsUnSupervisedWeights.csv")

########################   VALIDATION OF CLUSTERING ##############################################
##VALIDATION BY betweenss/totalss RATIO
clusters$betweenss /clusters$totss    #0.8964 or 89.64% Which indicates very very good fit.
#Ideally we want a clustering that has the properties of internal cohesion and
#external separation, i.e. the BSS/TSS ratio should approach 1. We get 0.8964

##CHECKING INTERNAL AND STABILITY MEASURES
#We describe two types of measures to check quality of clustering:
#(1) Internal validation measures take only the dataset and the clustering partition
#### as input and use intrinsic information in the data to assess the quality of the 
#### clustering. These measures are Connectivity,Dunn and Silhouette width
#
#### connectivity has a value between zero and 8 and should be minimized.
#### The Silhouette Width thus lies in the interval [-1, 1], and should be maximized
#### The Dunn Index has a value between zero and 8, and should be maximized.

df_validity = df_scores
rownames(df_validity) = df_timeliness$Provider.ID
intern <- clValid(df_validity[,-c(9,10)], nClust = 5, clMethods=c("hierarchical","kmeans","pam"),validation="internal",maxitems = 3648)
#Above line takes 1/2 minutes in execution on Ubuntu Linux. Please be patient.

summary(intern)


#(2)The stability measures are aspecial version of internal measures. They evaluate
####the consistency of a clustering result by comparing it with the clusters obtained
####after each column is removed, one at a time. These measures are:
####(a)Average Proportion of Non-overlap (APN)(b)Average Distance (AD)
####(c)Average Distance between Means (ADM)   (d)Figure of Merit (FOM)

####The APN is in the interval [0, 1], with values close to zero corresponding with highly consistent clustering results.
####The AD has a value between zero and 8, and smaller values are preferred.
####The ADM has a value between zero and 8, and again smaller values are prefered.
####The FOM has a value between zero and 8, with smaller values equaling better performance.

stab <- clValid(df_validity[,-c(9,10)], nClust = 5,clMethods=c("hierarchical","kmeans","pam"),validation="stability", maxitems = 3648)
#Above line takes 1/2 minute on Ubuntu. Please be patient
summary(stab)
#The output of summary(stab) shows again that, for given data-set,hierarchical/pam would
##give bit more stability. But values obtained by k-means are also fairly close to optimum


################################################################################################################
