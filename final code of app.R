#clean data

getwd()
pricing <- read.table(file="pricing.csv", header=T, sep=","
                      ,colClasses = c("factor","factor","factor","numeric"
                                      ,"numeric","factor","numeric"))
str(pricing)
head(pricing)
orders <- read.table(file="numberoforders.csv", header=T, sep=","
                     ,colClasses = c("numeric","numeric","numeric","numeric"
                                     ,"numeric","numeric","numeric","numeric"))

#remove missing variables
for (i in 1:ncol(pricing)){
  pricing[which(pricing[,i]=='NA'),i] <- NA 
}

for (i in 1:ncol(orders)){
  orders[which(orders[,i]=='NA'),i] <- NA 
}

source("DataQualityReportOverall.R")
DataQualityReportOverall(pricing)

#CompleteCases IncompleteCases CompleteCasePct
#1         32573               0             100

DataQualityReportOverall(orders)
#CompleteCases IncompleteCases CompleteCasePct
#1        456548               0             100

d <- orders[,c(9,1:8)]
names(d)[1] <- "y"
names(d)

te=pricing[,1:8]
library(caret)
str(d)
#remove data with>80% correlation
descrCor <-  cor(d[,2:9])                          
highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.750)
filteredDescr <- d[,2:9][,-highlyCorDescr] 
descrCor2 <- cor(filteredDescr)  
summary(descrCor2[upper.tri(descrCor2)])
d <- cbind(d$y, filteredDescr)
names(d)[1] <- "y"

#descriptive analytics
#statistical summary

#visualisations
names(d)[1] <- "num_orders"
hist(d$num_orders, breaks=seq(from=10,to=25000,by=100),
     main="Histogram of orders per week",
     xlab="Number of orders per week", col="blue",xlim = c(0,500))
summary(d)
#id               week          center_id     
#Min.   :   13.0   Min.   :1000000   Min.   :  1.00   Min.   : 10.00  
#1st Qu.:   54.0   1st Qu.:1124998   1st Qu.: 39.00   1st Qu.: 43.00  
#Median :  136.0   Median :1250182   Median : 76.00   Median : 76.00  
#Mean   :  261.8   Mean   :1250095   Mean   : 74.77   Mean   : 82.11  
#3rd Qu.:  324.0   3rd Qu.:1375136   3rd Qu.:111.00   3rd Qu.:110.00  
#Max.   :15336.0   Max.   :1499999   Max.   :145.00   Max.   :186.00  

#meal_id     checkout_price   emailer_for_promotion homepage_featured
#Min.   :1062   Min.   :  2.97   Min.   :0.00000       Min.   :0.0000   
#1st Qu.:1558   1st Qu.:228.95   1st Qu.:0.00000       1st Qu.:0.0000   
#Median :1993   Median :296.82   Median :0.00000       Median :0.0000   
#Mean   :2024   Mean   :332.24   Mean   :0.08115       Mean   :0.1092   
#3rd Qu.:2539   3rd Qu.:445.23   3rd Qu.:0.00000       3rd Qu.:0.0000   
#Max.   :2956   Max.   :866.27   Max.   :1.00000       Max.   :1.0000   

summary(d$num_orders)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#13.0    54.0   136.0   261.9   324.0 24299.0 

summary(d$checkout_price)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#2.97  228.95  296.82  332.24  445.23  866.27 


#removing outliers
d=subset(d, num_orders<=11000 && meal_id <= 3000)????
  d=d[-c(420866),] 
d=d[-c(13924),] 
d=d[-c(14051),] 
d=d[-c(16905),] 
d=d[-c(19701),] 
d=d[-c(413352),]

library(ggplot2)
ggplot(data=d)+
  geom_point(aes(x=checkout_price,y=num_orders))+
  geom_abline(aes(slope =coef(lm(num_orders~checkout_price,
                                 data=d))[2],intercept =coef(lm(num_orders~checkout_price,
                                                                data=d))[1]),color="red")

ggplot(data=d)+
  geom_point(aes(x=meal_id,y=num_orders))+
  geom_abline(aes(slope =coef(lm(num_orders~meal_id,
                                 data=d))[2],intercept =coef(lm(num_orders~meal_id,
                                                                data=d))[1]),color="red")
#clustering
names(d)[1] <- "y"
inTrain <- createDataPartition(y = d$y,   
                               p = .70,   
                               list = F)
train <- d[inTrain,]  
test <- d[-inTrain,]

set.seed(123)  # set this to replicate results
cost_df <- data.frame()
for(k in 1:10){
  kmeans_tr <- kmeans(train, centers=k, nstart=25, iter.max=100)
  kmeans_te <- kmeans(test, centers=k, nstart=25, iter.max=100)
  cost_df <- rbind(cost_df, cbind(k, kmeans_tr$tot.withinss
                                  , kmeans_te$tot.withinss))
}
names(cost_df) <- c("cluster", "tr_cost", "te_cost")
par(mfrow=c(1,1))
cost_df[,2:3] <- cost_df[,2:3]/1000
plot(x=cost_df$cluster, y=cost_df$tr_cost, main="k-Means Elbow Plot"
     , col="blue", pch=19, type="b", cex.lab=1.2
     , xlab="Number of Clusters", ylab="MSE (in 1000s)")
points(x=cost_df$cluster, y=cost_df$te_cost, col="green")

#no of ideal clusters is 4 in both and same shape so they seem replicable-cluster 2 
#and 3 seems best

cost_df <- data.frame() #accumulator for cost results
cost_df
for(k in 1:10){
  kmeans_tr <- kmeans(x=d[,2:ncol(d)], centers=k, nstart=25, iter.max=100)
  cost_df <- rbind(cost_df, cbind(k, kmeans_tr$tot.withinss))
}
names(cost_df) <- c("cluster", "cost")
cost_df

# create an elbow plot
par(mfrow=c(1,1))
cost_df[,2:3] <- cost_df[,2:3]/1000
plot(x=cost_df$cluster, y=cost_df$cost, main="k-Means Elbow Plot"
     , col="blue", pch=19, type="b", cex.lab=1.2
     , xlab="Number of Clusters", ylab="MSE (in 1000s)", xaxt="n")
axis(1, xaxp=c(0, 200, 20), las=2)
#cluster 2 and 3 seem best in d too

kmeans_2=kmeans(d, centers=2, nstart=25, iter.max=100)

kmeans_3=kmeans(d, centers=3, nstart=25, iter.max=100)

source("multiplot.R")
library(useful)
p1 <- plot(kmeans_2, data=d)
p2 <- plot(kmeans_3, data=d)
multiplot(p1, p2)

rm(p1, p2) 

library(cluster)
sil=silhouette(kmeans_2$cluster,dist(d,method="euclidean"))
plot(sil,main="Silhouette plot - K-means", border=NA,col=colors()
     [sample(x=length(colors()))])

library(cluster)
sil=silhouette(kmeans_3$cluster,dist(d,method="euclidean"))
plot(sil,main="Silhouette plot - K-means", border=NA,col=colors()
     [sample(x=length(colors()))])


#Predictive analytics
#preprocessing data
preProcValues <- preProcess(d[,2:ncol(d)], method = c("range"))
d <- predict(preProcValues, d)

preProcValues <- preProcess(orders[,2:ncol(orders)], method = c("range"))
te <- predict(preProcValues, orders)

#datapartioning
inTrain <- createDataPartition(y = d$y,   
                               p = .70,   
                               list = F)
train <- d[inTrain,]  
test <- d[-inTrain,]

#m1 with all variables
ctrl <- trainControl(method="cv",     
                     number=5,        
                     classProbs = F,  
                     summaryFunction = defaultSummary, 
                     allowParallel=T)
m1 <- train(y ~ .,               # model specification
            data = train,        
            method = "lm",     
            trControl = ctrl,    
            metric = "RMSE"       # performance measure
)
m1
defaultSummary(data=data.frame(obs=train$y, pred=predict(m1, newdata=train))
               , model=m1)
defaultSummary(data=data.frame(obs=test$y, pred=predict(m1, newdata=test))
               , model=m1)
#model isnt overfit RSME differnce is 7% and test performs better than train
#-Rsq of train is 0.193 but fir test it is 0.196

#m2 with all variables but log(y)
m2 <- train(log(y) ~ .,               # model specification
            data = train,        
            method = "lm",     
            trControl = ctrl,    
            metric = "RMSE"       # performance measure
)

defaultSummary(data=data.frame(obs=train$y, pred=predict(m2, newdata=train))
               , model=m2)
# test 
defaultSummary(data=data.frame(obs=test$y, pred=predict(m2, newdata=test))
               , model=m2)
#m2 has worse performave than m1 as R square decreases to 0.18 for 
#train and 0183 for test- however model isnt overfit

#regression with y and checkout_price only 
m3 <- train(y ~ checkout_price,               # model specification
            data = train,        
            method = "lm",     
            trControl = ctrl,    
            metric = "RMSE"       # performance measure
)
m3
defaultSummary(data=data.frame(obs=train$y, pred=predict(m3, newdata=train))
               , model=m3)
# test 
defaultSummary(data=data.frame(obs=test$y, pred=predict(m3, newdata=test))
               , model=m3)
#worse than m1 as r sq decreased to 0.079 for train and 0.08 for test and RMSE inc to 8%

m3 <- train(y ~ checkout_price+meal_id,               # model specification
            data = train,        
            method = "lm",     
            trControl = ctrl,    
            metric = "RMSE"       # performance measure
)
m3
defaultSummary(data=data.frame(obs=train$y, pred=predict(m3, newdata=train))
               , model=m3)
# test 
defaultSummary(data=data.frame(obs=test$y, pred=predict(m3, newdata=test))
               , model=m3)

#using regression trees 
library(rpart)
d$meal_id=factor(as.character(d$meal_id))
d$center_id=factor(as.character(d$center_id))

rTree=rpart(formula=y~.,
            data=train1,
            method="anova"
            
)
rTree_orders=predict(rTree,test1)

pred=data.frame(Real_Orders=test1$y,rTree_orders)
write.table(x=prediction_rTree, sep=",", file="RTree_orders.csv", row.names=F)


#using h2o to predict
library(caret)
library(h2o)

h2o.init(nthreads=12, max_mem_size="64g")
h2o.clusterInfo()

data <- as.h2o(d)

y <- "y"                                # target variable to learn
x <- setdiff(names(data), y)                # feature variables are all other columns
parts <- h2o.splitFrame(data, 0.7, seed=99) # randomly partition data into 80/20
train <- parts[[1]]                         # random set of training obs??
test <- parts[[2]]  

RF=h2o.randomForest(x,y,train)
DL=h2o.deeplearning(x,y,train) 
GBM=h2o.gbm(x,y,train)

h2o.performance(RF,train)
h2o.performance(GBM,test)
h2o.performance(DL,train)
h2o.performance(DL,test)
h2o.performance(GBM,train)
h2o.performance(RF,test)


#> h2o.performance(RF,train)
#H2ORegressionMetrics: drf

#MSE:  14430.34
#RMSE:  120.1264
#MAE:  68.12996
#RMSLE:  0.5496222
#Mean Residual Deviance :  14430.34
#> h2o.performance(RF,test)
#H2ORegressionMetrics: drf

#MSE:  39010.92
#RMSE:  197.5118
#MAE:  95.41908
#RMSLE:  0.6425205
#Mean Residual Deviance :  39010.92

#> h2o.performance(GBM,test)
#H2ORegressionMetrics: gbm

#MSE:  59116.61
#RMSE:  243.1391
#MAE:  130.5314
#RMSLE:  NaN
#Mean Residual Deviance :  59116.61
#> h2o.performance(GBM,train)
#H2ORegressionMetrics: gbm

#MSE:  60033.7
#RMSE:  245.0178
#MAE:  130.8696
#RMSLE:  NaN
#Mean Residual Deviance :  60033.7

#> h2o.performance(DL,train)
#H2ORegressionMetrics: deeplearning

#MSE:  86186.45
#RMSE:  293.5753
#MAE:  153.8859
#RMSLE:  NaN
#Mean Residual Deviance :  86186.45

#> h2o.performance(DL,test)
#H2ORegressionMetrics: deeplearning

#MSE:  83423.76
#RMSE:  288.8317
#MAE:  153.642
#RMSLE:  NaN
#Mean Residual Deviance :  83423.76

#none of the models is overfit, RMSE of train is bigger than test for all

xgb <- h2o.xgboost(x, y, training_frame = train,
                   validation_frame = test,
                   stopping_rounds = 5,
                   ntrees =30,
                   gamma = 0.0)

summary(xgb)
#Variable Importances: 
# variable relative_importance scaled_importance percentage
#1               meal_id  24520222720.000000          1.000000   0.336786
#2        checkout_price  20385925120.000000          0.831392   0.280001
#3     homepage_featured   9089016832.000000          0.370674   0.124838
#4             center_id   8476797952.000000          0.345706   0.116429
#5 emailer_for_promotion   6984195584.000000          0.284834   0.095928
#6                  week   3236694528.000000          0.132001   0.044456
#7                    id    113764520.000000          0.004640   0.001563

auto <- h2o.automl(x, y, train, max_runtime_secs=300)
auto
str(auto)

data2 <- as.h2o(te)

p <- h2o.predict(auto, data2)
p <- as.data.frame(p)
head(p)

h2o_results <- data.frame(meal_id=te$meal_id,Id=te$id, Ordersperweek=p$predict)
write.table(x=h2o_results, sep=",", file="ordersperweek1.csv", row.names=F)

#shinyapp

library(shiny)
library(rpart)
library(readr)

# load data
pricing <- read_csv('pricing.csv', col_names = T)
orders <- read_csv('numberoforders.csv', col_names = T)
mealInfo <- read_csv('meal_info.csv', col_names = T)
centerInfo <- read_csv('fulfilment_center_info.csv', col_names = T)

# create data for modeling 
modelData <- orders[,c(9,3,4)]
modelData$center_id <- factor(as.character(modelData$center_id))
modelData$meal_id <- factor(as.character(modelData$meal_id))

# data partitioning
inTrain <- createDataPartition(y = modelData$num_orders,  
                               p = .70,  
                               list = F)
train <- modelData[inTrain,]  
test <- modelData[-inTrain,]

rTree <- rpart(
  formula = num_orders ~ .,
  data    = train,
  method  = "anova"
)

pred <- data.frame(Orders = test$num_orders, predicted = predict(rTree, test))

ui <- fluidPage(
  titlePanel(
    title = "Order Prediction System",
    windowTitle = "Order Prediction System"
  ),
  hr(),
  div(
    img(src = "https://upload.cc/i1/2020/10/09/75qxrT.png", height = 150, width = 187),
    style="text-align:center; margin-bottom: 20px"
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = 'mealSelector',
        label = "Select Meal ID",
        choices = unique(orders$meal_id),
        selected = 1
      ),
      selectInput(
        inputId = 'centerSelector',
        label = "Select a center",
        choices = unique(orders$center_id),
        selected = 1
      ),
      
    ),
    
    mainPanel(
      navbarPage(
        title = "",
        tabPanel(
          "General Information",
          tableOutput('mealInfoTable'),
          tableOutput('centerInfoTable')
        ),
        
        tabPanel(
          "Regression Prediction",
          uiOutput('prediction'),
          plotOutput('plot')
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  prediction <- reactive({
    predict(rTree, data.frame(meal_id=input$mealSelector, center_id=input$centerSelector))
  })
  
  output$mealInfoTable <- renderTable(
    mealInfo[mealInfo$meal_id==input$mealSelector,]
  )
  
  output$centerInfoTable <- renderTable(
    centerInfo[centerInfo$center_id==input$centerSelector,]
  )
  
  
  output$prediction <- renderUI({
    div(
      strong(
        paste('Predicted number of orders for meal ', input$mealSelector, ' and center', input$centerSelector, ' is: ', 
              round(prediction()[[1]], digits=0))
      )
    )
  })
  
  output$plot <- renderPlot(
    plot(pred$Orders, pred$predicted, main = 'Number of Orders VS Predicted Number of Orders in the Train set', 
         xlab = 'Number of Orders', ylab = 'Predicted Number of Orders')
  )
  
}

shinyApp(ui, server)
