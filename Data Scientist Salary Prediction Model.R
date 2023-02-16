install.packages("tree")
library(tree)
install.packages("pROC")
library(pROC)
source("DataAnalyticsFunctions.R")
salaries <- read.csv("ds_salaries.csv")
####################################################################################

##DATA CLEANING
#drop columns salaries and currency, because we will use salary_in_usd as our response variable
#drop column employee_residence, because we believe that it is more likely that the salary is based on the company location
model_salaries <- lm(salary_in_usd~.,data = salaries)
summary(model_salaries)
drop <- c("salary","salary_currency", "employee_residence")
DATA <- salaries[,!(names(salaries) %in% drop)]

#we look at the weight of each job title in the data, based on this information, we designed our job categories accordingly
cntjobtitle<-data.frame(table(DATA$job_title))
weight <- c()
for(i in 1:length(cntjobtitle$Freq)){
  weight = append(weight,cntjobtitle[i,"Freq"]/sum(cntjobtitle$Freq) )
}
cntjobtitle<-data.frame(cntjobtitle, weight)
cntjobtitle<-cntjobtitle[order(cntjobtitle$weight, decreasing= TRUE),]
#we noticed that 23.6% of the job titles are data scientist, 21.7% are data engineer...

#categorize job titles into 7 categories, using software_engineer as baseline because it appears to have the smallest weight in the data
DATA$data_scientist <- ifelse(DATA$job_title == "Data Scientist" | DATA$job_title == "Lead Data Scientist" | DATA$job_title == "Applied Data Scientist" | DATA$job_title == "Data Specialist" | DATA$job_title == "Data Science Consultant" | DATA$job_title == "Principal Data Scientist" | DATA$job_title == "Staff Data Scientist", 1, 0)
DATA$data_engineer <- ifelse(DATA$job_title == "Big Data Engineer" | DATA$job_title == "Lead Data Engineer" | DATA$job_title == "Data Engineer" | DATA$job_title == "Cloud Data Engineer" | DATA$job_title == "Data Science Engineer" | DATA$job_title == "Big Data Architect" | DATA$job_title == "Analytics Engineer" | DATA$job_title == "Data Analytics Engineer" | DATA$job_title == "Principal Data Engineer" | DATA$job_title == "Data Architect", 1, 0)
DATA$data_analyst <- ifelse(DATA$job_title == "BI Data Analyst" | DATA$job_title == "Finance Data Analyst" | DATA$job_title == "Product Data Analyst" | DATA$job_title == "Data Analyst" | DATA$job_title == "Business Data Analyst" | DATA$job_title == "Marketing Data Analyst" | DATA$job_title == "Financial Data Analyst" | DATA$job_title == "Principal Data Analyst", 1, 0)
DATA$machine_learning_engineer <- ifelse(DATA$job_title == "Machine Learning Engineer" | DATA$job_title == "ML Engineer" | DATA$job_title == "Computer Vision Engineer" | DATA$job_title == "Machine Learning Developer" | DATA$job_title == "Machine Learning Infrastructure Engineer" | DATA$job_title == "AI Scientist" | DATA$job_title == "Applied Machine Learning Scientist" | DATA$job_title == "NLP Engineer", 1, 0)
DATA$research_scientist <- ifelse(DATA$job_title == "Research Scientist" | DATA$job_title == "3D Computer Vision Researcher" | DATA$job_title == "Machine Learning Scientist", 1, 0)
DATA$manager <- ifelse(DATA$job_title == "Data Engineering Manager" | DATA$job_title == "Data Science Manager" | DATA$job_title == "Data Analytics Manager" | DATA$job_title == "Head of Machine Learning" | DATA$job_title == "Lead Machine Learning Engineer" | DATA$job_title == "Lead Data Analyst" | DATA$job_title == "Director of Data Science" | DATA$job_title == "Machine Learning Manager" | DATA$job_title == "Head of Data" | DATA$job_title == "Director of Data Engineering" | DATA$job_title == "Head of Data Science" | DATA$job_title == "Data Analytics Lead", 1, 0)
DATA$software_engineer <- ifelse(DATA$job_title == "Computer Vision Software Engineer" | DATA$job_title == "ETL Developer", 1, 0)

#create dummy variables for employment_type, use FL (Freelance) as baseline
DATA$fullTime <- ifelse(DATA$employment_type == "FT", 1, 0)
DATA$contract <- ifelse(DATA$employment_type == "CT", 1, 0)
DATA$partTime <- ifelse(DATA$employment_type == "PT", 1, 0)
DATA$freeLance <- ifelse(DATA$experience_level == "FL", 1, 0)

#create dummy variables for experience level, use EX (executive) as baseline
DATA$midLevel <- ifelse(DATA$experience_level == "MI", 1, 0)
DATA$seniorLevel <- ifelse(DATA$experience_level == "SE", 1, 0)
DATA$entryLevel <- ifelse(DATA$experience_level == "EN", 1, 0)
DATA$executive <- ifelse(DATA$experience_level == "EX", 1, 0)

#create dummy variables for remote ratio, using full remote or remote_ratio = 100 as baseline
DATA$half_remote <- ifelse(DATA$remote_ratio == "50", 1, 0)
DATA$no_remote <- ifelse(DATA$remote_ratio == "0", 1, 0)
DATA$remote <- ifelse(DATA$remote_ratio == "100", 1, 0)

# create dummy variables for company location based on continents, using Australia as baseline
DATA$australia <- ifelse(DATA$company_location == "NZ" | DATA$company_location == "AU", 1,0)
DATA$africa <- ifelse(DATA$company_location == "NG" | DATA$company_location == "KE" | DATA$company_location == "SI" | DATA$company_location == "DZ", 1,0)
DATA$europe <- ifelse(DATA$company_location == "DE" | DATA$company_location == "GB" | DATA$company_location =="HU" | DATA$company_location == "FR" | DATA$company_location == "GR" | DATA$company_location == "NL" | DATA$company_location == "AT" | DATA$company_location == "ES" | DATA$company_location == "PT" | DATA$company_location == "DK" | DATA$company_location == "IT" | DATA$company_location == "HR" | DATA$company_location == "LU" | DATA$company_location == "PL" | DATA$company_location == "BE" | DATA$company_location == "UA" | DATA$company_location == "RO" | DATA$company_location == "MT" | DATA$company_location == "MD" | DATA$company_location == "CH" | DATA$company_location == "CZ" | DATA$company_location == "EE" | DATA$company_location == "IE", 1,0)
DATA$northAmerica <- ifelse(DATA$company_location == "HN" | DATA$company_location == "US" | DATA$company_location == "MX" | DATA$company_location == "CA" | DATA$company_location == "AS", 1,0)
DATA$southAmerica <- ifelse(DATA$company_location == "BR" | DATA$company_location == "CL" | DATA$company_location == "CO" | DATA$company_location == "", 1,0)
DATA$asia <- ifelse(DATA$company_location == "JP" | DATA$company_location == "IN" | DATA$company_location == "PK" | DATA$company_location == "CN" | DATA$company_location == "AE" | DATA$company_location == "SG" | DATA$company_location == "RU" |DATA$company_location == "IQ" | DATA$company_location == "IL" | DATA$company_location == "IR" | DATA$company_location == "VN" | DATA$company_location == "TR" | DATA$company_location == "MY", 1,0)

#create dummy variables for company size, using small company as baseline
DATA$mediumCompany <- ifelse(DATA$company_size == "M", 1, 0)
DATA$largeCompany <- ifelse(DATA$company_size == "L", 1, 0)
DATA$smallCompany <- ifelse(DATA$company_size == "S", 1, 0)

#cleaned Data 
dropUnprocessed <- c("X","work_year", "experience_level", "employment_type", "job_title", "remote_ratio", "company_location", "company_size", "software_engineer", "freeLance", "executive", "remote", "asia", "smallCompany")
DATA_CLEANED <- DATA[,!(names(DATA) %in% dropUnprocessed)]
################################################################################################################

#DATA MODELING

#Data Model for predicting the salaries (Regression)
installpkg("glmnet")
library(glmnet)

# Linear regression
model_linear <- lm(salary_in_usd ~ .,data = DATA_CLEANED)
summary(model_linear)

# Linear regression with interaction + Lasso 
#### the features need to be a matrix ([,-1] removes the first column which is the intercept)
Mx<- model.matrix(salary_in_usd ~.^2, data = salaries)[,-1]
My<- salaries$salary_in_usd

### If we omit the lambda, the function will solve for all values of lambda
### it takes a bit longer but not much longer at all.
lasso <- glmnet(Mx,My)
### By running for all vaules of lambda we get many models back
### now we have the summary
summary(lasso)
### lasso$a0 gives you the intercept
### lasso$beta gives you all the coefficients. Many were set to zero.
### lasso$lambda has the value of lambda stored.
#lasso$lambda[1:5]
### They are in decreasing order so the most sparse solution is beta[,1]
### For each coefficient we can plot its "Path" as we change lambda
#par(mar=c(1.5,1.5,0.75,1.5))
#par(mai=c(1.5,1.5,0.75,1.5))

## Make 2 plots side by side: mfrow=c(1,2)  # c(nrow,ncol)
#par(mfrow=c(1,2))
#coef_ind <- 5
#par(mar=c(1.5,0.5,0.75,0.5))
#par(mai=c(1.5,0.5,0.75,0.5))
#plot(log(lasso$lambda),lasso$beta[coef_ind,], ylab="Coefficient value", main=paste("Coefficient for",colnames(Mx)[coef_ind]),xlab = expression(paste("log(",lambda,")")),type="l")
#coef_ind <- 2
#par(mar=c(1.5,0.5,0.75,0.5))
#par(mai=c(1.5,0.5,0.75,0.5))

#plot(log(lasso$lambda),lasso$beta[coef_ind,], ylab="Coefficient value", main=paste("Coefficient for",colnames(Mx)[coef_ind]),xlab = expression(paste("log(",lambda,")")),type="l")

## make it back to 1 plot only
#par(mfrow=c(1,1))
#par(mar=c(1.5,1.5,1.5,1.5))
#par(mai=c(1.5,1.5,1.5,1.5))

### we can see all the 453 coefficients
### (note that the x axis is not lambda but L1 norm.
### The L1 norm is large when lambda is small and vice-versa.)
#plot(lasso, xvar="lambda", main="# of non-zero coefficients", ylab ="Coefficient values", xlab = expression(paste("log(",lambda,")")))

### Now that we can actually compute the Lasso for all values of lambda,
### the whole "path" of models can be evaluated by a OOS experiment
### we can attempt to use cross valiadation to actually pick lambda.
### the following command yields a cross validation procedure
### the following command takes some time.
lassoCV <- cv.glmnet(Mx,My)
### We can plot the fitting graph
### red dots are mean values and the bars are the uncertainty
#par(mar=c(1.5,1.5,2,1.5))
#par(mai=c(1.5,1.5,2,1.5))
#plot(lassoCV, main="Fitting Graph for CV Lasso \n \n # of non-zero coefficients  ", xlab = expression(paste("log(",lambda,")")))

### There are some rules that people like to use:
### The minimum of the mean values stored in lambda.min
### 1se to the right stored in lambda.1se
### if we had to compute lambda.min we can simply write
lassoCV$lambda.min
### if we had to compute lambda.1se we can simply write
lassoCV$lambda.1se

# Regression model with Lasso where lambda set to min
lassoMin <- glmnet(Mx,My,lambda = lassoCV$lambda.min)
summary(lassoMin)
support(lassoMin$beta)
lassoMin$beta
colnames(Mx)[support(lassoMin$beta)]
length(support(lassoMin$beta)) #68

# Regression model with Lasso where lambda set to 1se
lasso1se <- glmnet(Mx,My,lambda = lassoCV$lambda.1se)
summary(lasso1se)
support(lasso1se$beta)
colnames(Mx)[support(lasso1se$beta)]
length(support(lasso1se$beta)) #59

# Ridge Regression
ridgeCV <- cv.glmnet(Mx,My, alpha=0)
### if we had to compute lambda.min we can simply write
ridgeCV$lambda.min
### if we had to compute lambda.1se we can simply write
ridgeCV$lambda.1se

# Regression model with Ridge where lambda set to min
ridgeMin <- glmnet(Mx,My, alpha = 0, lambda = ridgeCV$lambda.min)
summary(ridgeMin)
support(ridgeMin$beta)
colnames(Mx)[support(ridgeMin$beta)]
length(support(ridgeMin$beta)) #2253

# Regression model with Ridge where lambda set to 1se
ridge1se <- glmnet(Mx,My, alpha = 0, lambda = ridgeCV$lambda.1se)
summary(ridge1se)
support(ridge1se$beta)
colnames(Mx)[support(ridge1se$beta)]
length(support(ridge1se$beta)) #2253

#### we select a model based on the proposed rules
#### and perform cross validation
set.seed(123)
nfold <- 10
n <- nrow(salaries)
nreg<- nrow(DATA_CLEANED)
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
foldidreg <- rep(1:nfold,each=ceiling(nreg/nfold))[sample(1:nreg)]

# perform cross validation on linear regression
LR <- c()
for(k in 1:nfold){ 
  train <- which(foldidreg!=k) # train on all but fold `k'
  
  ## fit the two regressions and null model
  reg <- lm(salary_in_usd ~., data = DATA_CLEANED, subset = train)
  
  ## get predictions: type=response so we have probabilities
  pred.reg <- predict(reg, newdata=DATA_CLEANED[-train,], type="response")
  
  ## calculate and R2
  LR[k] <- R2(y=DATA_CLEANED$salary_in_usd[-train], pred=pred.reg)
  print(paste("Iteration",k,"of",nfold,"(thank you for your patience)"))
}



#PL.OOS <- data.frame(PL.min=rep(NA,nfold), PL.1se=rep(NA,nfold)) 
L.OOS <- data.frame(L.min=rep(NA,nfold), L.1se=rep(NA,nfold))
#L.pred <- data.frame(L.min=rep(NA,nfold), L.1se=rep(NA,nfold))

R.OOS <- data.frame(R.min=rep(NA,nfold), R.1se=rep(NA,nfold))

# data train for post-Lasso 
#features.min <- support(lassoMin$beta)
#length(features.min)
#features.1se <- support(lasso1se$beta)
#length(features.1se) 
#data.min <- data.frame(Mx[,features.min],My)
#data.1se <- data.frame(Mx[,features.1se],My)

set.seed(123)
for(k in 1:nfold){
  train <- which(foldid!=k) # train on all but fold `k'
  
  ### This is the CV for the Post Lasso Estimates
  ##rmin <- glm(My~., data=data.min, subset=train)
  ##r1se <- glm(My~., data=data.1se, subset=train)
  
  ##predmin <- predict(rmin, newdata=data.min[-train,], type="response")
  ##pred1se  <- predict(r1se, newdata=data.1se[-train,], type="response")
  
  ##PL.OOS$PL.min[k] <- R2(y=My[-train], pred=predmin)
  ##PL.OOS$PL.1se[k] <- R2(y=My[-train], pred=pred1se)
  
  ### This is the CV for the Ridge Estimates
  ridgemin  <- glmnet(Mx[train,],My[train], alpha = 0, lambda = ridgeCV$lambda.min)
  ridge1se  <- glmnet(Mx[train,],My[train], alpha = 0, lambda = ridgeCV$lambda.1se)
  
  predridgemin <- predict(ridgemin, newx=Mx[-train,], type="response")
  predridge1se  <- predict(ridge1se, newx=Mx[-train,], type="response")
  
  R.OOS$R.min[k] <- R2(y=My[-train], pred=predridgemin)
  R.OOS$R.1se[k] <- R2(y=My[-train], pred=predridge1se)
  
  
  ### This is the CV for the Lasso estimates  
  lassomin  <- glmnet(Mx[train,],My[train], lambda = lassoCV$lambda.min)
  lasso1se  <- glmnet(Mx[train,],My[train], lambda = lassoCV$lambda.1se)
  
  predlassomin <- predict(lassomin, newx=Mx[-train,], type="response")
  #L.pred$L.min[k] <- predlassomin
  predlasso1se  <- predict(lasso1se, newx=Mx[-train,], type="response")
  #L.pred$L.1se[k] <- predlasso1se
  
  L.OOS$L.min[k] <- R2(y=My[-train], pred=predlassomin)
  L.OOS$L.1se[k] <- R2(y=My[-train], pred=predlasso1se)
  
  print(paste("Iteration",k,"of",nfold,"completed"))
}

#L.pred
R2performance <- cbind(L.OOS, R.OOS, LR)
avgOOSR2 <- colMeans(R2performance)
barplot(colMeans(R2performance), las=2,xpd=FALSE, ylim=c(0,1) , xlab="", ylab = bquote( "Average Out of Sample " ~ R^2))
avgOOSR2
########################

#Data Model for predicting the probabilities (Logistic Regression)
average_europe <- mean(DATA_CLEANED$salary_in_usd[DATA_CLEANED$europe==1])
average_australia <- mean(DATA_CLEANED$salary_in_usd[DATA_CLEANED$australia==1])
average_africa <- mean(DATA_CLEANED$salary_in_usd[DATA_CLEANED$africa == 1])
average_northAmerica <- mean(DATA_CLEANED$salary_in_usd[DATA_CLEANED$northAmerica==1])
average_southAmerica <- mean(DATA_CLEANED$salary_in_usd[DATA_CLEANED$southAmerica==1])
average_asia <- mean(DATA_CLEANED$salary_in_usd[DATA_CLEANED$europe==0 & DATA_CLEANED$africa==0 & DATA_CLEANED$australia==0 & DATA_CLEANED$northAmerica==0 & DATA_CLEANED$southAmerica==0])

elite <- c()
for(i in 1:nrow(DATA_CLEANED)){
  if(DATA_CLEANED$europe[i] == 1 & DATA_CLEANED$salary_in_usd[i]>average_europe){
    elite<-append(elite,1)}
  else if(DATA_CLEANED$australia[i] == 1 & DATA_CLEANED$salary_in_usd[i]>average_australia){
    elite<-append(elite,1)}
  else if(DATA_CLEANED$africa[i] == 1 & DATA_CLEANED$salary_in_usd[i]>average_africa){
    elite<-append(elite,1)}
  else if(DATA_CLEANED$northAmerica[i] == 1 & DATA_CLEANED$salary_in_usd[i]>average_northAmerica){
    elite<-append(elite,1)}
  else if(DATA_CLEANED$southAmerica[i] == 1 & DATA_CLEANED$salary_in_usd[i]>average_southAmerica){
    elite<-append(elite,1)}
  else if(DATA_CLEANED$africa[i] == 0 & DATA_CLEANED$europe[i] == 0 & DATA_CLEANED$australia[i] == 0 & DATA_CLEANED$northAmerica[i] == 0 & DATA_CLEANED$southAmerica[i]==0 & DATA_CLEANED$salary_in_usd[i]>average_asia){
    elite<-append(elite,1)}
  else{
    elite<-append(elite,0)
  }
}
#salaries$elite<- elite
#salaries$elite <- ifelse(salaries$elite==0,"No","Yes")
#salaries$elite <- factor(elite)
#useforprediction1 <- salaries[4:6,]
#DATA_CLEANED$elite <- ifelse(elite==0,"No","Yes")
#DATA_CLEANED$elite <- factor(elite)
#useforprediction <- DATA_CLEANED[4:20,]

# define dataset for logistic regression
DATA_CLEANED_LOGISTIC <- DATA_CLEANED
DATA_CLEANED_LOGISTIC$elite <- ifelse(elite==0,"No","Yes")
DATA_CLEANED_LOGISTIC<-DATA_CLEANED_LOGISTIC[,!(names(DATA_CLEANED_LOGISTIC) %in% c("salary_in_usd"))]
# fit logistic model
model.logistic <-glm(elite=="Yes"~., data=DATA_CLEANED_LOGISTIC, family="binomial")
summary(model.logistic)

### create a vector of fold memberships (random order)
#nfold <- 2 
#n <- nrow(DATA_CLEANED_LOGISTIC) # the number of observations
# Holdout data for predictions
#DATA_CLEANED_LOGISTIC2 <- DATA_CLEANED_LOGISTIC
#DATA_CLEANED_LOGISTIC2$fold <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
#DATA_CLEANED_LOGISTIC2$fold <- ifelse(DATA_CLEANED_LOGISTIC$fold==2,0,1)

#prediction<-predict(model.logistic,newdata = DATA_CLEANED_LOGISTIC, type="response")
#prediction

#threshold <- .4
#TP <- sum((model.logistic$fitted >= threshold)*model.logistic$y)
#FP <- sum((model.logistic$fitted >= threshold)*(!model.logistic$y))
##FN <- sum((model.logistic$fitted <  threshold)*model.logistic$y)
#TN <- sum((model.logistic$fitted <  threshold)*(!model.logistic$y))
#LR3.FPR <- FP / (FP + TN)
#LR3.TPR <- TP / (TP + FN)
#threshold
#LR3.FPR
#LR3.TPR


pred.lr <- predict(model.logistic, newdata=DATA_CLEANED_LOGISTIC, type="response")
#My <- DATA_CLEANED_LOGISTIC2$elite[DATA_CLEANED_LOGISTIC2$fold == 1]=="Yes"
My <- DATA_CLEANED_LOGISTIC$elite=="Yes"
### threshold = .5
performance.lr.50 <- FPR_TPR(pred.lr>=0.50, My)
performance.lr.50
### threshold = .6
performance.lr.60 <- FPR_TPR(pred.lr>=0.60, My)
performance.lr.60
### threshold = .75
performance.lr.75 <- FPR_TPR(pred.lr>=0.75, My)
performance.lr.75
### threshold = .25
performance.lr.25 <- FPR_TPR(pred.lr>=0.25, My)
performance.lr.25
### threshold = .4
performance.lr.40 <- FPR_TPR(pred.lr>=0.40, My)
performance.lr.40

#Confusion Matrix
confusion.matrix.60 <- matrix(c(performance.lr.60$TP, performance.lr.60$FP, performance.lr.60$FN, performance.lr.60$TN), ncol=2, byrow = TRUE)
colnames(confusion.matrix.60) <- c('Elite','Not Elite')
rownames(confusion.matrix.60) <- c('Elite','Not Elite')
confusion.table.60 <- as.table(confusion.matrix.60)
confusion.table.60
confusion.matrix.75 <- c(performance.lr.75$TP, performance.lr.75$FP, performance.lr.75$FN, performance.lr.75$TN)
confusion.matrix.75
confusion.matrix.25 <- c(performance.lr.25$TP, performance.lr.25$FP, performance.lr.25$FN, performance.lr.25$TN)
confusion.matrix.25
confusion.matrix.40 <- c(performance.lr.40$TP, performance.lr.40$FP, performance.lr.40$FN, performance.lr.40$TN)
confusion.matrix.40

source("PerformanceCurves.R")
install.packages("dplyr")
library(dplyr)
roccurve<- roc(p=pred.lr,y=My,bty="n")
index <- c(50)
radius <- 0.03 *rep(1,length(index))
color <- c("red")
symbols(roccurve[index ,], circles=radius, inches = FALSE,ylim=c(0,1), xlim=c(0,1), ylab="True positive rate", xlab="False positive rate", bg=color)
FPR_TPR(pred.lr>=0.5 , My)

index <- c(25,50)
radius <- 0.03 *rep(1,length(index))
color <- c("red","grey")
symbols(roccurve[index ,], circles=radius, inches = FALSE,ylim=c(0,1), xlim=c(0,1), ylab="True positive rate", xlab="False positive rate", main="ROC Curve", bg=color)
FPR_TPR(pred.lr>=0.25 , My)

index <- c(10,25,50)
color <- c("red","grey","grey")
radius <- 0.03 *rep(1,length(index))
symbols(roccurve[index ,], circles=radius, inches = FALSE,ylim=c(0,1), xlim=c(0,1), ylab="True positive rate", xlab="False positive rate", main="ROC Curve",bg=color)
FPR_TPR(pred.lr>=0.1 , My)

index <- c(1, 10,25,50)
color <- c("red","grey","grey","grey")
radius <- 0.03 *rep(1,length(index))
symbols(roccurve[index ,], circles=radius, inches = FALSE,ylim=c(0,1), xlim=c(0,1), ylab="True positive rate", xlab="False positive rate", main="ROC Curve",bg=color)
FPR_TPR(pred.lr>=0 , My)

index <- c(75, 1, 10,25,50)
color <- c("red","grey","grey","grey","grey")
radius <- 0.03 *rep(1,length(index))
symbols(roccurve[index ,], circles=radius, inches = FALSE,ylim=c(0,1), xlim=c(0,1), ylab="True positive rate", xlab="False positive rate",main="ROC Curve", bg=color)
FPR_TPR(pred.lr>=0.75 , My)

index <- c(100, 75, 1, 10,25,50)
color <- c("red","grey","grey","grey","grey","grey")
radius <- 0.03 *rep(1,length(index))
symbols(roccurve[index ,], circles=radius, inches = FALSE,ylim=c(0,1), xlim=c(0,1), ylab="True positive rate", xlab="False positive rate", main="ROC Curve",bg=color)
FPR_TPR(pred.lr>=1 , My)


# based on our logistic model to predict the probability of each job candidate being an elite
probElite <- predict(model.logistic, newdata=DATA_CLEANED_LOGISTIC, type="response")
# compare probElite to threshold we have selected at 0.6
EliteorNot <- ifelse(probElite >= 0.6, 1, 0)
predSalary <- predict(lassomin, newx=Mx, type="response")
expectedRevenue <- EliteorNot*predSalary*0.1
sum(expectedRevenue)
sum(salaries$salary_in_usd)
