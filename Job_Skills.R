jobs.df <- read.csv("Listings.csv")
jobs.df <- as.data.frame(jobs.df)
View(jobs.df)
summary(jobs.df)
str(jobs.df)
dim(jobs.df)

shortened.df <- jobs.df[,-c(0:24)]
View(shortened.df)

summary(shortened.df)

jobskills.df <- shortened.df[,-c(22:27)]

View(jobskills.df)

library(caret)
#job <- jobskills.df$recruiter

lm <- glm(job ~ ., data = jobskills.df)
summary(lm)

#jobskills.df <- lapply(jobskills.df)


#jobskills.df <- as.data.frame(lapply(jobskills.df[,],factor))

#jobs_final.df <- as.data.frame(model.matrix( 
  #~ ., 
  #data = jobskills.df))

#jobskills.df <- as.data.frame(model.matrix(jobskills.df[,],factor))




View(jobskills.df)

typeof(jobs.df)

#str(jobs_final.df)



summary(jobskills.df)
#the summary of the last 4 attributes/skills reveal that none of the applicants had taken them

#removing the outcome variable

#jobskills_numdata.df <- jobskills.df,-c("recruiter") # Remove the recruiter Column
#View(jobskills_numdata.df)

#scaling the variable


set.seed(69) # Try with different seeds 


train.index <- sample(c(1:dim(jobskills.df)[1]), dim(jobskills.df)[1]*0.6)  
train.df <- jobskills.df[train.index, ]
valid.df <- jobskills.df[-train.index, ]

library(neuralnet)
library(keras)

#train.df <- dummy_cols(jobskills.df, select_columns = )
View(train.df)

#nnjob_yes <- neuralnet(recruiter ~., data=train.df, hidden = 7, 
                     #  act.fct = "logistic", linear.output = FALSE, stepmax = 1000000,
                      # rep = 4, threshold = 0.0055,
                       #lifesign = 'full')

nnjob_yes <- neuralnet(recruiter ~., data=train.df, act.fct = "logistic",
                       linear.output = FALSE, hidden = c(2,2), lifesign = "minimal", 
                       err.fct = "ce", stepmax = 1e7)
#accuracy 69.64%


plot(nnjob_yes, rep = "best")

outnn1 <- compute(nnjob_yes, rep = 1, train.df)
p1 <- outnn1$net.result # get the probabilities
pred1 <- ifelse(p1>0.7,1,0) # convert probabilities into classification
tabl <- table(pred1, train.df$recruiter)
tabl

#run from after this
error1 <- 1 - sum(diag(tabl)) / sum(tabl)
error1

validnn1 <- compute(nnjob_yes, rep = 1, valid.df)
pv1 <- validnn1$net.result # get the probabilities
predv1 <- ifelse(pv1>0.7,1,0) # convert probabilities into classification
tabvl <- table(predv1, valid.df$recruiter)
tabvl
confusionMatrix(tabvl, positive = "0")
