rm(list=ls(all=TRUE)); set.seed(1)
install.packages(c("amap", "Amelia", "arules", "arulesViz", "car", "cluster",
                   "DMwR", "effects", "fpc", "gbm", "ggplot2", "MASS", "nnet", "party",
                   "polytomous", "pvclust", "randomForest", "rgl", "rms", "rpart",
                   "tree"), dependencies=TRUE)
#######################You can skip through this part to the spot with a large amount of pound signs, this just shows some of the things I tried first. 
## load and inspect data
DFs <- read.delim(file.choose(), row.names=1) # <disfluencies_1.csv>
head(DFs)
summary(DFs)    #Just looking over the tables to get a general idea of the data
attach(DFs)

#Dependent variable = FREQDISFL, a discrete variable
#Independent variables = SEX (bin), MOVEDWHEN (cat), REALITYTV (num), SOCNETWORK (num)

#H0=There is no correlation between the number of disfluencies and the predictors (independent variables and their interactions).
#H1=There is a correlation between the number of disfluencies and the predictors (independent variables and their interactions).

hist(DFs$FREQDISFL)
plot(ecdf(DFs$FREQDISFL), verticals=TRUE) #Nothing weird seems to be going on here

# -> t-test for independent samples
model.01 <- lm(FREQDISFL ~ SEX, data=RTs)
summary(model.01)

(preds.hyp <- data.frame(ima <- effect("SEX", model.01)))

plot(ima, ylim=range(FREQDISFL), xlab="SEX", grid=TRUE)
stripchart(FREQDISFL ~ SEX, vertical=TRUE, method="jitter")
text(seq(SEX), preds.hyp$fit, "-", cex=6); grid()   #Here it appears that males seem to have a slightly higher mean frequency of disfluencies. Here is a different way to represent it:
stripchart(FREQDISFL ~ SEX, method="jitter", xlim=c(0,220), xlab="Frequency of Disfluencies", ylab="Sex")
rug(jitter(FREQDISFL), side=1) #This shows a general spread of the frequency of disfluencies based on sex, less obvious than the first plot however.


#checking categorical predictors, i.e. MOVEDWHEN
summary(model.02 <- lm(FREQDISFL ~ MOVEDWHEN, data=RTs))

drop1(model.02, test="F")  # p-value of the droppable predictor(s)
Anova(model.02, type="II") # p-value of all predictors

(preds.hyp <- data.frame(fam <- effect("MOVEDWHEN", model.02)))

plot(fam, ylim=range(FREQDISFL), xlab="MOVEDWHEN", grid=TRUE)
stripchart(FREQDISFL ~ MOVEDWHEN, vertical=TRUE, method="jitter")
text(seq(MOVEDWHEN), preds.hyp$fit, "-", cex=6); grid() #With these graphs we can see that there is an increase in frequency of disfluencies, with adults having the lowest average, high school the next highest, and then primary school having the highest frequency of disfluencies.

#Checking numerical predictors, i.e. REALITYTV and SOCNETWORK

summary(model.03 <- lm(FREQDISFL ~ REALITYTV, data=DFs))
drop1(model.03, test="F")  # p-value of the droppable predictor(s)
Anova(model.03, type="II") # p-value of all predictors
(preds.hyp <- data.frame(fre <- effect("REALITYTV", model.03, xlevels=list(REALITYTV=seq(0, 5, 0.5)))))

summary(model.04 <- lm(FREQDISFL ~ SOCNETWORK, data=DFs))
drop1(model.04, test="F")  # p-value of the droppable predictor(s)
Anova(model.04, type="II") # p-value of all predictors
(preds.hyp <- data.frame(fre <- effect("SOCNETWORK", model.04, xlevels=list(SOCNETWORK=seq(0, 5, 0.5)))))

plot(fre, ylim=range(c(0,75)), xlab="REALITYTV", grid=TRUE)
plot(jitter(FREQDISFL) ~ jitter(REALITYTV))

plot(fre, ylim=range(c(0,65)), xlab="SOCNETWORK", grid=TRUE)
plot(jitter(FREQDISFL) ~ jitter(SOCNETWORK))


summary(model.05 <- lm(FREQDISFL ~ REALITYTV * SOCNETWORK, data=DFs))
drop1(model.05, test="F")  # p-value of the droppable predictor(s)
Anova(model.05, type="II") # p-value of all predictors
(preds.hyp <- data.frame(intact <- effect("REALITYTV:SOCNETWORK", model.05, xlevels=list(SOCNETWORK=0:200))))
plot(intact, grid=TRUE, ylim=c(0, 600), xlab="REALITYTV", ylab="FREQDISFL")

plot3d(preds.hyp$REALITYTV, preds.hyp$SOCNETWORK, preds.hyp$fit) #Experimenting with different graphical models

#The following is where I will be working to test a linear model selection process with using all of the above predictors, the previous predictors were mostly practice to become familiar with the processes. I guess what is previous does not really matter in the grand scheme of things, but the next selection process should give me the data I'm looking for once it is complete.

model.06 <- lm(FREQDISFL ~ (SEX+MOVEDWHEN+SOCNETWORK+REALITYTV)^3, data=DFs)
summary(model.06)
drop1(model.06, test="F")
Anova(model.06, type="III")

#################### I'm leaving everything above this just to show the work that I did playing with the data, but most of what you're looking for is probably below.

library(MASS)
model.last <- stepAIC(lm(FREQDISFL ~ 1, data=DFs[complete.cases(DFs),]),
                      direction="both",
                      scope=list(lower= ~1, upper=~ (SEX + MOVEDWHEN + SOCNETWORK + REALITYTV)^3))

#Using stepAIC we can see that there are several models that are viable for our purposes. Using the function exp((AICmin???AICi)/2) we can determine the relative likelihood of each model. The lowest AIC value is 2098.37. The next lowest value is 2098.95, which is  0.748 times as likely as the first model to minimize information loss, but has one less variable, so it somewhat simplifies the interactions between variables. For this example, R has chosen the model that considers SOCNETWORK + REALITYTV + MOVEDWHEN + REALITYTV:MOVEDWHEN, since its AIC is the lowest. 

summary(model.last) #SOCNETWORK AND REALITYTV seem to be major predictors, with MOVEDWHENhigh school and REALITYTV:MOVEDWHENhighschool also being large predictors.
par(mfrow=c(2,2)) #Visualizing all the graphs at once
plot(model.last)
par(mfrow=c(1,1)) #In case we wanted to reset the graph viewer
#Time to analyze these graphs. The Residuals vs Fitted and Scale-location graphs show a scatter-cloud dispersion, which is good, since it indicates that the variances of the residuals are fairly homogenous and normally distributed from the sample population. Normal Q-Q shows that there are few outliers except 3 towards the top tail of the dashed line, but it mostly follows the expected result. Residuals vs Leverage also looks good since the majority of points are concentrated on the left side of the graph, decreasing as they move right. Overall, this model.last seems to have all the characteristics of a solid model.

soc<-effect("SOCNETWORK", model.last); soc #Looking at some data from the major predictors
realt<-effect("REALITYTV", model.last); realt
mvw<-effect("MOVEDWHEN", model.last); mvw
rtvmw<-effect("REALITYTV:MOVEDWHEN", model.last); rtvmw

plot(rtvmw) #Visualizing some of the data. You can plot all of the above variables for a more graphical representation of the data

(preds.hyp <- data.frame(intact <- effect("REALITYTV:MOVEDWHEN", model.last, xlevels=list(FREQUENCY=0:5))))
plot(intact, grid=TRUE, ylim=c(50,150), xlab="REALITYTV", ylab="DF")


#We can now confidently use model.last to do all sorts of predictions for whatever variables we want to account for. 

