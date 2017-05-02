library(caret)
data(GermanCredit)
head(GermanCredit)
str(GermanCredit)
summary(GermanCredit)

# Converting Class variable from Bad/Good to 0/1

GermanCredit$Class=ifelse(GermanCredit$Class=="Bad",0,1)
GermanCredit$Class[1:5]


 

# We will split our dataset into Train and Test data. 
#The Train dataset will be used to build the model and
#the Test dataset will used to validate how well our model is performing.

set.seed(1234)
inTrain=createDataPartition(GermanCredit$Class, p=0.7, list=FALSE)
inTrain
Training=GermanCredit[inTrain,]
Testing=GermanCredit[-inTrain,]
dim(Training)
dim(Testing)


summary(Training$Class)
sum(Training$Class)
sum(Testing$Class)

# Multicollinearity
#install.packages("usdm")
library(usdm)
vifstep(GermanCredit[,-10], th=2)
a=vifstep(GermanCredit[,-10], th=2)
GermanCredit2=exclude(GermanCredit,a)

fit10=glm(Class~.,
          data = Training[,c("Class",names(GermanCredit2))],
          family=binomial(link="logit"))

summary(fit10)


fit0=glm(Class~ Duration+InstallmentRatePercentage+ResidenceDuration+Age+NumberExistingCredits+
           NumberPeopleMaintenance+Telephone+ForeignWorker+CheckingAccountStatus.0.to.200+
           CheckingAccountStatus.gt.200+CheckingAccountStatus.none+CreditHistory.ThisBank.AllPaid+
           CreditHistory.Delay+CreditHistory.Critical+Purpose.UsedCar+Purpose.Furniture.Equipment+
           Purpose.Radio.Television+Purpose.DomesticAppliance+Purpose.Repairs+Purpose.Education+
           Purpose.Retraining+Purpose.Business+ Purpose.Other+SavingsAccountBonds.100.to.500+
           SavingsAccountBonds.500.to.1000+SavingsAccountBonds.gt.1000+SavingsAccountBonds.Unknown+
           EmploymentDuration.1.to.4+EmploymentDuration.4.to.7+
           EmploymentDuration.Unemployed+Personal.Female.NotSingle+Personal.Male.Married.Widowed+
           OtherDebtorsGuarantors.CoApplicant+OtherDebtorsGuarantors.Guarantor+Property.Insurance+
           Property.CarOther+OtherInstallmentPlans.Stores+OtherInstallmentPlans.None+
           Housing.Own+Job.UnskilledResident+Job.Management.SelfEmp.HighlyQualified,
         data=Training,family=binomial(link="logit"))

summary(fit0)

#Select the model obtained from VIF for stepwise
library(MASS)
step=stepAIC(fit0, direction="both")

fitA= glm(Class~ Duration + InstallmentRatePercentage + Age + NumberExistingCredits + 
            Telephone + CheckingAccountStatus.gt.200 + CheckingAccountStatus.none + 
            CreditHistory.Critical + Purpose.UsedCar + Purpose.Furniture.Equipment + 
            Purpose.Radio.Television + Purpose.Retraining + Purpose.Business + 
            Purpose.Other + SavingsAccountBonds.100.to.500 + SavingsAccountBonds.500.to.1000 + 
            SavingsAccountBonds.gt.1000 + SavingsAccountBonds.Unknown + 
            EmploymentDuration.4.to.7 + Personal.Female.NotSingle + OtherDebtorsGuarantors.Guarantor + 
            Property.Insurance + Property.CarOther + OtherInstallmentPlans.None + 
            Housing.Own, data=Training,family=binomial(link="logit"))

summary(fitA)

fitB=update(fitA, .~. -Property.Insurance-Pupose.Other-Purpose.Business-Purpose.Retraining-CreditHistory.ThisBank.AllPaid-Age-
              -SavingsAccountBonds.100.to.500-CheckingAccountStatus.gt.200-Purpose.Furniture.Equipment, data=Training)

summary(fitB)

fitC=update(fitB, .~. -Purpose.Other-SavingsAccountBonds.100.to.500-Property.CarOther-
              OtherInstallmentPlans.None , data=Training)
summary(fitC)

fitD=update(fitC, .~. -SavingsAccountBonds.500.to.1000 -Telephone -Housing.Own, data=Training)
summary(fitD)

Pred=predict(fitD, newdata=Training[,-10], type="response" )
Pred1=ifelse(Pred<0.5,0,1)
confusionMatrix(table(Training$Class,Pred1,dnn=list('actual','predicted')))
table(Training$Class,Pred1)
#install.packages("ResourceSelection")
library(ResourceSelection)
hoslem.test(Training$Class,fitted(fitD),g=10)

#install.packages("survey")
library(survey)
regTermTest(fitD,"Personal.Female.NotSingle")


library(pscl)
pR2(fitD)
#install.packages("pscl")
#install.packages("InformationValue")
library(InformationValue)
plotROC(actuals=Training$Class,predictedScores=as.numeric(fitted(fitD)))

ks_plot(actuals=Training$Class,predictedScores=as.numeric(fitted(fitD)))
ks_stat(actuals=Training$Class,predictedScores=as.numeric(fitted(fitD)))










