#Statistical Computing (III) Assignment
#Quiz(1a)
A<- matrix(c(17,7,3,8,6,12),nrow=3,ncol=2,byrow=T,
           dimnames=list(c("Row1","Row2","Row3"),c("Col","Col2") ));A
#Quiz(1b)
#Additional column
g_1<- as.matrix(c(11,8,15),1,1,byrow=T)
colnames(g_1)<- c("Col3")
rownames(g_1)<- c("Row1","Row2","Row3")
g_1
#Combining columns of matrix A and g_1 to have  new matrix B
B<-cbind(A,g_1);B
#Re-ordering matrix B, so that last column3 will be between column 1 and 2.
B[,c(1,3,2)]

#Quiz(1c)
#To obtain the same output set.seed(1)
set.seed(1)
#Generating random normal matrix of  3 by 3 matrix rounding off to 4 decimal places
C<-round(matrix(rnorm(9) , nrow = 3,ncol=3,byrow=T,
         dimnames=list(c("Row1","Row2","Row3"),c("Col1","Col2","Col3") ) ),4);C


#Quiz(1d)
D<-B%*%C;D

#Quiz (1e) Row 3 , col2 and col3 elements
Ro3Co23<- D[3,c(2:3)];Ro3Co23

#QUIZ 2
#load MASS package
library("MASS")

#load Cars93 dataset
#Dimension of the data set; Length and width 
dim(Cars93)
#First five rows of data
head(Cars93)
#Last five rows of data
tail(Cars93)

#Names of columns in the dataset
names(Cars93)
#Data type of data set
str(Cars93)

#Summary of whole dataset.
summary(Cars93)

#Quiz(2a) Price of cars with no AirBugs
#Method 1 None AirBags
#Counts of various Airbags and that of None airbags
table(Cars93$AirBags)
#Price of the cars with None airbags
PriceNo_Air<- Cars93$Price[Cars93$AirBags=="None"]
#Average price of the cars with None airbags.
AvgPrice_NoAir<- mean(PriceNo_Air);AvgPrice_NoAir

#2a Alternative Method2 None AirBags Price
None_AirBagsPrice<- subset(Cars93, AirBags =="None",select=Price)
mean(None_AirBagsPrice$Price)

#2b Average Horsepower of Sporty cars  
#Method 1
#Counts of various Car types including Sporty cars 
table(Cars93$Type)
#Horsepowers of cars in the Sporty type.
SportyHorsepower<- Cars93$Horsepower[Cars93$Type=="Sporty"]
Avg_Horse<- mean(SportyHorsepower);Avg_Horse

#Method 2 Alternative 
SportyHorsepower1<- subset(Cars93,Type=="Sporty",select=Horsepower)
AvgSporty1<-mean(SportyHorsepower1$Horsepower);AvgSporty1

#2c Enginesize of midsize cars
#Counts of various Car Engines with  Type Midsize 
table(Cars93$Typ)
#or
summary(Cars93$Type)

#Enginesize of cars in the Midsize type.
#Method 1
EngineMid<- Cars93$EngineSize[Cars93$Type=="Midsize"]
#Average of Enginesize of cars with the Midsize type.
EngineSizeAverage_midsize<-mean(EngineMid);EngineSizeAverage_midsize

#Method 2 alternative
Engine_Midsize<- subset(Cars93, Type =="Midsize",select=EngineSize)
Avg_Engine_Midsize<- mean(Engine_Midsize$EngineSize);Avg_Engine_Midsize

#Quiz3
#Quiz (3i)
library("survival")
library("readxl")
survdata<-read_xlsx("C:/Users/user/Desktop/survdat.xlsx");survdata
kmsu<- survfit(Surv(survdata$minutes,survdata$status)~1)
summary(kmsu)
plot(kmsu,xlab="Time(minutes)",ylab="Survival Probability")

#Quiz(3ii)
#knowinng the numbers of customers with more than 10 minutes wait
count<- nrow(subset(survdata,time_wait>=10,select=time_wait));count
#Sample of customers
total_count<- length(survdata$minutes)-1;total_count

#R function that takes 2 arguments to calculate the approximaate cost of customers 
#refund in a day when waiting
#argument1(refund cost),argument2(Customers per day) and count from above code
#function name approx_cost
approx_cost<- function( refund,customers_perday){
result= refund*(customers_perday/total_count)*count
print(result)
}
#inputing refund cost and Number of customers gives approximate cost
approx_cost(50,2000)
#The approximate cost is 16666.67












----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------








