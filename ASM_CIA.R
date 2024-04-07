#Reading .CSV file
df=read.csv('Pizza_survey.csv',header = TRUE)
head(df)

#Average Rating
mean(df$Quality_Rating)
mean(df$Size_rating) 

str(df)

#Median rating
median(df$Quality_Rating)
median(df$Size_rating)

#Kurtosis
library(moments)
kurtosis(df$Quality_Rating) 
kurtosis(df$Size_rating)

#skewness
skewness(df$Quality_Rating) #Negatively skewness
skewness(df$Size_rating)


Mode 
mode<-function(v){
  uniqv<-unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
mode(df$Quality_Rating)
mode(df$Size_rating)


#Standard Deviation
sd(df$Quality_Rating)
sd(df$Size_rating)

#Variance
var(df$Size_rating)
var(df$Quality_Rating)

#ANOVA test using Pizza_size and Brand 
H0="Mean Size Order is Equal to all the Brands"
H1="Mean Size Order is not Equal to all the Brands"
anova_result <- aov(Pizza_size ~ Brand, data =df)
res=summary(anova_result)
res
p_value=res[[1]][1,5]
p_value
los=0.05
if(p_value>los)
{
  print("Accept HO")
  print(H0)
}else{
  print("Do not Accept H0")
  print(H1)
}

#ANNOVA using Quality_rating and Restaurants
H0="Mean Quality ratingis Equal to all the Restaurants"
H1="Mean Quality rating is not Equal to all the Restaurants"
anova_result <- aov(Quality_Rating ~ Restaurants, data =df)
res=summary(anova_result)
res
p_value=res[[1]][1,5]
p_value
los=0.05
if(p_value>los)
{
  print("Accept HO")
  print(H0)
}else{
  print("Do not Accept H0")
  print(H1)
}

#Bar Chat Visualization
barplot(table(df$Menu_list), col="chocolate1", main = "Bar Chart of Menu_List")
barplot(table(df$Restaurants), col="mediumorchid4", main = "Bar Chart of Restaurants")
barplot(table(df$Order_Methods),col="chocolate3",main="Bar Chart of Ordering_Method")

#Pie Plot Visualization
table(df$Frequency)
pie(table(df$Frequency),labels = c('Daily','Occasionally','Thrice in a week','Twice in a week'),
    main = "Pie Chart of Consumption")
table(df$Restaurants)
pie(table(df$Restaurants),col = c('maroon','mediumorchid4','chocolate1','peru','salmon'),
    labels = c('Dominos','Others','Pappa Johns','Pizza express','Pizza Hut'), 
    main = "Pie Chart of Restaurants")

#Histogram based on Menu_list
hist(table(df$Menu_list),col='chocolate1')
hist(table(df$Restaurants),col='chocolate1')

#Box plot Visualization
boxplot(table(df$Menu_list),horizontal=TRUE, xlab = "Menu_list",main="Boxplot for Menu_List" )
boxplot(table(df$Order_Methods),horizontal=TRUE, xlab = "Order_Methods",main="Boxplot for Order_Methods")

#Using Regression Method Least square method
df$Gender.=factor(df$Gender.,levels = c("Male","Female"),labels=c("1","2"))
df$Gender.=as.numeric(df$Gender.)
str(df$Menu_list)

df$Menu_list=factor(df$Menu_list,levels=c("Margherita","Mushroom ",
                            "Other","Pepperoni","Vegies","Chicken"),
                    labels = c("1","2","3","4","5","6"))
df$Menu_list=as.numeric(df$Menu_list)
result=lm(Gender.~Menu_list,data=df)
result
summary(result)
res=resid(result)
head(res)
plot(fitted(result),res)
plot(density(res))

#Gender with Restaurants
result2=lm(Gender.~Restaurants,data=df)
result2
summary(result2)
res1=resid(result2)
res1
plot(fitted(result2),res1)
plot(density(res1))

#MLR multiple linear Regression:
#Back Ward Selection
df$Restaurants=factor(df$Restaurants,levels = c("Dominos","Pizza express","Pizza hut","Pappa johns","Other"),
                      labels = c("1","2","3","4","5"))
df$Restaurants=as.numeric(df$Restaurants)
set.seed(123)

model=lm(formula = Restaurants~Size_rating+Quality_Rating+Pizza_size,data=df)
summary(model)

set.seed(123)
model2=lm(formula = Restaurants~Quality_Rating+Pizza_size,data=df)
summary(model2)

model3=lm(formula = Restaurants~Pizza_size,data=df)
summary(model3)

# Chi-square test for independence between Brand and Factor_Influence?
H0='There is an association between Brand and Factor_Influencing'
H1='There is not association between Brand and Factor_Influencing'
res1=chisq.test(table(df$Brand, df$Factors_Influencing))
res1
pvalue=res1$p.value
pvalue
los=0.05
if(pvalue>los)
{
  print("Accept H0")
  print(H0)
}else{
  print("Cannot Accept H0")
  print(H1)
}



#MANOVA test
H0='Mean of Quality_Rating, Size_Rating and Restaurants are same'
H1='Mean of Quality_Rating, Size_Rating and Restaurants are different'
df$Menu_list=factor(df$Menu_list,levels=c("Margherita","Mushroom ","Other","Pepperoni","Vegies","Chicken"),
                    labels = c("1","2","3","4","5","6"))
df$Menu_list=as.numeric(df$Menu_list)
manovatest=manova(cbind(df$Quality_Rating,df$Menu_list)~df$Restaurants)
manovatest
#Using Wilks Test Method
wilks=summary(manovatest,test="Wilks")       
wilks
los=0.05

#For Pvalue
pvalue1=wilks$stats["df$Restaurants","Pr(>F)"]
pvalue1
if(pvalue1>los)
{
  print("Accept H0")
  print(H0)
}else{
  print("Cannot Accept H0")
  print(H1)
}
#Using Roy Test Method
roy=summary(manovatest,test="Roy")       
roy
los=0.05

#For Pvalue
pvalue2=roy$stats["df$Restaurants","Pr(>F)"]
pvalue2
if(pvalue2>los)
{
  print("Accept H0")
  print(H0)
}else{
  print("Cannot Accept H0")
  print(H1)
}
#Using Hotelling-Lawley Test Method
HotellingLawley=summary(manovatest,test="Hotelling-Lawley")       
HotellingLawley
los=0.05

#For Pvalue
pvalue=HotellingLawley$stats["df$Restaurants","Pr(>F)"]
pvalue
if(pvalue>los)
{
  print("Accept H0")
  print(H0)
}else{
  print("Recept H0")
  print(H1)
}

#Contingency Table
contingency_table=table(df$Gender.,df$Like_it)
contingency_table

contingency_table=table(df$Gender.,df$Menu_list)
contingency_table

contingency_table=table(df$Brand,df$Quality_Rating)
contingency_table

contingency_table=table(df$Restaurants,df$Order_Methods)
contingency_table

#Logistic Regression
df$Like_it <- ifelse(df$Like_it == "YES", 1, 0)
logistic=glm(Like_it~Menu_list+Pizza_size,data=df,family=binomial)
logistic
prediction=predict(logistic,type="response")
head(prediction)
table(df$Like_it,prediction>0.5)
accuracy=mean((prediction>0.5)==df$Like_it)
accuracy


