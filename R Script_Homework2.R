setwd("C://Users/Ritika Agarwal/Desktop/R/Homework 2-Measurement")

list.of.packages <- c("tidyverse", "knitr","dbplyr","dplyr","ivpack","haven","broom","desc","lmtest","stargazer","tibble","ggplot2","psych")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")
invisible(lapply(list.of.packages, library, character.only = TRUE))

df<-read_dta(file= "Data HW 2/civ88_mort.dta")
describe(df)

#transforming the weights to account for the fact that the probability of a woman 
#getting sampled is not the same in every household 
new_weights<-(df$pweight*df$n_wom)
#new_weights1<-new_weights/4418.927

df<-cbind(df,new_weights)
summary(df)
comp_weights<-df %>%
  select(pweight,new_weights,n_wom)

kable(tail(comp_weights,round=3,n=10,caption='Comparison of Old and New Weights of Sample'))


sum(new_weights)
summary(df)

#now we have our new weights, and we can see that for houesholds with more women, 
#we have a larger weight assigned because the probability for a women to get sampled 
#there is much lower than a hh with 1 woman. Thus the weights are now representative 
#the population of women in the sample 

#computing the child mortality rates 

#mortality= no.dboys/no.boys
#method1: number of boys/girls born in 0 to 4 years and number of them dead in 0 to 4 years 

bmor1<-df$dboys_l5/df$boys_l5
describe(bmor1) 

#we see that bmor1, has only 1431 observations which are valid because the rest are divided by 0, changing them to NaN. 
#the linear model directly omits them, as these families have had no children in the past 0 to 4 years 
#thus they are removed from the sample automatically 

df<-cbind(df,bmor1)
Reg1<-lm(bmor1~1, data=df, weights=new_weights)
summary(Reg1)

#mean= 0.103490 se: 0.007 t value: 14.2 

gmor1<-df$dgirls_l5/df$girls_l5
describe(gmor1)

df<-cbind(df,gmor1)
Reg2<-lm(gmor1~1, data=df, weights=new_weights)
summary(Reg2)

stargazer(Reg1,Reg2,type="latex",title="Mortality Method 1",
          covariate.labels="Mean",column.sep.width = "10pt",header=FALSE)
# mean= 0.08 se: 0.006 t value= 11.83 

#we see only 1428 observations are used as the others are cases where no girls were born
#thus no girls died 
#thus they don't effect the mortality rate 

#method 2 
#taking children born 5 to 9 years ago and divide the number of deaths in 0 to 4 years 
#by the number of children 

bboys_5to9<-df$boys-df$boys_l5
describe(bboys_5to9) #we see here that it has 2707 observations 

bgirls_5to9<-df$girls-df$girls_l5
describe(bgirls_5to9) #2707

dboys_before5<-df$dboys_u5-df$dboys_l5
describe(dboys_before5) #2707

dgirls_before5<-df$dgirls_u5-df$dgirls_l5
describe(dgirls_before5) #2707 

df<-cbind(df,bboys_5to9,bgirls_5to9,dboys_before5,dgirls_before5)

bmor2<-df$dboys_before5/df$bboys_5to9
describe(bmor2)
#we see that the current calculation has only 1332 observations 

Reg3<-lm(bmor2~1, data=df, weights=new_weights)
summary(Reg3)
#we see a mean= 0.120 se=0.008 tvalue=14.56 
#1375 obs deleted

gmor2<-df$dgirls_before5/df$bgirls_5to9
describe(gmor2)
#we see that the calculation has only 1291 values 

Reg4<-lm(gmor2~1, data=df, weights=new_weights)
summary(Reg4)

df<-cbind(df,bmor2,gmor2)
stargazer(Reg3,Reg4,type="latex",title="Mortality Method 2",
          covariate.labels="Mean",column.sep.width = "10pt",header=FALSE)

#we see mean=0.116 se=0.008 tvalue=14 
#1416 observations deleted due to missingness 

#PART 2 OF HOMEWORK 

nutridf<-read_dta("Data HW 2/civ88-08_nutri.dta")

#identifying the missing values and outliers
weights_1988<-nutridf %>%
   filter(year==1988)%>%
  select(year,weightkg) 
p<-summary(weights_1988)

describe(weights_1988)
#we see that there are 6482 weight variables available
#the total hh for 1988 are 7266 #max= 73kgs and #min is 0.85
#mean weight of child is 12.5 sd=4.63

weights_1993<-nutridf %>%
  filter(year==1993) %>%
  select(year,weightkg)
q<-summary(weights_1993)
describe(weights_1993)
#we see that there are 9905 hh and 7109 weight observations 
#min= 0.01 and #max= 90.99
#mean = 11.98 sd= 3.75

weights_2008<-nutridf %>%
  filter(year==2008)%>%
  select(year,weightkg)
r<-summary(weights_2008)
describe(weights_2008)
#we see that there are 9244 hh and 7333 weight observations
#the min= 0.2 and the max=950 
#we see a huge standard deviation, possibly because of the problems in reporting 
#mean= 41.59, sd= 58.92
table2<-cbind(p,q,r)
kable(table2,caption='Average Weights over the Years',col.names =c("1988","1993","2008"))
#we see overall, there are 5491 missing values in the weightkg observations
#let's see whether we can remove these missing observations and still maintain
#a sample of good quality 

missing <- nutridf %>% 
  summarise_all(funs(mean(is.na(.))))

kable(missing,caption=' Missing Values of Height & Weight in Dataset')

print(missing)
#we see that around 20.8% of weight obs & 26.3% of height obs are missing 

#it is therefore important to see that these values are missing and will affect the way we 
#can interpret our results 
#however, for now we omit the missing values as we cannot impute them. 

height_1988<-nutridf%>%
  filter(year==1988)%>%
  select(year,height)
describe(height_1988) #out of 7266, 786 values missing #10.8% missing

height_1993<-nutridf%>%
  filter(year==1993)%>%
  select(year,height)

describe(height_1993)#out of 9905, 2815 missing, #28.4% values missing 

height_2008<-nutridf %>%
  filter(year==2008)%>%
  select(year,height)

describe(height_2008)#out of 9244 observations, 3355 obs missing, 36.29%missing 
m<-summary(height_1988)
n<-summary(height_1993)
o<-summary(height_2008)
table3<-cbind(m,n,o)
kable(table3,caption='Inspecting Height for Missing Values & Outliers',
      col.names =c("1988","1993","2008"))

#the share of missing values in each year is very important for our inferences
#of stunting and other rates and observations for each year and we should keep it in mind
#however, since we cannot impute them, we must omit them and keep these fig in mind

nutriclean<-na.omit(nutridf)
describe(nutriclean)
summary(nutriclean)

#now all our variables are clean with no NA 
#we see through summary that, height has lots of outlier values, and we are interested in removing them 

a<-boxplot(nutriclean$height)

nutriclean1<-nutriclean$height
bench1<-98.00 +1.5*IQR(nutriclean$height)
bench2<-75.67-1.5*IQR(nutriclean$height)
bench1
bench2
#thus we see that we want remove any outliers above 131.49 and below 42.18

nutriclean1<-nutriclean1[nutriclean1<bench1]
nutriclean1<-nutriclean1[nutriclean1>bench2]
boxplot(nutriclean1)

b<-boxplot(nutriclean1)

nutricleandf<-nutriclean %>%
  filter(height>42.18, height<131.4875)

describe(nutricleandf)
summary(nutricleandf) # we see that we have lost only 2.6% of the obs lost 

#for further calculations, we will use nutricleandf which is filtered for height outliers 

#PART 2 QUESTION 2 
#computing z score for each child

z<- nutricleandf$height - nutricleandf$medheight
describe(z) #18887 obs 
summary(z) 

zscore<- z/nutricleandf$stdheight
describe(zscore)
summary(zscore) 

kable(head(zscore),caption='Z scores computation',col.names = "Zscore")

nutricleandf<-cbind(nutricleandf,zscore)
#we can observe that the range is extremely high due to the outlier values 
#we thus need to clean for zscore as we are interested in them further  

#cleaning zscore

c<-boxplot(nutricleandf$zscore)

nutriclean2<-nutricleandf$zscore
bench3<- -0.02253 + 1.5*IQR(nutricleandf$zscore)
bench4<- -2.6397 -1.5*IQR(nutricleandf$zscore)
bench3
bench4
#thus we see that we want to remove any outliers above 3.90and below -6.565

nutriclean2<-nutriclean2[nutriclean2<bench3]
nutriclean2<-nutriclean2[nutriclean2>bench4]
boxplot(nutriclean2)

c<-boxplot(nutriclean2)

describe(nutriclean2) #thus we see 17611 values remain in zscore out of 18887 

#6% of the values but have to be removed to account for extreme outliers
#we should make note of this fact 

nutricleandf<-nutricleandf%>%
  filter(zscore>-6.565,zscore<3.903)

describe(nutricleandf)

newdataset<-nutricleandf%>%
  select(year,pweight,age,height,zscore,conspc88)
u<-summary(newdataset)

kable(u,digits=3,caption='Checking the Dataset')

#we see only 17611 observations are left

#Stunted children are defined as children showing a height stature that is lower than the median of the
#reference population by 2 standard deviations measured on the same reference population. That is children
#such as the z-score is lower than -2 (z <= -2), where the z-score is given by the following formula

#stunting rate 
#dummy 1=north, 0=south 

#creating dummy for stunted children 

stunted<-ifelse(nutricleandf$zscore<=-2,1,0)

nutricleandf<-cbind(nutricleandf,stunted)

#thus we have created a variable stunted, which takes value 1 when stunted, and 0 when not

#1988

nstuntcomp88<-nutricleandf%>%
  filter(age>=24,age<=48,north==1,year==1988) %>%
  select(year,north,zscore,age,pweight,height,stunted)

stunt88N<-nstuntcomp88 %>%
  filter(stunted==1)%>%
  select(year,north,zscore,age,pweight,height,stunted)

sum(stunt88N$pweight)
sum(nstuntcomp88$pweight)
avgsr88n<-189.090/647.546 #29.2% stunt rate in north

sstuntcomp88<-nutricleandf%>%
  filter(age>=24,age<=48,north==0,year==1988) %>%
  select(year,north,zscore,age,pweight,height,stunted)

stunt88S<-sstuntcomp88 %>%
  filter(stunted==1)%>%
  select(year,north,zscore,age,pweight,height,stunted)

sum(stunt88S$pweight)
sum(sstuntcomp88$pweight)
avgsr88s<-426.143/1838.602 #the stunt rate is 23.17% in south

#we see that the stunting rate is infact smaller in south region than north

#1993

nstuntcomp93<-nutricleandf%>%
  filter(age>=24,age<=48,north==1,year==1993)%>%
  select(year,north,zscore,age,pweight,height,stunted)

stunt93N<-nstuntcomp93 %>%
  filter(stunted==1)%>%
  select(year,north,zscore,age,pweight,height,stunted)

sum(stunt93N$pweight)
sum(nstuntcomp93$pweight)
avgsr93n<-374.806/907.469 #stunt rate in north #41.30%

sstuntcomp93<-nutricleandf%>%
  filter(age>=24,age<=48,north==0,year==1993) %>%
  select(year,north,zscore,age,pweight,height,stunted)

stunt93S<-sstuntcomp93 %>%
  filter(stunted==1) %>%
  select(year,north,zscore,age,pweight,height,stunted)

sum(stunt93S$pweight)
sum(sstuntcomp93$pweight)
avgsr93s<-1285.39/2947.12 #43.61


#2008

nstuntcomp08<-nutricleandf%>%
  filter(age>=24,age<=48,north==1,year==2008)%>%
  select(year,north,zscore,age,pweight,height,stunted)

stunt08N<-nstuntcomp08 %>%
  filter(stunted==1)%>%
  select(year,north,zscore,age,pweight,height,stunted)

sum(stunt08N$pweight)
sum(nstuntcomp08$pweight)
avgsr08n<-272.53/585.12 #46.57% stunting

sstuntcomp08<-nutricleandf%>%
  filter(age>=24,age<=48,north==0,year==2008) %>%
  select(year,north,zscore,age,pweight,height,stunted)

stunt08S<-sstuntcomp08 %>%
  filter(stunted==1) %>%
  select(year,north,zscore,age,pweight,height,stunted)

sum(stunt08S$pweight)
sum(sstuntcomp08$pweight)
avgsr08s<-799.55/1974.02 #40.50%

Stuntingtable<-cbind(avgsr88n,avgsr88s,avgsr93n,avgsr93s,avgsr08n,avgsr08s)

kable(Stuntingtable,caption='Stunting Rates in North and South Over Time',
      col.names=c("North 1988","South 1988","North 1993","South 1993",
                  "North 2008","South 2008"))

#PART 2 Q3)B)

#stunt rate girls/boys 1988

bstuntcomp88<-nutricleandf%>%
  filter(age>=24,age<=48,sexe==1,year==1988) %>%
  select(year,sexe,zscore,age,pweight,height,stunted)

stunt88b<-bstuntcomp88%>%
  filter(stunted==1)%>%
  select(year,sexe,zscore,age,pweight,height,stunted)

sum(stunt88b$pweight)
sum(bstuntcomp88$pweight)
avgsr88boys<-326.399/1272.552 #25.64% 

gstuntcomp88<-nutricleandf%>%
  filter(age>=24,age<=48,sexe==0,year==1988) %>%
  select(year,sexe,zscore,age,pweight,height,stunted)

stunt88g<-gstuntcomp88 %>%
  filter(stunted==1) %>%
  select(year,sexe,zscore,age,pweight,height,stunted)

sum(stunt88g$pweight)
sum(gstuntcomp88$pweight)
avgsr88girls<-288.833/1213.597 #23.79%

#stunt rate girls/boys 1993


bstuntcomp93<-nutricleandf%>%
  filter(age>=24,age<=48,sexe==1,year==1993) %>%
  select(year,sexe,zscore,age,pweight,height,stunted)

stunt93b<-bstuntcomp93%>%
  filter(stunted==1)%>%
  select(year,sexe,zscore,age,pweight,height,stunted)

sum(stunt93b$pweight)
sum(bstuntcomp93$pweight)
avgsr93boys<-875.01/1958.69 #44.67% 

gstuntcomp93<-nutricleandf%>%
  filter(age>=24,age<=48,sexe==0,year==1993) %>%
  select(year,sexe,zscore,age,pweight,height,stunted)

stunt93g<-gstuntcomp93 %>%
  filter(stunted==1) %>%
  select(year,sexe,zscore,age,pweight,height,stunted)

sum(stunt93g$pweight)
sum(gstuntcomp93$pweight)
avgsr88girls<-785.18/1895.901 #41.41%


#stunt rate girls/boys 2008

bstuntcom08<-nutricleandf%>%
  filter(age>=24,age<=48,sexe==1,year==2008) %>%
  select(year,sexe,zscore,age,pweight,height,stunted)

stunt08b<-bstuntcomp08%>%
  filter(stunted==1)%>%
  select(year,sexe,zscore,age,pweight,height,stunted)

sum(stunt08b$pweight)
sum(bstuntcomp08$pweight)
avgsr08boys<-580.09/1337.92 #43.35% 

gstuntcomp08<-nutricleandf%>%
  filter(age>=24,age<=48,sexe==0,year==2008) %>%
  select(year,sexe,zscore,age,pweight,height,stunted)

stunt08g<-gstuntcomp08 %>%
  filter(stunted==1) %>%
  select(year,sexe,zscore,age,pweight,height,stunted)

sum(stunt08g$pweight)
sum(gstuntcomp08$pweight)
avgsr08girls<-500.03/1231.23 #40.61%

Stuntingtable2<-cbind(avgsr88boys,avgsr88girls,
                      avgsr93boys,avgsr93girls,avgsr08boys,
                      avgsr08girls)

kable(Stuntingtable2,caption='Stunting Rates in Boys and Girls Over Time',
      col.names=c("Boys 1988","Girls 1988","Boys 1993","Girls 1993","Boys 2008","Girls 2008"))


#Part 2 Question 4

#1988

data1988<-nutricleandf%>%
  filter(year==1988)

Reg5<-lm(stunted~conspc88, data=data1988, weights=pweight)
summary(Reg5)

#WE SEE A NEGATIVE RELATIONSHIP, THAT IS WHEN INCOME RISES, THE STUNTING REDUCES
#POOR CHILDREN ARE MORE STUNTED IN 1988
#THE REG COEFF=-2.190e
#P VALUE= SIGNIFICANT, so is T
#WHILE WE CAN SEE THE RELATION, IT'S NOT VERY STRONG

#1993

data1993<-nutricleandf%>%
  filter(year==1993)

Reg6<-lm(stunted~conspc88, data=data1993, weights=pweight)
summary(Reg6)
#we see that the coefficient is 5.280 but it isn't significant at 5%level

#2008

data2008<-nutricleandf%>%
  filter(year==2008)

Reg7<-lm(stunted~conspc88, data=data2008, weights=pweight)
summary(Reg7)

stargazer(Reg5,Reg6,Reg7,type="latex",
          title="Number of Stunted Children and Consumption of Households",
          covariate.labels="Gradient",column.sep.width = "10pt",header=FALSE)

plot5 <- ggplot(data1988, aes(x=conspc88, y = stunted)) + 
  geom_smooth(method='lm') +
  theme_classic() 
plot5

plot6<-ggplot(data1993, aes(x=conspc88, y = stunted)) + 
  geom_smooth(method='lm') +
  theme_classic() 
plot6

plot7<-ggplot(data2008, aes(x=conspc88, y = stunted)) + 
  geom_smooth(method='lm') +
  theme_classic() 
plot7

Data_US <- read_csv("C:/Users/Ritika Agarwal/Desktop/R/Homework 2- Measurement/Data HW 2/final data.csv")
Reg10<-lm(US_unemp ~ US_mig + US_percap, data = Data_US)
stargazer(Reg10,header=FALSE,title="Regression of Unemployment on Migration and Per Capita Income in US for 1990-2017", 
          notes=c("source: The World Bank,The OECD, World Development Indicators"))