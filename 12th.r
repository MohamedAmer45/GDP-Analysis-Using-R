#Data Sets
dataset1 = read.csv('GDP & GDP Per Capita At Current Prices.csv')
dataset2 = read.csv('GDP Per Capita Average Annual Growth Rate(1990-2011).csv')
dataset3 = read.csv('Top GDP Per Capita.csv')
dataset4 = read.csv('North America vs Europe.csv')
dataset5 = read.csv('GDP And Quality of Life index.csv')
dataset6 = read.csv('Arabian Countries Before and After 2011.csv')

###################################################################################################
###################################################################################################

#Subsets
sub1=subset(dataset1,dataset1$Year=="2017")
sub1

sub2=subset(dataset1,dataset1$ï..Country=="Egypt")
sub2

sub3=subset(dataset1,dataset1$ï..Country=="Germany")
sub3

sub4 = subset(dataset6,dataset6$Year=="2010")
sub4

sub5 = subset(dataset6,dataset6$Year=="2011")
sub5
###################################################################################################
###################################################################################################

#Descriptive Statistics

#Summary

##GDP in 2017
summary(sub1$GDP)
var(sub1$GDP)
sd(sub1$GDP)
range(sub1$GDP)


##GDP Per Capita in 2017

summary(sub1$GDP.Per.Capita)

sd(sub1$GDP.Per.Capita)
range(sub1$GDP.Per.Capita)
var(sub1$GDP.Per.Capita)
##GDP Average Annual Growth (1990 - 2011)

summary(dataset2$Value)

sd(dataset2$Value)
range(dataset2$Value)
var(dataset2$Value)


###################################################################################################
#PLOTS

##Bar Charts
barplot(sub2$GDP,xlab = "Year",
        ylab ="GDP"
        ,main="GDP In The Last 8 Years (Egypt)"
        ,col = "red"
        ,names.arg = sub2$Year)

barplot(sub2$GDP,xlab = "Year",
        ylab ="GDP"
        ,main="GDP In The Last 8 Years (Germany)"
        ,col = "green"
        ,names.arg = sub2$Year)
###Top 10 GDP Per Capita
barplot(dataset3$GDP.Per.Capita,xlab = "Country",
        ylab = "GDP Per Capita in USD",
        main = "Top 10 Country GDP Per Capita",
        names.arg = dataset3$ï..Country)

###################################################################################################
#Box Plot
boxplot(dataset3$GDP.Per.Capita,xlab = "Country",
        ylab = "GDP Per Capita in USD",
        main = "Top 10 Country GDP Per Capita",
        names.arg = dataset3$ï..Country)

boxplot(sub2$GDP
        ,main="GDP In The Last 8 Years (Germany)"
        ,names.arg = sub2$Year,las=1)

###################################################################################################
#Pie Chart

pie(dataset3$GDP.Per.Capita,xlab = "Country",
        ylab = "GDP Per Capita in USD",
        main = "Top 10 Country GDP Per Capita",
        labels = dataset3$ï..Country)

pie(sub2$GDP,xlab = "Year",
        ylab ="GDP"
        ,main="GDP In The Last 8 Years (Norway)"
        ,labels = sub2$Year)
###################################################################################################
#X,Y Plots

plot(sub3$Year,sub3$GDP,xlab ="Year",
        ylab ="GDP"
        ,main="GDP In The Last 8 Years (Germany)"
        ,col = "red"
        ,las=1,pch=8)

plot(sub2$Year,sub2$GDP,xlab ="Year",
     ylab ="GDP"
     ,main="GDP In The Last 8 Years (Egypt)"
     ,col = "black")
###################################################################################################
#Dot Plot
dotchart(dataset3$GDP.Per.Capita,xlab = "GDP Per Capita in USD",
    main = "Top 10 Country GDP Per Capita",
    labels = dataset3$ï..Country)

dotchart(sub3$Year,xlab ="Year",
     ylab ="GDP"
     ,main="GDP In The Last 8 Years (Germany)"
     ,col = "red"
     ,las=1,pch=8)
###################################################################################################
###################################################################################################
#Confidence Interval

##Confidence Interval (Using z distribution)
avg_z <- mean(sub1$GDP.Per.Capita)
sd_z <- sd(sub1$GDP.Per.Capita)
n_z <- 212
error_z <- qnorm(0.975) * sd_z/sqrt(n_z)
#Lower Bound
avg_z - error_z
#Upper Bound
avg_z + error_z


CI(sub1$GDP.Per.Capita, ci = 0.95)

###################################################################################################

##Confidence Interval (Using t distribution)
avg_t <- mean(dataset3$GDP.Per.Capita)
sd_t <- sd(dataset3$GDP.Per.Capita)
n_t <- 10
error_t <- qt(0.975,df=n_t-1)*sd_t/sqrt(n_t)
#Lower Bound
avg_t - error_t
#Upper Bound
avg_t + error_t


CI(dataset3$GDP.Per.Capita, ci = 0.95)
####################################################################################################
##################################################################################################
#One Sample t test

# H0: mu < 20000
mean(sub1$GDP.Per.Capita)

One_sample_test <- t.test(sub1$GDP.Per.Capita,
       mu=20000,alternative = "less",
       conf.level = 0.95)

One_sample_test

#two-sided
One_sample_test2 <-t.test(sub1$GDP.Per.Capita,
       mu=20000,alternative = "two.sided",
       conf.level = 0.95)
One_sample_test2

####################################################################################################
##################################################################################################
#Two Sample t test

#H0: mean GDP of EUROPE = of North America
#two- sided test
#Assume Non - equal variances

two_sample_test <- t.test(dataset4$GDP ~ dataset4$Continent,
                          mu=0,
                          alternative="two.sided",
                          conf=0.95)

two_sample_test



####################################################################################################
##################################################################################################
#Linear Regression

cor(dataset5$Quality.of.Life.Index, dataset5$GDP)

model <- lm(dataset5$Quality.of.Life.Index ~ dataset5$GDP)

summary(model)

coef(model)

library(ggplot2)
ggplot() +
        geom_point(aes(x = dataset5$GDP, y = dataset5$Quality.of.Life.Index),
                   colour = 'red') +
        geom_line(aes(x = dataset5$GDP, y = predict(model)),
                  colour = 'blue') +
        ggtitle('GDP vs Quality of Life') +
        xlab('GDP Per Capita ') +
        ylab('Quality of Life Index')

####################################################################################################
##################################################################################################
#Chi-Squared Test

#H0 Assume that the GDP for germany is eqaul to the same value every year

chisq.test(sub3$GDP)

####################################################################################################
####################################################################################################
#Wilcoxon Signed Rank

boxplot(sub4$GDP,sub5$GDP)

#H0: Median change in GDP is 0
#two-sided test

wilcox.test(sub4$GDP,sub5$GDP,mu=0,
            alt="two.sided",
            paired = T,
            conf.int = T,
            conf.level = 0.95)

