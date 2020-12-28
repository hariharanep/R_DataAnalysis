setwd("~/Desktop")
data = read.csv('flavors_of_cacao.csv')
data = data[, c(1,2,5,6,7,8,9)]
data$Cocoa.Percent = as.numeric(sub("%", "", data$Cocoa.Percent))

#Outputs summary statistics of data
summary(data)

#Plots simple visualization of Rating values for Chocolate Bars in Dataset
plot(x = c(1:1795),y = data$Rating, ylab="Rating", xlab="Chocolate Bar Index")

#Plots simple visualization of Relationship between Rating values and Cocoa.Percent
#for Chocolate Bars
plot(x = data$Cocoa.Percent,y = data$Rating, ylab="Rating", xlab="Cocoa.Percent")

attach(data)

#Is there a significant relationship between the Cocoa.Percent value and the Rating value for a certain chocolate bar?
mod = lm(Rating~Cocoa.Percent)
#Need two graphs returned by this to check assumptions of Simple Linear Regression Test
plot(mod)
summary(mod)

#Is there a certain country that produces chocolate bars with a significantly different average rating than the other countries?
mod = lm(Rating~Company.Location)
#code below checks standard deviation assumption of One Way Anova Test
locations = unique(Company.Location)
deviations = c()
for (i in 1:length(locations)) { deviations[i] = sd(data[data$Company.Location == locations[i], ]$Rating);}
deviations
#line below checks normality assumption of One Way Anova Test
qqnorm(aov(mod)$residuals)
anova(mod)

#Is there a certain type of Bean that produces chocolate bars with a significantly different ratings than the other types of Bean?
mod = lm(Rating~Bean.Type)
#code below checks standard deviation assumption of One Way Anova Test
bean_types = unique(Bean.Type)
deviations = c()
for (i in 1:length(bean_types)) { deviations[i] = sd(data[data$Bean.Type == bean_types[i], ]$Rating);}
deviations
#line below checks normality assumption of One Way Anova Test
qqnorm(aov(mod)$residuals)
anova(mod)




