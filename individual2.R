dat<- readxl::read_xls('GSS.xls')
library(ggplot2)

colnames(dat)
#Question 1
names(dat) = c('Year', 'party', 'income', 'sex', 'education', 'age', 'marital', 'work hrs', 'id', 'happiness')
dat$marital = as.factor(dat$marital)
dat$sex = as.factor(dat$sex)
dat$party = as.factor(dat$party)
dat$happiness = as.factor(dat$happiness)
dat$age = as.factor(dat$age)
dat$education = as.factor(dat$education)
dat$income = as.factor(dat$income)



dat1 = dat[dat$marital != "No answer",]
dat1 = dat1[dat1$happiness == "Not too happy" | dat1$happiness == "Pretty happy" | dat1$happiness == "Very happy",]
dat1 = na.omit(dat1)
dat1$marital = factor(dat1$marital, levels = c("Married", "Never married", "Divorced", "Widowed", "Separated"))


ggplot(dat1, aes(x= marital, fill = happiness)) + geom_bar() + xlab("Marital Status") + ylab("Count") + ggtitle("Relationship between Happiness and Marital Status")


#Question 2
dat2 = dat1
ggplot(dat2, aes(x = sex, fill = happiness)) + geom_bar(position = 'dodge') + facet_wrap(~marital)


#Question 3
dat3<- dat[dat$party != "Don't know" & dat$party != "No answer" & dat$party != "Other party", ]
dat3 <- na.omit(dat3)

levels(dat3$party)<- c("Don't know", "Weak Democrat", "Weak Republican", "Independent", "No answer", "Democrat", "Republican", "Other Party", "Strong Democrat", "Strong Republican")
levels(dat3$party)

dat3 <- dat3[dat3$happiness == "Not too happy" | dat3$happiness == "Pretty happy" | dat3$happiness == "Very happy",]

dat3$party <- factor(dat3$party, levels = c("Strong Democrat", "Democrat", "Weak Democrat", "Independent", "Weak Republican", "Republican", "Strong Republican", "Other Party"))


ggplot(dat3, aes(x= party, fill = happiness)) + geom_bar(position = 'dodge') + xlab("Political Party") + ylab("Count") + ggtitle("Relationship between Happiness and Political party") + scale_fill_discrete(name = "Happiness")

#Question 4
#What year seems to have the most happiest and least happiest respondents?



#What is the relationship between income and highest year of school completed?
levels(dat$income)
dat4c<- dat[dat$income != "Don't know" & dat$income != "No answer" & dat$income != "Not applicable" & dat$income != "Refused", ]
dat4c<- dat4c[dat4c$education != "Don't know" & dat4c$education != "No answer", ]

labels<- paste(sort(as.integer(levels(dat4c$education))))
print(labels)

dat4c$education = factor(dat4c$education, levels = labels)


dat4c<- dat4c[dat4c$education != "0" & dat4c$education != "1" & dat4c$education != "2" & dat4c$education != "3" & dat4c$education != "4", ]

dat4c = na.omit(dat4c)


ggplot(dat4c, aes(x= income, fill = education)) + geom_bar(position='dodge') + xlab("Income") + ylab("Count") + ggtitle("Relationship between Education Level and Income") + scale_fill_discrete(name = "Education Level")



#How does the age of each respondent affect what political party they are in?
dat4d<- dat[dat$party != "Don't know" & dat$party != "No answer" & dat$party != "Other party", ]
dat4d<- dat4d[dat4d$age != "Don't know" & dat4d$age != "No answer", ]
dat4d <- na.omit(dat4d)

levels(dat4d$party)
levels(dat4d$party)<- c("Don't know", "Weak Democrat", "Weak Republican", "Independent", "No answer", "Democrat", "Republican", "Other Party", "Strong Democrat", "Strong Republican")
levels(dat4d$party)
dat4d$party <- factor(dat4d$party, levels = c("Strong Democrat", "Democrat", "Weak Democrat", "Independent", "Weak Republican", "Republican", "Strong Republican"))

levels(dat4d$party)


ggplot(dat4d, aes(x= party, fill = age)) + geom_bar() + xlab("Political Party") + ylab("Count") + ggtitle("Relationship between age of respondent and political party") + scale_fill_discrete(name = "Education Level")

