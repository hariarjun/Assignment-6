#Question 1:Import the Titanic Dataset from the link Titanic Data Set.
#Perform the following:
#  a. Preprocess the passenger names to come up with a list of titles that represent families
#and represent using appropriate visualization graph.

titanic3_1_$FamilyName <- sapply(strsplit(titanic3_1_$name, " "), head, 1)

titanic3_1_$FamilyName <- gsub(",","",titanic3_1_$FamilyName)

titanic3_1_ <- transform(titanic3_1_, Family = ifelse(as.numeric(as.character(sibsp))>0,"F",ifelse(as.numeric(as.character(parch))>0,"F","S")))

familiesdf <- sqldf('SELECT * FROM titanic3_1_ WHERE Family ="F" ')

plot(table(familiesdf$FamilyName),xlab ="Family Name Titles", ylab = "Title Count", main ="List of Titles of Families")

barplot(sort$Freq, names.arg = sort$FamilyNames, horiz = FALSE, las = 1, cex.names = 0.8, xlab = "Titles", ylab = "Title Count", main = "Family Title Wise Distribution", ylim = c(1,12))

#b. Represent the proportion of people survived from the family size using a graph.

library(sqldf)
survivors <- sqldf('SELECT survived, FamilyName FROM familiesdf')
FamSize <- as.data.frame(survivors %>% group_by(FamilyName) %>% count(FamilyName))
head(FamSize)
FamSurvive <- as.data.frame(survivors %>% group_by(FamilyName) %>% summarise(survived = sum(survived)))
head(FamSurvive)
SizeSurvive <- cbind(FamSize, FamSurvive)
head(SizeSurvive)
colnames(SizeSurvive)[2] <- "Size"
SizeSurvive <- SizeSurvive[-3]
SizeSurvive %>% group_by(Size) %>% count(Size)
SizeFamCount <- as.data.frame(SizeSurvive %>% group_by(Size) %>% count(Size))
SizeSurvCount <- as.data.frame(SizeSurvive) %>% group_by(Size) %>% summarise(survived = sum(survived))
head(SizeFamCount)
head(SizeSurvCount)
SizeSurvivalAnalaysis <- cbind(SizeFamCount, SizeSurvCount)
head(SizeSurvivalAnalaysis)
SizeSurvivalAnalaysis <- SizeSurvivalAnalaysis[-3]
attach(SizeSurvivalAnalaysis)
SizeSurvivalAnalaysis$PropSurv <- round((survived / (Size * n)),2)*100
detach(SizeSurvivalAnalaysis)
head(SizeSurvivalAnalaysis)

#Plotting
plot(x = SizeSurvivalAnalaysis$Size, y = SizeSurvivalAnalaysis$PropSurv, xlab = "Family Size", ylab = "Proportion Survived", main = "Family Size Wise Survival Analysis")

#c. Impute the missing values in Age variable using Mice Library, create two different
#graphs showing Age distribution before and after imputation.

hist(titanic3_1_$age)                               
xyplot(titanic3_1_, age~pclass, pch=18,cex=1, main = "Distribution of Original and Imputed    Age on Pclass")
summary(titanic3_1_$age)
skewness(titanic3_1_$age, na.rm = TRUE)
kurtosis(titanic3_1_$age, na.rm = TRUE)
library(mice)
tempdata <- mice(titanic3_1_, m=5, method = 'pmm', maxit = 50, seed = 500)
summary(tempdata)
completetitanic <- complete(tempdata,5)
hist(completetitanic$age)
summary(completetitanic$age)
mean(completetitanic$age, na.rm = FALSE)
mean(completetitanic$age, na.rm = TRUE)
skewness(completetitanic$age, na.rm = TRUE)
kurtosis(completetitanic$age, na.rm = TRUE)
plot(x = completetitanic$pclass, y = completetitanic$age, xlab = "Passenger Class", ylab = "Passenger Age", main = "Post Imputing Distribution of Age on PClass")
xyplot(tempdata, age~pclass, pch=18,cex=1, main = "Distribution of Original and Imputed Age on pclass")
xyplot(tempdata, age~fare, pch=18,cex=1, main = "Distribution of Original and Imputed Age on fare")