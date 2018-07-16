#Question 1. Import the Titanic Dataset from the link Titanic Data Set.
#Perform the following:
#  a. Is there any difference in fares by different class of tickets?
#  Note - Show a boxplot displaying the distribution of fares by class

boxplot(fare~pclass,data= titanic,
        main="Fares Versus Pclass",xlab="Fares",ylab="Class",col=topo.colors(3))

#b. Is there any association with Passenger class and gender?
#  Note - Show a stacked bar chart

counts<-table(titanic$sex,titanic$pclass)
barplot(counts, main = "Distribution of Class by gender", xlab="Pclass", col=c("blue", "red"), legend = c("Female","Male"), names.arg = c("Pclass1st", "Pclass2nd","Pclass3rd"))
#Performing chi-square test for association
chisq.test(titanic$pclass ,titanic$sex)

#it represents that there is association with Passenger class and gender