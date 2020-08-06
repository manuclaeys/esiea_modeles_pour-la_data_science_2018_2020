

######EXO 2
#l
library(tree)
data <- read.csv2('/home/manue/Documents/manue/Cours/cour ESIEA/cour esiea 5A DTM 2018-2019/Salaries.csv', sep =',', header = TRUE)
data$X <- NULL
salaire <- as.data.frame(data)
summary(data)

#lorsque le nombre de param?tres du mod?le est trop ?lev?, le sur-apprentissage
#nous guette (overfitting en anglais).
#Le classifieur "colle" trop aux donn?es et,au lieu d'int?grer
#les informations essentielles qui se rapportent ?
#la population, il ing?re les particularit?s de l'?chantillon 
#d'apprentissage

###lin?aire 
tree.Lin <- tree(salary ~ yrs.service + yrs.since.phd, data=salaire)
plot(tree.Lin)
text(tree.Lin, cex=.75)



###lin?aire log
tree.model <- tree(log(salary) ~ yrs.service + yrs.since.phd, data=salaire)
plot(tree.model)
text(tree.model, cex=.75)

#Pourquoi log sur salaire ?
#=>Expliqu? en TD




salar.deciles <- quantile(salaire$salary, 0:10/10)
cut.prices    <- cut(salaire$salary, salar.deciles, include.lowest=TRUE)
plot(salaire$yrs.service, salaire$yrs.since.phd, col=grey(10:2/11)[cut.prices], pch=20, xlab="yrs.service",ylab="yrs.since.phd")
partition.tree(tree.model, ordvars=c("yrs.service","yrs.since.phd"), add=TRUE)
#Commentez partition.tree
#=> Expliqu? en TD (les points sont reli? ? leurs classes)

#Pr?diction de rank

plot(salaire$yrs.since.phd, salaire$salary, pch=19, col=as.numeric(salaire$rank))
partition.tree(tree.model, label="Species", add=TRUE)
legend("topright",legend=unique(salaire$rank), col=unique(as.numeric(salaire$rank)), pch=19)



summary(tree.model)


###Prune (pas dans le td)



tree.model2 <- tree(log(salary) ~ yrs.service + yrs.since.phd, data=salaire, mindev=0.001)
plot(tree.model2)
text(tree.model2, cex=.75)
summary(tree.model2)

#Attention coquille dans le TD ==> On demande d'essayer de 
#d'?laguer les deux arbres
pruned.tree <- prune.tree(tree.model2, best=4)
plot(pruned.tree)
text(pruned.tree)

plot(tree.model)
text(tree.model)
pruned.tree <- prune.tree(tree.model)
plot(pruned.tree)
text(pruned.tree)



#### La fonction prune permet d'imposer une ?l?gation avec le param?tre best. 
#On ne peut pas pruner une arbres sans param?tre!
#La fonction tree vous fait un arbre par d?faut, 
#selon votre volont?e vous pouvez l'?laguer (prune) ou le faire grandir (mindev petit)



######EXO 2
#charger les donn?es - attention aux options
data <- read.csv2('/home/manue/Documents/manue/Cours/cour ESIEA/cour esiea 5A DTM 2018-2019/titanic.csv', sep =',', header = TRUE)
titanic <- as.data.frame(data)

###Summary
summary(titanic)

###Commentez

###Installer et charger
library(rpart)
library(party)
library(rpart.plot)


str(titanic)

myFormula <- survived ~  class  + age  #

##CART##
fit <- rpart(myFormula, method="class", data=titanic)

printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

# plot tree
plot(fit)
text(fit)
 

library(rattle)
library(rpart.plot)

library(RColorBrewer)

#On am?liore le graphique qui n'est pas tr?s lisible
fancyRpartPlot(fit)

##Commentez les info
#proba , % de la pop gauche = Y droit= N

library(partykit)
##CTREE##
titanic_tree <- ctree(myFormula, data=titanic)
print(titanic_tree)
plot(titanic_tree)



###Ajouter la variable sex et refaire avec les deux types d'arbres

##Faire avec tout les param?tres
myFormula2 <- survived ~  class  + age + sex  #
titanic_tree2 <- ctree(myFormula2, data=titanic)
print(titanic_tree2)
plot(titanic_tree2)

fit2 <- rpart(myFormula2, method="class", data=titanic)
fancyRpartPlot(fit2)


#CTREE vs CART
#http://stats.stackexchange.com/questions/12140/conditional-inference-trees-vs-traditional-decision-trees






# check the prediction
predictions <- predict(titanic_tree, titanic)
table(titanic$survived, predictions)



predictions <- predict(titanic_tree2, titanic)
table(titanic$survived, predictions)




######Random Forests 


# Random Forest prediction 
#Random forests are an ensemble learning method for classification 
#(and regression) that operate by constructing a multitude of decision 
#trees at training time and outputting the class that is the mode of the 
#classes output by individual trees
#Random forests improve predictive accuracy by generating a large number 
#of bootstrapped trees (based on random samples of variables), classifying a case using 
#each tree in this new "forest", and deciding a final predicted outcome by combining the 
#results across all of the trees (an average in regression, a majority vote in classification). 
#Breiman and Cutler's random forest approach is implimented via the randomForest package. 

library(randomForest)
r <- randomForest(survived ~., data=titanic, importance=TRUE, do.trace=100, ntree=100)
print(r)
plot(r)
predictions <- predict(r, titanic)
table(titanic$survived, predictions)



r <- randomForest(survived ~  class  + age , data=titanic, importance=TRUE, do.trace=100, ntree=100)
print(r)
plot(r)
predictions <- predict(r, titanic)
table(titanic$survived, predictions)



