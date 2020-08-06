#'''
#I. Basic Methods to select the best model as a function of complexity
#Training et Test sets to select the best models
#'''

# 0) Chargement et installation des librairies
#install.packages('ISLR')
#install.packages('leaps')
library(ISLR)
library(leaps)

# 1) Hiiters dataset dans lequel on supprime les données manquantes
Hitters=na.omit(Hitters)

# 2) Création de l échantillon test, 180 individus sur 263
set.seed(1)
train=sample(seq(263),180,replace=FALSE)
train=sample(1:263,180,replace=FALSE)
toto <- Hitters[train,]
# 3.a) On tente de modéliser le salaire des joueurs par un pack de 19 variables
#    Pour chaque complexité allant de 1 à 19, on sélectionne le meilleur modèle
#    grâce à une forward selection (sur le training set)
#    On va donc obtenir 19 modèles comprenant entre une et 19 variables

best_models19_forward=regsubsets(Salary~.,data=Hitters[train,],nvmax=19,method='forward')

# 3.b) Pour accéder aux cpefficient du modèle 5, on appelle la fonction coeff
#      Pour accéder aux RSS des modèles, on lance la fonction summary

coef(best_models19_forward,5)
summary(best_models19_forward)$rss

#4) On a 19 modèles, il faut choisir le meilleur. Pour ce faire, on va appliquer
#    chacun des modèles sur le test set et calculer la MSE
mse=rep(NA,19)
test=model.matrix(Salary~.,data=Hitters[-train,])
for(i in 1:19){
  coefi=coef(best_models19_forward,id=i)
  pred=test[,names(coefi)]%*%coefi
  mse[i]=mean((Hitters$Salary[-train]-pred)^2)
}  
  
  
# 5) Ici on plot les RMSE des 19 modèles sur le training et sur le test set
#    On choisit le modèle qui a la RMSE la plus petite sur le test set
plot(sqrt(mse),ylab='Root MSE des 19 modèles',pch=19,ylim=c(300,400),type='b')
points(sqrt(best_models19_forward$rss[-1]/180),col='blue',pch=19,type='b')
legend('topright',legend=c('Training set','Test set'),col=c('blue','black'),pch=19)

  

#'''
#II. Basic Methods to select the best model as a function of complexity
#Cross Validation to select the best models
#'''

# 1) On construit la fonction predictFUN car il n'existe pas de fonction predict
#    dans le package leaps
predictFUN=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  mat[,names(coefi)]%*%coefi
}

  
# 2.a) Ici on construit manuellement une 10-folds cross validation pour selectionner
#    le meilleur modèle
#    Pour chacun des 10*k-1 subsets, on va produire 19 meilleurs modèles,
#    un pour chaque compléxité
#    On va compute la MSE des 19 modèles sur chacun des 10 k subsets
#    Et enfin, on moyenne cette MSE pour chacun des 19 modèles
set.seed(11)
folds=sample(rep(1:10,length=nrow(Hitters)))
RSS_training=matrix(NA,10,19)
for(k in 1:10){
  models19_training_cv=regsubsets(Salary~.,data=Hitters[folds!=k,],
                                  nvmax=19,method='forward')
  for(i in 1:19){
    pred=predictFUN(models19_training_cv,Hitters[folds==k,],id=i)
    RSS_training[k,i]=mean( (Hitters$Salary[folds==k]-pred)^2)
  }
}
# 2.b) On plot les moyennes des 10 RMSE des 19 modèles
#      Les modèles 6 à x sont à peu près équivalents. Donc on choisit le modèle
#      avec la complexité la plus faible donc le modèle de complexité 6
RMSE_cv=sqrt(apply(RSS_training,2,mean))
plot(RMSE_cv,pch=19,type='b')
  
###LASSO
# 0) Installation et chargement des librairies nécessaires
#install.packages('glmnet')
library(glmnet)

# 1) Les fonctions dont on va se servir prennent en paramètres des matrices et vecteur
#    On les construit. Le -1 pour supprimer la 1er colonne qui correspond à une constante de 1
#    Variables explicatives : x
#    Variable de réponse : y
x=model.matrix(Salary~.-1,data=Hitters) 
y=Hitters$Salary
  
# 2.a) En une ligne, on va effectuer une 10 folds cross validation
#    Pour chacun des 10*k-1 folds on construit 71 modèles pour 71 valeurs du paramètre lambda
#    Sur les 10 keme fold, on applique nos 71 modèles, on collecte la MSE et on la moyenne
#    Enfin, on plot la moyenne des MSE
#    NB : en réalité cv.glmnet crée 99 modèles pour 99 valeurs de lambda différents
#    seulement, il exclue de lui même les modèles avec un lambda trop proche de zéro
#    qui correspond à une simple régression des moindres carrés
#    NB : alpha=1 pour un modèle LASSO ; alpha=0 pour un modèle RIDGE
lasso_model_cv=cv.glmnet(x,y,alpha=1)
plot(lasso_model_cv)
?cv.glmnet

# 2.b) On récupère le meilleur modèle, celui associé à la MSE la plus faible
#      On voit que certaines variables sont à zéro
#      Le modèle LASSO fait de la selection de variables
numero_du_best_model=which(lasso_model_cv$lambda==lasso_model_cv$lambda.min)
lasso_model_cv$glmnet.fit$beta[,numero_du_best_model]

 
  
  