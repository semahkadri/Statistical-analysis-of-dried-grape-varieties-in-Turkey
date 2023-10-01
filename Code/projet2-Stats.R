install.packages("Factoshiny") #ACP dashboard
install.packages("FactoMineR")
install.packages("shapiro")
install.packages("devtools")
install.packages(c("FactoMineR", "factoextra")) # for ACP

library(ggplot2)
library(tidyr)
library(stringi)
library(Hmisc)
library(shapiro)
library(nortest)      
library("corrplot")
library(stats)
library("FactoMineR")
library("factoextra")
library("Factoshiny")
library("devtools")



projet1 = read.table(file = "C:/Users/User/Desktop/projet_stat/Raisin.csv", header = TRUE ,sep = ",")


class(projet1)
dim(projet1)
head(projet1)
str(projet1)
summary(projet1)
colnames(projet1)
dim(projet1)

summary(is.na(projet1))


as.data.frame(colSums(is.na(projet1)))


#Les valeurs abérrantes 1ére méthode ;

boxplot(projet1[,c('Area','MajorAxisLength','MinorAxisLength','Eccentricity','ConvexArea','Extent','Perimeter')])

for (x in c('Area','MajorAxisLength','MinorAxisLength','Eccentricity','ConvexArea','Extent','Perimeter'))
{
  value = projet1[,x][projet1[,x] %in% boxplot.stats(projet1[,x])$out]
  projet1[,x][projet1[,x] %in% value] = NA
} 

as.data.frame(colSums(is.na(projet1)))



#Les valeurs abérrantes 2éme méthode ;

#Area
boxplot(projet1$Area)
summary(projet1$Area)
Q1 = 58239   
Q3 = 103057  
Vmax=Q3+1.5*(Q3-Q1)
Vmin=Q1-1.5*(Q3-Q1)

which(projet1$Area<Vmin)
which(projet1$Area>Vmax)

projet1$Area[86]=NA
projet1$Area[468]=NA
projet1$Area[470]=NA
projet1$Area[476]=NA
projet1$Area[480]=NA
projet1$Area[488]=NA
projet1$Area[491]=NA
projet1$Area[507]=NA
projet1$Area[508]=NA
projet1$Area[516]=NA
projet1$Area[517]=NA
projet1$Area[523]=NA
projet1$Area[542]=NA
projet1$Area[553]=NA
projet1$Area[561]=NA
projet1$Area[575]=NA
projet1$Area[581]=NA
projet1$Area[582]=NA
projet1$Area[553]=NA
projet1$Area[561]=NA
projet1$Area[575]=NA
projet1$Area[581]=NA
projet1$Area[582]=NA
projet1$Area[591]=NA
projet1$Area[617]=NA
projet1$Area[623]=NA
projet1$Area[651]=NA
projet1$Area[657]=NA
projet1$Area[658]=NA
projet1$Area[667]=NA
projet1$Area[669]=NA
projet1$Area[672]=NA
projet1$Area[679]=NA
projet1$Area[772]=NA
projet1$Area[726]=NA
projet1$Area[739]=NA
projet1$Area[740]=NA
projet1$Area[742]=NA
projet1$Area[748]=NA
projet1$Area[768]=NA
projet1$Area[792]=NA
projet1$Area[796]=NA
projet1$Area[806]=NA
projet1$Area[809]=NA
projet1$Area[819]=NA

summary(projet1$Area)

#MajorAxisLength 
boxplot(projet1$MajorAxisLength)
summary(projet1$MajorAxisLength)

Q1 = 340.8   
Q3 = 487.7  
Vmax=Q3+1.5*(Q3-Q1)
Vmin=Q1-1.5*(Q3-Q1)

which(projet1$MajorAxisLength<Vmin)
which(projet1$MajorAxisLength>Vmax)

a = which(projet1$MajorAxisLength>Vmax)

projet1$MajorAxisLength[a]=NA
summary(projet1$MajorAxisLength)
#MinorAxisLength 

boxplot(projet1$MinorAxisLength)
summary(projet1$MinorAxisLength)

  Q1 = 215.4   
  Q3 = 271.7  
Vmax=Q3+1.5*(Q3-Q1)
Vmin=Q1-1.5*(Q3-Q1)

which(projet1$MinorAxisLength<Vmin)
which(projet1$MinorAxisLength>Vmax)

a = which(projet1$MinorAxisLength>Vmax)

projet1$MinorAxisLength[a]=NA
summary(projet1$MinorAxisLength)


#Eccentricity 
boxplot(projet1$Eccentricity)
summary(projet1$Eccentricity)

  Q1 = 0.7401   
  Q3 = 0.8410  
Vmax=Q3+1.5*(Q3-Q1)
Vmin=Q1-1.5*(Q3-Q1)

which(projet1$Eccentricity<Vmin)
which(projet1$Eccentricity>Vmax)

c = which(projet1$Eccentricity<Vmin)
c

projet1$Eccentricity[c]=NA
summary(projet1$Eccentricity)


#ConvexArea    
boxplot(projet1$ConvexArea)
summary(projet1$ConvexArea)

  Q1 = 59977           
  Q3 = 107313    
Vmax=Q3+1.5*(Q3-Q1)
Vmin=Q1-1.5*(Q3-Q1)

which(projet1$ConvexArea<Vmin)
which(projet1$ConvexArea>Vmax)

d = which(projet1$ConvexArea>Vmax)
d

projet1$ConvexArea[d]=NA
summary(projet1$ConvexArea)


#Extent 
boxplot(projet1$Extent)
summary(projet1$Extent)

  Q1 = 0.6709           
  Q3 = 0.7344    
Vmax=Q3+1.5*(Q3-Q1)
Vmin=Q1-1.5*(Q3-Q1)

which(projet1$Extent<Vmin)
which(projet1$Extent>Vmax)

e = which(projet1$Extent<Vmin)
e
f = which(projet1$Extent>Vmax)
f

projet1$Extent[e]=NA
projet1$Extent[f]=NA


summary(projet1$Extent)

#Perimeter   
boxplot(projet1$Perimeter)
summary(projet1$Perimeter)

  Q1 = 949.9     
  Q3 = 1295.4    
Vmax=Q3+1.5*(Q3-Q1)
Vmin=Q1-1.5*(Q3-Q1)

which(projet1$Perimeter<Vmin)
which(projet1$Perimeter>Vmax)

g = which(projet1$Perimeter>Vmax)
g

projet1$Perimeter[g]=NA
summary(projet1$Perimeter)



#Plot avec Classe 

# on s'interesse à la variable area class
boxplot(projet1$Area~projet1$Class)
boxplot(projet1$Area~projet1$Class)$out 
indab = which(projet1$Area %in% boxplot(projet1$Area~projet1$Class)$out)
indab
for (a in indab )
  projet1$Area[a]=NA

# on s'interesse à la variable MajorAxisLength class
boxplot(projet1$MajorAxisLength~projet1$Class)
boxplot(projet1$MajorAxisLength~projet1$Class)$out 
indab = which(projet1$MajorAxisLength %in% boxplot(projet1$MajorAxisLength~projet1$Class)$out)
indab
for (a in indab )
  projet1$MajorAxisLength[a]=NA

# on s'interesse à la variable MinorAxisLength class
boxplot(projet1$MinorAxisLength~projet1$Class)
boxplot(projet1$MinorAxisLength~projet1$projet1)$out 
indab = which(projet1$MinorAxisLength %in% boxplot(projet1$MinorAxisLength~projet1$Class)$out)
indab
for (a in indab )
  projet1$MinorAxisLength[a]=NA

# on s'interesse à la variable Eccentricity class
boxplot(projet1$Eccentricity~projet1$Class)
boxplot(projet1$Eccentricity~projet1$Class)$out 
indab = which(projet1$Eccentricity %in% boxplot(projet1$Eccentricity~projet1$Class)$out)
indab
for (a in indab )
  projet1$Eccentricity[a]=NA

# on s'interesse à la variable ConvexArea class
boxplot(projet1$ConvexArea~projet1$Class)
boxplot(projet1$ConvexArea~projet1$Class)$out 
indab = which(projet1$ConvexArea %in% boxplot(projet1$ConvexArea~projet1$Class)$out)
indab
for (a in indab )
  projet1$ConvexArea[a]=NA

# on s'interesse à la variable Extent class
boxplot(projet1$Extent~projet1$Class)
boxplot(projet1$Extent~projet1$Class)$out 
indab = which(projet1$Extent %in% boxplot(projet1$Extent~projet1$Class)$out)
indab
for (a in indab )
  projet1$Extent[a]=NA

# on s'interesse à la variable Perimeter class
boxplot(projet1$Perimeter~projet1$Class)
boxplot(projet1$Perimeter~projet1$Class)$out 
indab = which(projet1$Perimeter %in% boxplot(projet1$Perimeter~projet1$Class)$out)
indab
for (a in indab )
  projet1$Perimeter[a]=NA


summary(projet1)

#Les valeurs manquantes 


summary(is.na(projet1))

na.fail(projet1)
sum(is.na(projet1)) / prod(dim(projet1))     # ==> 0.03 => 3% ; Delete


projet1[projet1==""]<-NA
projet1<-projet1[complete.cases(projet1),]
projet1["Class"][projet1["Class"] != "Kecimen" &projet1["Class"] != "Besni" ] =NA

as.data.frame(colSums(is.na(projet1)))


#Analyse univaríee

summary(projet1)


Area = c(projet1[,"Area"])
MajorAxis = c(projet1[,"MajorAxisLength"])
MinorAxis = c(projet1[,"MinorAxisLength"])
Convex = c(projet1[,"ConvexArea"])
Eccent = c(projet1[,"Eccentricity"])
Extent = c(projet1[,"Extent"])
Perim = c(projet1[,"Perimeter"])


hist(Area,col="darkmagenta")
shapiro.test(Area)
hist(MajorAxis,col="darkmagenta")
shapiro.test(MajorAxis)
hist(MinorAxis,col="darkmagenta")
shapiro.test(MinorAxis)
hist(Convex,col="darkmagenta")
shapiro.test(Convex)
hist(Eccent,col="darkmagenta")
shapiro.test(Eccent)
hist(Extent,col="darkmagenta")
shapiro.test(Extent)
hist(Perim,col="darkmagenta")
shapiro.test(Perim)


#modalité : 
#discrete - continue 

projet1[,"Area"]             #Continue ; Elles  peuvent prendre n'importe quelle des valeurs numériques entières. 
projet1[,"MajorAxisLength"]  #Continue ; Elles peuvent prendre n'importe quelle valeur numérique entière ou décimale.
projet1[,"MinorAxisLength"]  #Continue ; Elles peuvent prendre n'importe quelle valeur numérique entière ou décimale.
projet1[,"ConvexArea"]       #Discrètes ; Elles ne peuvent prendre que des valeurs numériques entières. 
projet1[,"Eccentricity"]     #Continue ; Elles peuvent prendre n'importe quelle valeur numérique entière ou décimale.
projet1[,"Extent"]           #Continue ; Elles peuvent prendre n'importe quelle valeur numérique entière ou décimale.
projet1[,"Perimeter"]        #Continue ; Elles peuvent prendre n'importe quelle valeur numérique entière ou décimale.

#nominal - oridinal 
projet1["Class"]            #nominale ; qualitative dont les modalités ne sont pas ordonnées

str(projet1)      # => 7 variables quantitatives et 1 variable qualitative

table(projet1$Class)     # ===> Besni 266 , Kecimen 357

tapply(projet1$MinorAxisLength, projet1$Class, mean)  #==> Mean de Axis dans les 2 classes 

wilcox.test(projet1$Area~projet1$Class)



#Analyse bivariée
"
H0 ∶ r = 0 ∶ absence de dépendance significative
H1 ∶ r ≠ 0 ∶ existence de dépendance significative

"

#Area
 # ==> trés fortement corrélé 0.95  ;   H1
cor.test(Area,MajorAxis,method = "spearman" )

 # ==> trés fortement corrélé 0.91    ;   H1
cor.test(Area,MinorAxis,method = "spearman" )

 # ==> faiblement corrélé positive 0.38
cor.test(Area,Eccent,method = "spearman" )

 # ==> trés fortement corrélé 0.97   ;   H1
cor.test(Area,Perim,method = "spearman" )


#MajorAxis
 # ==> trés fortement corrélé 0.75   ;   H1
cor.test(MajorAxis,MinorAxis,method = "spearman" )

 # ==> fortement corrélé 0.62   ;   H1
cor.test(MajorAxis,Eccent,method = "spearman" )

 # ==> faiblement corrélé négative -0.01   ;   H0
cor.test(MajorAxis,Extent,method = "spearman" )

 # ==> trés fortement corrélé 0.97   ;   H1
cor.test(MajorAxis,Perim,method = "spearman" )


#MinorAxis
 # ==> trés fortement corrélé 0.89   ;   H1
cor.test(MinorAxis,Convex,method = "spearman" )

# ==> NULL 0.0017   ;   H0
cor.test(MinorAxis,Eccent,method = "spearman" )

 # ==> faiblement corrélé 0.11   ;   H0
cor.test(MinorAxis,Extent,method = "spearman" )

 # ==> trés fortement corrélé 0.85   ;   H1
cor.test(MinorAxis,Perim,method = "spearman" )


#Eccent
 # ==> faiblement corrélé négative -0.32   ;   H0
cor.test(Eccent,Extent,method = "spearman" )

 # ==>  fortement corrélé 0.48   ;   H1
cor.test(Eccent,Perim,method = "spearman" )

#Extent
# ==> faiblement corrélé négative -0.098   ;   H0
cor.test(Extent,Perim,method = "spearman" )

#kruskal
library(plyr)

wilcox.test(projet1$Area~projet1$Class)
wilcox.test(projet1$MinorAxis~projet1$Class)
wilcox.test(projet1$MajorAxisLength~projet1$Class)
wilcox.test(projet1$ConvexArea~projet1$Class)
wilcox.test(projet1$Perim~projet1$Class)
wilcox.test(projet1$Extent~projet1$Class)
wilcox.test(projet1$Eccent~projet1$Class)




data = projet1
data$Class = NULL
head(data)

#Matrice de correlation
mcor <- cor(data)
mcor
#regression
RM = lm(MinorAxisLength ~ Area + MajorAxisLength + ConvexArea + Eccentricity + Extent + Perimeter,
         data=projet1)
summary(RM)              # 97%

#stratégie détaillée pour améliorer la performance du modèle de régréssion
#P_value >>>> supprimer variable de la regression
RM1 = lm(MinorAxisLength ~ Area + MajorAxisLength + ConvexArea + Eccentricity + Perimeter,
         data=projet1)
summary(RM1)              # 97%

RM2 = lm(MinorAxisLength ~ Area  + MajorAxisLength + Eccentricity + Perimeter, 
         data=projet1)
summary(RM2)              # 97%          lorsque on élimine Eccentricity : 96 !! 

RM3 = lm(MinorAxisLength ~ Area  + MajorAxisLength + Perimeter,
         data=projet1)
summary(RM3)               # 96% STOP

#PLOT
R = residuals(RM2)
plot(R)
qqnorm(R) ;qqline(R)

#réduction dimen 

#detlete Class 
data = projet1
data$Class = NULL
data

#PCA
PCA(data, scale.unit = TRUE, ncp = 5, graph = TRUE)

res.pca <- PCA(data, graph = FALSE)
print(res.pca)
#valeur propre
eig.val <- get_eigenvalue(res.pca)
eig.val
s
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))


#extraire les résultats
var <- get_pca_var(res.pca)
var

head(var$coord)
fviz_pca_var(res.pca, col.var = "black")

head(var$cos2)
corrplot(var$cos2, is.corr=FALSE)
#qualité de repr
fviz_cos2(res.pca, choice = "var", axes = 1:2)


# Colorer en fonction du cos2: qualité de représentation
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
)

head(var$contrib)


res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)

#choix des variables ? 
res.desc$Dim.1
res.desc$Dim.2

res = Factoshiny(projet1)

#regression avec pca
library(caret)
model <- train(
  MinorAxisLength~Area+MajorAxisLength+Eccentricity+ConvexArea+Extent+Perimeter, 
  data = data,
  method = 'lm',
  preProcess = c("pca")
)
model


#regression généralisé 
##### GLM


library(stats)

#modele gaussien
model1 <- glm(MinorAxisLength ~ Area + MajorAxisLength + ConvexArea + Eccentricity + Extent + 
               Perimeter + Class, data = projet1, family = "gaussian")
summary(model1)
#Calculer R-squared pour le modele 
with(summary(model1), 1 - deviance/null.deviance) #meilleur modele avec 98%

#modele poisson
model2 <- glm(MinorAxisLength ~ Area + MajorAxisLength + ConvexArea + Eccentricity + Extent + 
                Perimeter + Class , data = projet1, family = "poisson")
summary(model2)
with(summary(model2), 1 - deviance/null.deviance)

#modele binomial
model3 <- glm(as.factor(MinorAxisLength) ~ Area + MajorAxisLength + ConvexArea + Eccentricity +
                Extent + Perimeter + Class , data = projet1, family = "binomial")
summary(model3) #impossible

#modele gamma
model4 <- glm(MinorAxisLength ~ Area + MajorAxisLength + ConvexArea + Eccentricity +
                Extent + Perimeter + Class , data = projet1, family = Gamma)
summary(model4)
with(summary(model4), 1 - deviance/null.deviance)

#modele inverse gaussian
model5 <- glm(MinorAxisLength ~ Area + MajorAxisLength + ConvexArea + Eccentricity +
                Extent + Perimeter + Class , data = projet1, family = inverse.gaussian())
summary(model5)
with(summary(model5), 1 - deviance/null.deviance)


#modele inverse gaussian
model6 <- glm(MinorAxisLength ~ Area + MajorAxisLength + ConvexArea + Eccentricity +
                Extent + Perimeter + Class , data = projet1, family = quasi())
summary(model6)
with(summary(model6), 1 - deviance/null.deviance)

