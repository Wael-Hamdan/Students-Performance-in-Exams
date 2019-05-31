studentPerf <- read.csv("StudentsPerformance.csv")

#Analyse descriptive
summary(studentPerf)
#Ici "gender" est une variable binaire, "race.ethnicity", "parental.level.of.education", "lunch" et 
# "test.preparation.course" sont des variables qualitative nominales
# Les variables "math.score", "reading.score", "writing.score" sont quantitatives discrètes.

domaineScore = unique(c(studentPerf$math.score,studentPerf$reading.score,studentPerf$writing.score))
print(min(domaineScore))
print(max(domaineScore))
#Les scores aux trois épreuves "math.score", reading.score" et "writing.score" varient dans [[0;100]].
levels(studentPerf$gender)
# "gender" varie entre "female" et "male", c'est donc une variable binaire
levels(studentPerf$lunch)
# "lunch" varie entre "free/reduced" et "standard"
levels(studentPerf$race.ethnicity)
# "race.ethnicity" varie entre "group A", "group B", "group C", "group D" et "group E"
levels(studentPerf$parental.level.of.education)
# "parental.level.of.education" varie entre "associate's degree", "bachelor's degree", "high school" 
#"master's degree", "some college" et "some high school"  
#Nous remarquons qu'il n'y a pas de valeurs manquantes dans ce jeu de données.

#Nous étudions maintenant les liens statistiques entre les différentes variables.

#Analyses multivariées 

#Tests préalables : Test de normalité et d'homoscédasticité
#1)Test de normalité
#On vérifie que les données sont gaussiennes
hist(studentPerf$math.score,prob=T)
curve(dnorm(x,mean(studentPerf$math.score),sd(studentPerf$math.score)), add=T,col="red")

hist(studentPerf$reading.score,prob=T)
curve(dnorm(x,mean(studentPerf$reading.score),sd(studentPerf$reading.score)), add=T,col="red")

hist(studentPerf$writing.score,prob=T)
curve(dnorm(x,mean(studentPerf$writing.score),sd(studentPerf$writing.score)), add=T,col="red")

#Les données semblent être gaussiennes, nous effectuons alors un test de normalité afin de le vérifier.
#La taille de l'échantillon (1000 individus) est petite (<2000). Le test de Shapiro-Wilk est donc adaptée.
shapiro.test(studentPerf$math.score)
#Le test de Shapiro-Wilk donne une probabilité de dépassement de 0.0001455, supérieure au seuil de signification de 5%. L'hypothèse de normalité de math.score est donc tolérée.
shapiro.test(studentPerf$reading.score)
#Le test de Shapiro-Wilk donne une probabilité de dépassement de 0.0001055, supérieure au seuil de signification de 5%. L'hypothèse de normalité de reading.score est donc tolérée.
shapiro.test(studentPerf$writing.score)
#Le test de Shapiro-Wilk donne une probabilité de dépassement de 2.922e-05, supérieure au seuil de signification de 5%. L'hypothèse de normalité de writing.score est donc tolérée.
######NOTE : LES DATA NE SONT PAS NORMALES D4APRES SHAPIRO WILKS, ONE NE PEUT DONC PAS FAIRE LE TEST DE BARTLETT NI ANOVA

a <- rnorm(1000, 0, 1)
hist(a)
shapiro.test(a)


#2)Test d'homoscédasticité
bartlett.test(math.score~gender,data=studentPerf)
#p-value = 0.08995 > 5% => On rejette H0, on ne rejette donc pas l’hypothèse d’homoscédasticité.
bartlett.test(reading.score~gender,data=studentPerf)
#p-value = 0.4818 > 5% => On ne rejette pas H0, on ne rejette donc pas l’hypothèse d’homoscédasticité.
bartlett.test(writing.score~gender,data=studentPerf)
#p-value = 0.2603 > 5% => On ne rejette pas H0, on ne rejette donc pas l’hypothèse d’homoscédasticité.

#Nous considérons alors que les données sont gaussiennes et que les variances sont homogènes.
#On peut donc réaliser des ANOVA

#Analyse multivariée des scores en fonction du genre
cols = names(studentPerf)
def.par <-par(no.readonly=T)
par(mfrow=c(1,3))
for(i in 6:8) {
  plot(studentPerf[,i]~studentPerf$gender, xlab = "gender", ylab = cols[i])
}
#Il semble y avoir une différence notable entre les hommes et les femmes pour les scores reading.score et writing.score.
#Il semble aussi qu'il y a une légère différence pour le score math.score en fonction du genre. 
#Pour confirmer ou infirmer ces résultats, nous effectuons des ANOVA:
#ici et dans la suite du projet, nous fixerons le seuil alpha* à 5%
for(i in 6:8) {
  model = lm(studentPerf[,i]~1+gender, data = studentPerf)
  print(anova(model))
}
#ANOVA sur math.score en fonction de gender : p-value = 9.12e-08 < 5% => on rejette H0. 
#ANOVA sur reading.score en fonction de gender : p-value = 4.681e-15 < 5% => on rejette H0. 
#ANOVA sur writing.score en fonction de gender : p-value = 2.2e-16 < 5% => on rejette H0. 
#Il y a donc une différence significative entre les hommes et les femmes pour chacun des scores: math.score, reading.score et writing.score.

#Analyse multivariée des scores en fonction du groupe ethnique
cols = names(studentPerf)
def.par <-par(no.readonly=T)
par(mfrow=c(1,3))
for(i in 6:8) {
  plot(studentPerf[,i]~studentPerf$race.ethnicity, xlab = "race.ethnicity", ylab = cols[i])
}
#Pour chacun des 3 scores, il semble y avoir des différences en fonction de l'ethnie de l'étudiant.
#Les scores semblent être d'autant plus élévés que l'étudiant appartient au groupe E, puis D etc. jusqu'au groupe A dont 
#les scores semblent êtres les moins élevés.
#Pour confirmer ou infirmer ces résultats, nous effectuons des ANOVA:
for(i in 6:8) {
  model = lm(studentPerf[,i]~1+race.ethnicity, data = studentPerf)
  print(anova(model))
}
#ANOVA sur math.score en fonction de race.ethnicity : p-value = 1.373e-11 < 5% => on rejette H0. 
#ANOVA sur reading.score en fonction de race.ethnicity : p-value = 0.000178 < 5% => on rejette H0. 
#ANOVA sur writing.score en fonction de race.ethnicity : p-value = 1.098e-05 < 5% => on rejette H0. 
#Il y a donc une différence significative entre les groupes d'ethnie différente pour chacun des scores: math.score, reading.score et writing.score.


#Analyse multivariée des scores en fonction du niveau d'éducation des parents
cols = names(studentPerf)
def.par <-par(no.readonly=T)
par(mfrow=c(1,3))
for(i in 6:8) {
  plot(studentPerf[,i]~studentPerf$parental.level.of.education, xlab = "parental.level.of.education", ylab = cols[i])
}
#Pour chacun des 3 scores, les scores des étudiants dont le niveau d'éducation des parents 
#correspond à high school ou some high school (ce qui correspond au niveau bac) semblent se détacher avec des scores plus faibles en général. 
#Pour confirmer ou infirmer ces résultats, nous effectuons des ANOVA :
for(i in 6:8) {
  model = lm(studentPerf[,i]~1+parental.level.of.education, data = studentPerf)
  print(anova(model))
}
#ANOVA sur math.score en fonction de parental.level.of.education : p-value = 5.592e-06 < 5% => on rejette H0. 
#ANOVA sur reading.score en fonction de parental.level.of.education : p-value = 1.168e-08 < 5% => on rejette H0. 
#ANOVA sur writing.score en fonction de parental.level.of.education : p-value = 1.12e-13 < 5% => on rejette H0. 
#Il y a donc une différence significative entre les groupes de niveau d'éducation des parents est différent pour chacun des scores: math.score, reading.score et writing.score.


#Analyse multivariée des scores en fonction du type de déjeuner (gratuit/tarif réduit ou standard)
cols = names(studentPerf)
def.par <-par(no.readonly=T)
par(mfrow=c(1,3))
for(i in 6:8) {
  plot(studentPerf[,i]~studentPerf$lunch, xlab = "lunch", ylab = cols[i])
}
#Pour chacun des 3 scores, il semble y avoir une différence notable en fonction du déjeuner.
#Ceux ayant un déjeuner standard semblent avoir de meilleurs résultat.
#Interessant de voir corrélation entre type de déjeuner et niveau éducation parents car le problème vient peut etre du niveau d'éducation des parents.
#Pour confirmer ou infirmer ces résultats, nous effectuons des ANOVA:
for(i in 6:8) {
  model = lm(studentPerf[,i]~1+lunch, data = studentPerf)
  print(anova(model))
}
#ANOVA sur math.score en fonction de lunch : p-value = 2.2e-16 < 5% => on rejette H0. 
#ANOVA sur reading.score en fonction de lunch : p-value = 2.003e-13 < 5% => on rejette H0. 
#ANOVA sur writing.score en fonction de lunch : p-value = 3.186e-15 < 5% => on rejette H0. 
#Il y a donc une différence significative entre les groupes qui ont un déjeuner gratuit/réduit ou bien standard pour chacun des scores: math.score, reading.score et writing.score.


#Analyse multivariée des scores en fonction de si l'étudiant a complété ou non un test suite à un cours de préparation
cols = names(studentPerf)
def.par <-par(no.readonly=T)
par(mfrow=c(1,3))
for(i in 6:8) {
  plot(studentPerf[,i]~studentPerf$test.preparation.course, xlab = "test.preparation.course", ylab = cols[i])
}
#Pour chacun des 3 scores, il semble y avoir une différence notable entre les étudiants ayant passés un test préparatoire ou non.
#Ceux ayant passés un test semblent avoir de meilleurs scores.
#Pour confirmer ou infirmer ces résultats, nous effectuons des ANOVA:
for(i in 6:8) {
  model = lm(studentPerf[,i]~1+test.preparation.course, data = studentPerf)
  print(anova(model))
}
#ANOVA sur math.score en fonction de test.preparation.course : p-value = 1.536e-08 < 5% => on rejette H0. 
#ANOVA sur reading.score en fonction de test.preparation.course : p-value = 9.082e-15 < 5% => on rejette H0. 
#ANOVA sur writing.score en fonction de test.preparation.course : p-value = 2.2e-16 < 5% => on rejette H0. 
#Il y a donc une différence significative entre les groupes qui ont passé un test de préparation ou non pour chacun des scores: math.score, reading.score et writing.score.

corMat = cor(studentPerf[,c("math.score","reading.score", "writing.score")])
corMat

pairs(studentPerf[,c("math.score","reading.score", "writing.score")],main = "Student Performance - Graphes de dispersion")
pairs(studentPerf[,c("math.score","reading.score", "writing.score")],main = "Student Performance / Gender - Graphes de dispersion" ,col =c("blue","orange")[studentPerf$gender])
pairs(studentPerf[,c("math.score","reading.score", "writing.score")],main = "Student Performance / Lunch - Graphes de dispersion" ,col =c("blue","orange")[studentPerf$lunch])

#Les variables relatives aux scores semblent être corrélées en particulier "reading.score" et "writing.score"
#Ceci indique qu'un étudiant a en général des scores assez homogènes aux trois épreuves et surtout en reading et writing.

#On effectue alors une ACP afin d'obtenir des variables non corrélées linéairement.
#On effectue une ACP sur les variables quantitatives
studentPerf.acp = princomp(studentPerf[,c("math.score","reading.score", "writing.score")])
biplot(studentPerf.acp)
#L'orientation des flèches confirment que reading.score et writing.score sont très corrélées.
summary(studentPerf.acp)
#L'ACP donne 3 composantes principales, la variance expliquée cumulée des deux premières est d'environ 98,5%
#On choisit de perdre l'information du troisième axe factoriel
#Les deux nouvelles variables que l'on appelera "score1" et "score2" résument l'information des 3 variables utilisées pour l'ACP 

#On remplace les 3 variables initiales par les deux nouvelles variable obtenues par l'ACP de variance expliquée maximale
score1 = studentPerf.acp$scores[,1]
score2 = studentPerf.acp$scores[,2]
studentPerfAcp <- cbind(studentPerf[,1:5],score1,score2)

#classification automatique

#CAH (nous servira surtout à determiner le nombre de classes K)

#centrage réduction des données
#pour réduire influence sur les résulats causée par des variables à forte variance
d.studentPerfAcp = dist(studentPerfAcp[,c(6:7)])
cah.ward = hclust(d.studentPerfAcp,method="ward.D2")
plot(cah.ward, labels = F)
#Etant donné ce dendogramme, il parait judicieux de prendre un niveau de regroupement tel qu'il y ait 3 classes distinctes. 
rect.hclust(cah.ward,k=3)


#k-means avec k=3, nstart=25 pour prendre 25 configurations initiales et garder la meilleure
kmeans(studentPerfAcp[,c(6:7)], centers = 3, nstart = 25)
kmeans(studentPerf[,c(6:8)], centers = 3, nstart=25)


