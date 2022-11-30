## EXAM 2022 -R STUDIO - TALBI MOUNIR

library (fdm2id)

#  On Ouvre le fichier contenant les données d'apprentissage qui nous intéresse.

letters = read.table (file.choose (), fileEncoding = "UTF-8", header = TRUE, sep = ",")

#3. On sélectionne les lignes qui correspondent à la lettre « U » (respectivement, « V », « X » et « Y ») et
#  on place les données dans une table du (respectivement dv, dx et dy).

du=letters[which(letters[,1]=="U"),]
dv=letters[which(letters[,1]=="V"),]
dx=letters[which(letters[,1]=="X"),]
dy=letters[which(letters[,1]=="Y"),]


#4.On fusionne les quatres tables obtenues

d=rbind(du,dv,dx,dy)

dim(d)

# On Modifie la première colonne de d pour en faire un factor avec la 
#commande d [, 1] = factor (d[, 1]).

d [, 1] = factor (d[, 1])

#On Projete les données sur les deux premiers axes de l'ACP


d.pca = PCA (d [, -1], scale.unit = FALSE)
plot (d.pca)


# On Projete les données sur les deux premiers axes de l'AFD en colorant selon la classe d'appartenance

model1= CDA (d[,-1], d[,1])
plot(model1)

# On Affiche l'arbre de décision obtenu par la méthode CART ?

model2=CART (d[,-1], d[,1])

cartplot(model2)

# On Évalue les performances des méthodes de classification Naive Bayes Classifier, Analyse Factorielle
#Discriminante, Régression Logistique, k-NN et CART. Le protocole d'évaluation sera de type holdout
#répété 10 fois. Le critère d'évaluation sera le taux de succès.

performance (c (NB, CDA, LR , KNN,CART ), d [, -1], d [, 1],protocol="holdout", nruns=10, seed = 0)

#la méthode de classification qui semble produire le meilleur résultat prédictif est  la méthode KNN (car =0.9925 > ...>  )


# Affichons les matrices de confusion

performance (c (NB, CDA, LR , KNN,CART ), d [, -1], d [, 1],type="confusion", protocol="holdout", nruns=10, seed = 0)

#Gràce à la matrice de confusion les classes les plus difficile à séparer sont les classes:
# U et V (car =0.08) 
#U et Y (cat =0.07)
