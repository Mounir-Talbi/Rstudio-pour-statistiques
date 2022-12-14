## EXAM 2022 -R STUDIO - TALBI MOUNIR

library (fdm2id)

#  On Ouvre le fichier contenant les donn?es d'apprentissage qui nous int?resse.

letters = read.table (file.choose (), fileEncoding = "UTF-8", header = TRUE, sep = ",")

#3. On s?lectionne les lignes qui correspondent ? la lettre ? U ? (respectivement, ? V ?, ? X ? et ? Y ?) et
#  on place les donn?es dans une table du (respectivement dv, dx et dy).

du=letters[which(letters[,1]=="U"),]
dv=letters[which(letters[,1]=="V"),]
dx=letters[which(letters[,1]=="X"),]
dy=letters[which(letters[,1]=="Y"),]


#4.On fusionne les quatres tables obtenues

d=rbind(du,dv,dx,dy)

dim(d)

# On Modifie la premi?re colonne de d pour en faire un factor avec la 
#commande d [, 1] = factor (d[, 1]).

d [, 1] = factor (d[, 1])

#On Projete les donn?es sur les deux premiers axes de l'ACP


d.pca = PCA (d [, -1], scale.unit = FALSE)
plot (d.pca)


# On Projete les donn?es sur les deux premiers axes de l'AFD en colorant selon la classe d'appartenance

model1= CDA (d[,-1], d[,1])
plot(model1)

# On Affiche l'arbre de d?cision obtenu par la m?thode CART ?

model2=CART (d[,-1], d[,1])

cartplot(model2)

# On ?value les performances des m?thodes de classification Naive Bayes Classifier, Analyse Factorielle
#Discriminante, R?gression Logistique, k-NN et CART. Le protocole d'?valuation sera de type holdout
#r?p?t? 10 fois. Le crit?re d'?valuation sera le taux de succ?s.

performance (c (NB, CDA, LR , KNN,CART ), d [, -1], d [, 1],protocol="holdout", nruns=10, seed = 0)

#la m?thode de classification qui semble produire le meilleur r?sultat pr?dictif est  la m?thode KNN (car =0.9925 > ...>  )


# Affichons les matrices de confusion

performance (c (NB, CDA, LR , KNN,CART ), d [, -1], d [, 1],type="confusion", protocol="holdout", nruns=10, seed = 0)

#Gr?ce ? la matrice de confusion les classes les plus difficile ? s?parer sont les classes:
# U et V (car =0.08) 
#U et Y (cat =0.07)
