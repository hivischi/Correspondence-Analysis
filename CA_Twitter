# LEXICAL CORRESPONDENCE ANALYSIS
library(FactoMineR)
library(Factoshiny)
library(factoextra)
library(plotly)
library(GPArotation)
library(MASS)
library(ca)

doc_sent_df <- sentiment
doc_sent_matrix <- as.matrix(doc_sent_df)
sent_doc_matrix <- t(doc_sent_matrix)
sent_doc_df <- as.data.frame(sent_doc_matrix)

df_ordinato15 <- df_ordinato[1:15, ]
rownames(df_ordinato15)
row.names(termdoc_matrix)

#WORDS (15) x SENTIMENTS (10)
termdoc_matrix_new <- termdoc_matrix[c(126, 66, 95, 19, 24, 119, 28, 192, 106, 86, 78, 43, 166, 17, 157), ]

term_sent <- termdoc_matrix_new %*%  doc_sent_matrix
sent_term <- t(term_sent)

sent_term_df <- as.data.frame(sent_term)

doc_term <- t(termdoc_matrix_new)
doc_term_df <- as.data.frame(doc_term)

term_sent
sommacol <- colSums(term_sent)
tabcontingenza <- rbind(term_sent, sommacol) 
sommariga <- rowSums(tabcontingenza)
tabcontingenza <- cbind(tabcontingenza, sommariga)
tab_freq_rel <- tabcontingenza/70449
tab_freq_rel <- round(tab_freq_rel, 3)
tab_freqre_100 <- tab_freq_rel*100 


#CORRESPONDENCE ANALYSIS ON term_sent WITH factomineR
res.ca_termsent <- CA(term_sent)
summary(res.ca_termsent)
round(res.ca_termsent$row$cos2[, c(1, 2)], digits = 3)
round(res.ca_termsent$col$cos2[, c(1, 2)], digits = 3)


plot(res.ca_termsent)
HCPCshiny(res.ca_termsent)

fviz_ca_biplot(res.ca_termsent,repel=TRUE) #grafico parole e sentimenti
fviz_ca_row(res.ca_termsent,repel=TRUE)#grafico delle parole
fviz_ca_col(res.ca_termsent,repel=TRUE)#grafico dei sentimenti

par(mfrow=c(1, 2))

fviz_contrib(res.ca_termsent,choice="col",axes=1)#contributo delle colonne alle due dimenisioni
fviz_contrib(res.ca_termsent,choice="col",axes=2)
fviz_contrib(res.ca_termsent,choice="col",axes=c(1:2))
fviz_contrib(res.ca_termsent,choice="row",axes=1)
fviz_contrib(res.ca_termsent,choice="row",axes=2)
fviz_contrib(res.ca_termsent,choice="row",axes=c(1:2)) #contributo delle righe alle due dimenisioni

fviz_ca_row(res.ca_termsent,axes=c(1,2),c("point","text"),shape.row=19,repel=TRUE) #alcuni aggiustamenti grafici

#CORRESPONDENCE ANALYSIS WITH "ca"
ca_termsent <- ca(term_sent)
summary(ca_termsent)
plot.ca(ca_termsent, mass=c(TRUE, TRUE))
plot.ca(ca_termsent, arrows = c(TRUE, TRUE))

coordriga <- res.ca_termsent$row$coord[, 1:2]
coordriga <- as.matrix(coordriga)

plot(density(coordriga[, 1]))
plot(density(coordriga[, 2]))

den3d <- kde2d(coordriga[, 1], coordriga[,2], n = c(100,100))
plot_ly(x=den3d$x, y=den3d$y, z=den3d$z) %>% add_surface()

dist_coordriga <- dist(coordriga, method = "manhattan")
hclust_coordriga <- hclust(dist_coordriga,method = "ward.D2")
plot(hclust_coordriga, xlab = "", hang = -1)

#MODEL BASED CLUSTERING ON 2 DIM
library(mclust)
provacluster <- Mclust(coordriga)
summary(provacluster)
plot(provacluster)
plot(provacluster, "classification")
text(coordriga, labels = row.names(coordriga), col = provacluster$classification)

kmeans_termini <- kmeans(coordriga, centers = 3, nstart=10, algorithm = "MacQueen")
summary(kmeans_termini)
plot(coordriga, col=kmeans_termini$cluster, pch=19)
text(coordriga, labels = rownames(coordriga), col = kmeans_termini$cluster)

library(ppclust)
fuzzy_termini <- fcm(coordriga, centers =2)
plot(coordriga, col=fuzzy_termini$cluster)
text(coordriga, labels = rownames(coordtermini_2), col = fuzzy_termini$cluster)


###CORRESPONDENCE ANALYSIS ON DOC_TERM
res.ca_docterm <- CA(doc_term)
summary(res.ca_docterm)
plot(res.ca_docterm)
HCPCshiny(res.ca_docterm)
fviz_ca_col(res.ca_docterm,repel=TRUE)


for (i in 1:nrow(doc_term_df)) {
  for(j in 1:ncol(doc_term_df)) {
    if(doc_term_df[i,j]==0){
      doc_term_df[i,j]="No"
    }else{doc_term_df[i,j]="Yes"
    }
  }
}

for (i in 1:nrow(doc_sent_df)) {
  for(j in 1:ncol(doc_sent_df)) {
    if(doc_sent_df[i,j]==0){
      doc_sent_df[i,j]="No"
    }else{doc_sent_df[i,j]="Yes"
    }
  }
}

#PLSCA ON SENT_TERM AND SENT_DOC

for (i in 1:nrow(sent_doc_df)) {
  for(j in 1:ncol(sent_doc_df)) {
    if(sent_doc_df[i,j]==0){
      sent_doc_df[i,j]="No"
    }else{sent_doc_df[i,j]="Yes"
    }
  }
}

PLSCA_2 <- tepPLSCA(sent_term_df, sent_doc_df, make_data1_nominal = F, make_data2_nominal = T)
coordtermini_2 <- PLSCA_2$TExPosition.Data$fi[, 1:2]

dist_coordtermini2 <- dist(coordtermini_2)
hclust_termini2 <- hclust(dist_coordtermini2, method = "ward.D2")
plot(hclust_termini2, xlab = "", hang = -1)







cluster_termini_2 <- Mclust(coordtermini_2)
summary(cluster_termini_2)
plot(cluster_termini_2, "classification")
text(coordtermini_2, labels = rownames(coordtermini_2), col = cluster_termini_2$classification)
plot(cluster_termini_2, "BIC")
plot(cluster_termini_2, "uncertainty")
plot(cluster_termini_2, "density")

kmeans_termini <- kmeans(coordtermini_2, centers = 2, nstart=10, algorithm = "MacQueen")
summary(kmeans_termini)
plot(coordtermini_2, col=kmeans_termini$cluster, pch=19)
text(coordtermini_2, labels = rownames(coordtermini_2), col = kmeans_termini$cluster)

library(ppclust)
fuzzy_termini <- fcm(coordtermini_2, centers =2)
plot(coordtermini_2, col=fuzzy_termini$cluster)
text(coordtermini_2, labels = rownames(coordtermini_2), col = fuzzy_termini$cluster)






plot(density(term_sent[3, ]))
plot(density(term_sent[, 5]))

#celebr & disgust
par(mfrow=c(2, 1))
par(mar=c(2, 2, 2, 2)+0.1)
plot(density(term_sent[, 3]))
plot(density(term_sent[4, ]))
term_sent[4, ]
term_sent[, 3]


# AC ENCODED THROUGH 0, 1

for (i in 1:nrow(doc_term)) {
  for(j in 1:ncol(doc_term)) {
    if(doc_term[i,j]==0){
      doc_term[i,j]==0
    }else{doc_term[i,j]==1
    }
  }
}

for (i in 1:nrow(doc_sent_matrix)) {
  for(j in 1:ncol(doc_sent_matrix)) {
    if(doc_sent_matrix[i,j]==0){
      doc_sent_matrix[i,j]==0
    }else{doc_sent_matrix[i,j]==1
    }
  }
}


term_doc_01 <- t(doc_term)
term_sent_nuova <- term_doc_01 %*% doc_sent_matrix
is.numeric(doc_sent_matrix[1, 1])
