install.packages("FactoMineR")

install.packages("devtools")

devtools::install_github("kassambara/factoextra")

library(factoextra)

library(FactoMineR)

res.pca<-PCA(RD_1, graph=FALSE) #RD_1 is a dataframe


print(res.pca)

eigenvalues<-res.pca$eig

head(eigenvalues[, 1:2])

fviz_screeplot(res.pca, ncp=10)

head(res.pca$var$coord)

fviz_pca_var(res.pca)

fviz_pca_var(res.pca, alpha.var="contrib") + theme_minimal()

fviz_pca_biplot(res.pca)

fviz_contrib(res.pca, choice = "var", axes = 1, top = 50)

res_desc<-dimdesc(res.pca, axes = 1:3, proba = 0.05)

res_desc$Dim.3
