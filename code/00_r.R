data <- read_csv("/Users/czang/Documents/2020Fall/2079ML/data/Breast_GSE45827.csv")


matrix <- apply(data[,3:ncol(data)], 2, function(x) log(x) )


#transform the matrix in order to cluster features
# sample in colum, features in rown
matrix <- t(matrix)
dim(matrix)

#construct pca
res.pca <- prcomp(matrix, scale = T)
summary(res.pca)
fviz_eig(res.pca, ncp=10,addlabels = TRUE, ylim = c(0, 100))
fviz_pca_ind(res.pca,  label = "none", addEllipses = F)

loadings <- res.pca$rotation
fviz_pca_var(res.pca, col.var = "black")

var <- get_pca_var(res.pca)
var$contrib[,1]
fviz_cos2(res.pca, choice = "var", axes = 1)
