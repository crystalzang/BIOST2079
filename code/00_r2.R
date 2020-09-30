data <- read_csv("/Users/czang/Documents/2020Fall/2079ML/data/Breast_GSE45827.csv")

label <- data$type

matrix2 <- apply(data[,3:ncol(data)], 2, function(x) log(x) )


#transform the matrix in order to cluster features
# sample in colum, features in rown
dim(matrix2)

#construct pca
res.pca2 <- prcomp(matrix2, scale = T)
summary(res.pca2)

#variance explained
fviz_eig(res.pca2, ncp=10,addlabels = TRUE, ylim = c(0, 55))

#dim1, 2 scatterplot by cancer types
groups <- as.factor(label)
fviz_pca_ind(res.pca2,
             col.ind = groups, # color by groups
             #  palette = c("#00AFBB",  "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = TRUE
)


loadings <- res.pca2$rotation

dim(loadings)
loadings[1:10, 1:5]

# Contributions of variables to PCs
var2 <- get_pca_var(res.pca2)
var2
var2$contrib[,]

fviz_cos2(res.pca2, choice = "var", axes = 1:10) #choise: features
