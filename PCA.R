DEgenes <- read.table(file("clipboard"), sep="\t", header = T)
PCAdata <- DEgenes[,8:25]
pc <- prcomp(PCAdata, scale. = T)
summary(pc) #dalla cumulative proportion capisco quante componenti devo utilizzare per spiegare i dati
scores <- pc$x
screeplot(pc)
barplot(pc$sdev/pc$sdev[1])
biplot(pc, 1:2, cex=0.8,0.7)
scatterplot3d(pc$x[,1], pc$x[,2], pc$x[,3], main = "PCA 3D plot")

data3 <- read.table(file("clipboard"), sep="\t", header =T)

plot3d(pc$x[,1:3],xlab="Component 1",main="My 3D 
PCA",ylab="Component 2", zlab="Component 3", type="h", box=F, axes=F) 
spheres3d(pc$x[,1:3], radius=0.2, col="pink") 
grid3d(side="z", at=list(z=0)) 
text3d(pc$x[,1:3], text=rownames(pc$x), adj=1.3)

x11(height=6, width=12, pointsize=12); par(mfrow=c(1,2))
mycolors <- c("red", "green", "pink")
scatterplot3d(pc$x[,1:3], pch=20, color=mycolors)

# Performs principal component analysis after scaling the data.
# It returns a list with class "prcomp" that contains five components:
#   (1) the standard deviations (sdev) of the principal components,
#   (2) the matrix of eigenvectors (rotation),
#   (3) the principal component data (x),
#   (4) the centering (center) and
#   (5) scaling (scale) used.
pca <- prcomp(mydata, scale=T)
for(i in 1:dim(PCAdata)[1]) {
dataforPCA<-matrix(c(PCAdata[i,1],PCAdata[i,2],PCAdata[i,3],PCAdata[i,4],PCAdata[i,5],PCAdata[i,6],PCAdata[i,7],PCAdata[i,8],PCAdata[i,9],PCAdata[i,10],PCAdata[i,11],PCAdata[i,12],PCAdata[i,13],PCAdata[i,14],PCAdata[i,15],PCAdata[i,16],PCAdata[i,17],PCAdata[i,18]), nrow = 18, ncol = 67)}
)



##############################################################

z1 <- rnorm(10000, mean=1, sd=1);
z2 <- rnorm(10000, mean=3, sd=3);
z3 <- rnorm(10000, mean=5, sd=5);
z4 <- rnorm(10000, mean=7, sd=7);
z5 <- rnorm(10000, mean=9, sd=9);
mydata <- matrix(c(z1, z2, z3, z4, z5), 2500, 20, byrow=T,
dimnames=list(paste("R", 1:2500, sep=""), paste("C", 1:20, sep="")))

###############################################################

datiPCA <- read.table(file("clipboard"), sep="\t", header = T, row.names = 1)#row.names=1 legge l'header anche per le righe cioè capisce che anche le righe hanno un intestazione
pca <- prcomp(datiPCA, scale. = T)

plot3d(pca$x[,1:3],xlab="Component 1",main="My 3D 
PCA",ylab="Component 2", zlab="Component 3", type="h", box=F, axes=F) 
spheres3d(pca$x[,1:3], radius=0.2, col="pink") 
grid3d(side="z", at=list(z=0)) 
text3d(pca$x[,1:3], text=rownames(pc$x), adj=1.3)


> n <- dim(eu)[1]
> col <- rep("black", n)
> col[eu$Euro == 1] <- "red"
> plot(pca$scores[, 1:2], pch = 20, col = col, xlab = "PC1 (43%)",
ylab = "PC2 (32%)", main = "Indicatori Economici EU")
abline(h = 0, v = 0, col = "red", lty = "dashed")
text(pca$scores[, 1:2], labels = as.character(eu$Sigla), pos = 4,
+ cex = 0.6)
