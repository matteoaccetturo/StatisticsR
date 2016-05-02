dataset<-read.table("D:/Google Drive/Nefropatia diabetica/Cartel1.txt", header=TRUE)

for(i in 1:ncol(dataset)){
  norm=c(dataset[1:4,i]); DN=c(dataset[5:12,i]); w<-t.test(norm,DN); y<-wilcox.test(norm,DN); 
  x <- matrix(w$p.value:y$p.value, ncol = 2)
  write(x, file = "C:/Users/Matteo/Google Drive/Nefropatia diabetica/pvalues.txt",
        append = TRUE, sep = "\t", ncolumns = 2)
}

for(i in 1:ncol(dataset)){
  norm=c(dataset[1:4,i]); DN=c(dataset[5:12,i]); y<-wilcox.test(norm,DN); 
  write(y$p.value, file = "C:/Users/Matteo/Google Drive/Nefropatia diabetica/pvaluesWilk.txt",
        append = TRUE)
}

#test delle permutazioni
norm=c(dataset[1:4,18])
DN=c(dataset[5:12,18])
#t<-t.test(norm,DN)
nrep<-10000
out<-rep(NA,nrep)
delta<-mean(DN)-mean(norm)
tot<-c(DN,norm)
for(i in 1:nrep){
  DNrand<-sample(tot,8,replace=FALSE)
  normrand<-sample(tot,4,replace=FALSE)
  deltarand<-mean(DNrand)-mean(normrand)
  #trand<-t.test(normrand,DNrand)
  out[i]<-deltarand
  #out[i]<-trand$statistic
}
plot(table(out))
#plot(out)
abline(v=delta, col="red") #se metto h=delta la linea è orizzontale
#abline(v=t$statistic, col="red")
