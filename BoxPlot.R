y=c(-1.194,
      +     1.076,
      +     0.736,
      +     -0.734,
      +     0.116,
      +     1.106
      + )
y
[1] -1.194  1.076  0.736 -0.734  0.116  1.106
boxplot(y)
boxplot(x,y)
x=c(3.236,
      +     5.456,
      +     4.716,
      +     2.126,
      +     5.616,
      +     4.516
      + )
boxplot(x,y)
boxplot(x,y,xlab="conditions")
boxplot(x,y,xlab="Conditions")
boxplot(x,y,xlab="Conditions",ylab="ddCt")
boxplot(x,y,xlab="Conditions",ylab="ddCt",lab=c("N","T"))
boxplot(x,y,xlab="Conditions",ylab="ddCt",lab=c("N","T"),axes=FALSE)
axis(1, at=1:2, lab=c("N","T")
       + )
axis(2, las=1, at=-2:6)




tiff('C:/Documents and Settings/Accetturo/Documenti/Rplot.tif',
     width=3000,height=3000,res=600)
#oppure width=1800,height=1800,res=300
par(mfcol = c(2, 2))
#hsa-miR-140-5p (DN)
DN=c(2.88, 2.64, 1.89, 0.72, 9.17, 2.02, 8.92, 3.60)
Norm=c(1.09, 0.72, 1.28, 1.00)

boxplot(DN,Norm, border="black", 
        col="white", boxwex=0.4, at=1:2, axes=FALSE, ylab="FC")
stripchart(DN, vertical=TRUE, add=TRUE, method = "jitter", 
           pch = 21, col = "black", bg = "black", at=1)
stripchart(Norm, vertical=TRUE, add=TRUE, method = "jitter", 
           pch = 21, col = "black", bg = "black", at=2)
axis(2, las=2, at=c(0,2,4,6,8,10), labels = TRUE, tick = TRUE, line=0, pos=0.5,
     lwd=2, lwd.ticks = 2, xpd = TRUE, cex.axis=0.7)
axis(1, at=1:2, lab=c("DN","CTRL"), pos=0, 
     lwd=2, lwd.ticks = 2)
text(2,8,"hsa-miR-140-5p", cex=0.8)

#hsa-miR-141 (DN)
Norm1=c(1.48,  0.35,	1.95,	1.00)
DN1 =c(5.24,  2.67,	1.45,	4.75,	10.25,	4.75,	11.62,	4.75)

boxplot(DN1,Norm1, border="black", 
        col="white", boxwex=0.4, at=1:2, axes=FALSE, ylab="FC")
stripchart(DN1, vertical=TRUE, add=TRUE, method = "jitter", 
           pch = 21, col = "black", bg = "black", at=1, cex=0.7)
stripchart(Norm1, vertical=TRUE, add=TRUE, method = "jitter", 
           pch = 21, col = "black", bg = "black", at=2, cex=0.7)
axis(2, las=1, at=c(0,2,4,6,8,10,12), labels = TRUE, tick = TRUE, line=0, pos=0.5,
     lwd=1, lwd.ticks = 2, xpd = TRUE, cex.axis=0.7)
axis(1, at=1:2, lab=c("DN","CTRL"), pos=0, 
     lwd=2, lwd.ticks = 2)
text(2,10,"hsa-miR-141", cex=0.8)
#dev.copy(tiff,filename = "C:/Documents and Settings/Accetturo/Documenti/Rplot.tif",
#     units = "px", 
#     bg = "white", res = 300,
#     type = c("windows", "cairo"))
dev.off()

################################## 6 miRNAs

tiff('D:/Google Drive/Rigetto/per Matteo new/CT/BoxPlot-tot(24-05-2013)prova.tif',
     width=2500,height=2000,res=300)
#oppure width=1800,height=1800,res=300
par(mfrow = c(2, 4))

boxplot(CAMR29c,CTRL29c, range = 0, border="black", 
        col="white", boxwex=0.5, at=1:2, axes=FALSE, ylab="FC", cex=1.5, main="miR-29b-3p")
stripchart(CAMR29c, vertical=TRUE, add=TRUE, method = "jitter", 
           pch = 20, col = "black", cex=1.7, bg = "black", at=1)
stripchart(CTRL29c, vertical=TRUE, add=TRUE, method = "jitter", 
           pch = 20, col = "black", cex=1.7, bg = "black", at=2)
axis(2, las=2, at=c(0,0.5,1,1.5,2,2.5), labels = TRUE, tick = TRUE, line=0, pos=0.5,
     lwd=2, lwd.ticks = 2, xpd = TRUE, cex.axis=0.8)
axis(1, at=1:2, lab=c("CAMR","CTRL"), pos=0, 
     lwd=2, lwd.ticks = 2)
#text(2,0.5,"hsa_miR_29c", cex=0.8)

boxplot(CAMR148a,CTRL148a, range = 0, border="black", 
        col="white", boxwex=0.5, at=3:4, axes=FALSE, main="miR-148b-3p")
stripchart(CAMR148a, vertical=TRUE, add=TRUE, method = "jitter", 
           pch = 20, col = "black", cex=1.7, bg = "black", at=3)
stripchart(CTRL148a, vertical=TRUE, add=TRUE, method = "jitter", 
           pch = 20, col = "black", cex=1.7, bg = "black", at=4)
#axis(2, las=2, at=c(0,0.5,1,1.5,2), labels = TRUE, tick = TRUE, line=0, pos=0.5,
#     lwd=2, lwd.ticks = 2, xpd = TRUE, cex.axis=0.7)
axis(1, at=3:4, lab=c("CAMR","CTRL"), pos=-0.06, 
     lwd=2, lwd.ticks = 2)
#text(2,0.5,"hsa-miR-148a", cex=0.8)

boxplot(CAMR769,CTRL769, range = 0, border="black",ylim=c(0,1.2),
        col="white", boxwex=0.5, at=5:6, axes=FALSE, main="miR-769-5p")
stripchart(CAMR769, vertical=TRUE, add=TRUE, method = "jitter", 
           pch = 20, col = "black", cex=1.7, bg = "black", at=5)
stripchart(CTRL769, vertical=TRUE, add=TRUE, method = "jitter", 
           pch = 20, col = "black", cex=1.7, bg = "black", at=6)
# axis(2, las=2, at=c(0,0.4,0.8,1.2), labels = TRUE, tick = TRUE, pos=0.5,
#      lwd=2, lwd.ticks = 2, xpd = TRUE, cex.axis=0.7)
axis(1, at=5:6, lab=c("CAMR","CTRL"), pos=-0.15, 
     lwd=2, lwd.ticks = 2, outer = FALSE)
#text(2,0.3,"p-value = 0.01017", cex=0.8)

boxplot(CAMR195,CTRL195, range = 0, border="black", 
        col="white", boxwex=0.5, at=7:8, axes=FALSE, main="miR-16-5p")
stripchart(CAMR195, vertical=TRUE, add=TRUE, method = "jitter", 
           pch = 20, col = "black", cex=1.7, bg = "black", at=7)
stripchart(CTRL195, vertical=TRUE, add=TRUE, method = "jitter", 
           pch = 20, col = "black", cex=1.7, bg = "black", at=8)
# axis(2, las=2, at=c(0,0.5,1,1.5), labels = TRUE, tick = TRUE, line=0, pos=0.5,
#      lwd=2, lwd.ticks = 2, xpd = TRUE, cex.axis=0.7)
axis(1, at=7:8, lab=c("CAMR","CTRL"), pos=-0.01, 
     lwd=2, lwd.ticks = 2)
#text(2,0.5,"hsa-miR-195", cex=0.8)

boxplot(CAMR18a,CTRL18a, range = 0, border="black", 
        col="white", boxwex=0.5, at=9:10, ylab="FC", axes=FALSE, main="miR-18b-5p")
stripchart(CAMR18a, vertical=TRUE, add=TRUE, method = "jitter", 
           pch = 20, col = "black", cex=1.7, bg = "black", at=9)
stripchart(CTRL18a, vertical=TRUE, add=TRUE, method = "jitter", 
           pch = 20, col = "black", cex=1.7, bg = "black", at=10)
axis(2, at=c(0,0.5,1,1.5), labels = TRUE, tick = TRUE, line=0, pos=8.5,
     lwd=2, lwd.ticks = 2, xpd = TRUE, cex.axis=0.8)
axis(1, at=9:10, lab=c("CAMR","CTRL"), pos=0.0001, 
     lwd=2, lwd.ticks = 2)
#text(2,0.5,"hsa_miR_18a", cex=0.8)

boxplot(CAMR181c,CTRL181c, range = 0, border="black", 
        col="white", boxwex=0.5, at=11:12, axes=FALSE, main="miR-181a-5p")
stripchart(CAMR181c, vertical=TRUE, add=TRUE, method = "jitter", 
           pch = 20, col = "black", cex=1.7, bg = "black", at=11)
stripchart(CTRL181c, vertical=TRUE, add=TRUE, method = "jitter", 
           pch = 20, col = "black", cex=1.7, bg = "black", at=12)
#axis(2, las=2, at=c(0,0.3,0.6,0.9,1.2,1.5), labels = TRUE, tick = TRUE, line=0, pos=10.5,
#lwd=2, lwd.ticks = 2, xpd = TRUE, cex.axis=0.8)
axis(1, at=11:12, lab=c("CAMR","CTRL"), pos=0.03, 
     lwd=2, lwd.ticks = 2)
#text(2,0.5,"hsa-miR-181c", cex=0.8)

dev.off()



################################## 4 miRNAs

tiff('D:/Rigetto/per Matteo new/CT/BoxPlot-tot(30-05-2014).tif',
     width=3000,height=2000,res=300)
#oppure 
#width=1800,height=1800,res=300
par(mfrow = c(1, 4))
par(mar=c(5,5,4,0)) #margin size c(bottom, left, top, right) in lines: default = c(5, 4, 4, 2) + 0.1
par(oma=c(1,1,1,1)) #outer margin size c(bottom, left, top, right) in lines: default = c(0, 0, 0, 0) + 0.1

# tiff('D:/Rigetto/per Matteo new/CT/BoxPlot-mir-16(30-05-2014).tif',
#      width=2500,height=2000,res=300)
#oppure 
#width=1800,height=1800,res=300
#par(mfrow = c(1, 4))
boxplot(FC_CAMR_16, FC_CCMR_16, FC_CTRL_16, range = 0, border="black", ylim=c(0,4),
        col="white", boxwex=0.8, at=1:3, axes=FALSE, cex.lab=1.8, ylab="FC", cex.main=2, main="miR-16-5p")
stripchart(FC_CAMR_16, vertical=TRUE, add=TRUE, method = "jitter", 
           pch = 20, col = "black", cex=1.7, bg = "black", at=1)
stripchart(FC_CCMR_16, vertical=TRUE, add=TRUE, method = "jitter", 
           pch = 20, col = "black", cex=1.7, bg = "black", at=2)
stripchart(FC_CTRL_16, vertical=TRUE, add=TRUE, method = "jitter", 
           pch = 20, col = "black", cex=1.7, bg = "black", at=3)
axis(2, las=2, at=c(0,1,2,3,4), labels = TRUE, tick = TRUE, line=0, pos=0.3,
     lwd=1.8, lwd.ticks = 1.8, xpd = TRUE, cex.axis=2)
axis(1, at=1:3, lab=c("CAMR","CCMR","CTRL"), pos=-0.01, 
     lwd=2, lwd.ticks = 2)
#text(2,0.5,"hsa-miR-195", cex=0.8)
#dev.off()

# tiff('D:/Rigetto/per Matteo new/CT/BoxPlot-mir-29b(30-05-2014).tif',
#      width=2500,height=2000,res=300)
boxplot(FC_CAMR_29b,FC_CCMR_29b,FC_CTRL_29b, range = 0, border="black", ylim=c(0,4),
        col="white", boxwex=0.8, at=1:3, axes=FALSE, cex=1.5, cex.main=2, main="miR-29b-3p")
stripchart(FC_CAMR_29b, vertical=TRUE, add=TRUE, method = "jitter", 
           pch = 20, col = "black", cex=1.7, bg = "black", at=1)
stripchart(FC_CCMR_29b, vertical=TRUE, add=TRUE, method = "jitter", 
           pch = 20, col = "black", cex=1.7, bg = "black", at=2)
stripchart(FC_CTRL_29b, vertical=TRUE, add=TRUE, method = "jitter", 
           pch = 20, col = "black", cex=1.7, bg = "black", at=3)
# axis(2, las=1, at=c(0,1,2,3,4), labels = TRUE, tick = TRUE, line=0, pos=0.5,
#      lwd=2, lwd.ticks = 2, xpd = TRUE, cex.axis=0.8)
axis(1, at=1:3, lab=c("CAMR","CCMR","CTRL"), pos=-0.01, 
     lwd=2, lwd.ticks = 2)
#text(2,0.5,"hsa_miR_29c", cex=0.8)
#dev.off()

boxplot(FC_CAMR_148b,FC_CCMR_148b,FC_CTRL_148b, range = 0, border="black", ylim=c(0,4),
        col="white", boxwex=0.8, at=7:9, axes=FALSE, cex.main=2, main="miR-148b-3p")
stripchart(FC_CAMR_148b, vertical=TRUE, add=TRUE, method = "jitter", 
           pch = 20, col = "black", cex=1.7, bg = "black", at=7)
stripchart(FC_CCMR_148b, vertical=TRUE, add=TRUE, method = "jitter", 
           pch = 20, col = "black", cex=1.7, bg = "black", at=8)
stripchart(FC_CTRL_148b, vertical=TRUE, add=TRUE, method = "jitter", 
           pch = 20, col = "black", cex=1.7, bg = "black", at=9)
#axis(2, las=2, at=c(0,0.5,1,1.5,2), labels = TRUE, tick = TRUE, line=0, pos=0.5,
#     lwd=2, lwd.ticks = 2, xpd = TRUE, cex.axis=0.7)
axis(1, at=7:9, lab=c("CAMR","CCMR","CTRL"), pos=-0.01, 
     lwd=2, lwd.ticks = 2)
#text(2,0.5,"hsa-miR-148a", cex=0.8)

boxplot(FC_CAMR_769,FC_CCMR_769,FC_CTRL_769, range = 0, border="black",ylim=c(0,4),
        col="white", boxwex=0.8, at=10:12, axes=FALSE, cex.main=2, main="miR-769-5p")
stripchart(FC_CAMR_769, vertical=TRUE, add=TRUE, method = "jitter", 
           pch = 20, col = "black", cex=1.7, bg = "black", at=10)
stripchart(FC_CCMR_769, vertical=TRUE, add=TRUE, method = "jitter", 
           pch = 20, col = "black", cex=1.7, bg = "black", at=11)
stripchart(FC_CTRL_769, vertical=TRUE, add=TRUE, method = "jitter", 
           pch = 20, col = "black", cex=1.7, bg = "black", at=12)
# axis(2, las=2, at=c(0,0.4,0.8,1.2), labels = TRUE, tick = TRUE, pos=0.5,
#      lwd=2, lwd.ticks = 2, xpd = TRUE, cex.axis=0.7)
axis(1, at=10:12, lab=c("CAMR","CCMR","CTRL"), pos=-0.01, 
     lwd=2, lwd.ticks = 2, outer = FALSE)
#text(2,0.3,"p-value = 0.01017", cex=0.8)

dev.off()
