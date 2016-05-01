c5b9<-c(4.196667, 12.53, 17.5825, 19.1575)
nox4<-c(5.09, 6.016667, 11.78667, 18.53)
modello = lm(formula = nox4 ~ c5b9, x=TRUE, y=TRUE)
#lm è il modello lineare di Y in funzione di X (Y~X)
modello
mod = lm(nox4 ~ c5b9)
summary(mod)
cor (c5b9, nox4)
plot (c5b9, nox4)
abline(mod)
cor.test(c5b9, nox4)
qt(0.950, 2)
bmp(filename="C:/Documents and Settings/Matteo/Documenti/Federica/c5b9-nox4.bmp", width=600, bg="white") #scrive un file bmp con il grafico
plot (c5b9, nox4, ylim=c(0,22), xlim=c(0,22), pch=15, col="blue", axes=FALSE, xaxs = "r", yaxs = "r" )
#x e ylim serve per impostare min e max degli assi; pch sceglie il tipo di pallino per ogni punto; col è il colore del punto; axes FALSE toglie gli assi di default in modo da poterli disegnare autonomamente; x e yaxs se sono "i" fanno incontrare gli assi sullo 0, se sono "r" lasciano un 4% di margine
abline(mod, col="red") #di segna la linea secondo il modello mod
annotation<-c("T0", "T15", "T30", "T60") 
text(c5b9, nox4, annotation, cex=0.8, pos=4, col="red") #permette di scrivere qualcosa sul grafico, in questo caso annotation definito prima in rosso, cex è la grandezza del carattere, pos è la posizione (4= a destra, 3=sotto, ecc)
Axis(side=1, labels=TRUE)#disegna gli assi come li si vuole
Axis(side=2, labels=TRUE)
dev.off() #chiude lo strumento per scrivere files su disco

data <- data.frame(c5b9, nox4)#immette i dati in un dataframe
library(drc)#carica il pacchetto drc che contiene diverse funzioni per l'analisi di regressione non-lineare
library(aomisc)
modelexp <- drm(nox4 ~ c5b9, data=data, fct=expoGrowth())#purtroppo expoGrowth() è una funzione del pacchetto aomisc che non riesco a trovare

fit2 <- lm(data$nox4 ~ data$c5b9 + I(data$c5b9^2))
summary(fit2)
points(data$c5b9, predict(fit2), type="l", col="red", lwd=2)

andamento2 <- function(x) fit2$coefficient[3]*x^2 + fit2$coefficient[2]*x + fit2$coefficient[1]
curve(andamento2, col="red", lwd=2)
points(data$c5b9, data$nox4, type="p", lwd=3)

startA <- max(c5b9)-min(c5b9)
fitexp <- nls(data$nox4 ~ exp(data$c5b9))
summary(fitexp)
plot(data$c5b9, data$nox4, type="p", lwd=3)
andamentoexp <- function(x) fitexp$coefficient[1]*exp(fitexp$coefficient[2]*x)
lines(data$c5b9, predict(fitexp, list(x = data$c5b9)))#disegna la curva in modo discreto 
curve(andamentoexp, col="red", lwd=2)#disegna la curva in modo continuo
points(data$c5b9, data$nox4, type="p", lwd=3)#aggiunge i punti sulla curva

#funzione self-start
exponenInit=function(mCall,LHS,data) {
>  xy=sortedXyData(mCall[["x"]],LHS,data)
>  r=0.01				#Just guess r=0.01 to start the fit
>  A=min(xy$y)		#Use the minimum y value as an initial guess for A
>  value=c(r,A)
>  names(value)=mCall[c("r","A")]
>  return(value)
> }

c5b9<-c(4.196667, 12.53, 17.5825, 19.1575)
nox4<-c(5.09, 6.016667, 11.78667, 18.53)
data <- data.frame(c5b9, nox4)#immette i dati in un dataframe
modelloexp <- nls(data$nox4 ~ a*exp(b * data$c5b9), start = list(a = 0, b = 1), trace = F)
summary(modelloexp)
a <- round(summary(modelloexp)$coefficients[1, 1], 4)
b <- round(summary(modelloexp)$coefficients[2, 1], 4)
plot(data$nox4 ~ data$c5b9, main = "Fitted exponential function", sub = "Blue: fit; green: known")
s <- seq(0, 1, length = 100)
lines(s, s^3, lty = 2, col = "green")
lines(s, predict(modelloexp, list(x = s)), lty = 1, col = "blue")
text(0, 0.5, paste("y =e^ (", a, " + ", b, " * x)", sep = ""), pos = 4)
andamentoexp <- function(x) modelloexp
summary(modelloexp)

#esempio trovato in internet
beta <- 0.05
n <- 100 
temp <- data.frame(y = exp(beta * seq(n)) + rnorm(n), x = seq(n))
plot(temp$x, temp$y)
mod <- nls(y ~ exp(a + b * x), data = temp, start = list(a = 0, b = 0))
lines(temp$x, predict(mod, list(x = temp$x)))
summary(mod)




# Data
c5b9<-c(4.196667, 12.53, 17.5825, 19.1575)
nox4<-c(5.09, 6.016667, 11.78667, 18.53)
d <- data.frame(c5b9, nox4)

# Non-linear model
fit <- nls(nox4 ~ exp(b*c5b9),
                 data = d,
                 start = list(b = 0.01),
                 trace = F)


summary(fit)

# Plot
res <- data.frame(c5b9, pred = predict(fit))#dato il valore di X predice il valore di Y secondo il modello
plot(c5b9, nox4, ylim = c(0,22), xlim = c(0,22), type="p")
points(res[order(c5b9),], type='l', col="blue")#aggiunge la curva predetta
plot(res[order(c5b9),], type='l', col="blue")
andamentoexp <- function(x) exp(fit$coefficient[2]+fit$coefficient[1]*x)
curve(andamentoexp, col="red", lwd=2)#disegna la curva in modo continuo
points(data$c5b9, data$nox4, type="p", lwd=3)
plot(andamentoexp)


