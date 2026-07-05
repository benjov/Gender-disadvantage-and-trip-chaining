###################################################
#                                                 #
#           V E R S I O N   D E   R               #
#                                                 #
#                    3.5.1                        #
#                                                 #
###################################################

###################################################
#                                                 #
#          ESTIMACIÓNJ ÁREAS PEQUEÑAS             #
#               MODELO ESPACIAL                   #
#                                                 #
#                                                 #
###################################################


###############   OCTUBRE 2021 ####################

# Directorio de trabajo

setwd("D://ENOE_EAP_2017")
rm(list=ls())

library(foreign)
library(MASS)
library(nlme)
library(sae)
library(car)
library(lmtest)
library(maptools)
library(graphics)
library(Matrix)
library(spdep)
library(rgdal)
library(lattice)
library (openxlsx)
library(fdth)
library(normtest)
library(nortest)
library(moments)
miscolor<-rainbow(16, s = 1, v = 1, start = 0.04, end = max(1,16 )/16, alpha = .7)

###################################################
#        USUARIO:  PARÁMETROS DEL PROGRAMA        #
###################################################

Selpeaocu <-1     # 1.-PEA, 2.-OCU 3.-INF
Ajust <- 2        # 1.-LINEAL, 2.-CURVA (no mover este parámetro)
Vecindad <- "W"   # W, B, C , U, S  Vecindad Espacial (no mover este parámetro)
VecinosPea <- 4   # No mover este parámetro
VecinosOcu <- 4   # No mover este parámetro
VecinosInf <- 5   # No mover este parámetro


###################################################
#          USUARIO:  ARCHIVO DE ENTRADA           #
###################################################


if(Selpeaocu==1 | Selpeaocu==2 ){
nombarch <- "ENOE_PEA_OCU_2017.xlsx"  
} else {
nombarch <- "ENOE_INF_2017.xlsx"  
  }

###################################################
#            ASIGNACIÓN DE VARIABLES              #
###################################################

Pop <- 3        # POBLACIÓN

if(Selpeaocu==1){
   VariaY <- 4  # PEA   
   Error  <- 5     
} else {
       if(Selpeaocu==2){
          VariaY <- 6  # OCUPADOS
          Error  <- 7 
       } else {          
          VariaY <- 4  # OCUPADOS INFORMALES
          Error  <- 5 
       }
}        

if(Selpeaocu==1 | Selpeaocu==2 ){
   VariaX <- c(8, 9, 10) #REGRESORAS 
} else {
   VariaX <- c(6, 7, 8) #REGRESORAS 
  }



###################################################
#   MAPA DE LA REPUBLICA MUNICIPIOS CON MUESTRA   #
###################################################

options(digits=16)

# Lectura de archivos 
mapmexmu <- rgdal::readOGR("MUNI.shp", stringsAsFactors=FALSE) 
Muni_Muestra <- read.xlsx(nombarch, sheet = 1, skipEmptyRows = FALSE)

# Creacion de llave para realizar el "Merge"
CharEnt <- paste0("0", Muni_Muestra$ENTIDAD) # paste0("0", datos$ENTIDAD)
NumCha <- nchar(CharEnt)
Str_Ent <- substr(CharEnt, NumCha-1, NumCha)
CharMun <- paste0("00", Muni_Muestra$MUNICIP) # paste0("00", datos$MUNICIPIO)
NumCha <- nchar(CharMun)
Str_Mun <- substr(CharMun, NumCha-2, NumCha)
Muni_Muestra[, "llave"] <- paste0(Str_Ent, Str_Mun)
if(Selpeaocu==1 | Selpeaocu==2 ){
mapsonmue <- merge(mapmexmu[ ,c("ENMUESTRA", "LLAVE")],Muni_Muestra[ , c("T15ymas","PEA","PEA_EE","Ocupados","Ocupados_EE","RelDepEco","Hom15a44","ImssIssste","llave","ENTIDAD")],
all.x=FALSE , by.x = "LLAVE", by.y = "llave") #, sort = FALSE, all.x=FALSE
} else {
mapsonmue <- merge(mapmexmu[ ,c("ENMUESTRA", "LLAVE")],Muni_Muestra[ , c("T15ymas", "OcuInfor","OcuInfor_EE","ImssIssste","SeguPopu","Pob45ymas","llave","ENTIDAD")],
all.x=FALSE , by.x = "LLAVE", by.y = "llave") #, sort = FALSE, all.x=FALSE
  }
rng <- c(15,100)
my.at <- pretty(rng, 8)

# Mapas municipios con muestra
if(Selpeaocu==1){
mapsonmue$MPEA<-mapsonmue$PEA*100
p1<-spplot(mapsonmue, "MPEA", at=my.at,cuts=7,main="PROPORCIÓN PEA, SEGÚN ESTIMACIÓN DIRECTA ENOE 2017_1",key.space="right",xlab="Municipios de México")
plot(p1)
} else {
if(Selpeaocu==2){
mapsonmue$MOCU<-mapsonmue$Ocupados*100
p1<-spplot(mapsonmue, "MOCU", at=my.at,cuts=7,main="PROPORCIÓN OCUPADOS, SEGÚN ESTIMACIÓN DIRECTA ENOE 2017_1",key.space="right",xlab="Municipios de México")
plot(p1)
} else {
mapsonmue$MINF<-mapsonmue$OcuInfor*100
p1<-spplot(mapsonmue, "MINF", at=my.at,cuts=7,main="PROPORCIÓN INFORMALES, SEGÚN ESTIMACIÓN DIRECTA ENOE 2017_1",key.space="right",xlab="Municipios de México")
plot(p1)
   }
}   

##########################################################
#      CREA VECINDADES Y GENERA MATRIZ DE DISTANCIAS     #
##########################################################

#Crea vecindades sin frontera 
coordNC = coordinates(mapsonmue) # obtienen coordenadas de la cabeceras municipales
d05m = dnearneigh(coordNC, 0, 0.5, row.names=mapsonmue$LLAVE)
if(Selpeaocu==1){
nb.5NN = knn2nb(knearneigh(coordNC,k=VecinosPea),row.names=mapsonmue$LLAVE) 
} else {
if(Selpeaocu==2){
nb.5NN = knn2nb(knearneigh(coordNC,k=VecinosOcu),row.names=mapsonmue$LLAVE)
} else {
nb.5NN = knn2nb(knearneigh(coordNC,k=VecinosInf),row.names=mapsonmue$LLAVE)
 }
}
nb.5NN
distance = unlist(nbdists(nb.5NN, coordNC))
plot(mapsonmue,main="MUNICIPIOS DE MÉXICO. VECINOS MAS CERCANOS, SEGÚN ENOE_2017")
plot(nb.5NN, coordNC, add=T,col="RED", lwd=2)

#Transforma Archivos Mapas a proyecciones 

sidsmue<-mapsonmue
class(sidsmue)
sidssonmue<-sidsmue
sids_NAD_son_mue<-spTransform(sidssonmue, CRS("+init=epsg:3358"))
sids_SP_son_mue<-spTransform(sidssonmue, CRS("+init=ESRI:102719"))

#Este peso es basado en distancias euclidianas (pueden existir otras)
distrimue<-nbdists(nb.5NN, coordinates(sids_SP_son_mue))
idwmue<-lapply(distrimue, function(x) 1/(x/1000))
nb_idwb_mue<-nb2listw(nb.5NN, glist=idwmue, style=Vecindad)
nb_idwb_mue
summary(unlist(nb_idwb_mue$weights))

#Se crea a continuación la Matriz de Vecindades y Distancias, este tipo de archivo es el que utiliza la libreria SAE de la Dra. Isabel Molina
W<-nb2mat(nb_idwb_mue$neighbours,glist=nb_idwb_mue$weights, style=Vecindad)

                                  
###########################################################
#             SE EXAMINA CORRELACIÓN ESPACIAL             #
###########################################################

if(Selpeaocu==1){
VxNA <- lag.listw(nb2listw(nb.5NN, glist=idwmue, style=Vecindad), mapsonmue$PEA, NAOK=TRUE)
} else {
if(Selpeaocu==2){
VxNA <- lag.listw(nb2listw(nb.5NN, glist=idwmue, style=Vecindad), mapsonmue$Ocupados, NAOK=TRUE)
} else {
VxNA <- lag.listw(nb2listw(nb.5NN, glist=idwmue, style=Vecindad), mapsonmue$OcuInfor, NAOK=TRUE)
   }
}   
if(Selpeaocu==1){
promtotvec<-as.vector(mapsonmue$PEA)
} else {
if(Selpeaocu==2){
promtotvec<-as.vector(mapsonmue$Ocupados)
} else {
promtotvec<-as.vector(mapsonmue$OcuInfor)
   }
}   
testpea<-moran.test(promtotvec, listw=nb_idwb_mue,alternative="two.sided")

###########################################################
#              GRAFICA DE DISPERSIÓN ESPACIAL             #
###########################################################

if(Selpeaocu==1){
moran.plot(promtotvec, nb_idwb_mue,main="GRÁFICA DE DISPERSIÓN DE MORAN. PROPORCIÓN PEA. ESTIMACIÓN DIRECTA, SEGÚN ENOE 2017_1",cex.main=1.2,
xlab="PROPORCIÓN PEA. ESTIMACIÓN DIRECTA",ylab="Lags",xlim=c(.1,.95), ylim=c(.40,.8),
pch=19,col="red",zero.policy=FALSE,labels=FALSE,cex.lab=1.15)
} else {
if(Selpeaocu==2){
moran.plot(promtotvec, nb_idwb_mue,main="GRÁFICA DE DISPERSIÓN DE MORAN. PROPORCIÓN OCU. ESTIMACIÓN DIRECTA, SEGÚN ENOE 2017_1",cex.main=1.2,
xlab="PROPORCIÓN OCUPADOS. ESTIMACIÓN DIRECTA",ylab="Lags",xlim=c(.3,.75), ylim=c(.40,.8),
pch=19,col="red",zero.policy=FALSE,labels=FALSE,cex.lab=1.15)
} else {
moran.plot(promtotvec, nb_idwb_mue,main="GRÁFICA DE DISPERSIÓN DE MORAN. PROPORCIÓN INF. ESTIMACIÓN DIRECTA, SEGÚN ENOE 2017_1",cex.main=1.2,
xlab="PROPORCIÓN INFORMALES. ESTIMACIÓN DIRECTA",ylab="Lags",xlim=c(-.1, 1), ylim=c(-.1,1),
pch=19,col="red",zero.policy=FALSE,labels=FALSE,cex.lab=1.15)
   }
}   


###########################################################
# I. DE MORAN Y PRUEBA DE DISTRIBUCIÓN ALEATORIA ESPACIAL #
###########################################################

testpea

###################################################
#        SE SELECCIONA ARCHIVO DE TRABAJO         #
###################################################

options(digits=16)
datos<-as.data.frame(mapsonmue@data)
attach(datos)
str(datos)


###################################################
# FUNCION DE LA DRA. ISABEL MODIFICADA PARA KAPPA # 
###################################################


eblupFH2<-function (formula, vardir, method = "REML", MAXITER = 300, PRECISION = 1e-02, 
    data) 
{
    result <- list(convergence = TRUE, iterations = 0, estcoef = NA, 
        variance = NA, gofit = NA, EBLUP = NA)
    if (!missing(data)) {
        formuladata <- model.frame(formula, na.action = na.omit, 
            data)
        X <- model.matrix(formula, data)
        vardir <- data[, deparse(substitute(vardir))]
    }
    else {
        formuladata <- model.frame(formula, na.action = na.omit)
        X <- model.matrix(formula)
    }
    y <- formuladata[, 1]
    omitted <- na.action(formuladata)
    nomitted <- length(omitted)
    rowNAvardirini <- which(is.na(vardir))
    vardirini <- vardir
    if (nomitted > 0) 
        vardir <- vardir[-omitted]
    rowNAvardir <- which(is.na(vardir))
    nrowNAvardir <- length(rowNAvardir)
    if (nrowNAvardir > 0) {
        y <- y[-rowNAvardir]
        X <- X[-rowNAvardir, ]
        vardir <- vardir[-rowNAvardir]
    }
    m <- length(y)
    p <- dim(X)[2]
    Xt <- t(X)
    if (method == "ML") {
        Aest.ML <- 0
        Aest.ML[1] <- median(vardir)
        k <- 0
        diff <- PRECISION + 1
        while ((diff > PRECISION) & (k < MAXITER)) {
            k <- k + 1
            Vi <- 1/(Aest.ML[k] + vardir)
            XtVi <- t(Vi * X)
            Q <- solve(XtVi %*% X)
            P <- diag(Vi) - t(XtVi) %*% Q %*% XtVi
            Py <- P %*% y
            s <- (-0.5) * sum(Vi) + 0.5 * (t(Py) %*% Py)
            F <- 0.5 * sum(Vi^2)
            Aest.ML[k + 1] <- Aest.ML[k] + s/F
            diff <- abs((Aest.ML[k + 1] - Aest.ML[k])/Aest.ML[k])
        }
        A.ML <- max(Aest.ML[k + 1], 0)
        result$iterations <- k
        if (k >= MAXITER && diff >= PRECISION) {
            result$convergence <- FALSE
            return(result)
        }
        Vi <- 1/(A.ML + vardir)
        XtVi <- t(Vi * X)
        Q <- solve(XtVi %*% X)
        beta.ML <- Q %*% XtVi %*% y
        varA <- 1/F
        std.errorbeta <- sqrt(diag(Q))
        tvalue <- beta.ML/std.errorbeta
        pvalue <- 2 * pnorm(abs(tvalue), lower.tail = FALSE)
        Xbeta.ML <- X %*% beta.ML
        resid <- y - Xbeta.ML
        loglike <- (-0.5) * (sum(log(2 * pi * (A.ML + vardir)) + 
            (resid^2)/(A.ML + vardir)))
        AIC <- (-2) * loglike + 2 * (p + 1)
        BIC <- (-2) * loglike + (p + 1) * log(m)
        goodness <- c(loglike = loglike, AIC = AIC, BIC = BIC)
        coef <- data.frame(beta.ML, std.errorbeta, tvalue, pvalue)
        variance <- A.ML
	  randeff <- A.ML * Vi * resid	
        EBLUP <- Xbeta.ML + A.ML * Vi * resid
    }
    else if (method == "REML") {
        Aest.REML <- 0
        Aest.REML[1] <- median(vardir)
        k <- 0
        diff <- PRECISION + 1
        while ((diff > PRECISION) & (k < MAXITER)) {
            k <- k + 1
            Vi <- 1/(Aest.REML[k] + vardir)
            XtVi <- t(Vi * X)
            Q <- solve(XtVi %*% X)
            P <- diag(Vi) - t(XtVi) %*% Q %*% XtVi
            Py <- P %*% y
            s <- (-0.5) * sum(diag(P)) + 0.5 * (t(Py) %*% Py)
            F <- 0.5 * sum(diag(P %*% P))
            Aest.REML[k + 1] <- Aest.REML[k] + s/F
            diff <- abs((Aest.REML[k + 1] - Aest.REML[k])/Aest.REML[k])
        }
        A.REML <- max(Aest.REML[k + 1], 0)
        result$iterations <- k
        if (k >= MAXITER && diff >= PRECISION) {
            result$convergence <- FALSE
            return(result)
        }
        Vi <- 1/(A.REML + vardir)
        XtVi <- t(Vi * X)
        Q <- solve(XtVi %*% X)
        beta.REML <- Q %*% XtVi %*% y
        varA <- 1/F
        std.errorbeta <- sqrt(diag(Q))
        tvalue <- beta.REML/std.errorbeta
        pvalue <- 2 * pnorm(abs(tvalue), lower.tail = FALSE)
        Xbeta.REML <- X %*% beta.REML
        resid <- y - Xbeta.REML
        loglike <- (-0.5) * (sum(log(2 * pi * (A.REML + vardir)) + 
            (resid^2)/(A.REML + vardir)))
        AIC <- (-2) * loglike + 2 * (p + 1)
        BIC <- (-2) * loglike + (p + 1) * log(m)
        goodness <- c(loglike = loglike, AIC = AIC, BIC = BIC)
        coef <- data.frame(beta.REML, std.errorbeta, tvalue, 
            pvalue)
        variance <- A.REML
	      randeff <- A.REML * Vi * resid	
        EBLUP <- Xbeta.REML + A.REML * Vi * resid
    }
    else if (method == "FH") {
        Aest.FH <- NULL
        Aest.FH[1] <- median(vardir)
        k <- 0
        diff <- PRECISION + 1
        while ((diff > PRECISION) & (k < MAXITER)) {
            k <- k + 1
            Vi <- 1/(Aest.FH[k] + vardir)
            XtVi <- t(Vi * X)
            Q <- solve(XtVi %*% X)
            betaaux <- Q %*% XtVi %*% y
            resaux <- y - X %*% betaaux
            s <- sum((resaux^2) * Vi) - (m - p)
            F <- sum(Vi)
            Aest.FH[k + 1] <- Aest.FH[k] + s/F
            diff <- abs((Aest.FH[k + 1] - Aest.FH[k])/Aest.FH[k])
        }
        A.FH <- max(Aest.FH[k + 1], 0)
        result$iterations <- k
        if (k >= MAXITER && diff >= PRECISION) {
            result$convergence <- FALSE
            return(result)
        }
        Vi <- 1/(A.FH + vardir)
        XtVi <- t(Vi * X)
        Q <- solve(XtVi %*% X)
        beta.FH <- Q %*% XtVi %*% y
        varA <- 1/F
        varbeta <- diag(Q)
        std.errorbeta <- sqrt(varbeta)
        zvalue <- beta.FH/std.errorbeta
        pvalue <- 2 * pnorm(abs(zvalue), lower.tail = FALSE)
        Xbeta.FH <- X %*% beta.FH
        resid <- y - Xbeta.FH
        loglike <- (-0.5) * (sum(log(2 * pi * (A.FH + vardir)) + 
            (resid^2)/(A.FH + vardir)))
        AIC <- (-2) * loglike + 2 * (p + 1)
        BIC <- (-2) * loglike + (p + 1) * log(m)
        goodness <- c(loglike = loglike, AIC = AIC, BIC = BIC)
        coef <- data.frame(beta.FH, std.errorbeta, zvalue, pvalue)
        variance <- A.FH
    	  randeff <- A.FH * Vi * resid	
        EBLUP <- Xbeta.FH + A.FH * Vi * resid
    }
    else {
        stop("Unknown fitting method (method):", method)
    }
    if (nomitted > 0 || nrowNAvardir > 0) {
        ndom <- length(vardirini)
        EBLUPfinal <- matrix(NA, nrow = ndom, ncol = 1)
        rownames(EBLUPfinal) <- seq(1:ndom)
        rowsomitted <- unique(c(omitted, rowNAvardir))
        EBLUPfinal[-rowsomitted] <- EBLUP
        EBLUP <- EBLUPfinal
    }
    result$estcoef <- coef
    result$variance <- variance
    result$gofit <- goodness
    result$randeff <- randeff
    result$resid <- resid
    result$EBLUP <- EBLUP
    return(result)
}


###################################################
#                AJUSTE A PORCENTAJES             # 
###################################################

datos[,Error] <- datos[,Error]^2
datos[,Error] <- datos[,Error]*10000 # Error
Var <- c(VariaY, VariaX)
datos[,Var] <- datos[,Var]*100


###################################################
#    MODELADO DE LA VARIANZA (CURVA DE AJUSTE )   # 
###################################################

regis <- nrow(datos)
plot(1:regis, datos[,Error],col="blue")
xxVD = log((datos[,Pop]*datos[,VariaY])[datos[,Error]>0 & datos[,Error]<500])
yyVD = datos[,Error][datos[,Error]>0&datos[,Error]<500]
regreloess<- lm(yyVD ~ xxVD, data = datos)
plot(xxVD,yyVD, pch=16, col="blue",
  xlab="Log(Numero de Habitantes Conapo)",
  ylab="Varianza ENOE",
  main="Relacion Varianza ENOE vs No. Hab. CONAPO",xlim=c(6,20))
or   = order(xxVD)
lo   = loess(yyVD~xxVD)
hh   = predict(lo)
VDpp = predict(lo,newdata=xxVD[datos[,Error]==0])
lines(xxVD[or],hh[or])
points(xxVD[datos[,Error]==0],VDpp,pch=16,col="red")
abline(regreloess,col="BLACK",lwd=3,lty=1)

# Modelado de la varianza

newxxVD = log(datos[,Pop][datos[,Error]==0])
newlo   = loess(yyVD~xxVD,control = loess.control(surface = "direct"))
if(Ajust==1){
	#la siguiente ajusta por una linea recta
	newVDpp<-regreloess$coefficients[1]+regreloess$coefficients[2]*newxxVD
} else {
	#la siguiente ajusta por una curva
	newVDpp = predict(newlo,newdata=newxxVD)
}
VDp  = datos[,Error]
VDp[datos[,Error]==0] = newVDpp
VadA<-VDp
plot(1:regis, VadA, col="BLACK", pch=21, bg ="RED")
abline(h=0,col="BLUE",lty=1.5)

###################################################
#  GENERA MATRIZ DE DISPERSIONES Y CORRELACIONES  # 
###################################################

datosg <- datos[,c(VariaY, VariaX)]
panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="lightblue",border="red",...)
}
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
usr <- par("usr"); on.exit(par(usr))
par(usr = c(0, 1, 0, 1))
r <- (cor(x, y))
txt <- format(c(r, 0.123456789), digits=digits)[1]
txt <- paste(prefix, txt, sep="")
text(.5, .5, txt,cex = 4, col="BLUE")
}

panel.smoog<-function (x, y, col = par("col"), bg = NA, pch = par("pch"), 
    cex = 1.2, col.smooth = "red", span = 2/3, iter = 3, ...) 
{
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    reg<-lm(y ~ x)
    abline(reg, col="red",lwd=2)    
}
pairs(datosg,diag.panel=panel.hist,lower.panel=panel.smoog,
upper.panel=panel.cor,main="Coef. de correlación, histograma de frecuencias y gráficas de dispersión, según ENOE 2017_1")

###################################################
###################################################
#      SE GENERA EL MODELOS SAE ESPACIAL          # 
###################################################
###################################################

Nombres <- names(datos)
outcome <- Nombres[VariaY]
variables <- Nombres[VariaX]
formu <- as.formula( paste(outcome, paste(variables, collapse = " + "), sep = " ~ ") )
datos["VadA"] <- VadA
eblupDEFES <- eblupSFH(formula=formu, vardir= VadA,W,method="REML", data=datos)
print("Coeficientes de eblupFH")
eblupDEFES$fit$estcoef
eblupDEFES
esti<-eblupDEFES$eblup

##########################################################
#          SE CALCULA COEFICIENTES ESTANDARIZADOS        #
##########################################################

Sy <- sd(datos[,VariaY])
Sxi <- apply(datos[,VariaX], 2, sd)
Bxi <- eblupDEFES$fit$estcoef[, "beta"][2:(length(VariaX)+1)] 
Bexi <- Bxi*(Sxi/Sy)
print("Coeficientes estandarizados")
Bexi

##########################################################
##########################################################
#           CALCULA ERROR CUADRATICO MEDIO               # 
##########################################################
##########################################################

mseaES <- mseSFH(formu, vardir= VadA,W,method="REML", data=datos)
mseaES$mse


##########################################################
#         ANALISA EFECTOS ALEATORIOS ESPACIALES          #
##########################################################

sig2v = eblupDEFES$fit$refvar     
betas = eblupDEFES$fit$estcoef[,1]
y     = datos[,VariaY] # PEA
Dvec  = VadA
X <- as.matrix(cbind(rep(1,length(datos[,1])),datos[,VariaX]))
gamas = sig2v/(Dvec+sig2v)
residale = (y - X%*%betas)/(sqrt((Dvec+sig2v)))
par(mfrow=c(1,2))
hist(residale,br="SCOTT",col="lightblue", border="red",ylab="Frecuencias Relativas",
xlab="Efecto Aleatorio",main="DIST. EFECTOS ALEATORIOS. SEBLUP 2017",freq=F)
plot(density(residale, bw = "nrd0", kernel = c("gaussian"),weights = NULL, window = kernel),col="BLUE",
lty=1,lwd=3,main="DENSIDAD EFECTOS ALEATORIOS. SEBLUP 2017")

#Gráfica de cuantiles

if(Selpeaocu==1){
qqPlot(residale, xlab="Cuantiles Téoricos", ylab="Cuantiles Muestrales",
main="Q-Q PLOT. EFECTOS ALEATORIOS. NIVEL SIG.=10%. PROPORCIÓN PEA, ESTIMACIÓN SEBLUP 2017_1", pch=20,envelope=.90,col="RED",col.lines="BLACK",cex=2.0)
} else {
if(Selpeaocu==2){
qqPlot(residale, xlab="Cuantiles Téoricos", ylab="Cuantiles Muestrales",
main="Q-Q PLOT. EFECTOS ALEATORIOS. NIVEL SIG.=10%. PROPORCIÓN OCU., ESTIMACIÓN SEBLUP 2017_1", pch=20,envelope=.90,col="RED",col.lines="BLACK",cex=2.0)
} else {
qqPlot(residale, xlab="Cuantiles Téoricos", ylab="Cuantiles Muestrales",
main="Q-Q PLOT. EFECTOS ALEATORIOS. NIVEL SIG.=10%. PROPORCIÓN INF., ESTIMACIÓN SEBLUP 2017_1", pch=20,envelope=.90,col="RED",col.lines="BLACK",cex=2.0)
   }
}

#Efectos aleatorios espaciales

shap<-shapiro.test(residale)
kolm<-lillie.test(residale)
jarq<-jb.norm.test(residale)
alemat1<-matrix(0,nrow=3,ncol=1,byrow=FALSE,dimnames=list(c("SHAPIRO","KOL-SMIR","JARQUE"),c("EFECTOS ALEATORIOS M. ESPACIAL")))
alemat1[1,1]<-shap$p.value
alemat1[2,1]<-kolm$p.value
alemat1[3,1]<-jarq$p.value

#Normalidad efectos aleatorios
alemat1

# Normalidad Lang-Ryan (Ver Rao & Molina, pág. 144, primera edición)

gamDOS <- sig2v/(Dvec+sig2v)
zi <- residale
zio = zi[order(zi)]
wi = sig2v*gamDOS
wio = wi[order(zi)]
Fa = cumsum(wio)/sum(wio)
Fa[regis] = mean(Fa[(regis-1):regis])
qFa = qnorm(Fa)

if(Selpeaocu==1){
plot(zio,qFa,pch=16,col="blue",xlim=c(-3.5,3.5),ylim=c(-3.5,3.5),
main="NORMALIDAD EFECTOS ALEATORIOS PEA, ESTIMACIÓN SEBLUP 2017_1",
xlab="Cuantiles Muestrales",
ylab="Cuantiles Teóricos")
} else {
if(Selpeaocu==2){
plot(zio,qFa,pch=16,col="blue",xlim=c(-3.5,3.5),ylim=c(-3.5,3.5),
main="NORMALIDAD EFECTOS ALEATORIOS OCU, ESTIMACIÓN SEBLUP 2017_1",
xlab="Cuantiles Muestrales",
ylab="Cuantiles Teóricos")
} else {
plot(zio,qFa,pch=16,col="blue",xlim=c(-3.5,3.5),ylim=c(-3.5,3.5),
main="NORMALIDAD EFECTOS ALEATORIOS INF, ESTIMACIÓN SEBLUP 2017_1",
xlab="Cuantiles Muestrales",
ylab="Cuantiles Teóricos")
   }
}   
abline(lm(qFa~zio),lwd=2,col="red")


##########################################################
#              PRUEBAS  MULTICOLINEALIDAD                #
##########################################################

pru3<-eblupFH2(formu, vardir= VadA, method="REML", data=datos)
Xpo <- as.matrix(cbind(rep(1,regis), datos[,VariaX]))
V <- 1/(pru3$variance + as.vector(VadA))
XV<-t(V*Xpo)
Q<-solve(XV%*%Xpo)
vif.eblupFH <- function (matv) {
        v <- matv[-1, -1, drop = FALSE]
        nam <- names(v[1,])   
        d <- diag(v)^0.5
    v <- diag(solve(v/(d %o% d)))
    names(v) <- nam
    v
}

# Factores de inflación de la varianza, VIF > 10 puede indicar la existencia colinealidad
vif.eblupFH(Q)

kappa.eblupFH <- function (Xpo,
                       scale = TRUE, center = FALSE,
                       add.intercept = TRUE,
                       exact = FALSE) {
    X <- Xpo[,c(2:dim(Xpo)[2])]
    nam <- names(X[1,])
    if (add.intercept) {
        X <- cbind(rep(1), scale(X, scale = scale, center = center))
        kappa(X, exact = exact)
    } else {
		 X <- X[,-1, drop = FALSE] 
        kappa(scale(X, scale = scale, center = scale), exact = exact)
    }
}

# Número de condición,  valor(kappa) > 30 puede indicar la existencia colinealidad
kappa.eblupFH(Xpo)


##########################################################
#            PRUEBAS DE NORMALIDAD RESIDUOS              # 
##########################################################

resiA <- (datos[,VariaY]-esti)/sqrt(VadA)

par(mfrow=c(1,2))
hist(resiA,br="SCOTT",col="lightblue", border="red",ylab="Frecuencias Relativas",
xlab="Residuos",main="DISTRIBUCIÓN RESIDUOS. MODELO SEBLUP 2017",freq=F)
plot(density(resiA, bw = "nrd0", kernel = c("gaussian"),weights = NULL, window = kernel),col="BLUE",
lty=1,lwd=3,main="DENSIDAD RESIDUOS. MODELO SEBLUP 2017")

shapia<-shapiro.test(resiA)
kolmoa<-lillie.test(resiA)
jarqba<-jb.norm.test(resiA)


##########################################################
#        ARMA MATRIZ DE PRUEBA NORMALIDAD RESIDUOS       #
##########################################################

options(scipen=0)
resimat2<-matrix(0,nrow=3,ncol=1,byrow=FALSE,dimnames=list(c("SHAPIRO","KOLMO-SMIRNOF","JARQUE"),c("ESTANDARIZADOS PEARSON")))
resimat2[1,1]<-shapia$p.value
resimat2[2,1]<-kolmoa$p.value
resimat2[3,1]<-jarqba$p.value

#Normalidad residuos del modelo espacial
resimat2


##########################################################
#                   QQPLOTS RESIDUOS                     #
##########################################################

if(Selpeaocu==1){
qqPlot(resiA, xlab="Cuantiles Téoricos", ylab="Cuantiles Muestrales",
main="Q-Q RESIDUOS ESTANDARIZADOS NIVEL SIG.=10%. PROPORCIÓN PEA, ESTIMACIÓN SEBLUP 2017_1", pch=20,envelope=.90,col="RED",col.lines="BLACK",cex=1.8)
} else {
if(Selpeaocu==2){
qqPlot(resiA, xlab="Cuantiles Téoricos", ylab="Cuantiles Muestrales",
main="Q-Q RESIDUOS ESTANDARIZADOS NIVEL SIG.=10%. PROPORCIÓN OCU., ESTIMACIÓN SEBLUP 2017_1", pch=20,envelope=.90,col="RED",col.lines="BLACK",cex=1.8)
} else {
qqPlot(resiA, xlab="Cuantiles Téoricos", ylab="Cuantiles Muestrales",
main="Q-Q RESIDUOS ESTANDARIZADOS NIVEL SIG.=10%. PROPORCIÓN INF., ESTIMACIÓN SEBLUP: 2017_1", pch=20,envelope=.90,col="RED",col.lines="BLACK",cex=1.8)
   }
}

##########################################################
#                PRUEBAS DE HOMOCEDASTICIDAD             # 
##########################################################

pagan<-bptest(resiA ~ esti)
harri<-hmctest(resiA ~ esti)
goldf<-gqtest(resiA ~ esti)


##########################################################
#     ARMA MATRIZ DE PRUEBA HOMOCEDASTICIDAD RESIDUOS    #
##########################################################

options(scipen=0)
resimat3<-matrix(0,nrow=3,ncol=1,byrow=FALSE,dimnames=list(c("PAGAN","HARRIS","GOLDF"),c("ESTANDARIZADOS PEARSON")))
resimat3[1,1]<-pagan$p.value
resimat3[2,1]<-harri$p.value
resimat3[3,1]<-goldf$p.value

#Homocedasticidad residuos del modelo espacial
resimat3

##########################################################
#    GRÁFICAS DE DISPERSIÓN ESTIMADOS VS. RESIDUOS       #
##########################################################

if(Selpeaocu==1){
plot(esti,resiA,main="ESTIMACIÓN PROPORCIÓN PEA, SEBLUP 2017_1 VS. RESIDUOS ESTANDARIZADOS",xlab="VALORES ESTIMADOS", ylab="RESIDUOS",
col="BLACK",pch=19,cex.main=1.2,cex=1.0,cex.lab=1.2)
} else {
if(Selpeaocu==2){
plot(esti,resiA,main="ESTIMACIÓN PROPORCIÓN OCUPADOS, SEBLUP 2017_1 VS. RESIDUOS ESTANDARIZADOS",xlab="VALORES ESTIMADOS", ylab="RESIDUOS",
col="BLACK",pch=19,cex.main=1.2,cex=1.0,cex.lab=1.2)
} else {
plot(esti,resiA,main="ESTIMACIÓN PROPORCIÓN OCUPADOS INF., SEBLUP 2017_1 VS RESIDUOS ESTANDARIZADOS",xlab="VALORES ESTIMADOS", ylab="RESIDUOS",
col="BLACK",pch=19,cex.main=1.2,cex=1.0,cex.lab=1.2)
   }
}


##########################################################
#              AUTOCORRELACIÓN RESIDUOS                  #
##########################################################

acf(resiA,lag.max=10,main="RESIDUOS ESTANDARIZADOS, SEBLUP 2017_1")


##########################################################
##########################################################
#      ESTIMACIONES PARA MUNICIPIOS SIN MUESTRA          # 
##########################################################
##########################################################


##########################################################
#     SELECCIONA REGISTROS DE MUNICIPIOS SIN MUESTRA     #
##########################################################

nombarchd <- "ENOE_ARCHIVO_MAESTRO_2017_T1.xlsx"        
BASETOTAL<-read.xlsx(nombarchd, sheet = 1, skipEmptyRows = FALSE)
registotal<-nrow(BASETOTAL)
BASETOTAL$LLAVE<-paste0(BASETOTAL$CVE_ENT,BASETOTAL$CVE_MUN)
REGRESIPEA  <- datos
REGRESIMUPEA2 <- REGRESIPEA[,c("LLAVE", "ENTIDAD")]

names(REGRESIMUPEA2) <- c("LLAVE", "ENTI2")
Todo_Unido <- merge(BASETOTAL, REGRESIMUPEA2, all=TRUE, by="LLAVE", sort = FALSE)
basetotal_Menos <- Todo_Unido[is.na(Todo_Unido[,"ENTI2"]),] 
Quita <- names(basetotal_Menos)=="ENTI2"
basetotal_Menos <- basetotal_Menos[,!Quita]
if(Selpeaocu==1){
  write.xlsx(basetotal_Menos, "REGRENOPEA.xlsx")
} else {
  if(Selpeaocu==2){
	write.xlsx(basetotal_Menos, "REGRENOOCU.xlsx")
} else {
	write.xlsx(basetotal_Menos, "REGRENOINF.xlsx")	
  }
}
getwd()

datos$TipoMode<-rep(1,regis)
juntos2 <- data.frame(LLAVE=datos$LLAVE,T15ymas=datos$T15ymas,Estisi=esti,ECM=mseaES$mse,tipomod=datos$TipoMode)
juntos2$LLAVE<-as.character(juntos2$LLAVE)
juntos2a<- merge(juntos2,BASETOTAL, all.x=TRUE, by=intersect("LLAVE","LLAVE"), sort = FALSE)
juntos2c<- data.frame(LLAVE=juntos2a$LLAVE,MUNILEYE=juntos2a$Municipio,T15ymas=juntos2a$T15ymas.x,Estisi=(juntos2a$Estisi)/100,RECM=(sqrt(juntos2a$ECM))/100,tipomod=juntos2a$tipomod)



#########################################################
#        ESTIMACIONES DE MUNICIPIOS NO MUESTRADOS       #
#########################################################


#########################################################
#      PARTE NO MUESTRADOS PARA MUNICIPIOS CV>20%       #
#########################################################


if(Selpeaocu==1){
  datosno<-read.xlsx("REGRENOPEA.xlsx",sheet = 1, skipEmptyRows = FALSE)
} else {
if(Selpeaocu==2){
  datosno<-read.xlsx("REGRENOOCU.xlsx",sheet = 1, skipEmptyRows = FALSE)
} else {
  datosno<-read.xlsx("REGRENOINF.xlsx",sheet = 1, skipEmptyRows = FALSE)  
  }
}

if(Selpeaocu==1 | Selpeaocu==2 ){
datosno$RelDepEco<-datosno$RelDepEco*100
datosno$Hom15a44<-datosno$Hom15a44*100
datosno$ImssIssste<-datosno$ImssIssste*100
} else {
datosno$ImssIssste<-datosno$ImssIssste*100
datosno$SeguPopu<-datosno$SeguPopu*100
datosno$Pob45ymas<-datosno$Pob45ymas*100
}

if(Selpeaocu==1){
   datosnouno<-datosno[(datosno$CVPEA==0.0) | (datosno$CVPEA>20.0),]
} else {
if(Selpeaocu==2){
   datosnouno<-datosno[(datosno$CVOCU==0.0) | (datosno$CVOCU>20.0),]
} else {   
   datosnouno<-datosno[(datosno$CVINF==0.0) | (datosno$CVINF>20.0),]   
  }
}


attach(datosnouno)
regisnouno <- nrow(datosnouno)
if(Selpeaocu==1 | Selpeaocu==2 ){
Xpo<-cbind(rep(1,regis),datos$RelDepEco,datos$Hom15a44,datos$ImssIssste)
Xpo1<-cbind(rep(1,regisnouno),datosnouno$RelDepEco,datosnouno$Hom15a44,datosnouno$ImssIssste)
} else {
Xpo<-cbind(rep(1,regis),datos$ImssIssste,datos$SeguPopu,datos$Pob45ymas)
Xpo1<-cbind(rep(1,regisnouno),datosnouno$ImssIssst,datosnouno$SeguPopu,datosnouno$Pob45ymas)
}
eblupfs1a<-Xpo1%*%eblupDEFES$fit$estcoef[,1]
V <- 1/(eblupDEFES$fit$refvar + as.vector(VadA))
XV<-t(V*Xpo)
Q<-solve(XV%*%Xpo)
varianza<-eblupDEFES$fit$refvar+diag(Xpo1%*%Q%*%t(Xpo1))
sesgo<-((regis*sum((eblupDEFES$fit$refvar + as.vector(VadA))^-2))-((sum((eblupDEFES$fit$refvar + as.vector(VadA))^-1))^2))*2/((sum((eblupDEFES$fit$refvar + as.vector(VadA))^-1))^3)
ecmnm<-varianza+sesgo
datosnouno$LLAVE<-as.character(datosnouno$LLAVE)
llavedos<-datosnouno$LLAVE
datosnouno$TipoMode<-rep(2,regisnouno)
juntos3<-data.frame(LLAVE=llavedos,MUNILEYE=datosnouno$Municipio,T15ymas=datosnouno$T15ymas,Estisi=eblupfs1a/100,RECM=(sqrt(ecmnm))/100,tipomod=datosnouno$TipoMode)
detach(datosnouno) 


#########################################################
#      PARTE NO MUESTRADOS PARA MUNICIPIOS CV<20%       #
#########################################################


if(Selpeaocu==1){
   datosnodos<-datosno[(datosno$CVPEA>0.0) & (datosno$CVPEA<=20.0),]
} else {
   if(Selpeaocu==2){
      datosnodos<-datosno[(datosno$CVOCU>0.0) & (datosno$CVOCU<=20.0),]
  } else {      
      datosnodos<-datosno[(datosno$CVINF>0.0) & (datosno$CVINF<=20.0),]  
  }
}


attach(datosnodos)
regisnodos <- nrow(datosnodos)
if(Selpeaocu==1){
   VadAdos<-datosnodos$PEA_EE^2
   VadAdos<-VadAdos*10000   
} else {
   if(Selpeaocu==2){
   VadAdos<-datosnodos$Ocupados_EE^2
   VadAdos<-VadAdos*10000   
} else {
   VadAdos<-datosnodos$OcuInfor_EE^2
   VadAdos<-VadAdos*10000   
   }
}   

if(Selpeaocu==1 | Selpeaocu==2 ){
Xpo12<-as.matrix(cbind(rep(1,regisnodos),datosnodos$RelDepEco,datosnodos$Hom15a44,datosnodos$ImssIssste))
} else {
Xpo12<-as.matrix(cbind(rep(1,regisnodos),datosnodos$ImssIssste,datosnodos$SeguPopu,datosnodos$Pob45ymas))
}
gamasdos<-eblupDEFES$fit$refvar/(eblupDEFES$fit$refvar+VadAdos)

if(Selpeaocu==1){
eblups2 = gamasdos*datosnodos$PEA*100 + as.vector((1-gamasdos))*Xpo12%*%as.vector(eblupDEFES$fit$estcoef[,1])
} else {
   if(Selpeaocu==2){
eblups2 = gamasdos*datosnodos$Ocupados*100 + as.vector((1-gamasdos))*Xpo12%*%as.vector(eblupDEFES$fit$estcoef[,1])
} else {
eblups2 = gamasdos*datosnodos$OcuInfor*100 + as.vector((1-gamasdos))*Xpo12%*%as.vector(eblupDEFES$fit$estcoef[,1])
   }
}   

#########################################################
#              VARIANZA ASINTOTICA EBLUP>SEBLUP         #
#########################################################
#[1]   0.4821726723989845 PEA 2017 (6-OCT-2021)
#[1]   0.5666509072850004 OCU 2017 (6-OCT-2021)
#[1]   1.2345801504428840 INF 2017 (6-OCT-2021)
#B = 100
#s2B = rep(0,B)
#for(i in 1:B){
#sel = sample(1:regis,size=regis,replace=TRUE)
#ouB = eblupFH(formula=formu, vardir= VadA,method="REML", data=datos[sel,])
#print(i)
#s2B[i] = ouB$fit$refvar }
#Vbar = var( s2B[!is.na(s2B)] )
if(Selpeaocu==1){
Vbar=0.4821726723989845
} else {
   if(Selpeaocu==2){
Vbar=0.5666509072850004
} else {
Vbar=1.2345801504428840
   }
}   

ecmnmg1<-gamasdos*VadAdos
ecmnmg2<-eblupDEFES$fit$refvar*(1-gamasdos)^2*diag( Xpo12%*%solve(t(Xpo12)%*%diag(gamasdos)%*%Xpo12,t(Xpo12)) )
ecmnmg3<-Vbar*(gamasdos/eblupDEFES$fit$refvar)*(1-gamasdos)^2
ecmnmdos<-ecmnmg1+ecmnmg2+2*ecmnmg3


llavedosdos<-datosnodos$LLAVE
datosnodos$TipoMode<-rep(3,regisnodos)
juntos3dos<-data.frame(LLAVE=llavedosdos,MUNILEYE=datosnodos$Municipio,T15ymas=datosnodos$T15ymas,Estisi=eblups2/100,RECM=(sqrt(ecmnmdos))/100,tipomod=datosnodos$TipoMode)

#########################################################
#       GRABA ARCHIVO FINAL DE ESTIMACIONES DE PEA      #
#########################################################


juntos4<-as.data.frame(rbind(juntos2c,juntos3,juntos3dos))
detach(datosnodos)
attach(juntos4)
juntos4$LLAVE<-as.character(juntos4$LLAVE)
juntos5<-juntos4[order(juntos4$LLAVE), ]

if (registotal==regis+regisnouno+regisnodos)
   {
   if(Selpeaocu==1){
      write.xlsx(juntos5, "ESTIPEA_2017.xlsx")
   } else {
   if(Selpeaocu==2){   
      write.xlsx(juntos5, "ESTIOCU_2017.xlsx")
   } else {
      write.xlsx(juntos5, "ESTIINF_2017.xlsx")         
   }
  }   
} else {
      warning("ALERTA NO SE GRABARON ARCHIVOS DE PROCUCCIÓN")
      stop("CRUCIAL NO COINCIDEN EL NUMERO DE REGISTROS TOTAL")      
}
getwd()

#########################################################
#                 FINAL DEL PROGRAMA                    #
#########################################################


