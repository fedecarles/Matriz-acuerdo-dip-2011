# File-Name:       legismat.R
# Date:            14.12.11
# Author:          Federico Carlés
# Email:           fedecarles@gmail.com                                      
# Data:            129.csv, Honorable Cámara de Diputados de la Nación
# Packages Used:   base

## Los datos son las votaciones nominales del período 129, año 2011. Los datos
## están incompletos, así que usar con cautela. 

# setwd() Para poder leer el archivo hay que setear el working directory.

M129<-read.csv("VN/129.csv", header=F, sep=";")
M129<-M129[order(M129$V2),] #Ordena los datos alfabeticamente por bloque.

p129<-as.matrix(M129[,-(1:3)]) #vector con solo las votaciones.

# las siguientes lineas concatenan los apellidos de los legisladores
# y a que bloque pertenecen.
pos<-gregexpr("\\,", M129$V1) # Encuetra la posición del caracter "," en el vector de nombres.
pos<-as.numeric(pos)
pos<-pos-1 
nom<-substr(M129$V1,1,pos) # Extrate solo los apellidos.
bloque<-M129$V2 # Extrae el vector con los bloques.
nombres<-paste(nom," ","(",bloque,")", sep="") #Concatena el apellido y el bloque.  


# Función de afiliación. De Simon Jackman. Calcula la proporción en que dos 
# elementos de la matriz coinciden entre si.
agreement <- function(x){
n <- dim(x)[1]
k <- dim(x)[2]
a <- matrix(NA,n,n)
for(i in 1:n){
for(j in 1:n){
a[i,j] <- sum(x[i,]==x[j,],na.rm=TRUE)/sum(!is.na(x[i,]) & !is.na(x[j,]))
}
}
a[is.nan(a)] <- 0
a
}

M<-agreement(p129)
bkp<-M
rownames(M)<-nombres
colnames(M)<-nombres

M[upper.tri(M)] <- NA # Elimina la parte superior de la matriz simétrica.


# Grafica la matriz y la guarda en un archivo pdf.
pdf("legismat.pdf",width=10, height=10, paper="special")
par(mai=c(0.3,0.3,0.3,0.3), xaxt="n", yaxt="n", bty="n")
image(1:259,1:259,M, col=gray((32:0)/32), xlab="", ylab="")
abline(h = 1.5:dim(M), v = 1.5:dim(M), col = "white", lty = 1, lwd = 0.02)
mtext(rownames(M), side=4, at=1:259, las=1, cex=0.06, line=0.05)
mtext(colnames(M), side=1, at=1:259, las=2, cex=0.06, line=0.05)
dev.off()

