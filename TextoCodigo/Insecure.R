##############################################################
# Codigo desarrollado para analizar una seccion de la ENVIPE
# Por Villalobos, Trujano & Villarreal
# 26/01/18


################# Analisis General #########################
# 
# # Limpia directorio de Trabajo
# rm(list=ls())
# 
# # Base de datos con directorio
# ENVIPE <- read.csv("~/Documents/Psicometria/IRT/envipe_2016_csv/tper_vic1_envipe2016/conjunto_de_datos/tper_vic1_envipe2016.csv")
# 
# # Selecciona la seccion de interes
# insecure <- subset(ENVIPE,select=AP4_10_01:AP4_11_10) 
# 
# # Especificaciones generales de la seccion de la escala
# n_items <-  length(insecure[1,])  # numero de items
# n_obs <- length(insecure[,1])     # numero de observaciones
# 
# # Ciclo que reemplaza los numeros 3 y 9 con NAs
# for(i in 1:n_items){
#  insecure[,i][insecure[,i]%in%c(3,9)] <- NA 
# }
# 
# 
# # # Quita los que presentan mayor numero de NAs 
# # insecure <- insecure[,-c(13,7,8)]
# # 
# # # Hace el conteo de cuantos NAs hay por persona (n_obs)
# # count <- c()
# # for(i in 1:n_obs){
# #  count[i] <- sum(is.na(insecure[i,]))
# # }
# 
# # Selecciona todos los sujetos que tuvieron cero NAs
# insecure <- insecure[which(count==0),]

rm(list=ls())
# Ele
# insecure <- read.csv("~/Documents/Psicometria/IRT/envipe_2016_csv/clean_insecure.csv")
# Manu
insecure <- read.csv("~/Documents/IRT/envipe_2016_csv/clean_insecure.csv")

# Dario
setwd("~/IRT")
insecure <- read.csv("envipe_2016_csv/clean_insecure.csv")

# Convierte los si==1, no==0
insecure <- -1*(insecure-2)

# Redefinimos las informacion de nuestra base de datos
n_items <- length(insecure[1,])
n_obs <- length(insecure[,1])

setwd("~/Documents/Psicometria/ProyectoFinal/TextoCodigo")
pdf(file='Proporciones.pdf',width=6.2,height = 4)
palabras_preguntas <- c('1. salir noche','2. menor solo','3. visitar',
                        '4. tomar taxi', '5. transporte','6. efectivo',
                        '9. caminar','10. joyas','11. salir','12. tarjeta',
                        '14. centros','15. viajar', '1. reforzar', 
                        '2. cerraduras', '3. bardas', '4. alarmas', 
                        '5. vigilancia','6. vecinos','7. seguros',
                        '8. perro', '9. armas','10. mudarse')

par(mar=c(0,3,2,1),oma=c(1,1,1,1))
barplot(colMeans(insecure),las=2,horiz=T,axes=F,names.arg = palabras_preguntas,
        cex.names=0.5,xlim=c(0,1),col = 'turquoise3',border = 'turquoise3',
        col.axis='gray48')
axis(3,at=seq(0,1,0.1),tck=0.01,padj =1.5,cex.axis=0.7,col.axis='gray48',col='gray48')
dev.off()

#########Analisis IRT 1P##############
# Libreria necesaria
library("TAM")

# Corre el analisis de IRT de un solo parametro
irt_insecure <- tam(insecure)

# Parametro beta (dificultad del item)
InsecureItem <- irt_insecure$xsi$xsi

# Parametro theta (habilidad de la persona)
Insecure <- tam.wle(irt_insecure)
InsecurePersona <- Insecure$theta

# Histogramas
layout(matrix(c(1:2),ncol=1))
hist(InsecureItem,breaks=100)
hist(InsecurePersona,breaks=100)

#curvas teoricas dadas el minimo y maximo de habilidad para las thetas (items)
pdf(file="Curvas.pdf",width = 6.2, height=4)
layout(1)
par(mar=c(2,2,1,1),oma=c(1,1,1,1))
plot(0,type='n',xlim=c(-4,4),ylim=c(0,1),axes=F)
axis(1,cex.axis=0.7,tck=-0.03,padj=-1.5,col='gray48',col.axis='gray48')
mtext(expression(theta),1,line=1.8,col='gray48')
mtext(expression(f (theta)),2,las=2,line=1.5,col='gray48')
axis(2,las=2, cex.axis=0.7,tck=-0.03,hadj=0.5,col='gray48',col.axis='gray48')
for(i in 1:n_items){
 curve(plogis(x,InsecureItem[i]),from=-4,to=max(InsecurePersona),
       ylim=c(0,1),col='turquoise4',add=T)
 
}
dev.off()


id_InsecureItem <- cbind(InsecureItem,c(1:22))
ordered_id_InsecureItem <- id_InsecureItem[order(InsecureItem),]

# Para valores unicos de dificultad, cuantas thetas hay 
pdf(file="barras.pdf",width = 6.2, height=8)
layout(matrix(c(1:21),ncol=3,byrow=T))
par(mar=c(2,2,2,2),oma=c(1,1,1,1))
for(i in 1:length(unique(InsecurePersona))){
 theta_min <- subset(insecure,InsecurePersona==sort(unique(InsecurePersona))[i])
 theta_min_or <- theta_min[,c(ordered_id_InsecureItem[,2])]
 plot(0,type='n',xlim=c(0,22),ylim=c(0,1),axes=F)
 barplot(colMeans(theta_min_or),add=T,axes=F,las=2,names.arg=paste(ordered_id_InsecureItem[,2]),
         cex.names=0.8,tck=-0.03,col='turquoise4',border='turquoise4',
         col.axis='gray48',line=-0.5)
 text(paste('=',round(ordered_id_InsecureItem[i,1],3)),x = 14,y = 0.8,cex=0.8)
 text(expression(theta),x = 11,y = 0.8,cex=0.8)
 axis(2,at=seq(0,1,0.2),tck=0.01,cex.axis=0.6,col.axis='gray48',col='gray48',las=2,hadj = 0.5)
}
dev.off()

#thetas agrupadadas, proporcion que dijeron si de esa theta
n_theta <- length(unique(InsecurePersona))
theta_s <- sort(unique(InsecurePersona))
pdf(file='thetasagrupadas.pdf',width = 6.2,height = 4)
par(mar=c(3,3,1,1),oma=c(1,1,1,1))
plot(0,type="n",xlim=c(-3,6),ylim=c(0,1),axes=F)
axis(1,at=seq(-3,6),col='gray48',col.axis='gray48',cex.axis=0.8,tck=-0.02)
mtext(expression(theta),1,col='gray48',line=2)
axis(2,las=2,col='gray48',col.axis='gray48',cex.axis=0.8,tck=-0.02)
mtext('Proporcion de personas que afirmaron',2,col='gray48',line=2.5,las=2)
for(t in 1:n_items){
 prop <- by(insecure[,t],factor(InsecurePersona),mean)
 lines(theta_s,prop,col=rgb(1,0,0,alpha = .04*t),lwd=.1*t)
}
dev.off()

#########Analisis IRT 2P##############

tam(insecure,irtmodel='2PL')
