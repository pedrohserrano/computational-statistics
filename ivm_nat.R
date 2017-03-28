####COX MODEL###

op<-read.csv("ivm.csv",header=T)
op

#campos 
id<-op$id
bim<-op$bim
u_op<-op$u_op
pob_0a9<-op$pob_0a9
fam_ivm<-op$fam_ivm
fam_rec_ivm<-op$fam_rec_ivm
ivm<-op$ivm
rec_ivm<-op$rec_ivm
monto_ivm<-op$monto_ivm
int_total<-op$int_total 
neonatos<-op$neonatos
nuevos_ivm<-op$nuevos_ivm
natalidad<-op$natalidad
nat_mex<-op$nat_mex
crec_op<-op$crec_op
crec_mex<-op$crec_mex

reg.nat_id<-lm(natalidad~id)
par(mfrow=c(2,2))
plot(reg.nat_nat_mex,main="Regresion simple")
s.nat_id<-summary(reg.nat_id)
s.nat_id
anova(reg.nat_id)
#probamos regresion natalidad mexicana vs natalidad oportunidades
reg.nat_nat_mex<-lm(natalidad~nat_mex)
par(mfrow=c(2,2))
plot(reg.nat_nat_mex,main="Regresion Natalidad")
anova(reg.nat_nat_mex)
s.nat_nat_mex<-summary(reg.nat_nat_mex)
s.nat_nat_mex
#el modelo no se ajusta, por lo que la natalidad dentro de oportunidades no depende de la natalidad nacional

#probamos regresion crec natalidad mexicana vs crecnatalidad oportunidades
reg.crec_op_crec_mex<-lm(crec_op~crec_mex)
par(mfrow=c(2,2))
plot(reg.crec_op_crec_mex,main="Regresion Natalidad",col=4)
anova(reg.crec_op_crec_mex)
s.crec_op_crec_mex<-summary(reg.crec_op_crec_mex)
s.crec_op_crec_mex

#del QQ plot vemos que es una distribución de colas ligeras, se observa la normalidad de los errores
#vemos normalidad con los residuales y el test shapiro
res.crec_op_crec_mex<-reg.crec_op_crec_mex$resid
res.crec_op_crec_mex
par(mfrow=c(1,1))
plot(res.crec_op_crec_mex)
boxplot(res.crec_op_crec_mex, main="Box Graphic for Residuals")
(res.crec_op_crec_mex,col=2)
shapiro.test(res.crec_op_crec_mex)
#grafico de errores
error.bars(transpose(crec_op),res.crec_op_crec_mex,alpha=.01)

#cumple normalidad los residuales
#no así con la grafica de residuos contra los valores ajustados de y ya que no pareciera que la varianza no es constante

#con la prueba durbin watson obserbvamos el supuesto de los residuis no correlacionados
durbin.watson(reg.nuevos_ivm_natalidad)
#las variables son no correlacionadas

#vemos la prueba bonferroni para los outliers
outlier.test(reg.nuevos_ivm_natalidad,cutoff=Inf,n.mx=Inf)
shapiro.test(reg.crec_op_crec_mex)
#el modelo no se ajusta, por lo que la natalidad dentro de oportunidades no depende de la natalidad naciona

#visreg paravisualizar la regresion
visreg(reg.crec_op_crec_mex, main="Regresión Tasa Op vs Tasa Mex")
#checar error.bars#######

#graficamos el componente infanti vivir mejor
plot(ivm,main="Histórico IVM")
lines(ivm,col="blue")


#regresion completa con dependiente nuevos ivm 
reg<-lm(nuevos_ivm~fam_ivm+fam_rec_ivm+int_total+ivm+nat_mex+natalidad+neonatos+pob_0a9+rec_ivm)
anova(reg)
par(mfrow=c(2,2))
plot(reg,main="Regresion")
s.reg<-summary(reg)
s.reg
#observamos que ninguna variable es significativa, pero las que mayor información aportan son natalidad y e ivm (anova)

#probamos si los nuevos integrantes de oportunidades dependen de la natalidad dentro del programa y del los activos en ivm
reg.nuevos_ivm_natalidad_ivm<-lm(nuevos_ivm~natalidad+ivm)
anova(reg.nuevos_ivm_natalidad_ivm)
par(mfrow=c(2,2))
plot(reg.nuevos_ivm_natalidad_ivm,main="Regresion Relación nuevos ivm con ivm y natalidad")
s.nuevos_ivm_natalidad_ivm<-summary(reg.nuevos_ivm_natalidad_ivm)
s.nuevos_ivm_natalidad_ivm
#no son significativas, no hay evidencia suficiente para rechzar la hipótesis, se presume que no hay variable que explique el aumento de ingresos a ivm

#se prueba solo con la variable natalidad
reg.nuevos_ivm_natalidad<-lm(nuevos_ivm~natalidad)
anova(reg.nuevos_ivm_natalidad)
par(mfrow=c(2,2))
plot(reg.nuevos_ivm_natalidad,main="Regresion Relación ivm con neonatos")
s.nuevos_ivm_natalidad<-summary(reg.nuevos_ivm_natalidad)
s.nuevos_ivm_natalidad

#del QQ plot vemos que es una distribución de colas ligeras, se observa la normalidad de los errores
#vemos normalidad con los residuales y el test shapiro
res.nuevos_ivm_natalidad<-reg.nuevos_ivm_natalidad$resid
res.nuevos_ivm_natalidad

shapiro.test(res.nuevos_ivm_natalidad)
#cumple normalidad los residuales
#no así con la grafica de residuos contra los valores ajustados de y ya que no pareciera que la varianza no es constante

#con la prueba durbin watson obserbvamos el supuesto de los residuis no correlacionados
durbin.watson(reg.nuevos_ivm_natalidad)
#las variables son no correlacionadas

#vemos la prueba bonferroni para los outliers
outlier.test(reg.nuevos_ivm_natalidad,cutoff=Inf,n.mx=Inf)