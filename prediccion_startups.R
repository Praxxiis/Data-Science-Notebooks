datos <- Startups[,-4]
#Test de Shapiro-Wilk para saber si mis datos son normales
#si el p valor es mayor que el 5% entonces significa que no hay suficiente evidencia
#para rechazar la hipotesis de que sean normales (es decir si el p valor es mayor que
#el 5% entonces los datos son normales)
shapiro.test( x = datos$R.D.Spend )
shapiro.test( x = datos$Administration )
shapiro.test( x = datos$Marketing.Spend )
#por lo tanto todos los datos son normales
#visualizacion y exploracion de datos
par(mfrow=c(3, 2))
boxplot(datos$R.D.Spend, main = "Gastos en investigación y desarrollo") 
boxplot(datos$Administration, main = "Gastos en personal administrativo")
boxplot(datos$Marketing.Spend, main = "Gastos en publicidad")
boxplot(datos$Profit, main = "Ganancias")
#viendo correlaciones y correlaciones parciales
cor(datos, method = "pearson")
######## Identificación#######
library(ppcor) #correlacion parcial
pcor(datos)
#al parecer sera una pura variable (R.D.Spend)

#establecemos los ajustes
library(MASS)
fit <-lm(Profit ~ 1,data = Startups)
forw <- stepAIC(fit,scope=list(upper=~R.D.Spend+Administration +Marketing.Spend+State,lower=~1),direction = "forward")
#AIC sirve mas para la predicción
#verificando el modelo
ajusteFAIC <- lm(Profit ~ R.D.Spend + Marketing.Spend, data = Startups)
forw$anova 
summary(ajusteFAIC)
#Debo seguir haciendo el forward con el BIC puesto que el BIC es mejor para la prediccion y el AIC es mejor para el modelado
#primero definimos la dimensión
n <- dim(Startups)[1]
forwBIC <- stepAIC(fit,scope=list(upper=~R.D.Spend+Administration +Marketing.Spend+State,lower=~1),k=log(n),direction = "forward")
forwBIC$anova #nos da un modelo mas conciso

ajusteFBIC <- lm(Profit ~ R.D.Spend, data = Startups)
summary(ajusteFBIC)

#usando el forward con el test de Fisher
add1(fit, scope = ~R.D.Spend+Administration +Marketing.Spend+State, test = "F")
add1(update(fit,~ .+R.D.Spend ), scope=~R.D.Spend+Administration +Marketing.Spend+State, test = "F")
#Por lo tanto el test de Fisher junto al forward nos da el mismo resultado que el forward AIC

ajusteFF <- lm(Profit ~ R.D.Spend, data = Startups)
summary(ajusteFF)
##########################################################
#ahora con el backward y el AIC
fit1 <- lm(Profit ~ R.D.Spend+Administration +Marketing.Spend+State,data = Startups)
backw <- stepAIC(fit1, direction = "backward")
backw$anova

ajusteBAIC <- lm(Profit ~ R.D.Spend + Marketing.Spend, data= Startups)
summary(ajusteBAIC)

################# backward bic

n <- dim(Startups)[1]
backwBIC <- stepAIC(fit,scope=list(upper=~R.D.Spend+Administration +Marketing.Spend+State,lower=~1),k=log(n),direction = "forward")
backwBIC$anova #nos da un modelo mas conciso

ajusteBBIC <- lm(Profit ~ R.D.Spend, data = Startups)
summary(ajusteBBIC)

#############################################################
# Haciendo test de Fisher con backward
drop1(fit1, test = "F")
drop1(update(fit1,~ .-State), test = "F") #eliminando width del ajuste debido a que tiene el mayor pr
drop1(update(fit1,~ .-State-Administration), test = "F")
drop1(update(fit1,~ .-State-Administration-Marketing.Spend), test = "F")
#termino el algoritmo ya que todas las variables son significativas
#(p valor menor que el 5%)

ajusteBF <- lm(Profit ~ R.D.Spend, data = Startups)
summary(ajusteBF)

##############################################################
#haciendo el stepward
fitstep <- lm(Profit ~ R.D.Spend+Administration +Marketing.Spend+State,data = Startups)
stepw <- stepAIC(fitstep, direction = "both")
stepw$anova

ajustestep <- lm(Profit ~ R.D.Spend + Marketing.Spend, data = Startups)
summary(ajustestep)

##################################################
####### Me queda comparar todos los modelos (r**2) y cp de mallows
#Haciendo cp de mallows

modelo.completo <- lm(Profit ~. ,data = Startups)
modelo1 <- lm(Profit ~ R.D.Spend, data = Startups)
modelo2 <- lm(Profit ~ R.D.Spend + Marketing.Spend, data = Startups)

install.packages("olsrr")
library(olsrr)
ols_mallows_cp(modelo1, modelo.completo)
ols_mallows_cp(modelo2, modelo.completo)
#como el cp de mallows del modelo1 es mas cercano a k+1 (2), nos quedamos con el.

#ahora me queda la validacion del modelo, para esto tengo que analizar
#el problema de colinealidad y de outliers
library(car)
##################################outliers##############################
influencePlot(modelo1) 
##tengo que sacar los dos outliers
atipicos <- c(15, 50)
Startups.s.o <- Startups[-atipicos,]

fit.s.o <- lm(Profit ~ R.D.Spend, data = Startups.s.o)
summary(fit.s.o)
#el r2 al sacar los outliers subio de 94% a 96%
influencePlot(fit.s.o)

#outliers eliminados

#generando intervalos de confianza

confint(fit.s.o, "R.D.Spend", level = 0.95)

############################verificacion del supuesto de normalidad###################################
qqPlot(fit.s.o$residuals)

#### estadistica descriptiva para la normalidad ###

#verificacion de la normalidad
library(e1071)
kurtosis(fit.s.o$residuals) #me indica si tiene mucha cola
skewness(fit.s.o$residuals) #asimetría

library(nortest)
ad.test(fit.s.o$residuals) #si el p valor es mayor al 5% entonces si son normales
cvm.test(fit.s.o$residuals)
lillie.test(fit.s.o$residuals)
#todos los test indican normalidad


library(lmtest)
bptest(fit.s.o) #prueba para heterocedasticidad
# como es mayor al 5% la varianza de los residuos es homocedastica

plot(fit.s.o$fitted.values,fit.s.o$residuals)

########## Dividir variables test/train #############

n <- nrow(Startups.s.o) #número de observaciones
n2 <- floor(n*0.2)
x <- seq(1,n,1)
z <- sample(x,n2, replace = FALSE, prob = NULL) #Muestreo sin reemplazo
mz <-- z
Startups.training <- Startups.s.o[mz,] #muestra para construir el modelo
Startups.test <- Startups.s.o[z,]

#### evaluacion con la muestra test

#consturimos el modelo con los datos de entrenamiento

fit.training <- lm(Profit ~ R.D.Spend, data = Startups.training)
summary(fit.s.o)

# calculamos las predicciones con la muestra test

a.test <- predict(fit.training, newdata = data.frame(Startups.test), interval = "prediction")
#se calcula el mse: cuanto se equivoca el modelo.
mse <- mean((a.test[,1]-Startups.test[,2])^2)
mse
#comparamos graficamente el ajuste con los datos

plot(Startups.test[,2],a.test[,1]) #evaluacion visual del ajuste
abline(0,1)


################### Dos tipos de predicciones ###############################

### prediccion individual: 

#Se hace una prediccion sobre un individuo nuevo

Startup.nueva <- data.frame(137000.00, 127037.56, 330200.87, "Florida" ,"NA")
names(Startup.nueva) <- c("R.D.Spend", "Administration", "Marketing.Spend", "State", "Profit")

pred.fit.s.o <- predict(fit.s.o, newdata = data.frame(Startup.nueva), interval = "prediction")
pred.fit.s.o

# se obtiene la prediccion tomando en cuenta la incertidumbre de la estimacion y de las 
#caracteristicas propias de este individuo
#el resultado está en el intervalo entre lwr y upr

#en este caso como la reduccion de las variables implico quedarnos con solo una
#(R.D.Spend, la unica variable que importa para un cambio en las ganancias es la inversion en desarrollo)
#por lo tanto para las Startups en este caso no importa su locacion, gastos en administracion o gasto en
#publicidad





