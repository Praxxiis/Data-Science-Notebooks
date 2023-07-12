exams
summary(exams)
#creacion de tablas de frecuencias
## pruebdas de normalidad
install.packages("normtest")
library(nortest)
#aderson darling
ad.test(exams$math.score)
ad.test(exams$reading.score)
ad.test(exams$writing.score)
## por lo tanto los datos no son normales
###### viendo histogramas
hist(exams$math.score)
hist(exams$reading.score)
hist(exams$writing.score)
table(exams)
#estadisticos
install.packages("moments")
library(moments)
skewness(exams$math.score)
kurtosis(exams$math.score)
skewness(exams$reading.score)
kurtosis(exams$reading.score)
skewness(exams$writing.score)
kurtosis(exams$writing.score)
### debido a su kurtosis los datos no siguen una distribucion normal (por el momento)
#grafico de barras de genero
install.packages("lessR")
library(lessR)
barplot(table(exams$gender),col = c("lightblue","pink"),main = "Diagrama de barras de las frecuencias absolutas\n de la variable \"Sexo\"")
par(mfrow = c(2, 1))
barplot(table(exams$race.ethnicity), col = c("white", "lightblue", "mistyrose","lemonchiffon","#FFDAB9"))
barplot(table(exams$parental.level.of.education), col = c("#EEB4B4", "lightblue", "mistyrose","lemonchiffon","#FFDAB9","#528B8B"))
par(mfrow = c(2, 1))
barplot(table(exams$lunch), col = c("lightblue", "mistyrose"))
barplot(table(exams$test.preparation.course), col = c("mistyrose","lemonchiffon"))
############# boxplots ###########
par(mfrow = c(1, 3))
boxplot(exams$math.score)
boxplot(exams$reading.score)
boxplot(exams$writing.score)
#Por lo tanto se ven muchos outliers en todas las asignaturas

############# Trabajando con los datos #################

tabla1 <- table(exams$math.score,exams$gender)
tabla2 <- table(exams$math.score,exams$race.ethnicity)
tabla3 <- table(exams$math.score,exams$parental.level.of.education)
tabla4 <- table(exams$math.score,exams$lunch)
tabla5 <- table(exams$math.score,exams$test.preparation.course)

chisq.test(tabla1) #existe relacion
chisq.test(tabla2) #existe relacion
chisq.test(tabla3) #existe relacion
chisq.test(tabla4) #existe relacion
chisq.test(tabla5) #no existe relacion

install.packages("MASS")
library("MASS")

notas <- exams[,6:7:8]

cor(notas) #las correlaciones arrojan una fuerte correlacion entre todas las asignaturas

library("ppcor")
pcor(notas) #pero la correlacion parcial nos dice que en realidad matematica no tiene correlacion
#con ninguna asignatura

############ buscando modelos

#modelo forward

fit_f <- lm(math.score ~ 1, data = exams)
forw <- stepAIC(fit_f, scope=list(upper=~ gender + race.ethnicity + parental.level.of.education + lunch + test.preparation.course + reading.score + writing.score, lower=~1), direction = "forward")

ajusteFAIC <- lm(math.score ~ reading.score + gender + writing.score + lunch + test.preparation.course + race.ethnicity, data = exams)
forw$anova 
summary(ajusteFAIC)

#r**2 = 87.54%

#modelo forward bic

n <- dim(exams)[1]
forwBIC <- stepAIC(fit_f,scope=list(upper=~ gender + race.ethnicity + parental.level.of.education + lunch + test.preparation.course + reading.score + writing.score, lower=~1),k=log(n),direction = "forward")
forwBIC$anova

#lo mismo que forward AIC

#forward fisher
add1(fit_f, scope =~ gender + race.ethnicity + parental.level.of.education + lunch + test.preparation.course + reading.score + writing.score, test = "F")
add1(update(fit_f,~ .+race.ethnicity), scope=~gender + race.ethnicity + parental.level.of.education + lunch + test.preparation.course + reading.score + writing.score, test = "F")
add1(update(fit_f,~ .+race.ethnicity+lunch), scope=~gender + race.ethnicity + parental.level.of.education + lunch + test.preparation.course + reading.score + writing.score, test = "F")
add1(update(fit_f,~ .+race.ethnicity+lunch+reading.score), scope=~gender + race.ethnicity + parental.level.of.education + lunch + test.preparation.course + reading.score + writing.score, test = "F")
add1(update(fit_f,~ .+race.ethnicity+lunch+reading.score+gender), scope=~gender + race.ethnicity + parental.level.of.education + lunch + test.preparation.course + reading.score + writing.score, test = "F")
add1(update(fit_f,~ .+race.ethnicity+lunch+reading.score+gender+writing.score), scope=~gender + race.ethnicity + parental.level.of.education + lunch + test.preparation.course + reading.score + writing.score, test = "F")
add1(update(fit_f,~ .+race.ethnicity+lunch+reading.score+gender+writing.score+test.preparation.course), scope=~gender + race.ethnicity + parental.level.of.education + lunch + test.preparation.course + reading.score + writing.score, test = "F")

#exactamente lo mismo que todos los demas forward

#modelos backward

fit_b <- lm(math.score ~ gender + race.ethnicity + parental.level.of.education + lunch + test.preparation.course + reading.score + writing.score ,data = exams)
backw <- stepAIC(fit_b, direction = "backward")
backw$anova

summary(backw)

#exactamente lo mismo que en los forward, por lo tanto ya tenemos un modelo.
install.packages("carData")
library(car)

influencePlot(backw)

atipicos <- c(83,100,774,986)

exams.s.o <- exams[-atipicos]


fit_b_s.o <- lm(math.score ~ gender + race.ethnicity + parental.level.of.education + lunch + test.preparation.course + reading.score + writing.score ,data = exams.s.o)
backw_s.o <- stepAIC(fit_b_s.o, direction = "backward")
backw_s.o$anova
summary(fit_b_s.o)

#verificando normalidad

library(e1071)
kurtosis(fit_b_s.o$residuals)
skewness(fit_b_s.o$residuals)

library(nortest)
ad.test(fit_b_s.o$residuals)

#por lo tanto los datos son normales

plot(fit_b_s.o$fitted.values,fit_b_s.o$residuals)

#por lo tanto es homocedastico

#En resumen el modelo queda comprendido por todas las variables menos el nivel de educacion de los padres.
# con un R2 del 87,5 %

#falta hacer el train y test con los datos.




