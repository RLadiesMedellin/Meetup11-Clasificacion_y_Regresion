# -------------------------------------------------------------------------------------------------------
# Taller: Modelos de clasificación y regresión con R - RLadies Medellín 
# -------------------------------------------------------------------------------------------------------

# Modelos de regresión ----------------------------------------------------------------------------------
# Se harán algunos ejemplos con regresión lineal múltiple y polinomial
# -------------------------------------------------------------------------------------------------------

# Cargar librerías --------------------------------------------------------------------------------------

library(dplyr) 
library(ggplot2) 
library(rio)  # libreria para importar/exportar datos
library(MASS) # libreria para selección de variables
library(olsrr) # libreria que permite realizar un análisis de outliers o valores atípicos

# Cargar el conjunto de datos----------------------------------------------------------------------------
obesity1 <- import("./data/Obesity_t.csv")
colnames(obesity1)

# ggplot(data = obesity, aes(x = Edad, y = IMC)) +
#   geom_point() +
#   ggtitle("Edad vs. IMC") +
#   ylab("IMC")  +
#   theme_bw() 

# Despreciamos los datos que representan una "trampa" en la regresión
colnames(obesity)
obesity <- obesity1 %>%  dplyr::select(!c(Altura, Peso, Obesidad))

# Particionamos los datos en conjunto de datos de entrenamiento y conjunto de datos de prueba
n <- nrow(obesity)
semilla <- 15062022 # Semilla
set.seed(semilla)
porc <- 0.8 # Porcentaje de datos de entrenamiento
train <- sample(n,(n*porc))


obesity_train <- obesity[train,] 
obesity_test <- obesity[-train,]

# Regresión lineal Múltiple -------------------------------------------------------------------------------

# Ajuste de un modelo con algunas covariables
fit <- lm(IMC ~ Genero + Edad + Hist_familiar, data = obesity_train)
resumen <- summary(fit)

names(fit)
names(resumen)

# Ajuste de un modelo completo 
full.fit <- lm(IMC ~., data = obesity_train) # Ajuste del modelo completo
summary(full.fit) # Resumen del modelo

full.fit_summary <- summary(full.fit)

# ¿Cómo saber qué objetos tiene el modelo?
names(full.fit)
names(full.fit_summary)

# También se podría ajustar el mismo modelo usando la formula completa, de la siguiente manera
# full.fit <- lm(IMC ~ Genero + Edad + Hist_familiar + Altas_calorias + Vegetales + Num_comidas +
#                  Otras_comidas + Fuma + Agua + Calorias + Ejercicio + Tecnologia +
#                  Alcohol + Transporte, data = obesity_train)

# Podemos utilizar un método de selección de variables con la función stepAIC del paquete MASS
red.fit <- stepAIC(fit, trace=TRUE, direction="backward")
red.fit_summary <- summary(red.fit)

# ¿Cómo saber qué objetos tiene el modelo?
names(red.fit)
names(red.fit_summary)

red.fit_summary

# Evaluar residuales del modelo
par(mfrow = c(2,2))
plot(red.fit)

#  Otras formas de evaluar los supuestos sobre los residuales del modelo es usando algunas pruebas estadísticas

library(nortest)
shapiro.test(red.fit$residuals) # Test de Shapiro Wilk para normalidad
ad.test(red.fit$residuals) # Test de Anderson-Darling para normalidad

# Algunas formas de evaluar outliers 

# influence.measures(red.fit)
ols_plot_resid_lev(red.fit)

# ¿Cómo generar predicciones con el modelo? Usamos la función predict()
# Aviso: es necesario revisar residuales y realizar diagnósticos previamente

obesity_test$Predicciones <- predict(red.fit, obesity_test) # predict() recibe el modelo y un dataframe

# Regresión polinomial ---------------------------------------------------------------------------------

# Es posible incluir términos polinomiales con la función I() dentro del ajuste de un modelo de regresión

# I() permite definir los términos polinomiales específicos que se desean incluir

fit.edad <- lm(IMC ~ Edad + I(Edad^2) + I(Edad^3), data = obesity_train)
summary(fit.edad)

# ¿Cómo saber que término incluir? Puede ser analizado con medidas de evaluación de modelos

# Por ejemplo, usando el valor del MSE 


MSE<-vector()
for (i in 1:10){
  Modelo1 <- lm(IMC ~ poly(Edad, i), data = obesity_train)
  Pred1 <- predict(Modelo1, obesity_test)
  MSE[i]<-mean((Pred1 - obesity_test$IMC)^2)
}

graphics.off()
plot(1:10, MSE, xlab = "Grado", ylab = "MSE", type = "b", col = "#88398A",
     pch = 20, main = "Grado del polinomio que minimiza el MSE")


red.fit2 <- lm(IMC ~  Edad + I(Edad^2) + I(Edad^3) + Hist_familiar + Altas_calorias + Vegetales + 
            Num_comidas + Otras_comidas + Agua + Calorias + Ejercicio + 
            Tecnologia + Alcohol + Transporte, data = obesity_train)
summary(red.fit2)

# Otras pruebas ------------------------------------------------------------------------------------------------------
# Ajuste de un modelo completo con transformación
# full.fit2 <- lm(sqrt(IMC) ~., data = obesity_train) # Ajuste del modelo completo
# summary(full.fit2) # Resumen del modelo
# 
# red.fit2 <- stepAIC(full.fit2, trace=TRUE, direction="backward")
# red.fit2$anova
# 
# summary(red.fit2)
# 
# par(mfrow = c(2,2))
# plot(red.fit2)
# 
# load lmtest library
# library(lmtest)
# 
# perform Breusch-Pagan Test
# bptest(red.fit)
# 
# library(car)
# ncvTest(red.fit2)


