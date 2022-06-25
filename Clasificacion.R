

# Regresion logistic ordinal
require(foreign)
require(ggplot2)
require(MASS) # Usada
require(Hmisc)
require(reshape2)


#Dividing data into training and test set
#Random sampling 
samplesize <- round(0.80*nrow(obesity))
set.seed(15062022)
index <- sample(seq_len(nrow(obesity)), size = samplesize)
#Creating training and test set -c(3,4,18)
datatrain <- obesity[index,]
datatest <- obesity[-index,]

# Division Stephany
n <- nrow(obesity)
semilla <- 15062022 # Semilla
set.seed(semilla)

porc <- 0.8 # Porcentaje de datos de entrenamiento
train <- sample(n,(n*porc))
obesity_train <- obesity[train,] 
obesity_test <- obesity[-train,]

# Modelo 1
m1 <- polr(Obesidad ~ Genero + Edad + Hist_familiar
           + Altas_calorias + Vegetales + Num_comidas + 
             Otras_comidas + Fuma +  Agua + Calorias +
             Ejercicio + Tecnologia + Alcohol +
             Transporte, data = datatrain, Hess = TRUE)

summary(m1)

ctable <- coef(summary(m1))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable

# Modelo 2 - Modelo reducido
m2 <- polr(Obesidad ~ Edad  + Hist_familiar +
             Altas_calorias + Vegetales + Num_comidas + 
             Otras_comidas + Agua + Calorias +
             Ejercicio + Tecnologia + Alcohol + Transporte,
             data = datatrain, Hess = TRUE)

summary(m2)

ctable <- coef(summary(m2))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable

predm2 <- predict(m2,datatest)
tablam2 <- table(datatest$Obesidad, predm2)
sum(diag(tablam2)) / sum(tablam2)

# Modelo  -
library(rpart)
library(rpart.plot)
m3 <- rpart(Obesidad ~ Edad  + Hist_familiar +
              Altas_calorias + Vegetales + Num_comidas + 
              Otras_comidas + Agua + Calorias +
              Ejercicio + Tecnologia + Alcohol + Transporte,
            data = datatrain, method = "class")
rpart.plot(m3, type=0, extra=0,  
           box.palette = "auto")
           
rpart.rules(m3)

predm3 <- predict(m3, newdata= datatest, type='class')
tablam3 <- table(Verdadero=datatest$Obesidad, Clasificacion=predm3)
tablam3
sum(diag(tablam3)) / sum(tablam3)

