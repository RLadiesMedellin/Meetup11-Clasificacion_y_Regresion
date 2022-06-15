# -------------------------------------------------------------------------------------------------------
# Taller: Modelos de clasificación y regresión con R - RLadies Medellín 
# ------------------------------------------------------------------------------------------------------
# Preprocesamiento de datos 
# --------------------------------------------------------------------------------------------------

# Cargar librerías -----------------------------------------------------------------------------------------
library(dplyr)
library(rio)

# Cargar base de datos -------------------------------------------------------------------------------------
obesity <- import("./data/Obesity.csv")

# Preprocesamiento -----------------------------------------------------------------------------------------

#' Se tienen 17 columnas y 2111 registros en la base de datos, las columnas requieren preprocesamiento.

#' Hay columnas que deben ser convertidas a tipo factor.

#' Necesario cambiar nombres de columnas 

#' Algunas columnas son de tipo categórico según la descripción de la base de datos, pero al
#' momento de generar algunos datos simulados, quedaron algunos registros con valores continuos, por lo
#' que es necesario redondear y reemplazar estos valores para que haga sentido con las variables del 
#' cuestionario.

#' Es necesario crear la variable respuesta para regresión: Mass body index (IMC) = Weight/(Height^2) 
#' ya que esta no está presente en el conjunto de datos

# Cambio de nombres a las columnas
obesity <- obesity %>% rename(Genero = Gender,
                              Edad = Age,
                              Altura = Height,
                              Peso = Weight,
                              Hist_familiar = family_history_with_overweight,
                              Altas_calorias = FAVC,
                              Vegetales = FCVC,
                              Num_comidas = NCP,
                              Otras_comidas = CAEC,
                              Fuma = SMOKE,
                              Agua = CH2O,
                              Calorias = SCC,
                              Ejercicio = FAF,
                              Tecnologia = TUE,
                              Alcohol = CALC,
                              Transporte = MTRANS,
                              Obesidad = NObeyesdad)

obesity$Genero <- ifelse(obesity$Genero == "Female", "Femenino", "Masculino")
obesity$Genero <- factor(obesity$Genero, levels = c("Femenino", "Masculino"))

obesity$Hist_familiar <- ifelse(obesity$Hist_familiar == "yes", "Sí", "No")
obesity$Hist_familiar <- factor(obesity$Hist_familiar, levels = c("Sí", "No"))

obesity$Altas_calorias <- ifelse(obesity$Altas_calorias == "yes", "Sí", "No")
obesity$Altas_calorias <- factor(obesity$Altas_calorias, levels = c("Sí", "No"))

obesity$Vegetales <- round(obesity$Vegetales,0)
obesity <- obesity %>%  mutate(Vegetales = case_when(obesity$Vegetales == 1 ~ "Nunca",
                                                    obesity$Vegetales == 2 ~ "A veces",
                                                    obesity$Vegetales == 3 ~ "Siempre"))
obesity$Vegetales <- factor(obesity$Vegetales, levels = c("Nunca", "A veces", "Siempre"))

obesity$Num_comidas <- round(obesity$Num_comidas,0)
obesity$Num_comidas[obesity$Num_comidas == 4] <- 3
obesity <- obesity %>%  mutate(Num_comidas  = case_when(obesity$Num_comidas == 1 ~ "Entre 1-2",
                                                    obesity$Num_comidas == 2 ~ "3",
                                                    obesity$Num_comidas == 3 ~ "Más de 3"))
obesity$Num_comidas <- factor(obesity$Num_comidas, levels = c("Entre 1-2", "3", "Más de 3"))

obesity <- obesity %>%  
  mutate(Otras_comidas = case_when(obesity$Otras_comidas == "no" ~ "No",
                                   obesity$Otras_comidas == "Sometimes" ~ "Algunas veces",
                                   obesity$Otras_comidas == "Frequently" ~ "Frecuentemente",
                                   obesity$Otras_comidas == "Always" ~ "Siempre"))

obesity$Otras_comidas <- factor(obesity$Otras_comidas, levels = c("No", "Algunas veces", 
                                                                  "Frecuentemente", "Siempre"))

obesity$Fuma <- ifelse(obesity$Fuma == "yes", "Sí", "No")
obesity$Fuma  <- factor(obesity$Fuma, levels = c("Sí", "No"))

obesity$Agua <- round(obesity$Agua,0)
obesity <- obesity %>%  mutate(Agua = case_when(obesity$Agua == 1 ~ "Menos 1 L",
                                                    obesity$Agua == 2 ~ "1-2 L",
                                                    obesity$Agua == 3 ~ "Más 2 L"))
obesity$Agua <- factor(obesity$Agua, levels = c("Menos 1 L", "1-2 L", "Más 2 L"))

obesity$Calorias <- ifelse(obesity$Calorias == "yes", "Sí", "No")
obesity$Calorias <- factor(obesity$Calorias, levels = c("Sí", "No"))

obesity$Ejercicio <- round(obesity$Ejercicio,0)
obesity <- obesity %>%  
  mutate(Ejercicio = case_when(obesity$Ejercicio == 0 ~ "No",
                               obesity$Ejercicio == 1 ~ "1-2 Días",
                               obesity$Ejercicio == 2 ~ "2-4 Días",
                               obesity$Ejercicio == 3 ~ "4-5 Días"))
obesity$Ejercicio <- factor(obesity$Ejercicio, levels = c("No", "1-2 Días", "2-4 Días", "4-5 Días"))

obesity$Tecnologia <- round(obesity$Tecnologia,0)
obesity <- obesity %>%  
  mutate(Tecnologia = case_when(obesity$Tecnologia == 0 ~ "0-2 Horas",
                                obesity$Tecnologia == 1 ~ "3-5 Horas",
                                obesity$Tecnologia == 2 ~ "Más 5 Horas"))
obesity$Tecnologia <- factor(obesity$Tecnologia, levels = c("0-2 Horas", "3-5 Horas", "Más 5 Horas"))

obesity <- obesity %>%  
  mutate(Alcohol = case_when(obesity$Alcohol == "no" ~ "No",
                             obesity$Alcohol == "Sometimes" ~ "Algunas veces",
                             obesity$Alcohol == "Frequently" ~ "Frecuentemente",
                             obesity$Alcohol == "Always" ~ "Siempre"))

obesity$Alcohol <- factor(obesity$Alcohol, levels = c("No", "Algunas veces", 
                                                      "Frecuentemente", "Siempre"))

obesity <- obesity %>%  
  mutate(Transporte = case_when(obesity$Transporte == "Automobile" ~ "Carro",
                                obesity$Transporte == "Bike" ~ "Bicicleta",
                                obesity$Transporte == "Motorbike" ~ "Moto",
                                obesity$Transporte == "Walking" ~ "Caminata",
                                obesity$Transporte == "Public_Transportation" ~ "Transporte"))

obesity$Transporte <- factor(obesity$Transporte, levels = c("Carro", "Bicicleta", "Moto",
                                                            "Caminata", "Transporte"))

obesity <- obesity %>%  
  mutate(Obesidad = case_when(obesity$Obesidad == "Insufficient_Weight" ~ "Peso Insuficiente",
                              obesity$Obesidad == "Normal_Weight" ~ "Peso Normal",
                              obesity$Obesidad == "Obesity_Type_I" ~ "Obesidad Tipo I",
                              obesity$Obesidad == "Obesity_Type_II" ~ "Obesidad Tipo II",
                              obesity$Obesidad == "Obesity_Type_III" ~ "Obesidad Tipo III",
                              obesity$Obesidad == "Overweight_Level_I" ~ "Sobrepeso Nivel I",
                              obesity$Obesidad == "Overweight_Level_II" ~ "Sobrepeso Nivel II"))
obesity$Obesidad <- factor(obesity$Obesidad, 
                           levels = c("Peso Insuficiente", "Peso Normal", "Sobrepeso Nivel I", "Sobrepeso Nivel II",
                                      "Obesidad Tipo I", "Obesidad Tipo II","Obesidad Tipo III"))

obesity$IMC <- obesity$Peso / (obesity$Altura^2)

rio::export(obesity, "./data/Obesity_t.csv")
