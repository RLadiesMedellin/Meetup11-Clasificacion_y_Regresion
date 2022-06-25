# Cargar librerías -----------------------------------------------------------------------------------------
library(rio)
library(Amelia)
library(dplyr)
library(ggplot2)
library(cowplot)

# Cargar base de datos -------------------------------------------------------------------------------------
obesity <- Obesity_t

# Algunas características del conjunto de datos -------------------------------

dim(obesity) # dimensiones

head(obesity) # visualización del conjutno de datos

missmap(obesity) # visualización de valores faltantes en el conjunto de datos

summary(obesity)

# Algunos gráficos de análisis descriptivo ------------------------------------

genero <- ggplot(data = obesity, aes(Genero, fill = Genero)) +
            geom_bar(position="dodge") +
            ggtitle("Género") +
            ylab("Cantidad") + 
            scale_fill_manual(values=c("#EFD1F3", "#88398A")) +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(legend.position='top')

historial <- ggplot(data = obesity, aes(Hist_familiar, fill = Hist_familiar)) +
              geom_bar(position="dodge") +
              ggtitle("Historial familiar de sobrepeso") +
              ylab("Cantidad") + 
              scale_fill_manual(values=c("#EFD1F3", "#88398A")) +
              theme_bw() +
              theme(plot.title = element_text(hjust = 0.5)) +
              theme(legend.position='top')

plot_grid(genero, historial)

altas_c <- ggplot(data = obesity, aes(Altas_calorias, fill = Altas_calorias)) +
            geom_bar(position="dodge") +
            ggtitle("Consumo de alimentos con altas calorías") +
            ylab("Cantidad") + 
            scale_fill_manual(values=c("#EFD1F3", "#88398A")) +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(legend.position='top')

fuma <- ggplot(data = obesity, aes(Fuma, fill = Fuma)) +
          geom_bar(position="dodge") +
          ggtitle("Consumo de alimentos con altas calorías") +
          ylab("Cantidad") + 
          scale_fill_manual(values=c("#EFD1F3", "#88398A")) +
          theme_bw() +
          theme(plot.title = element_text(hjust = 0.5)) +
          theme(legend.position='top')

plot_grid(altas_c, fuma)

vegetales <- ggplot(data = obesity, aes(Vegetales, fill = Vegetales)) +
              geom_bar(position="dodge") +
              ggtitle("Consumo de vegetales en comidas") +
              ylab("Cantidad") +
              theme_bw() +
              theme(plot.title = element_text(hjust = 0.5)) +
              theme(legend.position='top')

agua <- ggplot(data = obesity, aes(Agua, fill = Agua)) +
          geom_bar(position="dodge") +
          ggtitle("Consumo de agua") +
          ylab("Cantidad") +
          theme_bw() +
          theme(plot.title = element_text(hjust = 0.5)) +
          theme(legend.position='top')

plot_grid(vegetales, agua)

num_comidas <- ggplot(data = obesity, aes(Num_comidas, fill = Num_comidas)) +
                geom_bar(position="dodge") +
                ggtitle("Número de comidas") +
                ylab("Cantidad") +
                theme_bw() +
                theme(plot.title = element_text(hjust = 0.5)) +
                theme(legend.position='top')

otras_comidas <- ggplot(data = obesity, aes(Otras_comidas, fill = Otras_comidas)) +
                  geom_bar(position="dodge") +
                  ggtitle("Otros alimentos") +
                  ylab("Cantidad") +
                  theme_bw() +
                  theme(plot.title = element_text(hjust = 0.5)) +
                  theme(legend.position='top')

plot_grid(num_comidas, otras_comidas)

ejercicio <- ggplot(data = obesity, aes(Ejercicio, fill = Ejercicio)) +
              geom_bar(position="dodge") +
              ggtitle(" Frecuencia de Ejercicio") +
              ylab("Cantidad") +
              theme_bw() +
              theme(plot.title = element_text(hjust = 0.5)) +
              theme(legend.position='top')

tecnologia <- ggplot(data = obesity, aes(Tecnologia, fill = Tecnologia)) +
                geom_bar(position="dodge") +
                ggtitle(" Frecuencia de Ejercicio") +
                ylab("Cantidad") +
                theme_bw() +
                theme(plot.title = element_text(hjust = 0.5)) +
                theme(legend.position='top')

plot_grid(ejercicio, tecnologia)

alcohol <- ggplot(data = obesity, aes(Alcohol, fill = Alcohol)) +
            geom_bar(position="dodge") +
            ggtitle(" Frecuencia de Ejercicio") +
            ylab("Cantidad") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(legend.position='top')

transporte <- ggplot(data = obesity, aes(Transporte, fill = Transporte)) +
                geom_bar(position="dodge") +
                ggtitle(" Frecuencia de Ejercicio") +
                ylab("Cantidad") +
                theme_bw() +
                theme(plot.title = element_text(hjust = 0.5)) +
                theme(legend.position='top')

plot_grid(alcohol, transporte)

# Histogramas para vairables numéricas --------------------------------------
ggplot(data = obesity, aes(Edad, fill = Edad)) + 
  geom_histogram(col = "#88398A", fill = "#EFD1F3") +
  ggtitle("Histograma para la Edad") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(data = obesity, aes(Peso, fill = Peso)) + 
  geom_histogram(col = "#88398A", fill = "#EFD1F3") +
  ggtitle("Histograma para el Peso") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(data = obesity, aes(Altura, fill = Altura)) + 
  geom_histogram(col = "#88398A", fill = "#EFD1F3") +
  ggtitle("Histograma para la Altura") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(data = obesity, aes(IMC, fill = IMC)) + 
  geom_histogram(col = "#88398A", fill = "#EFD1F3") +
  ggtitle("Histograma para el IMC") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

# Otros graficos

# Genero vs IMC y Obesidad
ggplot(data = obesity, aes(x = Genero, y = IMC, fill = Genero))  +
  geom_boxplot(col = "#88398A", fill = "#EFD1F3") +
  xlab("Género")
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(data = obesity, aes(x = Genero, fill = Obesidad))  +
  geom_bar() +
  xlab("Género") +
  ylab("Cantidad") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

# Edad vs IMC y Obesidad
ggplot(data = obesity, aes(x = Edad, y = IMC, fill = Genero))  +
  geom_point(col = "#88398A", fill = "#EFD1F3") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(data = obesity, aes(x = Obesidad, y = Edad,  fill = Obesidad))  +
  geom_boxplot(col = "#88398A", fill = "#EFD1F3")+ 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

# Hist_familiar vs IMC y Obesidad
ggplot(data = obesity, aes(x = Hist_familiar, y = IMC, fill = Hist_familiar))  +
  geom_boxplot(col = "#88398A", fill = "#EFD1F3") + 
  xlab("Historia familiar") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(data = obesity, aes(x = Hist_familiar, fill = Obesidad))  +
  geom_bar() +
  xlab("Historia familiar") +
  ylab("Cantidad") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

# Altas_calorias vs IMC y Obesidad
ggplot(data = obesity, aes(x = Altas_calorias, y = IMC, fill = Altas_calorias))  +
  geom_boxplot(col = "#88398A", fill = "#EFD1F3") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(data = obesity, aes(x = Altas_calorias, fill = Obesidad))  +
  geom_bar() +
  ylab("Cantidad") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

# Vegetales vs IMC y Obesidad
ggplot(data = obesity, aes(x = Vegetales, y = IMC, fill = Vegetales))  +
  geom_boxplot(col = "#88398A", fill = "#EFD1F3") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(data = obesity, aes(x = Vegetales, fill = Obesidad))  +
  geom_bar() +
  ylab("Cantidad") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

# Num_comidas vs IMC y Obesidad
ggplot(data = obesity, aes(x = Num_comidas, y = IMC, fill = Num_comidas))  +
  geom_boxplot(col = "#88398A", fill = "#EFD1F3") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(data = obesity, aes(x = Num_comidas, fill = Obesidad))  +
  geom_bar() +
  ylab("Cantidad") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

# Otras_comidas vs IMC y Obesidad
ggplot(data = obesity, aes(x = Otras_comidas, y = IMC, fill = Otras_comidas))  +
  geom_boxplot(col = "#88398A", fill = "#EFD1F3") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(data = obesity, aes(x = Otras_comidas, fill = Obesidad))  +
  geom_bar() +
  ylab("Cantidad") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 


# Fuma vs IMC y Obesidad
ggplot(data = obesity, aes(x = Fuma, y = IMC, fill = Fuma))  +
  geom_boxplot(col = "#88398A", fill = "#EFD1F3") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(data = obesity, aes(x = Fuma, fill = Obesidad))  +
  geom_bar() +
  ylab("Cantidad") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

# Agua vs IMC y Obesidad
ggplot(data = obesity, aes(x = Agua, y = IMC, fill = Agua))  +
  geom_boxplot(col = "#88398A", fill = "#EFD1F3") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(data = obesity, aes(x = Agua, fill = Obesidad))  +
  geom_bar() +
  ylab("Cantidad") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

# Calorias vs IMC y Obesidad
ggplot(data = obesity, aes(x = Calorias, y = IMC, fill = Calorias))  +
  geom_boxplot(col = "#88398A", fill = "#EFD1F3") + 
  xlab("Calorías") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(data = obesity, aes(x = Calorias, fill = Obesidad))  +
  geom_bar() +
  xlab("Calorías") +
  ylab("Cantidad") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

# Ejercicio vs IMC y Obesidad
ggplot(data = obesity, aes(x = Ejercicio, y = IMC, fill = Ejercicio))  +
  geom_boxplot(col = "#88398A", fill = "#EFD1F3") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(data = obesity, aes(x = Ejercicio, fill = Obesidad))  +
  geom_bar() +
  ylab("Cantidad") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

# Tecnologia vs IMC y Obesidad
ggplot(data = obesity, aes(x = Tecnologia, y = IMC, fill = Tecnologia))  +
  geom_boxplot(col = "#88398A", fill = "#EFD1F3") + 
  xlab("Tecnología") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(data = obesity, aes(x = Tecnologia, fill = Obesidad))  +
  geom_bar() +
  xlab("Tecnología") +
  ylab("Cantidad") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

# Alcohol vs IMC y Obesidad
ggplot(data = obesity, aes(x = Alcohol, y = IMC, fill = Alcohol))  +
  geom_boxplot(col = "#88398A", fill = "#EFD1F3") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(data = obesity, aes(x = Alcohol, fill = Obesidad))  +
  geom_bar() +
  ylab("Cantidad") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

# Transporte vs IMC y Obesidad
ggplot(data = obesity, aes(x = Transporte, y = IMC, fill = Transporte))  +
  geom_boxplot(col = "#88398A", fill = "#EFD1F3") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(data = obesity, aes(x = Transporte, fill = Obesidad))  +
  geom_bar() +
  ylab("Cantidad") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 
