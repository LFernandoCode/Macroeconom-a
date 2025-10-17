#############################################################
# Autor: Luis Fernando Flores
# Fecha: 17/10/2025
# Descripción: Análisis de los periodos recesivos en Bolivia
#              utilizando la nueva metodología del PIB,
#              series desestacionalizadas con X-13ARIMA-SEATS
#              y detección de ciclos con Harding-Pagan (Bry-Boschan)
#############################################################
#############################################################
#                      Librerías                             #
#############################################################
library(BCDating)
library(dplyr)
library(readxl)
library(seasonal)
library(zoo)
library(quantmod)
library(writexl)
#############################################################
#                      Datos                                #
#############################################################
data<-read_xlsx("E:/Análisis/Ciclos del PIB Bolivia/Base.xlsx",sheet=1)

#############################################################
#                      Procesamiento                                #
#############################################################

ts_pib <- ts(data$PIB_2017, start=c(1990,1), frequency=4)


#######################                                     #
#filtrado                                                   #
ts_pib <- window(ts_pib, start = c(1991, 1), end = c(2025, 2))  #
                                                            #
#######################                                     #
descomp <- seas(ts_pib)
pib_des <- final(descomp) 

#############################################################
#                      Modelo                                #
#############################################################
results<-BBQ(pib_des,mincycle=5,minphase=3,name="dating")
s<-results@states  
states<-1-(s+1)/2
states2<-ts(states,start=c(1991,1),frequency=4)

#############################################################
#                      Gráficos                                #
#############################################################

par(mfrow=c(2,1))
plot(results,pib_des)
plot(x=data$Fecha,y= data$varA_PIB_1990,type='l',lwd=1,ylab="")

#############################################


# Convertimos a data.frame con Año y Trimestre
fechas <- as.yearqtr(time(pib_des))
anio <- as.integer(format(fechas, "%Y"))
trimestre <- as.integer(format(fechas, "%q"))

df <- data.frame(
      Fecha = fechas,
      Anio = anio,
      Trimestre = trimestre,
      PIB = as.numeric(pib_des)
)

# Suma acumulada dentro del año
df <- df %>%
      arrange(Anio, Trimestre) %>%
      group_by(Anio) %>%
      mutate(
            Suma_Acumulada = cumsum(PIB)
      ) %>%
      ungroup()

# Variación interanual (comparando mismo trimestre año anterior)
df <- df %>%
      arrange(Fecha) %>%
      mutate(
            Var_Interanual = (Suma_Acumulada / lag(Suma_Acumulada, 4) - 1) * 100
      ) %>% 
      mutate(
            Var_anterior = (PIB / lag(PIB) - 1) * 100
      )

df$States <- as.numeric(states2)
df$pib_des <- as.numeric(pib_des)

################################################################################
#                              Gráficos conjuntos
################################################################################

par(mfrow = c(2,1), mar = c(3, 4, 2, 4), oma = c(0, 0, 0, 0))

# ------------------------------
# GRAFICO 1: Var_Interanual + States
# ------------------------------

barplot(df$States,
        col = "gray85",
        border = NA,
        space = 0,
        ylim = c(0, 1),
        ylab = "",
        axes = FALSE)

par(new = TRUE)
plot(df$Fecha, df$Var_Interanual, type = "l", lwd = 2, col = "blue",
     ylab = "", xlab = "",
     axes = FALSE)  
axis(2)  # eje Y
axis(1, at = seq(1992, 2025, by = 4))  
box()


legend("topleft",
       legend = c("Variación interanual", "Periodo de recesión"),
       col = c("blue", "gray85"),
       lwd = c(2, NA),
       pch = c(NA, 15),
       pt.cex = 2,
       bty = "n")

# ------------------------------
# GRAFICO 2: PIB desestacionalizado + States
# ------------------------------

barplot(df$States,
        col = "gray85",
        border = NA,
        space = 0,
        ylim = c(0, 1),
        ylab = "",
        axes = FALSE)

par(new = TRUE)
plot(df$Fecha, df$pib_des, type = "l", lwd = 2, col = "red",
     ylab = "", xlab = "",
     axes = FALSE)
axis(2)  # eje Y
axis(1, at = seq(1992, 2025, by = 4))  
box()


legend("topleft",
       legend = c("PIB desestacionalizado", "Periodo de recesión"),
       col = c("red", "gray85"),
       lwd = c(2, NA),
       pch = c(NA, 15),
       pt.cex = 2,
       bty = "n")