# Tutorial 1 de Fabian

source("lib/helpers.R")

# Crear los datos
pitch <- c(233,204,242,130,112,142)
sex <- c(rep("female",3),rep("male",3))
my.df <- data.frame(sex,pitch)

# Crear modelo lineal
xmdl <- lm(pitch ~ sex, my.df)
summary(xmdl)

# Otra forma de interpretar el intercepto
mean(my.df[my.df$sex=="female",]$pitch)

# Segundo set de datos
age <- c(14,23,35,48,52,67)
pitch <- c(252,244,240,233,212,204)
my.df <- data.frame(age,pitch)
xmdl <- lm(pitch ~ age, my.df)
summary(xmdl)

# Modelo centrado en la media
my.df$age.c <- my.df$age - mean(my.df$age)
xmdl <- lm(pitch ~ age.c, my.df)
summary(xmdl)

# Revisar supuestos