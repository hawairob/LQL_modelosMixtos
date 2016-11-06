# Tutorial 1 - LME
library(dplyr)
library(tidyr)
library(ggplot2)


# Pitch ~ sex -------------------------------------------------------------
# Crear la base que vamos a utilizar
my.df <- data.frame(
  pitch = c(233,204,242,130,112,142),
  sex = c(rep("female",3),rep("male",3))
)
head(my.df)

# Crear el modelo
xmdl <- lm(pitch ~ sex, data = my.df)
summary(xmdl)

# El coeficiente de la media absorbe el coeficiente para 'female'
xmdl$coefficients[1]
my.df %>%
  filter(sex == "female") %>%
  .$pitch %>% 
  mean

# 'sexmale' resulta de la media de 'male' menos el intercepto
my.df %>%
  filter(sex == "male") %>%
  .$pitch %>% 
  mean
128 - 226.33

# Graficar los datos
my.df %>%
  mutate(sex = ifelse(sex == "female", 0 , 1)) %>%
  ggplot(aes(x = sex, y = pitch)) +
  geom_point() +
  geom_smooth(method = "lm")


# pitch ~ age -------------------------------------------------------------
my.df <- data.frame(
  age = c(14, 23, 35, 48, 52, 67),
  pitch = c(252, 244, 240, 233, 212, 204)
  )
xmdl <- lm(pitch ~ age, data = my.df)
summary(xmdl)

my.df %>%
  ggplot(aes(x = age, y = pitch)) +
  geom_point() +
  geom_smooth(method = "lm")

# Centrar la vairable 'age'
my.df$age.c <- my.df$age - mean(my.df$age)
xmdl <- lm(pitch ~ age.c, data = my.df)
summary(xmdl)
# Cambia el intercepto, centrado en la media.


# Verificacion de supuestos -----------------------------------------------
# 1) Linealidad: grafica de residuales vs fit
data.frame(
  res = xmdl$residuals,
  obs = xmdl$fitted.values
  ) %>%
  ggplot(aes(x = obs, y = res)) +
  geom_point() +
  geom_hline(yintercept = 0)

# 2) Colinealilad: no menciona nada ¬_¬
# 3) Heteroscedasticidad: ver la de residuales
# 4) Normalidad de los residuales
data.frame(res = xmdl$residuals) %>%
  ggplot(aes(res)) +
  geom_histogram(bins = 6)
qqnorm(xmdl$residuals)

# 5) Absence of influential data points
dfbeta(xmdl) #Si quito ese punto, que pasa?
#muestra valores muy cercanos a 0

# 6) Independencia, pero no dice nada
