# Tutorial 2

#install.packages("lme4")
library(lme4)
source("lib/helpers.R")

# politeness <- read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")
# write.csv(politeness, "data/politeness_data.csv", row.names = FALSE)

# Leer datos
politeness <- read.csv("data/politeness_data.csv")
head(politeness)


# Exploratorio ------------------------------------------------------------
# Hay un dato faltante en frequency
apply(politeness, 2, function(x) sum(is.na(x)))

# Por individuo vemos diferencias entre hombres (mas bajos) y mujeres (altos)
politeness %>%
  ggplot(aes(factor(subject), frequency, fill = gender)) +
  geom_boxplot()

# Diferencia en frequency por attitude
politeness %>%
  ggplot(aes(factor(attitude), frequency)) +
  geom_boxplot()

# Attitude por male/female
politeness %>%
  ggplot(aes(factor(attitude), frequency, fill = gender)) +
  geom_boxplot()

# Scenario
politeness %>%
  ggplot(aes(factor(scenario), frequency)) +
  geom_boxplot()

# Scenario male / female  
politeness %>%
  ggplot(aes(factor(gender), frequency, fill = factor(scenario))) +
  geom_boxplot()


# Crear el modelo ---------------------------------------------------------
# Este modelo no considera el efecto del genero
politeness.model <- lmer(frequency ~ 
                           attitude +
                           (1|subject) + 
                           (1|scenario), 
                         data=politeness)
summary(politeness.model)

# Agregamos ese efecto
politeness.model <- lmer(frequency ~ 
                           attitude +
                           gender +
                           (1|subject) + 
                           (1|scenario), 
                         data=politeness)
summary(politeness.model)

# Comparar modelos
# Crear un modelo nulo, hay que agregar el REML
politeness.null <- lmer(frequency ~ 
                          gender +
                          (1|subject) + 
                          (1|scenario), 
                        data = politeness,
                        REML = FALSE)

# Modelo que teniamos
politeness.model <- lmer(frequency ~ 
                           attitude +
                           gender +
                           (1|subject) + 
                           (1|scenario), 
                         data = politeness,
                         REML = FALSE)
# se evalua comparando
anova(politeness.null, politeness.model)

# Ver los coeficientes
coef(politeness.model)

# Modelo que incluye los efectos de attitude por persona
politeness.model <- lmer(frequency ~ 
                           attitude +
                           gender + 
                           (1+attitude|subject) +
                           (1+attitude|scenario),
                         data = politeness,
                         REML = FALSE)
summary(politeness.model)
coef(politeness.model)

# Comparar
politeness.null <- lmer(frequency ~ 
                          gender +
                          (1+attitude|subject) +
                          (1+attitude|scenario),
                        data = politeness,
                        REML = FALSE)

anova(politeness.null, politeness.model)
