# Tutorial 2

#install.packages("lme4")
library(lme4)
source("lib/helpers.R")

# politeness <- read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")
# write.csv(politeness, "data/politeness_data.csv", row.names = FALSE)

politeness <- read.csv("data/politeness_data.csv")
head(politeness)

# Hay un dato faltante en frequency
apply(politeness, 2, function(x) sum(is.na(x)))

# Ver diferencias en el pitch por actitud y genero
politeness %>%
  filter(!is.na(frequency)) %>%
  mutate(attitude.gender = paste(attitude, gender, sep = "_")) %>%
  ggplot(aes(factor(attitude.gender), frequency, fill = gender)) +
  geom_boxplot()

# Crear el modelo
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
