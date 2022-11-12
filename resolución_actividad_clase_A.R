# Resolución actividad empírica, clase 9
library(AER)

# Importamos la dataset
fertility <- read.csv("C:/Users/admin/Documents/FACULTAD/UCEMA/Herramientas_econometricas/Diapositivas/clase_9/fertility.csv")
View(fertility)

# Note que Y = weeksworked, X = morekids, W = black, Z1 = twoboys, Z2 = twogirls 

# (1)
# Estimamos la primera etapa
model_1 = lm(morekids ~ twoboys + twogirls + age, data=fertility)
coeftest(model_1,vcov. = vcovHC, type = "HC1")

# Obtenemos la predicción

morekids_hat = model_1$fitted.values

# (2)
# Estimamos la segunda etapa

model_2 = lm(weeksworked ~ morekids_hat + age, data=fertility)
coeftest(model_2,vcov. = vcovHC, type = "HC1")

# (3)
# Estimamos el modelo con ivreg()

model_ivreg <- ivreg(weeksworked ~ morekids + age | twoboys + twogirls + age, 
                     data = fertility)
coeftest(model_ivreg, vcov = vcovHC, type = "HC1")

# (4)
# Tenemos dos instrumentos (twoboys y twogirls) y un regresor endógeno (morekids), 
# por lo que existe sobreidentificación

