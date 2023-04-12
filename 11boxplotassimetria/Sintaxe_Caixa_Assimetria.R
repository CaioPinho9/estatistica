library(data.table)

base <- fread(
        input = "dados_salarios_modif.csv",
        header = TRUE,
        na.strings = "NA",
        data.table = FALSE,
        dec = ","
)

####################
# GRÁFICO DE CAIXA #
####################

boxplot(base$salario_USD)

# Criar a variável "salario_USD_1000"
base$salario_USD_1000 <- base$salario_USD / 1000

boxplot(
        salario_USD_1000 ~ experiencia,
        data = base,
        main = "Comparação do Salário com o Tempo de Experiência",
        xlab = "Experiência",
        ylab = "Salário em USD (x1000)",
        col = c("pink", "lightblue", "yellow", "green")
)

# GRÁFICO DE CAIXA COM A BASE TEMPERATURA
dados <- fread(
        input = "temp_min.csv",
        header = TRUE,
        na.strings = "NA",
        data.table = FALSE,
        dec = ","
)

boxplot(
        Temp ~ Estado,
        data = dados,
        main = "Temperatura Mínima Mensal de 1991 a 2020",
        xlab = "Capitais dos Estados",
        ylab = "°C",
        col = "lightblue"
)


##############################################
# COEFICIENTE DE ASSIMETRIA EM BASE SIMULADA #
##############################################

library(moments)

# SIMULAÇÃO 1
dados1 <- rnorm(1000, mean = 65, sd = 6)
x_ <- mean(dados1)
md <- median(dados1)
q1 <- quantile(dados1, 0.25)
q3 <- quantile(dados1, 0.75)
hist(dados1, prob = TRUE)
lines(density(dados1), lwd = 2)
abline(v = md, col = "red", lwd = 2)
abline(v = x_, col = "blue", lwd = 2)
skewness(dados1)

# SIMULAÇÃO 2
dados2 <- rchisq(1000, 2)
x_ <- mean(dados2)
md <- median(dados2)
q1 <- quantile(dados2, 0.25)
q3 <- quantile(dados2, 0.75)
hist(dados2, prob = TRUE)
lines(density(dados2), lwd = 2)
abline(v = md, col = "red", lwd = 2)
abline(v = x_, col = "blue", lwd = 2)
skewness(dados2)


#################################################################
# COEFICIENTE DE ASSIMETRIA COM A BASE SALÁRIOS EM DATA SCIENCE #
#################################################################
x_ <- mean(base$salario_USD_1000)
md <- median(base$salario_USD_1000)
q1 <- quantile(base$salario_USD_1000, 0.25)
q3 <- quantile(base$salario_USD_1000, 0.75)
hist(base$salario_USD_1000, prob = TRUE)
lines(density(base$salario_USD_1000), lwd = 2)
abline(v = md, col = "red", lwd = 2)
abline(v = x_, col = "blue", lwd = 2)
skewness(base$salario_USD_1000)
