library(data.table)
base <-
  fread(
    input = paste0("dados_salarios_modif.csv"),
    header = T,
    na.strings = "NA",
    data.table = FALSE,
    dec = ","
  )

summary(base$salario_USD)
by(base$salario_USD, base$experiencia, quantile, probs = 0.30)
quantile(base$salario_USD)
quantile(base$salario_USD, probs = 0.30)

base1 <- base[base$ano == 2020, ]
quantile(base1$salario_USD)
base2 <- base[base$ano == 2021, ]
quantile(base2$salario_USD)
base3 <- base[base$ano == 2022, ]
quantile(base3$salario_USD)


library(psych)
resultados <- describeBy(salario_USD ~ ano, data = base, mat = T)
resultados$group1 <- as.numeric(resultados$group1)

library(ggplot2)
ggplot(data = resultados, aes(x = group1, y = mean)) +
  geom_line(color = "blue", lwd = 0.8) +
  geom_point(color = "black", lwd = 2)

ggplot(data = resultados, aes(x = group1)) +
  geom_line(aes(y = mean), color = "blue") +
  geom_line(aes(y = median), color = "red") +
  ylab("Salário USS") +
  xlab("Ano") +
  ggtitle("Evolução Salarial")

resultados2 <-
  describeBy(salario_USD ~ ano + experiencia, data = base, mat = T)
resultados2$group1 <- as.numeric(resultados$group1)

ggplot(data = resultados2, aes(x = group1, y = median, fill = group2)) +
  geom_line() +
  geom_line(aes(color = group2)) +
  scale_color_manual(values = c("black", "blue", "red", "green"))
