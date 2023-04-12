library(data.table)
base <-
  fread(
    input = paste0("dados_salarios_modif.csv"),
    header = T,
    na.strings = "NA",
    data.table = FALSE,
    dec = ","
  )

mean(base$salario_USD)
var(base$salario_USD)
sd(base$salario_USD)
cv = (sd(base$salario_USD, na.rm = TRUE) * 100) / mean(base$salario_USD, na.rm =
                                                         TRUE)
cv

library(pastecs)
result1 <- stat.desc(base$salario_USD)
format(result1, scientific = FALSE)

library(psych)
result2 <-
  describeBy(salario_USD ~ experiencia, data = base, mat = T)
result2.1 <- result2[, c(2, 4, 5, 6)]
result2.1$cv <- result2.1$sd / result2.1$mean * 100
# incluir o cálculo do coeficiente de variação na tabela result2

library(doBy)
result3 <- summaryBy(
  salario_USD ~ experiencia,
  data = base,
  FUN = function(x) {
    c(
      n = length(x),
      m = mean(x),
      s = sd(x),
      cv = sd(x) / mean(x) * 100
    )
  }
)
# incluir a ano na tabela do result3

library(ggplot2)
ggplot(result3) +
  geom_bar(
    aes(x = experiencia, y = salario_USD.m),
    stat = "identity",
    fill = "skyblue",
    alpha = 0.7
  ) +
  geom_errorbar(
    aes(
      x = experiencia,
      ymin = salario_USD.m - salario_USD.s,
      ymax = salario_USD.m + salario_USD.s
    ),
    width = 0.4,
    colour = "red",
    alpha = 0.7,
    size = 1
  )
# salve o gráfico no formato PNG
