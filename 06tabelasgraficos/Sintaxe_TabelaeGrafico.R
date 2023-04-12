# Leitura da base
library(data.table)
base <-
  fread(
    input = paste0("dados_salarios.csv"),
    header = T,
    na.strings = "NA",
    data.table = FALSE,
    dec = ","
  )

# Alterando a base de dados
base$trab_remoto <- as.character(base$trab_remoto)
library(dplyr)
base$trab_remoto <-
  recode(base$trab_remoto,
         `0` = "1:Não",
         `50` = "2:Parcial",
         `100` = "3:Total")
base$experiencia <-
  recode(
    base$experiencia,
    `EN` = "1:EN",
    `MI` = "2:MI",
    `SE` = "3:SE",
    `EX` = "4:EX"
  )

# UMA VARIAVEL QUALITATIVA: Tabela de frequencias, grafico de barras
# ou de setores circulares "experiencia"
freq.tabela <- table(base$experiencia, useNA = "ifany")
freq.tabela
porc.tabela <- round(prop.table(freq.tabela) * 100, 1)
porc.tabela
freq.tabela <- data.frame(freq.tabela, porc.tabela)
freq.tabela <- freq.tabela[, -3]
colnames(freq.tabela) <- c("Experiencia", "Frequencia", "Porcentagem")

png(file = "barras.png")
barplot(
  height = freq.tabela$Frequencia,
  names = freq.tabela$Experiencia,
  col = rgb(0.3, 0.6, 0.5, 0.5),
  xlab = "Experiência",
  ylab = "Frequência",
  main = ""
)
dev.off()

png(file = "setores.png")
pie(freq.tabela$Frequencia, freq.tabela$Experiencia,
    main = "Tempo de Experiência")
dev.off()


# DUAS VARIAVEIS QUALITATIVAS: Tabela cruzada ou grafico de barras "experiencia vs trab_remoto"
freq.tabela <-
  table(base$experiencia, base$trab_remoto, useNA = "ifany")
freq.tabela
porc.tabelaL <- round(prop.table(freq.tabela, 1) * 100, 1)
porc.tabelaL
tabela <- data.frame(table(base$experiencia, base$trab_remoto))
colnames(tabela) <- c("Experiencia", "Trab_Remoto", "Freq")

library(ggplot2)
ggplot(tabela, aes(fill = Trab_Remoto, y = Freq, x = Experiencia)) +
  geom_bar(position = "dodge", stat = "identity")

ggplot(tabela, aes(fill = Trab_Remoto, y = Freq, x = Experiencia)) +
  geom_bar(position = "fill", stat = "identity") +
  ylab("Porcentagem")


# UMA VARIÁVEL QUANTITATIVA: Histograma "salario_USD"
hist(base$salario_USD)
hist(base$salario_USD,
     breaks = 6)
hist(
  base$salario_USD,
  main = "Distribuição Salarial",
  xlab = "Salários",
  col = "darkmagenta",
  freq = FALSE
)


# UMA VARIÁVEL QUALITATIVA E UMA VARIÁVEL QUANTITATIVA:
# Tabela com médias "experiencia vs salario_USD"
tabela.medias <-
  aggregate(base$salario_USD,
            by = list(base$experiencia),
            FUN = "mean")
colnames(tabela.medias) <- c("Experiencia", "Sal_Medio")

library(ggplot2)
ggplot(tabela.medias, aes(x = Experiencia, y = Sal_Medio)) +
  geom_bar(stat = "identity")


# DUAS VARIAVEIS QUANTITATIVAS: Grafico de dispersao "base nova"
ggplot(data = iris, aes(Petal.Length, Petal.Width)) +
  geom_point()
png(file = "dispersao.png")
ggplot(data = iris, aes(Petal.Length, Petal.Width, color = Species)) +
  geom_point()
dev.off()


# DUAS VARIAVEIS QUANTITATIVAS, SENDO UMA DELAS O TEMPO: Grafico de linhas "ano vs salario_USD"
salario_ano <-
  aggregate(base$salario_USD, by = list(base$ano), FUN = "mean")
colnames(salario_ano) <- c("ano", "salario")
ggplot(data = salario_ano, aes(x = ano, y = salario)) +
  geom_line(color = "blue", lwd = 1.8) +
  geom_point(color = "red", lwd = 6)


# TRANSFORMACAO DE UMA VARIAVEL QUANTITATIVA PARA VARIAVEL QUALITATIVA "salario_USD"
library(psych)
base$Cat_Salario[base$salario_USD < 100000]  = "G1"
base$Cat_Salario[base$salario_USD >= 100000 &
                   base$salario_USD < 200000]  = "G2"
base$Cat_Salario[base$salario_USD >= 200000 &
                   base$salario_USD < 300000]  = "G3"
base$Cat_Salario[base$salario_USD >= 300000 &
                   base$salario_USD < 400000]  = "G4"
base$Cat_Salario[base$salario_USD >= 400000]  = "G5"
freq.tabela <- table(base$Cat_Salario, useNA = "ifany")
freq.tabela
