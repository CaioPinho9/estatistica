x = 2 * 3
y = 3 + 4
z = 3 ^ 4
y * z
x1 = c(1, 3, 4, 6, 7, 8)
x2 = c("1", "3", "4", "6", "7", "8")
mean(x1)
mean(x2)

# Estrutura dos Dados
idade = c(25, 32, 27, 33, 42, 21, 35, 45, 33, 25)
sexo = c("Masc",
         "Fem",
         "Fem",
         "Fem",
         "Masc",
         "Fem",
         "Masc",
         "Masc",
         "Fem",
         "Fem")
df = data.frame(idade, sexo)
mean(df$idade)

# CARREGAR O PACOTE
library(data.table)
# LEITURA DA BASE
dados =
  fread(
    input = paste0("dados_salarios.csv"),
    header = T,
    na.strings = "NA",
    data.table = FALSE,
    dec = ","
  )

# TAMANHO DA AMOSTRA
library(samplingbook)
#sample.size.prop(e=0.01, P = 0.5, N = Inf, level = 0.95)
sample.size.mean(e = 10000,
                 S = 71000,
                 N = 607,
                 level = 0.95)

# AMOSTRA SIMPLES AO ACASO
asa147 = dados[sample(nrow(dados), size = 147), ]
mean(asa147$salario_USD)

# CRIAR SUBCONJUNTO DE DADOS
dados1 = dados[dados$emprego == "FT",]
