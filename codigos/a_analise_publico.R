#instalação pacotes
install.packages("readxl")
library("readxl")

#usando xls
data <- read_excel("dados/umses_graduacao_2018_vtidy.xlsx")

#atribuindo os valores da coluna
coluna <- data$'genero'

# Dicionário de dados
  # Não informado = 1
  # Masculino = 2
  # Feminino = 3

nao_informado <- sum(coluna == '1')
masculino <- sum(coluna == '2')
feminino <- sum(coluna == '3')

total <- masculino + feminino + nao_informado