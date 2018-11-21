library("RColorBrewer")
library("readxl")
library(ggplot2)

cor <- function(numero_dados) {
  d=brewer.pal(n = numero_dados, name = "RdBu")
  return(d)
}

df <- read_excel("dados/umses_graduacao_2018_vtidy.xlsx")

distracao <- sum(df$distracao == '1')
usoindev <- sum(df$usoindev == '1')
prejintera <- sum(df$prejintera == '1')
bulling <- sum(df$bulling == '1')
continadeq <- sum(df$continadeq == '1')

labels <- c(
  "Distração em sala de aula",
  "Uso indevido",
  "Problemas de interação entre professores e alunos",
  "Cyberbullying",
  "Conteúdo inadequado"
)

dados <- c(
  distracao, usoindev, prejintera, bulling, continadeq
)

legenda = paste(labels, " - ", dados)
quantidade = length(labels)

pdf("graficos/dados_dificuldades.pdf")

barplot(dados, col=cor(quantidade), main="Dificuldades de uso", ylim = c(0, 60), legend = legenda)

dev.off()
