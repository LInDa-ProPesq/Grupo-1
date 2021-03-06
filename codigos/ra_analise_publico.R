#instalação pacotes
install.packages("readxl")
install.packages("RColorBrewer")
library("reshape2")
library("RColorBrewer")
library("readxl")

cor <- function(numero_dados) {
  d=brewer.pal(n = numero_dados, name = "RdBu")
  return(d)
}

#usando xls
data <- read_excel("dados/umses_graduacao_2018_vtidy.xlsx")

#### Análises de gênero

#atribuindo os valores da coluna genero
coluna_genero <- data$'genero'

# Dicionário de dados
  # Não informado = 1
  # Masculino = 2
  # Feminino = 3

nao_informado <- sum(coluna_genero == '1')
masculino <- sum(coluna_genero == '2')
feminino <- sum(coluna_genero == '3')
total <- masculino + feminino + nao_informado

#abre arquivo pdf para poder salvar o gráfico
pdf("graficos/dados_genero.pdf")

#Qtd. Pessoas por Gênero
dados <- c(nao_informado, masculino, feminino)
legenda <- c("Não Informado", "Masculino", "Feminino")
qtd <- length(legenda)
pct <- round(dados/sum(dados)*100)
legenda <- paste(legenda, pct)
legenda <- paste(legenda,"%",sep="")
pie(dados,labels = legenda, col=cor(qtd),
    main="Gênero Público")

#Gráfico de barras
legenda_barra <- c("Não Informado", "Masculino", "Feminino")
qtd <- length(legenda_barra)
legenda_barra <- paste(legenda_barra, pct)
barplot(dados, legend=legenda_barra, main="Gênero Público", 
        xlab="Gênero",col=cor(qtd),
        ylim = c(0, 1.5* max(slices, na.rm = T)))

#fecha pdf
dev.off()

#### Análises de idade

# Dicionário de dados idade
  #1. Entre 16 e 20 anos
  #2. Entre 21 e 25 anos
  #3. Entre 26 e 30 anos
  #4. Entre 30 e 35 anos
  #5. Entre 36 e 40 anos
  #6. Acima de 40 anos

coluna_idade <- data$'idade'

i16_20 <- sum(coluna_idade == '1')
i21_25 <- sum(coluna_idade == '2')
i26_30 <- sum(coluna_idade == '3')
i30_35 <- sum(coluna_idade == '4')
i36_40 <- sum(coluna_idade == '5')
i41 <- sum(coluna_idade == '6')

dados_idade <- c(i16_20, i21_25, i26_30, i30_35, i36_40, i41)

#Gráfico de barras
g1 <- "Entre 16 e 20 anos:"
g2 <- "Entre 21 e 25 anos:"
g3 <- "Entre 26 e 30 anos:"
g4 <- "Entre 30 e 35 anos:"
g5 <- "Entre 36 e 40 anos:"
g6 <- "Acima de 40 anos:"
legenda_idade <- c(g1, g2, g3, g4, g5, g6)
qtd <- length(legenda_idade)
legenda_idade <- paste(legenda_idade, dados_idade, "pessoas")

#abre arquivo pdf para poder salvar o gráfico
pdf("graficos/dados_idade.pdf")

barplot(dados_idade, legend=legenda_idade, main="Idade Público", 
        xlab="Idade",col=cor(qtd),
        ylim = c(0, 1.5* max(slices, na.rm = T)))

#fecha pdf
dev.off()

#### Análises característica dos respondentes

# Dicionário de profissões
  # 1. Professor
  # 2. Aluno
  # 3. Professor e aluno
  # 4. Prefiro não declarar"

coluna_prof <- data$'profal'

prof <- sum(coluna_prof == '1')
aluno <- sum(coluna_prof == '2')
prof_aluno <- sum(coluna_prof == '3')
na <- sum(coluna_prof == '4')

dados_profi <- c(prof, aluno, prof_aluno, na)

#Todos são alunos

#### Análises de trabalho

# Dicionário de dados
#1 - Desempregado
#2 - Jornada parcial
#3 - Jornada integral
#4 - Estagiário
#5 - Trabalha por conta própria
#6 - Afastado temporariamente
#7 - Aposentado
#8 - Bolsista Capes

coluna_trab <- data$'trabalha'

des <- sum(coluna_trab == '1')
jorn_p <- sum(coluna_trab == '2')
jorn_i <- sum(coluna_trab == '3')
estag <- sum(coluna_trab == '4')
conta_pro <- sum(coluna_trab == '5')
temp_afas <- sum(coluna_trab == '6')
aposent <- sum(coluna_trab == '7')
bolsi <- sum(coluna_trab == '8')

dados_trab <- c(des, jorn_p, jorn_i, estag, bolsi)

#Gráfico de barras
g1 <- "Desempregado:"
g2 <- "Jornada parcial:"
g3 <- "Jornada integral:"
g4 <- "Estagiário:"
g8 <- "Bolsista Capes:"

legenda_trab <- c(g1, g2, g3, g4, g8)
qtd <- length(legenda_trab)

#abre arquivo pdf para poder salvar o gráfico
pdf("graficos/dados_trabalho.pdf")

pct <- round(dados_trab/sum(dados_trab)*100)
legenda_trab <- paste(legenda_trab, pct)
legenda_trab <- paste(legenda_trab,"%",sep="")
pie(dados_trab,labels = legenda_trab, col=cor(qtd),
    main="Situação empregatícia")

#fecha pdf
dev.off()

# Dicionário de dados
# 1 - Solteiro (a)
# 2 - Casado (a)
# 3 - União Estável
# 4 - Viúvo (a)
# 5 - Separado (a)
# 6 - Prefiro não declarar

coluna_estadocivil <- data$'estadocivil'

solteiro <- sum(coluna_estadocivil == '1')
casado <- sum(coluna_estadocivil == '2')
uniao <- sum(coluna_estadocivil == '3')
viuvo <- sum(coluna_estadocivil == '4')
separado <- sum(coluna_estadocivil == '5')
ND <- sum(coluna_estadocivil == '6')

dados_estadocivil <- c(solteiro, casado, uniao, viuvo, separado, ND)

#Gráfico de barras
g1 <- "Solteiro (a):"
g2 <- "Casado (a):"
g3 <- "União Estável:"
g4 <- "Viúvo (a):"
g5 <- "Separado (a):"
g6 <- "Prefiro não declarar:"

legenda_estadocivil <- c(g1, g2, g3, g4, g5, g6)
qtd <- length(legenda_estadocivil)
legenda_estadocivil <- paste(legenda_estadocivil, dados_estadocivil)

#abre arquivo pdf para poder salvar o gráfico
pdf("graficos/dados_estadocivil.pdf")

barplot(dados_estadocivil, legend=legenda_estadocivil, main="Estado Civil", 
        xlab="Estado",col=cor(qtd),
        ylim = c(0, 1.1* max(dados_estadocivil, na.rm = T)))

#fecha pdf
dev.off()

# Dicionário de dados
# 1 - Sem filhos
# 2 - Um filho
# 3 - Dois filhos
# 4 - Três filhos
# 5 - Mais de três filhos

coluna_filhos <- data$'filhos'

na <- sum(coluna_estadocivil == '1')
um <- sum(coluna_estadocivil == '2')
dois <- sum(coluna_estadocivil == '3')
tres <- sum(coluna_estadocivil == '4')
tresmais <- sum(coluna_estadocivil == '5')

dados_filho <- c(na, um, dois, tres, tresmais)

#Gráfico de barras
g1 <- "Sem filhos:"
g2 <- "Um filho:"
g3 <- "Dois filhos:"
g4 <- "Três filhos:"
g5 <- "Mais de três filhos:"

legenda_filhos <- c(g1, g2, g3, g4, g5)

require(reshape2)
df <- data.frame(legenda_filhos, dados_filho)
print(df)
