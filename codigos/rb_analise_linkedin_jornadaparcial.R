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
data$ID <- seq.int(nrow(data))
agrupa <- as.data.frame(list(data$ID, data))
names(agrupa)[1] <- "ID"
write.csv(agrupa, "dados/structured_csv.csv",row.names=FALSE)

#atribuindo os valores da coluna genero
coluna_linkedin <- data$'linkedin'
coluna_jornadaparcial <- data$'trabalha'

usam <- sum(coluna_linkedin == '1')
nao_usam <- sum(coluna_linkedin == '0')

#abre arquivo pdf para poder salvar o gráfico
pdf("graficos/dados_linkedin_jornadaparcial.pdf")

# % Pessoas Utilizam Linkedin
dados <- c(usam, nao_usam)
legenda <- c("Usam LinkedIn", "N�o utilizam")
qtd <- length(legenda)
pct <- round(dados/sum(dados)*100)
legenda <- paste(legenda, pct)
legenda <- paste(legenda,"%",sep="")
pie(dados,labels = legenda, col=cor(qtd),
    main="Fazem jornada parcial e usam LinkedIn")
#fecha pdf
dev.off()

#seleciona onde são estagiarios e utilizam ou não a plataforma
estagiarios_linkedin <- subset(agrupa, agrupa$trabalha == 3, select = c(ID, linkedin))
usam_e_desem <- sum(estagiarios_linkedin$linkedin == '1')
nao_usam_e_estagiarios <- sum(estagiarios_linkedin$linkedin == '0')

#seleciona onde são empregados e utilizam ou não a plataforma
empregado_linkedin <- subset(agrupa, agrupa$trabalha == 2 | agrupa$trabalha == 3 | agrupa$trabalha == 4 | agrupa$trabalha == 5 | agrupa$trabalha == 8, select = c(ID, linkedin))
usam_e_em <- sum(empregado_linkedin$linkedin == '1')
nao_usam_e_empregado <- sum(empregado_linkedin$linkedin == '0')
