setwd('/home/matheus-lopes/Documentos/UNESP 2018/2 SEM/CIÊNCIA DE DADOS/Grupo-1')
if (!require(readxl)) install.packages('readxl')
library(readxl)

if (!require(RColorBrewer)) install.packages('RColorBrewer')
library(RColorBrewer)

cor <- function(numero_dados) {
  d=brewer.pal(n = numero_dados, name = "RdBu")
  return(d)
}

#usando xls
data <- read_excel("dados/umses_graduacao_2018_vtidy.xlsx")



#GRÁFICO DE USO DAS PLATAFORMAS
coluna_facebook <- sum(data$facebook)
coluna_twitter <- sum(data$twitter)
coluna_whatsapp <- sum(data$whatsapp)
coluna_linkedin <- sum(data$linkedin)
coluna_youtube <- sum(data$youtube)
coluna_instagram <- sum(data$instagram)
coluna_snapchat <- sum(data$snapchat)
coluna_tumblr <- sum(data$tumblr)
coluna_pinterest <- sum(data$pinterest)
outras_plataf <- 0
for (i in data$outras_plataformas){
  if(!is.na(i)){
    outras_plataf <- outras_plataf+1
  }
}

coluna_outras <- outras_plataf

#formatando para criar gráfico
dados <- c(coluna_facebook, coluna_twitter, coluna_whatsapp, coluna_linkedin, coluna_youtube, 
           coluna_instagram, coluna_snapchat, coluna_tumblr, coluna_pinterest, coluna_outras)

legenda <- c("Facebook", "Twitter", "WhatsApp", "Linkedin", "YouTube", "Instagram", "Snapchat", "Tumblr", "Pinterest", "Outras")

qtd <- length(legenda)
pct <- round(dados/sum(dados)*100)

barplot(dados, main="Uso de plataformas de Redes Sociais", col = rainbow(20), horiz=TRUE,
        width = 500, xlim = c(0,70),cex.names=0.8,
        xlab="Qtde de entrevistados que são usuários",
        las=1,
        names.arg=legenda)


#PERGUNTA 3 - TEMPO DE USO DAS REDES SOCIAIS

tabela <- table(data$tempogasto)
slices <- c(tabela)
lbls <- c("Nenhum/Não uso -", "5 a 10 min -", "10 a 30 min -", "30 a 1 hora -",
          "1 a 2 horas -","2 a 3 horas -","3 a 4 horas -","4 a 5 horas -","mais de 5 horas -");

pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Tempo de uso das mídias sociais por dia", radius = 1, init.angle = 0, border = 1)

#PERGUNTA 4 - MIDIA SOCIAL DEVE SER USADA POR PORFS?
tabela <- table(data$usoacademico)
counts <- c(tabela)
lbls <- c("Não","Sim", "Sim, porém com restrições", "Não sei / Não tenho opinião")

barplot(counts, main="Mídia social é uma ferramenta que pode/deve ser utilizada pelos professores?", space=1,
        xlab="Opinião dos entrevistados", names.arg = lbls, cex.names = 0.9, las=1, col = rainbow(15))



#PERGUNTA 5 - MELHOR FORMA DE APROXIMAR PROFS E ALUNOS?
tabela <- table(data$profchegaal)
counts <- c(tabela)
lbls <- c("Não","Sim", "Não sei / Não tenho opinião")

barplot(counts, main="A mídia social é a melhor forma dos professores se aproximarem de seus
alunos?", space=1,, ylim=c(0,30),
        xlab="Opinião dos entrevistados", names.arg = lbls, cex.names = 0.9, las=1, col = rainbow(8))

#PERGUNTA 6 - MELHORES RESULTADOS SE USARMOS REDES SOCIAIS?
tabela <- table(data$melhoraresul)
counts <- c(tabela)
lbls <- c("Não","Sim", "Não sei / Não tenho opinião")

barplot(counts, main="Os alunos alcançarão melhores resultados se as mídias sociais estiverem
integradas às aulas e/ou atividades?", space=1,, ylim=c(0,50),
        xlab="Opinião dos entrevistados", names.arg = lbls, cex.names = 0.9, las=1, col = rainbow(7))

#INSIGHT - QUAL A REDE SOCIAL MAIS USADA PELOS ENTREVISTADOS QUE APOIAM INTEGRAÇÃO DE REDES PARA MELHOR DESEMPENHO

#criando um arquivo estruturado com ID para as linhas
data$ID <- seq.int(nrow(data))
agrupa <- as.data.frame(list(data$ID, data))
names(agrupa)[1] <- "ID"
#write.csv(agrupa, "dados/structured_csv.csv",row.names=FALSE)

#excluindo os individuos que nao responderam sim para a pergunta 6
vetor_exclusao <- c()
id<-1
for(resp in agrupa$melhoraresul){
  if(resp != '2'){
    vetor_exclusao <- c(vetor_exclusao,id)
  }
  id<-id+1
}

agrupa <- agrupa[-c(vetor_exclusao), ]
data <- agrupa

#criando o gráfico
coluna_facebook <- sum(data$facebook)
coluna_twitter <- sum(data$twitter)
coluna_whatsapp <- sum(data$whatsapp)
coluna_linkedin <- sum(data$linkedin)
coluna_youtube <- sum(data$youtube)
coluna_instagram <- sum(data$instagram)
coluna_snapchat <- sum(data$snapchat)
coluna_tumblr <- sum(data$tumblr)
coluna_pinterest <- sum(data$pinterest)
outras_plataf <- 0
for (i in data$outras_plataformas){
  if(!is.na(i)){
    outras_plataf <- outras_plataf+1
  }
}

coluna_outras <- outras_plataf

#formatando para criar gráfico
dados <- c(coluna_facebook, coluna_twitter, coluna_whatsapp, coluna_linkedin, coluna_youtube, 
           coluna_instagram, coluna_snapchat, coluna_tumblr, coluna_pinterest, coluna_outras)

legenda <- c("Facebook", "Twitter", "WhatsApp", "Linkedin", "YouTube", "Instagram", "Snapchat", "Tumblr", "Pinterest", "Outras")

qtd <- length(legenda)
pct <- round(dados/sum(dados)*100)

barplot(dados, main="Plataformas mais usadas por aqueles que consideram\n que terão melhores resultados com a integração de mídias sociais com aulas/atividades", col = rainbow(30), horiz=TRUE,
        width = 500, xlim = c(0,40),cex.names=0.8,
        xlab="Qtde de usuários por plataforma",
        las=1,
        names.arg=legenda, space=1)

