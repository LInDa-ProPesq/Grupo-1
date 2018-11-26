install.packages("readxl")
install.packages("ggplot2")
library("readxl")
library(ggplot2)

df <- read_excel("dados/umses_graduacao_2018_vtidy.xlsx")
dicionario <- read_excel("dados/dicionario_de_dados.xlsx")

##########ANÁLISE EXPLORATÓRIA DO ASSUNTO#########

melhoraresul = df$melhoraresul
#1.Não
#2.Sim
#3.Não sei / Não tenho opinião

melhoraresul.nao <- sum(melhoraresul == '1')
melhoraresul.sim <- sum(melhoraresul == '2')
melhoraresul.nao_sei <- sum(melhoraresul == '3')

mr = c(melhoraresul.nao, melhoraresul.sim, melhoraresul.nao_sei)
legenda_analise <- c("Não", "Sim", "Não sei / Não tenho opinião")
qtd <- length(legenda_analise)
pct <- round(mr/sum(mr)*100)
legenda_analise <- paste(legenda_analise, pct)
legenda_analise <- paste(legenda_analise,"%",sep="")

pdf("graficos/tempogasto_analise.pdf", 10,7)
pie(mr, labels = legenda_analise, col = c("red","green2", "orange"),
    main = "Mídias sociais integradas às aulas podem \nmelhorar os resultados dos alunos?")

data = subset(df, select=c("tempogasto", "melhoraresul"))
dev.off()
################
melhoraresul.nao.tempo = subset(df, df$melhoraresul == 1,select=c("tempogasto"))
melhoraresul.sim.tempo = subset(df, df$melhoraresul == 2,select=c("tempogasto"))
qplot(melhoraresul.sim.tempo$tempogasto,
      geom="histogram",
      binwidth = 0.5,
      main = "Histograma de horas gastas \npara aqueles que acreditam que os resultados melhoram",
      xlab = "Classe de horas", 
      xlim=c(3.5,9.5))

ggplot(data=melhoraresul.sim.tempo, aes(melhoraresul.sim.tempo$tempogasto)) + 
  geom_histogram(breaks=seq(3.5, 9.5, by = 1), 
                 col="blue", 
                 fill="orange", 
                 alpha = .2) + 
  labs(title="Histogram for Age") +
  labs(x="Age", y="Count")
##################

#Tempo gasto pelos que acreditam que os resultados melhoram##############

range(melhoraresul.sim.tempo$tempogasto)
melhoraresul.sim.tempo.1 <- sum(melhoraresul.sim.tempo$tempogasto == 1)
melhoraresul.sim.tempo.2 <- sum(melhoraresul.sim.tempo$tempogasto == 2)
melhoraresul.sim.tempo.3 <- sum(melhoraresul.sim.tempo$tempogasto == 3)
melhoraresul.sim.tempo.4 <- sum(melhoraresul.sim.tempo$tempogasto == 4)
melhoraresul.sim.tempo.5 <- sum(melhoraresul.sim.tempo$tempogasto == 5)
melhoraresul.sim.tempo.6 <- sum(melhoraresul.sim.tempo$tempogasto == 6)
melhoraresul.sim.tempo.7 <- sum(melhoraresul.sim.tempo$tempogasto == 7)
melhoraresul.sim.tempo.8 <- sum(melhoraresul.sim.tempo$tempogasto == 8)
melhoraresul.sim.tempo.9 <- sum(melhoraresul.sim.tempo$tempogasto == 9)

#note que melhoraresul.sim.tempo.8 = 0
legenda_tempo_sim <- c("de 30 minutos até 1 hora", 
                  "de 1 a 2 horas", 
                  "de 2 a 3 horas", 
                  "de 3 a 4 horas",
                  "mais de 5 horas")
chart_sim <- c(melhoraresul.sim.tempo.4, 
          melhoraresul.sim.tempo.5, 
          melhoraresul.sim.tempo.6,
          melhoraresul.sim.tempo.7,
          melhoraresul.sim.tempo.9)

qtd <- length(legenda_tempo_sim)
pct <- round(chart_sim/sum(chart_sim)*100)
pct <- paste(pct,"%",sep="")

pdf("graficos/tempogasto_melhorasim.pdf", 10,7)
pie(chart_sim, labels = pct, col=c("lightcyan1", "lightgoldenrod1", "lightpink", "lightskyblue1","thistle1"))
legend("topleft", inset=.0, 
       title="Tempo gasto",
       legenda_tempo_sim, 
       fill=c("lightcyan1", "lightgoldenrod1", "lightpink", "lightskyblue1","thistle1"), 
       text.font=2, cex=0.90)
dev.off()

#Tempo gasto pelos que NÃO acreditam que os resultados melhoram#########

range(melhoraresul.nao.tempo$tempogasto)
melhoraresul.nao.tempo.1 <- sum(melhoraresul.nao.tempo$tempogasto == 1)
melhoraresul.nao.tempo.2 <- sum(melhoraresul.nao.tempo$tempogasto == 2)
melhoraresul.nao.tempo.3 <- sum(melhoraresul.nao.tempo$tempogasto == 3)
melhoraresul.nao.tempo.4 <- sum(melhoraresul.nao.tempo$tempogasto == 4)
melhoraresul.nao.tempo.5 <- sum(melhoraresul.nao.tempo$tempogasto == 5)
melhoraresul.nao.tempo.6 <- sum(melhoraresul.nao.tempo$tempogasto == 6)
melhoraresul.nao.tempo.7 <- sum(melhoraresul.nao.tempo$tempogasto == 7)
melhoraresul.nao.tempo.8 <- sum(melhoraresul.nao.tempo$tempogasto == 8)
melhoraresul.nao.tempo.9 <- sum(melhoraresul.nao.tempo$tempogasto == 9)

#note que melhoraresul.nao.tempo.4, melhoraresul.nao.tempo.5 e melhoraresul.nao.tempo.8 são iguais a 0  
legenda_tempo_nao <- c("de 5 a 10 minutos", 
                  "de 10 a 30 minutos",
                  "de 2 a 3 horas", 
                  "de 3 a 4 horas",
                  "mais de 5 horas")

chart_nao <- c(melhoraresul.nao.tempo.2,
          melhoraresul.nao.tempo.3, 
          melhoraresul.nao.tempo.6,
          melhoraresul.nao.tempo.7,
          melhoraresul.nao.tempo.9)

pct <- round(chart_nao/sum(chart_nao)*100)
pct <- paste(pct,"%",sep="")

pdf("graficos/tempogasto_melhoranao.pdf", 10,7)
pie(chart_nao, labels = pct, col=c("lightcyan1", "lightgoldenrod1", "lightpink", "lightskyblue1","thistle1"))
legend("topleft", inset=.0, 
       title="Tempo gasto",
       legenda_tempo_nao, 
       fill=c("lightcyan1", "lightgoldenrod1", "lightpink", "lightskyblue1","thistle1"), 
       text.font=2, cex=0.90)
dev.off()
#######################


########################
######### JUÇÃO DAS OPINIÕES "SIM" E "NÃO" E TEMPO GASTO ##############

l <- c(legenda_tempo_sim[1],
       legenda_tempo_sim[2],
       legenda_tempo_sim[3],
       legenda_tempo_sim[4],
       legenda_tempo_sim[5],
       legenda_tempo_nao[1],
       legenda_tempo_nao[2],
       legenda_tempo_nao[3],
       legenda_tempo_nao[4],
       legenda_tempo_nao[5])

ch <- c(chart_sim[1],
        chart_sim[2],
        chart_sim[3],
        chart_sim[4],
        chart_sim[5],
        chart_nao[1],
        chart_nao[2],
        chart_nao[3],
        chart_nao[4],
        chart_nao[5])

#LEGENDA
#1 - sim - "de 30 minutos até 1 hora",
#2 - sim - "de 1 a 2 horas", 
#3 - sim - "de 2 a 3 horas",
#4 - sim - "de 3 a 4 horas",
#5 - sim - "mais de 5 horas",
#1 - nao - "de 5 a 10 minutos", 
#2 - nao -"de 10 a 30 minutos",
#3 - nao -"de 2 a 3 horas", 
#4 - nao -"de 3 a 4 horas",
#5 - nao -"mais de 5 horas"


legenda <- round(ch/sum(ch)*100)
legenda <- paste(legenda,"%",sep="")
qtd <- length(l)
pct <- round(ch/sum(ch)*100)

pie(ch, labels = legenda,
    col=c("palegreen","palegreen2","palegreen3","palegreen4","darkseagreen4",
          "indianred1","indianred2","indianred3","indianred4","darkred"))

all_nao = c(melhoraresul.nao.tempo.1,
            melhoraresul.nao.tempo.2,
            melhoraresul.nao.tempo.3,
            melhoraresul.nao.tempo.4,
            melhoraresul.nao.tempo.5,
            melhoraresul.nao.tempo.6,
            melhoraresul.nao.tempo.7,
            melhoraresul.nao.tempo.8,
            melhoraresul.nao.tempo.9)

all_sim = c(melhoraresul.sim.tempo.1,
            melhoraresul.sim.tempo.2,
            melhoraresul.sim.tempo.3,
            melhoraresul.sim.tempo.4,
            melhoraresul.sim.tempo.5,
            melhoraresul.sim.tempo.6,
            melhoraresul.sim.tempo.7,
            melhoraresul.sim.tempo.8,
            melhoraresul.sim.tempo.9)

all = data.frame(
  all_sim = all_sim,
  all_nao = all_nao,
  stringsAsFactors=F
)
all_matrix = as.matrix(all)
colnames(all_matrix) <- c("all_sim","all_nao")

legenda_all = c("Nenhum",
                "5 a 10 minutos",
                "10 a 30 minutos",
                "30 a 60 minutos",
                "1 a 2 horas", 
                "2 a 3 horas",
                "3 a 4 horas",
                "4 a 5 horas",
                "> 5horas")

pdf("graficos/tempogasto_opiniao.pdf", 14, 6)

barplot(t(all[c('all_sim','all_nao')]), beside = TRUE,
        cex.names=0.85,
        xlab="Tempo gasto",
        names.arg=legenda_all,
        col=c("seagreen2","indianred1"),
        ylim=c(0,12))

legend("topleft", inset=.015, 
       title="Acredita nas mídias sociais como ferramentas de auxílio ao ensino",
       c("Sim","Não"), fill=c("seagreen2","indianred1"), text.font=2, cex=0.85)

dev.off()
