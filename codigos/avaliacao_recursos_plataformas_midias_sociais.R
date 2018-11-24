#instalação pacotes
#install.packages("readxl")
#install.packages("ggplot2")
library("ggplot2")
library("readxl")

data <- read_excel("dados/umses_graduacao_2018_vtidy.xlsx")


envio_info_pais_txt <- "Recurso A"
envio_info_pais <- data$evioinfo
envio_info_pais_excelente <- sum(envio_info_pais == '1')
envio_info_pais_bom <- sum(envio_info_pais == '2')
envio_info_pais_indiferente <- sum(envio_info_pais == '3')
envio_info_pais_ruim <- sum(envio_info_pais == '4')
envio_info_pais_muitoruim <- sum(envio_info_pais == '5')
envio_pais_max <- max(envio_info_pais_excelente)

envio_info_pais_arr <- c(
  envio_info_pais_excelente,
  envio_info_pais_bom,
  envio_info_pais_indiferente,
  envio_info_pais_ruim,
  envio_info_pais_muitoruim
)

motivos_promocionais_txt <- "Recurso B"#usando mídias sociais por motivos promocionais."
motivos_promocionais <- data$grandeuso
motivos_promocionais_excelente <- sum(motivos_promocionais == '1')
motivos_promocionais_bom <- sum(motivos_promocionais == '2')
motivos_promocionais_indiferente <- sum(motivos_promocionais == '3')
motivos_promocionais_ruim <- sum(motivos_promocionais == '4')
motivos_promocionais_muitoruim <- sum(motivos_promocionais == '5')

motivos_promocionais_arr <- c(
  motivos_promocionais_excelente,
  motivos_promocionais_bom,
  motivos_promocionais_indiferente,
  motivos_promocionais_ruim,
  motivos_promocionais_muitoruim
)

grupo_facebook_txt <- "Recurso C"#comunicar com os alunos."
grupo_facebook <- data$facegrupo
grupo_facebook_excelente <- sum(grupo_facebook == '1')
grupo_facebook_bom <- sum(grupo_facebook == '2')
grupo_facebook_indiferente <- sum(grupo_facebook == '3')
grupo_facebook_ruim <- sum(grupo_facebook == '4')
grupo_facebook_muitoruim <- sum(grupo_facebook == '5')

grupo_facebook_arr <- c(
  grupo_facebook_excelente,
  grupo_facebook_bom,
  grupo_facebook_indiferente,
  grupo_facebook_ruim,
  grupo_facebook_muitoruim
)

troca_infos_txt <- "Recurso D"#pesquisas e vídeos rapidamente. Quando alguém contribui com o grupo, seus membros recebem uma notificação. No Facebook para smartphone, estas trocas são enviadas diretamente para o dispositivo móvel dos alunos."
troca_infos <- data$trocainfo
troca_infos_excelente <- sum(troca_infos == '1')
troca_infos_bom <- sum(troca_infos == '2')
troca_infos_indiferente <- sum(troca_infos == '3')
troca_infos_ruim <- sum(troca_infos == '4')
troca_infos_muitoruim <- sum(troca_infos == '5')

troca_infos_arr <- c(
  troca_infos_excelente,
  troca_infos_bom,
  troca_infos_indiferente,
  troca_infos_ruim,
  troca_infos_muitoruim
)

compart_estud_profs_txt <- "Recurso E"#entre si."
compart_estud_profs <- data$compinfopal
compart_estud_profs_excelente <- sum(compart_estud_profs == '1')
compart_estud_profs_bom <- sum(compart_estud_profs == '2')
compart_estud_profs_indiferente <- sum(compart_estud_profs == '3')
compart_estud_profs_ruim <- sum(compart_estud_profs == '4')
compart_estud_profs_muitoruim <- sum(compart_estud_profs == '5')

compart_estud_profs_arr <- c(
  compart_estud_profs_excelente,
  compart_estud_profs_bom,
  compart_estud_profs_indiferente,
  compart_estud_profs_ruim,
  compart_estud_profs_muitoruim
)

pinterest_txt <- "Recurso F"#compartilhar, carregar, classificar e gerenciar imagens, vídeos e outros conteúdos multimídia. É ótimo para compartilhar recursos que os alunos acharem interessantes ou relevantes"
pinterest <- data$quadrovirtual
pinterest_excelente <- sum(pinterest == '1')
pinterest_bom <- sum(pinterest == '2')
pinterest_indiferente <- sum(pinterest == '3')
pinterest_ruim <- sum(pinterest == '4')
pinterest_muitoruim <- sum(pinterest == '5')

pinterest_arr <- c(
  pinterest_excelente,
  pinterest_bom,
  pinterest_indiferente,
  pinterest_ruim,
  pinterest_muitoruim
)

avaliacoes_recursos <- data.frame(
  "Avaliações" = c('1', '2', '3', '4', '5'),
  envio_info_pais_txt = envio_info_pais_arr,
  motivos_promocionais_txt = motivos_promocionais_arr,
  grupo_facebook_txt = grupo_facebook_arr,
  troca_infos_txt = troca_infos_arr,
  compart_estud_profs_txt = compart_estud_profs_arr,
  pinterest_txt = pinterest_arr
)

recursos = c(
  rep(envio_info_pais_txt, 5), 
  rep(motivos_promocionais_txt, 5), 
  rep(grupo_facebook_txt, 5),
  rep(troca_infos_txt, 5), 
  rep(compart_estud_profs_txt, 5), 
  rep(pinterest_txt, 5)
) # 5 avaliações

avaliacoes = rep(c(
  '1. Excelente', 
  '2. Bom', 
  '3. Indiferente',
  '4. Pobre',
  '5. Muito pobre'
), 6) # 6 recursos

valores = c(
  envio_info_pais_arr, motivos_promocionais_arr, grupo_facebook_arr,
  troca_infos_arr, compart_estud_profs_arr, pinterest_arr
)
valores[is.na(valores)] <- 0

dados = data.frame(recursos, avaliacoes, valores)

pdf("graficos/avaliacao_recursos_plataformas.pdf")

ggplot(dados, aes(fill=avaliacoes, y=valores, x=recursos)) +
  geom_bar(stat="identity") + labs(x = "Recursos", y = "Quantidades", fill = "Avaliações") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

dev.off()
