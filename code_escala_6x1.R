# SEPLAN - CGEES/DIEAS
# Autor: Yuri Cesar de Lima e Silva
# Produto: Escala 6x1 (PNADc - Trimestral)

# Fechar dados & gráficos
rm(list=ls())
graphics.off()

# Abrir pacotes:
library(PNADcIBGE) 
library(survey) 
library(convey) 
library(magrittr) 
library(gt)
library(dplyr)
library(openxlsx)
library(stringr)
library(ggplot2)
library(readxl)
library(tidyr)
library(reactable)
library(webshot2)


# Importar online microdados do ano e trimestres especificado
#dadosPNADc_2025.4 <- get_pnadc(year = 2025, quarter = 4)
#saveRDS(dadosPNADc_2025.4, "dadosPNADc_2025.4.rds")

# Importar bases já salvas do meu computador

#dadosPNADc <- readRDS("dadosPNADc_2025.3.rds")
dadosPNADc <- readRDS("dadosPNADc_2025.4.rds")

################################################################################
# Informalidade e formalidade
################################################################################

# Formais:
formais <- svytotal(x=~((VD4002=="Pessoas ocupadas") &
                                (VD4009=="Empregado no setor privado com carteira de trabalho assinada") |
                                (VD4009=="Trabalhador doméstico com carteira de trabalho assinada") |
                                (VD4009=="Empregado no setor público com carteira de trabalho assinada") |
                                (VD4009=="Militar e servidor estatutário") |
                                (VD4009=="Empregador" & V4019=="Sim") |
                                (VD4009=="Conta-própria" & V4019=="Sim")
                              ), design=subset(dadosPNADc,UF=="Roraima"), na.rm=TRUE)
formais[2]

# Informais:
informais <- svytotal(x=~((VD4002=="Pessoas ocupadas") &
                          (VD4009=="Empregado no setor privado sem carteira de trabalho assinada") |
                          (VD4009=="Trabalhador doméstico sem carteira de trabalho assinada") |
                          (VD4009=="Empregado no setor público sem carteira de trabalho assinada") |
                          (VD4009=="Trabalhador familiar auxiliar") |
                          (VD4009=="Empregador" & V4019=="Não") |
                          (VD4009=="Conta-própria" & V4019=="Não")
), design=subset(dadosPNADc,UF=="Roraima"), na.rm=TRUE)
informais[2]

# Trabalhadores formais (apenas CLT):
formais_clt <- svytotal(x=~((VD4002=="Pessoas ocupadas") &
                          (VD4009=="Empregado no setor privado com carteira de trabalho assinada") |
                          (VD4009=="Trabalhador doméstico com carteira de trabalho assinada") |
                          (VD4009=="Empregado no setor público com carteira de trabalho assinada")
), design=subset(dadosPNADc,UF=="Roraima"), na.rm=TRUE)
formais_clt[2]

# Total de pessoas ocupadas:
ocupadas <- svytotal(x=~(VD4002=="Pessoas ocupadas"), design=subset(dadosPNADc,UF=="Roraima"), na.rm=TRUE)
ocupadas [2]

################################################################################
# Total de CLTista afetados pela redução da jornada de trabalho
################################################################################
rr_41mais <- svytotal(x=~(V4039C>=41 & ((VD4002=="Pessoas ocupadas") &
                                       (VD4009=="Empregado no setor privado com carteira de trabalho assinada") |
                                       (VD4009=="Trabalhador doméstico com carteira de trabalho assinada") |
                                       (VD4009=="Empregado no setor público com carteira de trabalho assinada"))
), design=subset(dadosPNADc,UF=="Roraima"), na.rm=TRUE)
(rr_41mais[2]/formais_clt[2])*100

rr_37mais <- svytotal(x=~(V4039C>=37 & ((VD4002=="Pessoas ocupadas") &
                                          (VD4009=="Empregado no setor privado com carteira de trabalho assinada") |
                                          (VD4009=="Trabalhador doméstico com carteira de trabalho assinada") |
                                          (VD4009=="Empregado no setor público com carteira de trabalho assinada"))
), design=subset(dadosPNADc,UF=="Roraima"), na.rm=TRUE)
(rr_37mais[2]/formais_clt[2])*100


###############################################################################
# Massa salarial
###############################################################################
rr_massa_clt <- svytotal(
  ~I(VD4017),
  design = subset(
    dadosPNADc,
    UF=="Roraima" &
      VD4002=="Pessoas ocupadas" &
      VD4009 %in% c(
        "Empregado no setor privado com carteira de trabalho assinada",
        "Trabalhador doméstico com carteira de trabalho assinada",
        "Empregado no setor público com carteira de trabalho assinada"
      )
  ),
  na.rm = TRUE
)

rr_massa_clt

rr_acrescimo_40 <- svytotal(
  ~I(VD4017 * ((V4039C/40) - 1)),
  design = subset(
    dadosPNADc,
    UF=="Roraima" &
      V4039C > 40 &
      VD4002=="Pessoas ocupadas" &
      VD4009 %in% c(
        "Empregado no setor privado com carteira de trabalho assinada",
        "Trabalhador doméstico com carteira de trabalho assinada",
        "Empregado no setor público com carteira de trabalho assinada"
      )
  ),
  na.rm = TRUE
)

rr_acrescimo_40

rr_acrescimo_36 <- svytotal(
  ~I(VD4017 * ((V4039C/36) - 1)),
  design = subset(
    dadosPNADc,
    UF=="Roraima" &
      V4039C > 36 &
      VD4002=="Pessoas ocupadas" &
      VD4009 %in% c(
        "Empregado no setor privado com carteira de trabalho assinada",
        "Trabalhador doméstico com carteira de trabalho assinada",
        "Empregado no setor público com carteira de trabalho assinada"
      )
  ),
  na.rm = TRUE
)

rr_acrescimo_36

################################################################################
# Escala 6x1 - Formais (trabalhadores formais regidos pela CLT) - exclui militar, estatutário, empregador e conta-própria.
################################################################################

# Variável analisada: V4039C
# Quantas horas ... trabalhou efetivamente na semana de referência nesse "trabalho principal"?

formais_horasTrab_ativEcon_36menos <- svyby(formula=~(V4039C<=36 & ((VD4002=="Pessoas ocupadas") &
                                                              (VD4009=="Empregado no setor privado com carteira de trabalho assinada") |
                                                              (VD4009=="Trabalhador doméstico com carteira de trabalho assinada") |
                                                              (VD4009=="Empregado no setor público com carteira de trabalho assinada"))
                                              ), by=~V4013, design=subset(dadosPNADc,UF=="Roraima"), FUN=svytotal, na.rm=TRUE)
#formais_horasTrab_ativEcon_36menos[3]

formais_horasTrab_ativEcon_37a40 <- svyby(formula=~(V4039C>=37&V4039C<=40 & ((VD4002=="Pessoas ocupadas") &
                                                                      (VD4009=="Empregado no setor privado com carteira de trabalho assinada") |
                                                                      (VD4009=="Trabalhador doméstico com carteira de trabalho assinada") |
                                                                      (VD4009=="Empregado no setor público com carteira de trabalho assinada"))
), by=~V4013, design=subset(dadosPNADc,UF=="Roraima"), FUN=svytotal, na.rm=TRUE)
#formais_horasTrab_ativEcon_37a40[3]

formais_horasTrab_ativEcon_41mais <- svyby(formula=~(V4039C>=41 & ((VD4002=="Pessoas ocupadas") &
                                                                      (VD4009=="Empregado no setor privado com carteira de trabalho assinada") |
                                                                      (VD4009=="Trabalhador doméstico com carteira de trabalho assinada") |
                                                                      (VD4009=="Empregado no setor público com carteira de trabalho assinada"))
), by=~V4013, design=subset(dadosPNADc,UF=="Roraima"), FUN=svytotal, na.rm=TRUE)
#formais_horasTrab_ativEcon_41mais[3]


################################################################################
# Contabilizando o impacto médio por atividade econômica (CNAE)
################################################################################
impacto_40 <- svyby(
  ~I(ifelse(V4039C >= 41, ((V4039C/40) - 1)*100, 0)),
  design = subset(dadosPNADc,
                  UF=="Roraima" &
                    VD4002=="Pessoas ocupadas" &
                    (VD4009=="Empregado no setor privado com carteira de trabalho assinada" |
                       VD4009=="Trabalhador doméstico com carteira de trabalho assinada" |
                       VD4009=="Empregado no setor público com carteira de trabalho assinada")),
  by = ~V4013,FUN = svymean, na.rm = TRUE)
colnames(impacto_40)[2] <- "Impacto_medio_40"

impacto_36 <- svyby(
  ~I(ifelse(V4039C >= 37, ((V4039C/36) - 1)*100, 0)),
  design = subset(dadosPNADc,
                  UF=="Roraima" &
                    VD4002=="Pessoas ocupadas" &
                    (VD4009=="Empregado no setor privado com carteira de trabalho assinada" |
                       VD4009=="Trabalhador doméstico com carteira de trabalho assinada" |
                       VD4009=="Empregado no setor público com carteira de trabalho assinada")),
  by = ~V4013,FUN = svymean,na.rm = TRUE)
colnames(impacto_36)[2] <- "Impacto_medio_36"

impactos_cnae <- impacto_40 %>%
  left_join(impacto_36[,c("V4013","Impacto_medio_36")],
            by="V4013") %>%
  select("V4013","Impacto_medio_36","Impacto_medio_40") %>%
  rename(cnae = V4013)


# Organização da base de dados:
cnae <- read.xlsx("cnae.xlsx", colNames = FALSE)
colnames(cnae) <- c("cnae","Descrição_CNAE")
colnames(formais_horasTrab_ativEcon_36menos)[1] <- "cnae"

len_alvo <- max(nchar(as.character(formais_horasTrab_ativEcon_36menos$cnae)), nchar(as.character(cnae$cnae)))
len_alvo

cnae <- cnae %>%
  mutate(cnae_pad = str_pad(as.character(cnae), width = len_alvo, side = "left", pad = "0"))

formais_horasTrab_ativEcon_36menos <- formais_horasTrab_ativEcon_36menos %>%
  mutate(cnae_pad = str_pad(as.character(cnae), width = len_alvo, side = "left", pad = "0"))

impactos_cnae <- impactos_cnae %>%
  mutate(cnae_pad = str_pad(as.character(cnae), width = len_alvo, side = "left", pad = "0"))

base_cnae <- formais_horasTrab_ativEcon_36menos %>%
  left_join(cnae %>% select(cnae_pad, Descrição_CNAE),
            by = "cnae_pad")

impactos_cnae <- base_cnae %>%
  left_join(impactos_cnae %>% select(cnae_pad, Impacto_medio_36, Impacto_medio_40),
            by = "cnae_pad")

base_formais <- data.frame(formais_horasTrab_ativEcon_36menos[,1],
                           base_cnae[7],
                           formais_horasTrab_ativEcon_36menos[,3],
                           formais_horasTrab_ativEcon_37a40[,3],
                           formais_horasTrab_ativEcon_41mais[,3],
                           total=formais_horasTrab_ativEcon_36menos[,3]+
                             formais_horasTrab_ativEcon_37a40[,3]+
                             formais_horasTrab_ativEcon_41mais[,3],
                           afetados_40=100*formais_horasTrab_ativEcon_41mais[,3]/
                             (formais_horasTrab_ativEcon_36menos[,3]+
                                formais_horasTrab_ativEcon_37a40[,3]+
                                formais_horasTrab_ativEcon_41mais[,3]),
                           afetados_36=100*(formais_horasTrab_ativEcon_41mais[,3]+
                                              formais_horasTrab_ativEcon_37a40[,3])/
                             (formais_horasTrab_ativEcon_36menos[,3]+
                                formais_horasTrab_ativEcon_37a40[,3]+
                                formais_horasTrab_ativEcon_41mais[,3]),
                           impacto40=impactos_cnae[9],
                           impacto36=impactos_cnae[8])

colnames(base_formais) <- c("CNAE","Atividades Econômicas","36menos","37a40","41mais","Total","% afetados (40h)","% afetados (36h)","Impacto 40h","Impacto 36h")


# Visualização dos dados

base_formais <- base_formais[order(base_formais$`Total`, decreasing = TRUE), ]


tabela0<-base_formais %>%
  dplyr::filter(Total >= 300) %>%
  gt() %>%
  cols_align(
    align = "center",
    columns = c("CNAE","36menos","37a40", "41mais", "Total","% afetados (40h)","% afetados (36h)","Impacto 40h","Impacto 36h")
  ) %>%
  tab_options(
    table.font.size = px(12)
  ) %>%
  fmt_number(columns = where(is.numeric), decimals = 0, locale = "pt_BR") %>%
  fmt_number(columns = `% afetados (40h)`, decimals = 2, locale = "pt_BR") %>%
  fmt_number(columns = `% afetados (36h)`, decimals = 2, locale = "pt_BR") %>%
  fmt_number(columns = `Impacto 36h`, decimals = 2, locale = "pt_BR") %>%
  fmt_number(columns = `Impacto 40h`, decimals = 2, locale = "pt_BR") %>%
  cols_label(
    "CNAE" = "CNAE",
    "Atividades Econômicas" = "Atividades Econômicas",
    "36menos" = "Jornadas de 36h ou menos",
    "37a40" = "Jornadas de 37h a 40h",
    "41mais" = "Jornadas de 41h ou mais",
    "Total" = "Total",
    "% afetados (40h)" = "Afetados em % (se o novo limite for 40h)",
    "% afetados (36h)" = "Afetados em % (se o novo limite for 36h)",
    "Impacto 40h" = "Aumento médio do custo por hora em % (se o novo limite for 40h)",
    "Impacto 36h" = "Aumento médio do custo por hora em % (se o novo limite for 36h)"
  ) %>%
  tab_header(
    title = md("**Tabela 1 – Impacto do fim da escala 6x1 em Roraima (empregos CLT)**<br>
              **Por atividade econômica (CNAE) com 300 ou mais empregados**")
  )


tabela1<-base_formais %>%
  dplyr::filter(Total >= 300) %>%
  dplyr::select("CNAE","Atividades Econômicas","36menos","37a40","41mais","Total") %>%
  gt() %>%
  cols_align(
    align = "center",
    columns = c("36menos","37a40", "41mais", "Total")
  ) %>%
  tab_options(
    table.font.size = px(14)
  ) %>%
  fmt_number(columns = where(is.numeric), decimals = 0, locale = "pt_BR") %>%
  cols_label(
    "CNAE" = "CNAE",
    "Atividades Econômicas" = "Atividades Econômicas",
    "36menos" = "Jornadas de 36h ou menos",
    "37a40" = "Jornadas de 37h a 40h",
    "41mais" = "Jornadas de 41h ou mais",
    "Total" = "Total"
    ) %>%
  tab_header(
    title = md("**Tabela 1 – Estrutura dos empregos por jornada de trabalho (empregos CLT)**<br>
              **Por atividade econômica (CNAE) com 300 ou mais empregados**")
  )


tabela2<-base_formais %>%
  dplyr::filter(Total >= 300) %>%
  dplyr::select("CNAE","Atividades Econômicas","% afetados (40h)","Impacto 40h") %>%
  gt() %>%
  cols_align(
    align = "center",
    columns = c("% afetados (40h)","Impacto 40h")
  ) %>%
  tab_options(
    table.font.size = px(14)
  ) %>%
  fmt_number(columns = where(is.numeric), decimals = 0, locale = "pt_BR") %>%
  fmt_number(columns = `% afetados (40h)`, decimals = 2, locale = "pt_BR") %>%
  fmt_number(columns = `Impacto 40h`, decimals = 2, locale = "pt_BR") %>%
  cols_label(
    "CNAE" = "CNAE",
    "Atividades Econômicas" = "Atividades Econômicas",
    "% afetados (40h)" = "Afetados em % (se o novo limite for 40h)",
    "Impacto 40h" = "Aumento médio do custo por hora em % (se o novo limite for 40h)",
  ) %>%
  tab_header(
    title = md("**Tabela 2 – Impacto da redução da jornada de trabalho para 40 horas semanais em Roraima (empregos CLT)**<br>
              **Por atividade econômica (CNAE) com 300 ou mais empregados**")
  )



tabela3<-base_formais %>%
  dplyr::filter(Total >= 300) %>%
  dplyr::select("CNAE","Atividades Econômicas","% afetados (36h)","Impacto 36h") %>%
  gt() %>%
  cols_align(
    align = "center",
    columns = c("% afetados (36h)","Impacto 36h")
  ) %>%
  tab_options(
    table.font.size = px(14)
  ) %>%
  fmt_number(columns = where(is.numeric), decimals = 0, locale = "pt_BR") %>%
  fmt_number(columns = `% afetados (36h)`, decimals = 2, locale = "pt_BR") %>%
  fmt_number(columns = `Impacto 36h`, decimals = 2, locale = "pt_BR") %>%
  cols_label(
    "CNAE" = "CNAE",
    "Atividades Econômicas" = "Atividades Econômicas",
    "% afetados (36h)" = "Afetados em % (se o novo limite for 36h)",
    "Impacto 36h" = "Aumento médio do custo por hora em % (se o novo limite for 36h)",
  ) %>%
  tab_header(
    title = md("**Tabela 3 – Impacto da redução da jornada de trabalho para 36 horas semanais em Roraima (empregos CLT)**<br>
              **Por atividade econômica (CNAE) com 300 ou mais empregados**")
  )


################################################################################
# Impacto no agregado do estado de Roraima
################################################################################
impacto_estado_40 <- svymean(
  ~I(ifelse(V4039C >= 41, ((V4039C/40) - 1)*100, 0)),
  design = subset(dadosPNADc,
                  UF=="Roraima" &
                    VD4002=="Pessoas ocupadas" &
                    (VD4009=="Empregado no setor privado com carteira de trabalho assinada" |
                       VD4009=="Trabalhador doméstico com carteira de trabalho assinada" |
                       VD4009=="Empregado no setor público com carteira de trabalho assinada")),
  na.rm = TRUE
)

impacto_estado_40[1]

impacto_estado_36 <- svymean(
  ~I(ifelse(V4039C >= 37, ((V4039C/36) - 1)*100, 0)),
  design = subset(dadosPNADc,
                  UF=="Roraima" &
                    VD4002=="Pessoas ocupadas" &
                    (VD4009=="Empregado no setor privado com carteira de trabalho assinada" |
                       VD4009=="Trabalhador doméstico com carteira de trabalho assinada" |
                       VD4009=="Empregado no setor público com carteira de trabalho assinada")),
  na.rm = TRUE
)

impacto_estado_36[1]


################################################################################
# Impactos por UF
################################################################################
impacto_uf_40 <- svyby(
  ~I(ifelse(V4039C >= 41, ((V4039C/40) - 1)*100, 0)),
  by = ~UF,
  design = subset(dadosPNADc,
                  VD4002=="Pessoas ocupadas" &
                    (VD4009=="Empregado no setor privado com carteira de trabalho assinada" |
                       VD4009=="Trabalhador doméstico com carteira de trabalho assinada" |
                       VD4009=="Empregado no setor público com carteira de trabalho assinada")),
  FUN = svymean,
  na.rm = TRUE
)

colnames(impacto_uf_40)[2] <- "Impacto_40"

impacto_uf_40[2]

grafico_base <- as.data.frame(impacto_uf_40) %>%
  select(UF, Impacto_40) %>%
  arrange(Impacto_40) %>%
  mutate(destaque = ifelse(UF == "Roraima", "RR", "Outros"))

grafico1<-ggplot(grafico_base,
       aes(x = reorder(UF, Impacto_40),
           y = Impacto_40,
           fill = destaque)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.2f", Impacto_40)),
            hjust = -0.15,
            size = 2.5) +
  coord_flip() +
  scale_fill_manual(values = c("RR" = "#7ba05b",
                               "Outros" = "#1f3e59"),
                    guide = "none") +
  labs(
    title = "Gráfico 1 - Impacto médio da redução para 40h por UF",
    subtitle = "Aumento médio do custo por hora (%) – trabalhadores CLT",
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid = element_blank(),   # <- remove toda a grade
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


###############################################################################
impacto_uf_36 <- svyby(
  ~I(ifelse(V4039C >= 37, ((V4039C/36) - 1)*100, 0)),
  by = ~UF,
  design = subset(dadosPNADc,
                  VD4002=="Pessoas ocupadas" &
                    (VD4009=="Empregado no setor privado com carteira de trabalho assinada" |
                       VD4009=="Trabalhador doméstico com carteira de trabalho assinada" |
                       VD4009=="Empregado no setor público com carteira de trabalho assinada")),
  FUN = svymean,
  na.rm = TRUE
)

colnames(impacto_uf_36)[2] <- "Impacto_36"

impacto_uf_36[2]


grafico_base36 <- as.data.frame(impacto_uf_36) %>%
  select(UF, Impacto_36) %>%
  arrange(Impacto_36) %>%
  mutate(destaque = ifelse(UF == "Roraima", "RR", "Outros"))

grafico2<-ggplot(grafico_base36,
       aes(x = reorder(UF, Impacto_36),
           y = Impacto_36,
           fill = destaque)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.2f", Impacto_36)),
            hjust = -0.15,
            size = 2.5) +
  coord_flip() +
  scale_fill_manual(values = c("RR" = "#7ba05b",
                               "Outros" = "#1f3e59"),
                    guide = "none") +
  labs(
    title = "Gráfico 2 - Impacto médio da redução para 36h por UF",
    subtitle = "Aumento médio do custo por hora (%) – trabalhadores CLT",
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid = element_blank(),   # <- remove toda a grade
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

