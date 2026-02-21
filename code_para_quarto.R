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
library(dplyr)
library(openxlsx)
library(stringr)
library(ggplot2)
library(readxl)
library(tidyr)
library(reactable)
library(htmltools)



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

base_formais1 <- base_formais[order(base_formais$`Total`, decreasing = TRUE), ]

base_formais2 <- base_formais[order(base_formais$`41mais`, decreasing = TRUE), ]

base_formais3 <- base_formais[order(base_formais$`Impacto 40h`, decreasing = TRUE), ]

base_formais4 <- base_formais[order(base_formais$`Impacto 36h`, decreasing = TRUE), ]


#base_formais <- base_formais[order(base_formais$`CNAE`, decreasing = FALSE), ]



###############################################################################
# Tabela interativa (substitui gt + gtsave) - ideal para Quarto HTML
###############################################################################

# 1) Escolha a base que você quer mostrar (ex.: ordenada por Total)
tabela_base <- base_formais %>%
  dplyr::filter(Total >= 300) %>%
  dplyr::arrange(dplyr::desc(Total))

# 2) Garantir tipos numéricos (importante para ordenação e formatação)
num_cols <- c("36menos","37a40","41mais","Total",
              "% afetados (40h)","% afetados (36h)","Impacto 40h","Impacto 36h")

tabela_base <- tabela_base %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(num_cols), as.numeric))

# 3) Criar objeto reactable (interativo)
# Função para formatar número em padrão brasileiro
fmt_br <- function(x, digits = 2) {
  formatC(x, format = "f", digits = digits, big.mark = ".", decimal.mark = ",")
}

tabela_interativa <- reactable(
  tabela_base,
  searchable = TRUE,
  filterable = TRUE,
  highlight  = TRUE,
  striped    = TRUE,
  bordered   = TRUE,
  compact    = TRUE,
  fullWidth  = TRUE,
  defaultPageSize = 15,
  wrap = TRUE,
  
  columns = list(
    
    CNAE = colDef(
      name = "CNAE",
      minWidth = 80,
      style = list(fontWeight = "600")
    ),
    
    `Atividades Econômicas` = colDef(
      name = "Atividades Econômicas",
      minWidth = 320,
      style = list(whiteSpace = "normal")
    ),
    
    `36menos` = colDef(
      name = "36h ou menos",
      align = "right",
      cell = function(value) fmt_br(value, 0)
    ),
    
    `37a40` = colDef(
      name = "37h a 40h",
      align = "right",
      cell = function(value) fmt_br(value, 0)
    ),
    
    `41mais` = colDef(
      name = "41h ou mais",
      align = "right",
      cell = function(value) fmt_br(value, 0)
    ),
    
    Total = colDef(
      name = "Total",
      align = "right",
      cell = function(value) fmt_br(value, 0),
      style = list(fontWeight = "600")
    ),
    
    `% afetados (40h)` = colDef(
      name = "Afetados (%) – 40h",
      align = "right",
      cell = function(value) fmt_br(value, 2)
    ),
    
    `% afetados (36h)` = colDef(
      name = "Afetados (%) – 36h",
      align = "right",
      cell = function(value) fmt_br(value, 2)
    ),
    
    `Impacto 40h` = colDef(
      name = "Impacto médio (%) – 40h",
      align = "right",
      cell = function(value) {
        color <- ifelse(value >= 20, "#8b0000",
                        ifelse(value >= 15, "#b22222", "#2f4f4f"))
        div(style = list(color = color, fontWeight = "600"),
            fmt_br(value, 2))
      }
    ),
    
    `Impacto 36h` = colDef(
      name = "Impacto médio (%) – 36h",
      align = "right",
      cell = function(value) {
        color <- ifelse(value >= 20, "#8b0000",
                        ifelse(value >= 15, "#b22222", "#2f4f4f"))
        div(style = list(color = color, fontWeight = "600"),
            fmt_br(value, 2))
      }
    )
  ),
  
  theme = reactableTheme(
    tableStyle = list(
      fontSize = "11.5px",
      fontFamily = "Arial"
    ),
    headerStyle = list(
      backgroundColor = "#f2f2f2",
      fontWeight = "bold",
      borderBottom = "2px solid #444"
    ),
    rowStyle = list(
      borderBottom = "1px solid #e6e6e6"
    )
  )
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

ggplot(grafico_base,
       aes(x = reorder(UF, Impacto_40),
           y = Impacto_40,
           fill = destaque)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.2f", Impacto_40)),
            hjust = -0.15,
            size = 3.5) +
  coord_flip() +
  scale_fill_manual(values = c("RR" = "#7ba05b",
                               "Outros" = "#1f3e59"),
                    guide = "none") +
  labs(
    title = "Impacto médio da redução para 40h por UF",
    subtitle = "Aumento médio do custo por hora (%) – trabalhadores CLT",
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 12) +
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

ggplot(grafico_base36,
       aes(x = reorder(UF, Impacto_36),
           y = Impacto_36,
           fill = destaque)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.2f", Impacto_36)),
            hjust = -0.15,
            size = 3.5) +
  coord_flip() +
  scale_fill_manual(values = c("RR" = "#7ba05b",
                               "Outros" = "#1f3e59"),
                    guide = "none") +
  labs(
    title = "Impacto médio da redução para 36h por UF",
    subtitle = "Aumento médio do custo por hora (%) – trabalhadores CLT",
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),   # <- remove toda a grade
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

