usethis::use_git()
usethis::use_github()


library(ggthemes)
library(ggpubr)
library(tidyverse)
library(readr)
library(esquisse)
library(readxl)
# library(rstatix)
 library(sp)
 library(sf)
 library(tmap)
library(leaflet)
library(arsenal)
library(knitr)
library(qwraps2)
options(scipen=999)

install.packages("prettydoc")
install.packages("rmdformats")


#### TABELAS -----------------
TABELA1 <- read_excel("data/tabelas/micro_meso_uf_macro_brasil/6778_condicaodoprodutor+niveis.xlsx", na = "X") 
TABELA1$COD[TABELA1$NIVEL == "BR"] <- 000 #alterando o código do Brasil que está igual ao da região Norte

TABELA2 <- read_excel("data/tabelas/micro_meso_uf_macro_brasil/6878_areaestabelecimentos+niveis.xlsx", na = "X") 
TABELA2$COD[TABELA2$NIVEL == "BR"] <- 000
TABELA2 <- TABELA2 %>% #deletando colunas pra que elas não fiquem duplicadas quando for juntar as tabelas
  select(., -NIVEL) %>% 
  select(., -LOCALIZACAO)

TABELA3 <- read_excel("data/tabelas/micro_meso_uf_macro_brasil/6884_estabelecimentos_pessoalocupadolacoparentesco+niveis.xlsx", na = "X") 
TABELA3$COD[TABELA3$NIVEL == "BR"] <- 000
TABELA3 <- TABELA3 %>%
  select(., -NIVEL) %>% 
  select(., -LOCALIZACAO)

TABELA4 <- read_excel("data/tabelas/micro_meso_uf_macro_brasil/6884_pessoalocupado+niveis.xlsx", na = "X") 
TABELA4$COD[TABELA4$NIVEL == "BR"] <- 000
TABELA4 <- TABELA4 %>%
  select(., -NIVEL) %>% 
  select(., -LOCALIZACAO)

TABELA5 <- read_excel("data/tabelas/micro_meso_uf_macro_brasil/6884_pessoalocupadolacoparentesco+niveis.xlsx", na = "X") 
TABELA5$COD[TABELA5$NIVEL == "BR"] <- 000
TABELA5 <- TABELA5 %>%
  select(., -NIVEL) %>% 
  select(., -LOCALIZACAO)

TABELA6 <- read_excel("data/tabelas/micro_meso_uf_macro_brasil/6898_valordaproducao+niveis.xlsx", na = "X") 
TABELA6$COD[TABELA6$NIVEL == "BR"] <- 000
TABELA6 <- TABELA6 %>%
  select(., -NIVEL) %>% 
  select(., -LOCALIZACAO)

TABELA7 <- read_excel("data/tabelas/micro_meso_uf_macro_brasil/6901_num-estab-rendas+niveis.xlsx", na = "X") 
TABELA7$COD[TABELA7$NIVEL == "BR"] <- 000
TABELA7 <- TABELA7 %>%
  select(., -NIVEL) %>% 
  select(., -LOCALIZACAO)

TABELA8 <- read_excel("data/tabelas/micro_meso_uf_macro_brasil/6901_valorreceitas+niveis.xlsx", na = "X") 
TABELA8$COD[TABELA8$NIVEL == "BR"] <- 000
TABELA8 <- TABELA8 %>%
  select(., -NIVEL) %>% 
  select(., -LOCALIZACAO)

#### PROCEDIMENTOS COM AS TABELAS -----------------

#PARA VER (se) QUAIS LINHAS ESTÃO DUPLICADAS
TABELA1[duplicated(TABELA1$COD) | duplicated(TABELA1$COD, fromLast = TRUE), ]


#Join de todas as tabelas 
todastabelas <- TABELA1 %>% 
  left_join(., TABELA2, by = "COD") %>%
  left_join(., TABELA3, by = "COD") %>%
  left_join(., TABELA4, by = "COD") %>%
  left_join(., TABELA5, by = "COD") %>%
  left_join(., TABELA6, by = "COD") %>%
  left_join(., TABELA7, by = "COD") %>%
  left_join(., TABELA8, by = "COD") 


#Filtragem por recorte geográfico
microrregioes <- todastabelas %>% filter(NIVEL == "MI")
mesorregioes <- todastabelas %>% filter(NIVEL == "ME")
estados <- todastabelas %>% filter(NIVEL == "UF")
macrorregioes <- todastabelas %>% filter(NIVEL == "GR")
brasil <- todastabelas %>% filter(NIVEL == "BR")


#Criando colunas de código do estado e da região 
microrregioes <- microrregioes %>%
  mutate(COD_ESTADO = COD, 
         COD_REGIAO = COD)
microrregioes$COD_ESTADO <- substr(microrregioes$COD_ESTADO, 0, 2)
microrregioes$COD_REGIAO <- substr(microrregioes$COD_REGIAO, 0, 1)
microrregioes$COD_REGIAO[microrregioes$COD_REGIAO == "1"] <- "Norte"
microrregioes$COD_REGIAO[microrregioes$COD_REGIAO == "2"] <- "Nordeste"
microrregioes$COD_REGIAO[microrregioes$COD_REGIAO == "3"] <- "Sudeste"
microrregioes$COD_REGIAO[microrregioes$COD_REGIAO == "4"] <- "Sul"
microrregioes$COD_REGIAO[microrregioes$COD_REGIAO == "5"] <- "Centro-Oeste"


mesorregioes <- mesorregioes %>%
  mutate(COD_ESTADO = COD, 
         COD_REGIAO = COD)
mesorregioes$COD_ESTADO <- substr(mesorregioes$COD_ESTADO, 0, 2)
mesorregioes$COD_REGIAO <- substr(mesorregioes$COD_REGIAO, 0, 1)

#### CÁLCULO DAS PORCENTAGENS -----------------

#Calculando porcentagens

# microrregioes_pct <- microrregioes %>%
#   mutate(
#     prop_REC_PROD_MP = R_VALOR_MP_1 / R_VALOR_T_1 ,
#     prop_REC_OUTR_MP = R_VALOR_MP_2 / R_VALOR_T_2 ,
#     prop_REC_FORA_MP = R_VALOR_MP_302 / R_VALOR_T_302 ,
#     prop_REC_PROG_MP = (R_VALOR_MP_301 + R_VALOR_MP_303 + R_VALOR_MP_304 + R_VALOR_MP_305 + R_VALOR_MP_306 + R_VALOR_MP_307) / (R_VALOR_T_301 + R_VALOR_T_303 + R_VALOR_T_304 + R_VALOR_T_305 + R_VALOR_T_306 + R_VALOR_T_307) ,
#     prop_REC_PROD_AF = R_VALOR_AF_1 / R_VALOR_T_1 ,
#     prop_REC_OUTR_AF = R_VALOR_AF_2 / R_VALOR_T_2 ,
#     prop_REC_FORA_AF = R_VALOR_AF_302 / R_VALOR_T_302 ,
#     prop_REC_PROG_AF = (R_VALOR_AF_301 + R_VALOR_AF_303 + R_VALOR_AF_304 + R_VALOR_AF_305 + R_VALOR_AF_306 + R_VALOR_AF_307) / (R_VALOR_T_301 + R_VALOR_T_303 + R_VALOR_T_304 + R_VALOR_T_305 + R_VALOR_T_306 + R_VALOR_T_307) ,
#     prop_REC_PROD_GP = (R_VALOR_T_1 - (R_VALOR_AF_1 + R_VALOR_MP_1)) / R_VALOR_T_1 ,
#     prop_REC_OUTR_GP = (R_VALOR_T_2 - (R_VALOR_AF_2 + R_VALOR_MP_2)) / R_VALOR_T_2 ,
#     prop_REC_FORA_GP = (R_VALOR_T_302 - (R_VALOR_AF_302 + R_VALOR_MP_302)) / R_VALOR_T_302 ,
#     prop_REC_PROG_GP = ((R_VALOR_T_301 + R_VALOR_T_303 + R_VALOR_T_304 + R_VALOR_T_305 + R_VALOR_T_306 + R_VALOR_T_307) - ((R_VALOR_MP_301 + R_VALOR_MP_303 + R_VALOR_MP_304 + R_VALOR_MP_305 + R_VALOR_MP_306 + R_VALOR_MP_307) + (R_VALOR_AF_301 + R_VALOR_AF_303 + R_VALOR_AF_304 + R_VALOR_AF_305 + R_VALOR_AF_306 + R_VALOR_AF_307))) / (R_VALOR_T_301 + R_VALOR_T_303 + R_VALOR_T_304 + R_VALOR_T_305 + R_VALOR_T_306 + R_VALOR_T_307) ,
#     prop_NEstab_AF = PROD_AF / PROD_T ,
#     prop_NEstab_MP = PROD_MP / PROD_T ,
#     prop_NEstab_GP = (PROD_T - (PROD_MP + PROD_AF)) / PROD_T ,
#     prop_AREA_AF = AREA_AF / AREA_T ,
#     prop_AREA_MP = AREA_MP / AREA_T ,
#     prop_AREA_GP = (AREA_T - (AREA_AF + AREA_MP)) / AREA_T ,
#     prop_OCU_AF = O_AF / O_T ,
#     prop_OCU_MP = O_MP / O_T ,
#     prop_OCU_GP = (O_T - (O_AF + O_MP)) / O_T ,
#     prop_VP_AF = VP_AF / VP_T ,
#     prop_VP_MP = VP_MP / VP_T ,
#     prop_VP_GP = (VP_T - (VP_AF + VP_MP)) / VP_T 
#   )

microrregioes_pct <- microrregioes %>%
  mutate(MP_prop_REC_PROD = R_VALOR_MP_1 / R_VALOR_T_1 ,
         MP_prop_REC_OUTR = R_VALOR_MP_2 / R_VALOR_T_2 ,
         MP_prop_REC_FORA = R_VALOR_MP_302 / R_VALOR_T_302 ,
         MP_prop_REC_PROG = (R_VALOR_MP_301 + R_VALOR_MP_303 + R_VALOR_MP_304 + R_VALOR_MP_305 + R_VALOR_MP_306 + R_VALOR_MP_307) / (R_VALOR_T_301 + R_VALOR_T_303 + R_VALOR_T_304 + R_VALOR_T_305 + R_VALOR_T_306 + R_VALOR_T_307) ,
         
         AF_prop_REC_PROD = R_VALOR_AF_1 / R_VALOR_T_1 ,
         AF_prop_REC_OUTR = R_VALOR_AF_2 / R_VALOR_T_2 ,
         AF_prop_REC_FORA = R_VALOR_AF_302 / R_VALOR_T_302 ,
         AF_prop_REC_PROG = (R_VALOR_AF_301 + R_VALOR_AF_303 + R_VALOR_AF_304 + R_VALOR_AF_305 + R_VALOR_AF_306 + R_VALOR_AF_307) / (R_VALOR_T_301 + R_VALOR_T_303 + R_VALOR_T_304 + R_VALOR_T_305 + R_VALOR_T_306 + R_VALOR_T_307) ,
         
         GP_prop_REC_PROD = ((R_VALOR_T_1 - (R_VALOR_AF_1 + R_VALOR_MP_1)) / R_VALOR_T_1) ,
         GP_prop_REC_OUTR = (R_VALOR_T_2 - (R_VALOR_AF_2 + R_VALOR_MP_2)) / R_VALOR_T_2 ,
         GP_prop_REC_FORA = (R_VALOR_T_302 - (R_VALOR_AF_302 + R_VALOR_MP_302)) / R_VALOR_T_302 ,
         GP_prop_REC_PROG = ((R_VALOR_T_301 + R_VALOR_T_303 + R_VALOR_T_304 + R_VALOR_T_305 + R_VALOR_T_306 + R_VALOR_T_307) - ((R_VALOR_MP_301 + R_VALOR_MP_303 + R_VALOR_MP_304 + R_VALOR_MP_305 + R_VALOR_MP_306 + R_VALOR_MP_307) + (R_VALOR_AF_301 + R_VALOR_AF_303 + R_VALOR_AF_304 + R_VALOR_AF_305 + R_VALOR_AF_306 + R_VALOR_AF_307))) / (R_VALOR_T_301 + R_VALOR_T_303 + R_VALOR_T_304 + R_VALOR_T_305 + R_VALOR_T_306 + R_VALOR_T_307) ,
         
         AF_prop_NEstab = PROD_AF / PROD_T ,
         MP_prop_NEstab = PROD_MP / PROD_T ,
         GP_prop_NEstab = (PROD_T - (PROD_MP + PROD_AF)) / PROD_T ,
         
         AF_prop_AREA = AREA_AF / AREA_T ,
         MP_prop_AREA = AREA_MP / AREA_T ,
         GP_prop_AREA = (AREA_T - (AREA_AF + AREA_MP)) / AREA_T ,
         
         AF_prop_OCU = O_AF / O_T ,
         MP_prop_OCU = O_MP / O_T ,
         GP_prop_OCU = (O_T - (O_AF + O_MP)) / O_T ,
         
         AF_prop_VP = VP_AF / VP_T ,
         MP_prop_VP = VP_MP / VP_T ,
         GP_prop_VP = (VP_T - (VP_AF + VP_MP)) / VP_T ,
         
         MP_prop_REC_TOT = R_VALOR_MP_TOT / R_VALOR_T_TOT,
         AF_prop_REC_TOT = R_VALOR_AF_TOT / R_VALOR_T_TOT,
         GP_prop_REC_TOT = (R_VALOR_T_TOT - ( R_VALOR_AF_TOT + R_VALOR_MP_TOT )) / R_VALOR_T_TOT,
         
         #porcentagem tipo 2, usando o total de tudo
         MP_prop2_REC_PROD = R_VALOR_MP_1 / R_VALOR_T_TOT ,
         MP_prop2_REC_OUTR = R_VALOR_MP_2 / R_VALOR_T_TOT ,
         MP_prop2_REC_FORA = R_VALOR_MP_302 / R_VALOR_T_TOT ,
         MP_prop2_REC_PROG = (R_VALOR_MP_301 + R_VALOR_MP_303 + R_VALOR_MP_304 + R_VALOR_MP_305 + R_VALOR_MP_306 + R_VALOR_MP_307) / R_VALOR_T_TOT ,
         AF_prop2_REC_PROD = R_VALOR_AF_1 / R_VALOR_T_TOT ,
         AF_prop2_REC_OUTR = R_VALOR_AF_2 / R_VALOR_T_TOT ,
         AF_prop2_REC_FORA = R_VALOR_AF_302 / R_VALOR_T_TOT ,
         AF_prop2_REC_PROG = (R_VALOR_AF_301 + R_VALOR_AF_303 + R_VALOR_AF_304 + R_VALOR_AF_305 + R_VALOR_AF_306 + R_VALOR_AF_307) /  R_VALOR_T_TOT ,
         GP_prop2_REC_PROD = (R_VALOR_T_1 - (R_VALOR_AF_1 + R_VALOR_MP_1)) /  R_VALOR_T_TOT ,
         GP_prop2_REC_OUTR = (R_VALOR_T_2 - (R_VALOR_AF_2 + R_VALOR_MP_2)) /  R_VALOR_T_TOT ,
         GP_prop2_REC_FORA = (R_VALOR_T_302 - (R_VALOR_AF_302 + R_VALOR_MP_302)) /  R_VALOR_T_TOT ,
         GP_prop2_REC_PROG = ((R_VALOR_T_301 + R_VALOR_T_303 + R_VALOR_T_304 + R_VALOR_T_305 + R_VALOR_T_306 + R_VALOR_T_307) - ((R_VALOR_MP_301 + R_VALOR_MP_303 + R_VALOR_MP_304 + R_VALOR_MP_305 + R_VALOR_MP_306 + R_VALOR_MP_307) + (R_VALOR_AF_301 + R_VALOR_AF_303 + R_VALOR_AF_304 + R_VALOR_AF_305 + R_VALOR_AF_306 + R_VALOR_AF_307))) / R_VALOR_T_TOT ,
         
         #porcentagem tipo 3, usando o total da AF, da MP e da GP
         
  )

microrregioes_pct <- microrregioes_pct %>%
  select(-(4:162))

#### ANÁLISE DESCRITIVA -----------------

# Multiple variables stored in column names
microrregioes_pct_pivot <- 
  microrregioes_pct %>%
  pivot_longer(
    cols = -c(1:5),
  names_to = c("TIPOLOGIA", ".value"),
  names_pattern = "(..)_(.......+)"
)



#########
colnames(microrregioes_pct_pivot3)
microrregioes_pct_pivot3 <- microrregioes_pct_pivot
microrregioes_pct_pivot3$TIPOLOGIA <- factor(microrregioes_pct_pivot3$TIPOLOGIA, levels = c("GP", "MP", "AF"))

# REC_TOT - pct tipo 1 ####
REC_TOT <- 
  microrregioes_pct_pivot3 %>% filter(!is.na(prop_REC_TOT)) %>% 
  ggplot(aes(x = TIPOLOGIA, y = prop_REC_TOT, color = TIPOLOGIA, label = LOCALIZACAO)) +
  labs(x = "", y = "Porcentagem de participação", title = "Receitas obtidas nos estabelecimentos/pelos produtores") +
  scale_color_brewer(palette = "Dark2", direction=-1, guide = "none") +
  theme_light() +
  theme(legend.position='none',
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.spacing = unit(1, "lines")) +
  facet_grid(vars(), vars(COD_REGIAO)) +
  geom_violin(fill = "gray80", size = 0.5, alpha = .5) +
  geom_jitter(alpha = .25, width = .3) +
  stat_summary(fun= mean, fun.min=mean, fun.max=mean, geom="crossbar", width=0.8, size = 0.2, color="black") +
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, by = 0.25), 
                     labels=function(x) paste0(x*100,"%")) +
  scale_x_discrete(labels=c("Outros (patronal)", "Pronamp", "Agricultura Familiar")) +
  coord_flip()


ggplotly(REC_TOT, tooltip = c("label")) %>%
  layout(margin=list(t = 115),
    title = list(text = paste0('Receitas obtidas nos estabelecimentos/pelos produtores',
                                    '<br>',
                                    '<sup>',
                                    'Participação por tipologia de propriedade nas receitas nas microrregiões',
                                    '</sup>')))


# REC_PROD - pct tipo 1 ####
REC_PROD <- 
  microrregioes_pct_pivot3 %>% filter(!is.na(prop_REC_PROD)) %>% ggplot(aes(x = TIPOLOGIA, y = prop_REC_PROD,
                 color = TIPOLOGIA, label = LOCALIZACAO)) +
  labs(x = "", y = "Porcentagem de participação", title = "Receitas de produção (animal e vegetal)", subtitle = "Participação por tipologia nas receitas de produção nas microrregiões") +
  scale_color_brewer(palette = "Dark2", direction=-1, guide = "none") +
  theme_light() +
  theme(legend.position = 'none',
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.spacing = unit(1, "lines")) +
  facet_grid(vars(), vars(COD_REGIAO)) +
  geom_violin(fill = "gray80", size = 0.5, alpha = .5) +
  geom_jitter(alpha = .25, width = .3) +
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, by = 0.25), 
                     labels=function(x) paste0(x*100,"%")) +
  scale_x_discrete(labels=c("Outros (patronal)", "Pronamp", "Agricultura Familiar")) +
  coord_flip()

# REC_OUTR - pct tipo 1 ####
REC_OUTR <- 
  microrregioes_pct_pivot3 %>% filter(!is.na(prop_REC_OUTR)) %>% 
  ggplot(aes(x = TIPOLOGIA, y = prop_REC_OUTR, color = TIPOLOGIA, label = LOCALIZACAO)) +
  labs(x = "", y = "Porcentagem de participação", title = "Outras* receitas do estabelecimento", subtitle = "Participação por tipologia outras* receitas do estabelecimento", caption = "* Outras receitas do estabelecimento: desinvestimentos, serviço de turismo rural, exploração mineral, atividade de artesanato, tecelagem, etc") +
  scale_color_brewer(palette = "Dark2", direction=-1, guide = "none") +
  theme_light() +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(color = "grey"),
        panel.spacing = unit(1, "lines")) +
  facet_grid(vars(), vars(COD_REGIAO)) +
  geom_violin(fill = "gray80", size = 0.5, alpha = .5) +
  geom_jitter(alpha = .25, width = .3) +
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, by = 0.25), 
                     labels=function(x) paste0(x*100,"%")) +
  scale_x_discrete(labels=c("Outros (patronal)", "Pronamp", "Agricultura Familiar")) +
  coord_flip()


# REC_FORA - pct tipo 1 ####
REC_FORA <- 
  microrregioes_pct_pivot3 %>% filter(!is.na(prop_REC_FORA)) %>% 
  ggplot(aes(x = TIPOLOGIA, y = prop_REC_FORA, color = TIPOLOGIA, label = LOCALIZACAO)) +
  labs(x = "", y = "Porcentagem de participação", title = "Rendas obtidas em atividades fora do estabelecimento", subtitle = "Participação por tipologia nas rendas obtidas em atividades fora do estabelecimento") +
  scale_color_brewer(palette = "Dark2", direction=-1, guide = "none") +
  theme_light() +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.spacing = unit(1, "lines")) +
  facet_grid(vars(), vars(COD_REGIAO)) +
  geom_violin(fill = "gray80", size = 0.5, alpha = .5) +
  geom_jitter(alpha = .25, width = .3) +
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, by = 0.25), 
                     labels=function(x) paste0(x*100,"%")) +
  scale_x_discrete(labels=c("Outros (patronal)", "Pronamp", "Agricultura Familiar")) +
  coord_flip()


# REC_PROG - pct tipo 1 ####
REC_PROG <- 
  microrregioes_pct_pivot3 %>% filter(!is.na(prop_REC_PROG)) %>% 
  ggplot(aes(x = TIPOLOGIA, y = prop_REC_PROG, color = TIPOLOGIA, label = LOCALIZACAO)) +
  labs(x = "", y = "Porcentagem de participação ", title = "Outras receitas do produtor", subtitle = "Participação por tipologia em outras receitas do produtor (programas sociais/aposentadorias/premios)") +
  scale_color_brewer(palette = "Dark2", direction=-1, guide = "none") +
  theme_light() +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.spacing = unit(1, "lines")) +
  facet_grid(vars(), vars(COD_REGIAO)) +
  geom_violin(fill = "gray80", size = 0.5, alpha = .5) +
  geom_jitter(alpha = .25, width = .3) +
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, by = 0.25), 
                     labels=function(x) paste0(x*100,"%")) +
  scale_x_discrete(labels=c("Outros (patronal)", "Pronamp", "Agricultura Familiar")) +
  coord_flip()




# REC_PROD - pct tipo 2 ####
REC_PROD2 <- 
  microrregioes_pct_pivot3 %>% filter(!is.na(prop2_REC_PROD)) %>% ggplot(aes(x = TIPOLOGIA, y = prop2_REC_PROD,
                                                                            color = TIPOLOGIA, label = LOCALIZACAO)) +
  labs(x = "", y = "Porcentagem de participação", title = "Receitas de produção (animal e vegetal)", subtitle = "Participação por tipologia nas receitas de produção nas microrregiões") +
  scale_color_brewer(palette = "Dark2", direction=-1, guide = "none") +
  theme_light() +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.spacing = unit(1, "lines")) +
  facet_grid(vars(), vars(COD_REGIAO)) +
  geom_violin(fill = "gray80", size = 0.5, alpha = .5) +
  geom_jitter(alpha = .25, width = .3) +
  scale_y_continuous(#limits=c(0,1), breaks=seq(0, 1, by = 0.25), 
    labels=function(x) paste0(x*100,"%")) +
  scale_x_discrete(labels=c("Outros (patronal)", "Pronamp", "Agricultura Familiar")) +
  coord_flip()

# REC_OUTR - pct tipo 2 ####
REC_OUTR2 <- 
  microrregioes_pct_pivot3 %>% filter(!is.na(prop2_REC_OUTR)) %>% 
  ggplot(aes(x = TIPOLOGIA, y = prop2_REC_OUTR, color = TIPOLOGIA, label = LOCALIZACAO)) +
  labs(x = "", y = "Porcentagem de participação", title = "Outras* receitas do estabelecimento", subtitle = "Participação por tipologia outras* receitas do estabelecimento", caption = "* Outras receitas do estabelecimento: desinvestimentos, serviço de turismo rural, exploração mineral, atividade de artesanato, tecelagem, etc") +
  scale_color_brewer(palette = "Dark2", direction=-1, guide = "none") +
  theme_light() +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(color = "grey"),
        panel.spacing = unit(1, "lines")) +
  facet_grid(vars(), vars(COD_REGIAO)) +
  geom_violin(fill = "gray80", size = 0.5, alpha = .5) +
  geom_jitter(alpha = .25, width = .3) +
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, by = 0.25), 
                     labels=function(x) paste0(x*100,"%")) +
  scale_x_discrete(labels=c("Outros (patronal)", "Pronamp", "Agricultura Familiar")) +
  coord_flip()


# REC_FORA - pct tipo 2 ####
REC_FORA2 <- 
  microrregioes_pct_pivot3 %>% filter(!is.na(prop2_REC_FORA)) %>% 
  ggplot(aes(x = TIPOLOGIA, y = prop2_REC_FORA, color = TIPOLOGIA, label = LOCALIZACAO)) +
  labs(x = "", y = "Porcentagem de participação", title = "Rendas obtidas em atividades fora do estabelecimento", subtitle = "Participação por tipologia nas rendas obtidas em atividades fora do estabelecimento") +
  scale_color_brewer(palette = "Dark2", direction=-1, guide = "none") +
  theme_light() +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.spacing = unit(1, "lines")) +
  facet_grid(vars(), vars(COD_REGIAO)) +
  geom_violin(fill = "gray80", size = 0.5, alpha = .5) +
  geom_jitter(alpha = .25, width = .3) +
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, by = 0.25), 
                     labels=function(x) paste0(x*100,"%")) +
  scale_x_discrete(labels=c("Outros (patronal)", "Pronamp", "Agricultura Familiar")) +
  coord_flip()


# REC_PROG - pct tipo 2 ####
REC_PROG2 <- 
  microrregioes_pct_pivot3 %>% filter(!is.na(prop2_REC_PROG)) %>% 
  ggplot(aes(x = TIPOLOGIA, y = prop2_REC_PROG, color = TIPOLOGIA, label = LOCALIZACAO)) +
  labs(x = "", y = "Porcentagem de participação ", title = "Outras receitas do produtor", subtitle = "Participação por tipologia em outras receitas do produtor (programas sociais/aposentadorias/premios)") +
  scale_color_brewer(palette = "Dark2", direction=-1, guide = "none") +
  theme_light() +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.spacing = unit(1, "lines")) +
  facet_grid(vars(), vars(COD_REGIAO)) +
  geom_violin(fill = "gray80", size = 0.5, alpha = .5) +
  geom_jitter(alpha = .25, width = .3) +
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, by = 0.25), 
                     labels=function(x) paste0(x*100,"%")) +
  scale_x_discrete(labels=c("Outros (patronal)", "Pronamp", "Agricultura Familiar")) +
  coord_flip()



### Número de estabelecimentos agropecuários ### 





graf ESTABELECIMENTOS - Prop de estabelecimentos, message=FALSE, warning=FALSE, include=FALSE}
NEstab <- 
  microrregioes_pct_pivot3 %>% filter(!is.na(prop_NEstab)) %>% 
  ggplot(aes(x = TIPOLOGIA, y = prop_NEstab, color = TIPOLOGIA, label = LOCALIZACAO)) +
  labs(x = "", y = "Participação ", title = "Proporção de estabelecimentos", subtitle = "Participação por tipologia no número de estabelecimentos") +
  scale_color_brewer(palette = "Dark2", direction=-1, guide = "none") +
  theme_light() +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.spacing = unit(1, "lines")) +
  facet_grid(vars(), vars(COD_REGIAO)) +
  geom_violin(fill = "gray80", size = 0.5, alpha = .5) +
  geom_jitter(alpha = .25, width = .3) +   stat_summary(fun= mean, fun.min=mean, fun.max=mean, geom="crossbar", width=0.8, size = 0.2, color="black") +
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, by = 0.3), 
                     labels=function(x) paste0(x*100,"%")) +
  scale_x_discrete(labels=c("Outros (patronal)", "Pronamp", "Agricultura Familiar")) +
  coord_flip()



grafPLOT ESTABELECIMENTOS - Prop de estabelecimentos, , out.width='100%'}
gg_NEstab <- ggplotly(NEstab, tooltip = c("label")) %>%
  layout(margin = margens,
         title = list(text = paste0('Proporção de estabelecimentos',
                                    '<br>',
                                    '<sup>',
                                    'Participação por tipologia no número de estabelecimentos',
                                    '</sup>')))

gg_NEstab


mapa ESTABELECIMENTOS - Prop de estabelecimentos, message=FALSE, warning=FALSE, include=FALSE}
## AF
tm_AF_NEstab <-
  SHP_Micro_join[!is.na(SHP_Micro_join$prop_NEstab),] %>%
  filter(prop_NEstab >= 0) %>%
  filter(TIPOLOGIA == "AF") %>%
  tm_shape() +
  tm_fill(col = "prop_NEstab", 
          title = "Proporção de estabelecimentos<br/>- AF", 
          id = "LOCALIZACAO", palette="BuGn", 
          legend.format=list(fun=function(x) paste0(formatC(x*100, digits=0, format="f"), "%"))) +
  tm_borders(lwd = 0.2) +
  #tm_facets(by = "TIPOLOGIA", nrow = 1, sync = TRUE, free.coords = TRUE) +
  tm_view(view.legend.position = c("left", "bottom")) +
  tm_shape(SHP_Estados) +
  tm_borders(lwd = 1) 

## MP
tm_MP_NEstab <-
  SHP_Micro_join[!is.na(SHP_Micro_join$prop_NEstab),] %>%
  filter(prop_NEstab >= 0) %>%
  filter(TIPOLOGIA == "MP") %>%  
  tm_shape() +
  tm_fill(col = "prop_NEstab", title = "Proporção de estabelecimentos -<br/>Pronamp", id = "LOCALIZACAO", palette="Oranges",
          legend.format=list(fun=function(x) paste0(formatC(x*100, digits=0, format="f"), "%"))) +
  tm_borders(lwd = 0.2) +
  #tm_facets(by = "TIPOLOGIA", nrow = 1, sync = TRUE, free.coords = TRUE) +
  tm_view(view.legend.position = c("left", "bottom")) +
  tm_shape(SHP_Estados) +
  tm_borders(lwd = 1) 

## GP
tm_GP_NEstab <-
  SHP_Micro_join[!is.na(SHP_Micro_join$prop_NEstab),] %>%
  filter(prop_NEstab >= 0) %>%
  filter(TIPOLOGIA == "GP") %>%
  tm_shape() +
  tm_fill(col = "prop_NEstab", title = "Proporção de estabelecimentos -<br/>Outros (patronal)", id = "LOCALIZACAO", palette="Purples",
          legend.format=list(fun=function(x) paste0(formatC(x*100, digits=0, format="f"), "%"))) +
  tm_borders(lwd = 0.2) +
  #tm_facets(by = "TIPOLOGIA", nrow = 1, sync = TRUE, free.coords = TRUE) +
  tm_view(view.legend.position = c("left", "bottom")) +
  tm_shape(SHP_Estados) +
  tm_borders(lwd = 1) 



mapaPLOT ESTABELECIMENTOS - Prop de estabelecimentos, }
tmap_arrange(tm_AF_NEstab, tm_MP_NEstab, tm_GP_NEstab, 
             sync = TRUE, 
             nrow = 1)






### **Var 3** - Área total dos estabelecimentos agropecuários {.tabset .tabset-fade}

graf ESTABELECIMENTOS - Prop área dos estabelecimentos, message=FALSE, warning=FALSE, include=FALSE}
AREA <- 
  microrregioes_pct_pivot3 %>% filter(!is.na(prop_AREA)) %>% 
  ggplot(aes(x = TIPOLOGIA, y = prop_AREA, color = TIPOLOGIA, label = LOCALIZACAO)) +
  labs(x = "", y = "Participação ", title = "Proporção de área total dos estabelecimentos", subtitle = "Participação por tipologia na área total dos estabelecimentos") +
  scale_color_brewer(palette = "Dark2", direction=-1, guide = "none") +
  theme_light() +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.spacing = unit(1, "lines")) +
  facet_grid(vars(), vars(COD_REGIAO)) +
  geom_violin(fill = "gray80", size = 0.5, alpha = .5) +
  geom_jitter(alpha = .25, width = .3) +   stat_summary(fun= mean, fun.min=mean, fun.max=mean, geom="crossbar", width=0.8, size = 0.2, color="black") +
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, by = 0.3), 
                     labels=function(x) paste0(x*100,"%")) +
  scale_x_discrete(labels=c("Outros (patronal)", "Pronamp", "Agricultura Familiar")) +
  coord_flip()



grafPLOT ESTABELECIMENTOS - Prop área dos estabelecimentos, , out.width='100%'}
gg_AREA <- ggplotly(AREA, tooltip = c("label")) %>%
  layout(margin = margens,
         title = list(text = paste0('Proporção de área total dos estabelecimentos',
                                    '<br>',
                                    '<sup>',
                                    'Participação por tipologia na área total dos estabelecimentos',
                                    '</sup>')))

gg_AREA


mapa ESTABELECIMENTOS - Prop área dos estabelecimentos, message=FALSE, warning=FALSE, include=FALSE}
## AF
tm_AF_AREA <-
  SHP_Micro_join[!is.na(SHP_Micro_join$prop_AREA),] %>%
  filter(prop_AREA >= 0) %>%
  filter(TIPOLOGIA == "AF") %>%
  tm_shape() +
  tm_fill(col = "prop_AREA", 
          title = "Proporção de área total<br/>dos estabelecimentos<br/>- AF", 
          id = "LOCALIZACAO", palette="BuGn", 
          legend.format=list(fun=function(x) paste0(formatC(x*100, digits=0, format="f"), "%"))) +
  tm_borders(lwd = 0.2) +
  #tm_facets(by = "TIPOLOGIA", nrow = 1, sync = TRUE, free.coords = TRUE) +
  tm_view(view.legend.position = c("left", "bottom")) +
  tm_shape(SHP_Estados) +
  tm_borders(lwd = 1) 

## MP
tm_MP_AREA <-
  SHP_Micro_join[!is.na(SHP_Micro_join$prop_AREA),] %>%
  filter(prop_AREA >= 0) %>%
  filter(TIPOLOGIA == "MP") %>%  
  tm_shape() +
  tm_fill(col = "prop_AREA", title = "Proporção de área total<br/>dos estabelecimentos -<br/>Pronamp", id = "LOCALIZACAO", palette="Oranges",
          legend.format=list(fun=function(x) paste0(formatC(x*100, digits=0, format="f"), "%"))) +
  tm_borders(lwd = 0.2) +
  #tm_facets(by = "TIPOLOGIA", nrow = 1, sync = TRUE, free.coords = TRUE) +
  tm_view(view.legend.position = c("left", "bottom")) +
  tm_shape(SHP_Estados) +
  tm_borders(lwd = 1) 

## GP
tm_GP_AREA <-
  SHP_Micro_join[!is.na(SHP_Micro_join$prop_AREA),] %>%
  filter(prop_AREA >= 0) %>%
  filter(TIPOLOGIA == "GP") %>%
  tm_shape() +
  tm_fill(col = "prop_AREA", title = "Proporção de área total<br/>dos estabelecimentos -<br/>Outros (patronal)", id = "LOCALIZACAO", palette="Purples",
          legend.format=list(fun=function(x) paste0(formatC(x*100, digits=0, format="f"), "%"))) +
  tm_borders(lwd = 0.2) +
  #tm_facets(by = "TIPOLOGIA", nrow = 1, sync = TRUE, free.coords = TRUE) +
  tm_view(view.legend.position = c("left", "bottom")) +
  tm_shape(SHP_Estados) +
  tm_borders(lwd = 1) 



mapaPLOT ESTABELECIMENTOS - Prop área dos estabelecimentos, }
tmap_arrange(tm_AF_AREA, tm_MP_AREA, tm_GP_AREA, 
             sync = TRUE, 
             nrow = 1)





####  **Var 4** - Pessoal ocupado nos estabelecimentos agropecuários  #### 
graf ESTABELECIMENTOS - Pessoal ocupado nos estabelecimentos, message=FALSE, warning=FALSE, include=FALSE}
OCU <- 
  microrregioes_pct_pivot3 %>% filter(!is.na(prop_OCU)) %>% 
  ggplot(aes(x = TIPOLOGIA, y = prop_OCU, color = TIPOLOGIA, label = LOCALIZACAO)) +
  labs(x = "", y = "Participação ", title = "Proporção de pessoal ocupado nos estabelecimentos", subtitle = "Participação por tipologia no número de ocupados nos estabelecimentos") +
  scale_color_brewer(palette = "Dark2", direction=-1, guide = "none") +
  theme_light() +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.spacing = unit(1, "lines")) +
  facet_grid(vars(), vars(COD_REGIAO)) +
  geom_violin(fill = "gray80", size = 0.5, alpha = .5) +
  geom_jitter(alpha = .25, width = .3) +   
  stat_summary(fun= mean, fun.min=mean, fun.max=mean, geom="crossbar", width=0.8, size = 0.2, color="black") +
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, by = 0.3), 
                     labels=function(x) paste0(x*100,"%")) +
  scale_x_discrete(labels=c("Outros (patronal)", "Pronamp", "Agricultura Familiar")) +
  coord_flip()



grafPLOT ESTABELECIMENTOS - Pessoal ocupado nos estabelecimentos, , out.width='100%'}
gg_OCU <- ggplotly(OCU, tooltip = c("label")) %>%
  layout(margin = margens,
         title = list(text = paste0('Proporção de pessoal ocupado nos estabelecimentos',
                                    '<br>',
                                    '<sup>',
                                    'Participação por tipologia no número de ocupados nos estabelecimentos',
                                    '</sup>')))

gg_OCU


#### mapa ESTABELECIMENTOS - Pessoal ocupado nos estabelecimentos  #### 
## AF
tm_AF_OCU <-
  SHP_Micro_join[!is.na(SHP_Micro_join$prop_OCU),] %>%
  filter(prop_OCU >= 0) %>%
  filter(TIPOLOGIA == "AF") %>%
  tm_shape() +
  tm_fill(col = "prop_OCU", 
          title = "Proporção de pessoal ocupado<br/>nos estabelecimentos<br/>- AF", 
          id = "LOCALIZACAO", palette="BuGn", 
          legend.format=list(fun=function(x) paste0(formatC(x*100, digits=0, format="f"), "%"))) +
  tm_borders(lwd = 0.2) +
  #tm_facets(by = "TIPOLOGIA", nrow = 1, sync = TRUE, free.coords = TRUE) +
  tm_view(view.legend.position = c("left", "bottom")) +
  tm_shape(SHP_Estados) +
  tm_borders(lwd = 1) 

## MP
tm_MP_OCU <-
  SHP_Micro_join[!is.na(SHP_Micro_join$prop_OCU),] %>%
  filter(prop_OCU >= 0) %>%
  filter(TIPOLOGIA == "MP") %>%  
  tm_shape() +
  tm_fill(col = "prop_OCU", title = "Proporção de pessoal ocupado<br/>nos estabelecimentos -<br/>Pronamp", id = "LOCALIZACAO", palette="Oranges",
          legend.format=list(fun=function(x) paste0(formatC(x*100, digits=0, format="f"), "%"))) +
  tm_borders(lwd = 0.2) +
  #tm_facets(by = "TIPOLOGIA", nrow = 1, sync = TRUE, free.coords = TRUE) +
  tm_view(view.legend.position = c("left", "bottom")) +
  tm_shape(SHP_Estados) +
  tm_borders(lwd = 1) 

## GP
tm_GP_OCU <-
  SHP_Micro_join[!is.na(SHP_Micro_join$prop_OCU),] %>%
  filter(prop_OCU >= 0) %>%
  filter(TIPOLOGIA == "GP") %>%
  tm_shape() +
  tm_fill(col = "prop_OCU", title = "Proporção de pessoal ocupado<br/>nos estabelecimentos -<br/>Outros (patronal)", id = "LOCALIZACAO", palette="Purples",
          legend.format=list(fun=function(x) paste0(formatC(x*100, digits=0, format="f"), "%"))) +
  tm_borders(lwd = 0.2) +
  #tm_facets(by = "TIPOLOGIA", nrow = 1, sync = TRUE, free.coords = TRUE) +
  tm_view(view.legend.position = c("left", "bottom")) +
  tm_shape(SHP_Estados) +
  tm_borders(lwd = 1) 




tmap_arrange(tm_AF_OCU, tm_MP_OCU, tm_GP_OCU, 
             sync = TRUE, 
             nrow = 1)






### **Var 5** - Valor da produção nos estabelecimentos agropecuários #### 

VP <- 
  microrregioes_pct_pivot3 %>% filter(!is.na(prop_VP)) %>% 
  ggplot(aes(x = TIPOLOGIA, y = prop_VP, color = TIPOLOGIA, label = LOCALIZACAO)) +
  labs(x = "", y = "Participação ", title = "Proporção de valor da produção dos estabelecimentos", subtitle = "Participação por tipologia no valor da produção dos estabelecimentos") +
  scale_color_brewer(palette = "Dark2", direction=-1, guide = "none") +
  theme_light() +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.spacing = unit(1, "lines")) +
  facet_grid(vars(), vars(COD_REGIAO)) +
  geom_violin(fill = "gray80", size = 0.5, alpha = .5) +
  geom_jitter(alpha = .25, width = .3) +   stat_summary(fun= mean, fun.min=mean, fun.max=mean, geom="crossbar", width=0.8, size = 0.2, color="black") +
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, by = 0.3), 
                     labels=function(x) paste0(x*100,"%")) +
  scale_x_discrete(labels=c("Outros (patronal)", "Pronamp", "Agricultura Familiar")) +
  coord_flip()


grafPLOT ESTABELECIMENTOS - Valor da produção nos estabelecimentos, , out.width='100%'}
gg_VP <- ggplotly(VP, tooltip = c("label")) %>%
  layout(margin = margens,
         title = list(text = paste0('Proporção de valor da produção dos estabelecimentos',
                                    '<br>',
                                    '<sup>',
                                    'Participação por tipologia no valor da produção dos estabelecimentos',
                                    '</sup>')))

gg_VP






####### #######

microrregioes_pct_pivot3 %>%
  select(TIPOLOGIA, prop2_REC_PROG) %>%
  filter(TIPOLOGIA == "AF") %>%
  summarise(mean = mean(prop2_REC_PROG, na.rm = TRUE))



our_summary1 <-
  list("Rendas do estabelecimento/produtor" =
         list("min"       = ~ min(prop_REC_TOT),
              "max"       = ~ max(prop_REC_TOT),
              "média"  = ~ mean(prop_REC_TOT, na.rm = TRUE)),
       "Estabelecimentos" =
         list("min"       = ~ min(prop_NEstab),
              "median"    = ~ median(prop_NEstab),
              "max"       = ~ max(prop_NEstab),
              "mean (sd)" = ~ mean(prop_NEstab, na.rm = TRUE)),
       "Área dos estabelecimentos" =
         list("min"       = ~ min(prop_AREA),
              "max"       = ~ max(prop_AREA),
              "mean (sd)" = ~ mean(prop_AREA, na.rm = TRUE))
  )

install.packages("qwraps2") 
library(qwraps2)

### By number of Cylinders
by_cyl <- summary_table(dplyr::group_by(microrregioes_pct_pivot3, "COD_REGIAO"), our_summary1)
by_cyl


by_cyl_am <- summary_table(microrregioes_pct_pivot3, summaries = our_summary1, by = c("COD_REGIAO", "TIPOLOGIA"))
by_cyl_am




teste <- microrregioes_pct_pivot3 %>%
  group_by(TIPOLOGIA, COD_REGIAO) %>%
  summarise(
            Min = (100*first(prop_AREA)),
            Media = (100*mean(prop_AREA, na.rm = TRUE)),
            Max = (100*max(prop_AREA, na.rm = TRUE)),
  )

kable(teste)


summary_table(teste[, c("Min", "Media", "Max")])




### ---------------------

ggarrange(REC_PROD, REC_PROD2, REC_OUTR, REC_OUTR2, REC_FORA, REC_FORA2, REC_PROG, REC_PROG2, ncol = 2)
ggarrange(REC_PROD, REC_OUTR, REC_FORA, REC_PROG, ncol = 1)
ggarrange(REC_PROD2, REC_OUTR2, REC_FORA2, REC_PROG2, ncol = 1)





microteste <- microrregioes %>%
  select(starts_with("R_VALOR_AF"))





### Composição das rendas da produção ####



SHP_Microrregioes <- read_sf("data/shapes/BR_Microrregioes_2019_simplif.shp")
SHP_Estados <- read_sf("data/shapes/estados_2010_simplif.shp")

st_crs(SHP_Microrregioes)
tmap_mode("view")
# tmap_mode("plot") #> tmap mode set to plotting


# Mudando o nome pra ficar igual ao da tabela
names(SHP_Microrregioes)[1] <- "COD"

# Juntando o shape e a tabela
SHP_Micro_join <- SHP_Microrregioes %>%
  left_join(., microrregioes_pct_pivot3, by = "COD")


# Mapa

tm_AF_REC_PROD <-
SHP_Micro_join[!is.na(SHP_Micro_join$prop_REC_PROD),] %>%
  filter(prop_REC_PROD > 0) %>%
  filter(TIPOLOGIA == "AF") %>%
tm_shape() +
  tm_fill(col = "prop_REC_PROD", 
          title = "Receitas da produção<br/>do estabelecimento - AF", 
          id = "LOCALIZACAO", palette="BuGn", 
          legend.format=list(fun=function(x) paste0(formatC(x*100, digits=0, format="f"), "%"))) +
  tm_borders(lwd = 0.2) +
  #tm_facets(by = "TIPOLOGIA", nrow = 1, sync = TRUE, free.coords = TRUE) +
  tm_view(view.legend.position = c("left", "bottom")) +
tm_shape(SHP_Estados) +
  tm_borders(lwd = 1) 


tm_MP_REC_PROD <-
SHP_Micro_join[!is.na(SHP_Micro_join$prop_REC_PROD),] %>%
  filter(prop_REC_PROD > 0) %>%
  filter(TIPOLOGIA == "MP") %>%  
tm_shape() +
  tm_fill(col = "prop_REC_PROD", title = "Receitas da produção<br/>do estabelecimento -<br/>Pronamp", id = "LOCALIZACAO", palette="Oranges") +
  tm_borders(lwd = 0.2) +
  #tm_facets(by = "TIPOLOGIA", nrow = 1, sync = TRUE, free.coords = TRUE) +
  tm_view(view.legend.position = c("left", "bottom")) +
  tm_shape(SHP_Estados) +
  tm_borders(lwd = 1) 

tm_GP_REC_PROD <-
SHP_Micro_join[!is.na(SHP_Micro_join$prop_REC_PROD),] %>%
  filter(prop_REC_PROD > 0) %>%
  filter(TIPOLOGIA == "GP") %>%
tm_shape() +
  tm_fill(col = "prop_REC_PROD", title = "Receitas da produção<br/>do estabelecimento -<br/>Outros (patronal)", id = "LOCALIZACAO", palette="Purples") +
  tm_borders(lwd = 0.2) +
  #tm_facets(by = "TIPOLOGIA", nrow = 1, sync = TRUE, free.coords = TRUE) +
  tm_view(view.legend.position = c("left", "bottom")) +
  tm_shape(SHP_Estados) +
  tm_borders(lwd = 1) 


tmap_arrange(tm_AF_REC_PROD, tm_MP_REC_PROD, tm_GP_REC_PROD, 
             sync = TRUE, 
             nrow = 1)









# make dataset with a few variables to summarize
trial2 <- microrregioes %>% select(VP_T, VP_AF, VP_MP)

# summarize the data with our package
table1 <- gtsummary::tbl_summary(trial2)
