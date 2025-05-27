library(tidyverse)

df <- Consolidado_consorcios_2020_2024

df <- df |>
        mutate(Codigo_do_segmento = recode(Codigo_do_segmento, 
                                           '1' = 'Bens Imóveis',
                                           '2' = 'Meios de transporte industriais e comerciais',
                                           '3' = 'Demais veículos automotores',
                                           '4' = 'Motocicletas e motonetas',
                                           '5'= 'Outros bens móveis duráveis',
                                           '6' = 'Serviços turísticos'
                                           ))

colnames(df)[4] <- "Segmento"
colnames(df)[colnames(df) == "Nome_da_Administradora"] <- "Administradora"
  
adesoes_estado <- df |>
  select(Data_base, Administradora,Segmento, UF, Adesoes)

# Inclui todos os consórcios
adesoes_estado |>
  group_by(Administradora, Segmento, UF) |>
  summarise(Adesoes = sum(Adesoes))

maiores_adesoes <- adesoes_estado |>
  group_by(Administradora) |>
  summarise(Adesoes = sum(Adesoes)) |>
  arrange(desc(Adesoes)) |>
  head(4)

# Adesoes por estado das 4 maiores administradoras
adesoes_estado <- adesoes_estado |>
  filter(Administradora %in% maiores_adesoes$Administradora)
#View(adesoes_estado)

g_maiores_adm <- ggplot(data = maiores_adesoes, aes(x = Administradora, y = Adesoes)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_y_continuous(labels = scales::comma) +
  geom_text(aes(label = Adesoes), vjust = 1.6, color = "white", size = 2.5) +
  ggtitle("Administradoras com maior adesão - 2020 a 2024") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8),
        plot.title = element_text(size = 10, hjust = 0.5),
        panel.background = element_blank())
g_maiores_adm
  
# Adesoes por estado das 4 maiores administradoras
adm_estados <- adesoes_estado |>
  group_by(Administradora, UF) |>
  summarise(Adesoes = sum(Adesoes))

#honda <- adm_estados |>
#  filter(Administradora == "ADM CONS NAC HONDA LTDA")
#View(honda)

## Mapas de atuação dos maiores consórcios por região
#install.packages('dplyr')
#install.packages('geobr')
#install.packages('gridExtra')
#install.packages('svglite')
library(dplyr)
library(geobr)
library(sf)
library(tidyverse)
library(gridExtra)
library(scales)
library(svglite)

mapa_brasil <- read_state('all', year=2020)
colnames(adm_estados)[colnames(adm_estados) == 'UF'] <- 'abbrev_state'
base_mapa <- mapa_brasil %>%
  left_join(adm_estados, by = "abbrev_state")

plot_map <- function(adm) {
  base_filtrada <- base_mapa %>% filter(Administradora == adm)
  ggplot() +
    geom_sf(data = base_filtrada, aes(fill = Adesoes), alpha = 0.7) +
    scale_fill_gradient2(
      low = 'white', 
      high = 'red', 
      na.value = 'grey', 
      labels = label_number(scale = 1, big.mark = ".", decimal.mark = ",")
    ) +
    labs(title = paste("Mapa -", adm, "2020 a 2024"), fill = "Adesões") +
    theme_void()+
    theme(plot.title = element_text(size = 10, hjust = 0.5))
}

adms <- unique(base_mapa$Administradora)
for (adm in adms) {
  g <- plot_map(adm)
  ggsave(filename = paste0("mapa_", adm, "2020-2024", ".jpeg"), plot = g, width = 8, height = 6)
}

## Adesões por segmento
adesoes_segmento <- adesoes_estado |>
  group_by(Segmento) |> summarise(Adesoes = sum(Adesoes)) |>
  ggplot(aes(y = Segmento, x = Adesoes), alpha=0.2) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_x_continuous(labels = scales::comma) +
  ggtitle("Adesões a consórcios por segmento - 2020 a 2024")+
  theme_minimal()+
  theme(axis.text.y=element_text(size=8),
        plot.title = element_text(size = 10, hjust = 0.5),
        panel.background = element_blank(),
        axis.title.y = element_blank())
adesoes_segmento

## Evoluções de adesão por período
adesoes <- df |>
  group_by(Data_base) |> summarise(Adesoes = sum(Adesoes))

adesoes <- adesoes |> 
  mutate(Label = ifelse(row_number() == 1 | row_number() == n(), Adesoes, NA))

adesoes_plot <- ggplot(data = adesoes, aes(x = Data_base, y = Adesoes)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = Label), vjust = -0.5, size = 3) +  # Exibe rótulos no primeiro e último ponto
  labs(x = "Período", y = "Adesões") +
  scale_y_continuous(labels = scales::comma) +  # Formata os valores do eixo Y sem notação científica
  ggtitle("Adesões a consórcios - 2020 a 2024")+
  theme_minimal()+
  theme(plot.title = element_text(size=10, hjust=0.5),
        panel.background = element_blank(),
        axis.title.y = element_blank())
adesoes_plot

## Aumento percentual de adesão a consórcios
adesoes <- df |>
  group_by(Data_base) |> summarise(Adesoes = sum(Adesoes))
perc <- round((((adesoes[nrow(adesoes),2] - adesoes[1,2])/adesoes[1,2])*100), 2)
print(perc)

## Administradoras por período
adm_periodo <- df |> select(Data_base, Administradora) |>
  group_by(Administradora, Data_base) |>
  distinct()

qntd_adm_periodos <- data.frame(Periodo = c(unique(adm_periodo$Data_base)),
                                Administradoras = sapply(unique(adm_periodo$Data_base), 
                                                         function(x) sum(adm_periodo$Data_base == x)))

g_qntd_adm_periodos <- ggplot(data = qntd_adm_periodos, 
                              aes(x = factor(Periodo), y = Administradoras, group = 1)) +
  geom_line() +
  geom_point() +
  geom_text(data = qntd_adm_periodos[c(1, nrow(qntd_adm_periodos)), ], 
            aes(label = Administradoras), 
            vjust = ifelse(row.names(qntd_adm_periodos[c(1, nrow(qntd_adm_periodos)), ]) == 1, -0.55, -0.55), 
            hjust = c(1.5, 0.5)) + # Primeiro ponto à esquerda, último centralizado
  ggtitle("Quantidade de administradoras por período - 2020 a 2024")+
  theme_minimal() +
  expand_limits(y = 1.01 * max(qntd_adm_periodos$Administradoras))+  # Adiciona espaço extra no eixo Y
  xlab("Período")+
  theme(plot.title = element_text(size=10, hjust=0.5),
        panel.background = element_blank(),
        axis.title.y = element_blank())
g_qntd_adm_periodos

## Porto Seguro
top_adesoes <- maiores_adesoes |>
  head(10) |>
  arrange(desc(Adesoes))

# 10 maiores adms de consorcios
g_top_adm <- ggplot(data = top_adesoes, aes(x = reorder(Administradora, -Adesoes), 
                                            y = Adesoes)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_y_continuous(labels = scales::comma) +
  geom_text(aes(label = Adesoes), vjust = 1.6, color = "white", size = 2.5) +
  ggtitle("Administradoras com maior adesão (10 maiores) - 2020 a 2024") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, angle=45, hjust=1,
                                   color = ifelse(top_adesoes$Administradora=="PORTO SEGURO ADM. CONS. LTDA",
                                                  "red", "black")),
        plot.title = element_text(size = 10, hjust = 0.5),
        panel.background = element_blank())+
  labs(x=element_blank(), y="Adesões")
g_top_adm

# Consorcios Porto Seguro no Brasil
g_porto <- plot_map("PORTO SEGURO ADM. CONS. LTDA")
ggsave(filename = paste0("mapa_", "PORTO SEGURO ADM. CONS. LTDA",
                         "2020-2024", ".jpeg"), plot = g_porto, width = 8, height = 6)


df_porto <- adesoes_estado |>
  filter(Administradora == "PORTO SEGURO ADM. CONS. LTDA")
df_porto$Data_base <- NULL
df_porto <- df_porto |>
  group_by(Segmento) |>
  summarise(Adesoes = sum(Adesoes))

g_seg_porto <- ggplot(data=df_porto, aes(x=Segmento, y=Adesoes))+
  geom_bar(stat = "identity", width = 0.5)+
  scale_y_continuous(labels = scales::comma) +
  geom_text(aes(label = Adesoes), vjust = -1, color = "red", size = 3) +
  ggtitle("Segmentos de consórcios - Porto Seguro")+
  theme_minimal()+
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        panel.background = element_blank())+
  labs(x=element_blank(), y="Adesões")
ggsave(filename="Segmentos porto.svg", plot = g_seg_porto, width = 10, height = 6)
  
## Informações adicionais - Itau
itau <- df |>
  filter(Administradora == "ITAU ADM DE CONSORCIOS LTDA") |>
  select(Data_base, Administradora, Segmento, UF, Adesoes,
         Consorciados_ativos_contemplados_por_sorteio,
         Consorciados_ativos_nao_contemplados,
         Quantidade_de_consorciados_ativos_contemplados_por_sorteio_no_trimestre)

med_ad_periodo_total_itau <- mean(itau$Adesoes) # média de adesões período completo

# 2 maiores segmentos de atuação
maiores_segmentos_itau <- itau |>
  group_by(Segmento) |>
  summarise(Adesoes = sum(Adesoes)) |>
  arrange(desc(Adesoes)) |>
  head(2)

# média de consorciados ativos não contemplados no maior segmento
maior_segmento_itau <- itau |>
  filter(Segmento == maiores_segmentos_itau$Segmento[1])
med_atv_nao_contemp_itau <- mean(maior_segmento_itau$Consorciados_ativos_nao_contemplados)

med_atv_contemp_itau <- mean(maior_segmento_itau$Consorciados_ativos_contemplados_por_sorteio)

itau_contemplados_trimestre <- maior_segmento_itau %>% 
  group_by(Data_base, Administradora, Segmento) %>% 
  summarise(Quantidade_de_consorciados_ativos_contemplados_por_sorteio_no_trimestre =
              sum(Quantidade_de_consorciados_ativos_contemplados_por_sorteio_no_trimestre))

med_contemp_trimestre_itau <- mean(itau_contemplados_trimestre$Quantidade_de_consorciados_ativos_contemplados_por_sorteio_no_trimestre)


## Informações adicionais - BRADESCO CONS. LTDA.
bradesco <- df |>
  filter(Administradora == "BRADESCO CONS. LTDA.") |>
  select(Data_base, Administradora, Segmento, UF, Adesoes,
         Consorciados_ativos_contemplados_por_sorteio,
         Consorciados_ativos_nao_contemplados,
         Quantidade_de_consorciados_ativos_contemplados_por_sorteio_no_trimestre)

med_ad_periodo_total_bradesco <- mean(bradesco$Adesoes) # média de adesões período completo

# 2 maiores segmentos de atuação
maiores_segmentos_bradesco <- bradesco |>
  group_by(Segmento) |>
  summarise(Adesoes = sum(Adesoes)) |>
  arrange(desc(Adesoes)) |>
  head(2)

# média de consorciados ativos não contemplados no maior segmento
maior_segmento_bradesco <- bradesco |>
  filter(Segmento == maiores_segmentos_bradesco$Segmento[1])
med_atv_nao_contemp_bradesco <- mean(maior_segmento_bradesco$Consorciados_ativos_nao_contemplados)

med_atv_contemp_bradesco <- mean(maior_segmento_bradesco$Consorciados_ativos_contemplados_por_sorteio)

bradesco_contemplados_trimestre <- maior_segmento_bradesco %>% 
  group_by(Data_base, Administradora, Segmento) %>% 
  summarise(Quantidade_de_consorciados_ativos_contemplados_por_sorteio_no_trimestre =
              sum(Quantidade_de_consorciados_ativos_contemplados_por_sorteio_no_trimestre))

med_contemp_trimestre_bradesco <- mean(bradesco_contemplados_trimestre$Quantidade_de_consorciados_ativos_contemplados_por_sorteio_no_trimestre)


## Informações adicionais - BB CONSORCIOS
bb <- df |>
  filter(Administradora == "BB CONSORCIOS") |>
  select(Data_base, Administradora, Segmento, UF, Adesoes,
         Consorciados_ativos_contemplados_por_sorteio,
         Consorciados_ativos_nao_contemplados,
         Quantidade_de_consorciados_ativos_contemplados_por_sorteio_no_trimestre)

med_ad_periodo_total_bb <- mean(bb$Adesoes) # média de adesões período completo

# 2 maiores segmentos de atuação
maiores_segmentos_bb <- bb |>
  group_by(Segmento) |>
  summarise(Adesoes = sum(Adesoes)) |>
  arrange(desc(Adesoes)) |>
  head(2)

# média de consorciados ativos não contemplados no maior segmento
maior_segmento_bb <- bb |>
  filter(Segmento == maiores_segmentos_bb$Segmento[1])
med_atv_nao_contemp_bb <- mean(maior_segmento_bb$Consorciados_ativos_nao_contemplados)

med_atv_contemp_bb <- mean(maior_segmento_bb$Consorciados_ativos_contemplados_por_sorteio)

bb_contemplados_trimestre <- maior_segmento_bb %>% 
  group_by(Data_base, Administradora, Segmento) %>% 
  summarise(Quantidade_de_consorciados_ativos_contemplados_por_sorteio_no_trimestre =
              sum(Quantidade_de_consorciados_ativos_contemplados_por_sorteio_no_trimestre))

med_contemp_trimestre_bb <- mean(bb_contemplados_trimestre$Quantidade_de_consorciados_ativos_contemplados_por_sorteio_no_trimestre)


## Informações adicionais - ADM CONS NAC HONDA LTDA
honda <- df |>
  filter(Administradora == "ADM CONS NAC HONDA LTDA") |>
  select(Data_base, Administradora, Segmento, UF, Adesoes,
         Consorciados_ativos_contemplados_por_sorteio,
         Consorciados_ativos_nao_contemplados,
         Quantidade_de_consorciados_ativos_contemplados_por_sorteio_no_trimestre)

med_ad_periodo_total_honda <- mean(honda$Adesoes) # média de adesões período completo

# 2 maiores segmentos de atuação
maiores_segmentos_honda <- honda |>
  group_by(Segmento) |>
  summarise(Adesoes = sum(Adesoes)) |>
  arrange(desc(Adesoes)) |>
  head(2)

# média de consorciados ativos não contemplados no maior segmento
maior_segmento_honda <- honda |>
  filter(Segmento == maiores_segmentos_honda$Segmento[1])
med_atv_nao_contemp_honda <- mean(maior_segmento_honda$Consorciados_ativos_nao_contemplados)

med_atv_contemp_honda <- mean(maior_segmento_honda$Consorciados_ativos_contemplados_por_sorteio)

honda_contemplados_trimestre <- maior_segmento_honda %>% 
  group_by(Data_base, Administradora, Segmento) %>% 
  summarise(Quantidade_de_consorciados_ativos_contemplados_por_sorteio_no_trimestre =
              sum(Quantidade_de_consorciados_ativos_contemplados_por_sorteio_no_trimestre))

med_contemp_trimestre_honda <- mean(honda_contemplados_trimestre$Quantidade_de_consorciados_ativos_contemplados_por_sorteio_no_trimestre)

# Teste
resumo <- function(dados, adm) {
  df_adm <- dados |>
    filter(Administradora == adm) |>
    select(Data_base, Administradora, Segmento, UF, Adesoes,
           Consorciados_ativos_contemplados_por_sorteio,
           Consorciados_ativos_nao_contemplados,
           Quantidade_de_consorciados_ativos_contemplados_por_sorteio_no_trimestre)
  
  med_ad_periodo_total_df_adm <- mean(df_adm$Adesoes) # média de adesões período completo
  
  maiores_segmentos_df_adm <- df_adm |>
    group_by(Segmento) |>
    summarise(Adesoes = sum(Adesoes)) |>
    arrange(desc(Adesoes)) |>
    head(2)
  
  # média de consorciados ativos não contemplados no maior segmento
  maior_segmento_df_adm <- df_adm |>
    filter(Segmento == maiores_segmentos_df_adm$Segmento[1])
  med_atv_nao_contemp_df_adm <- mean(maior_segmento_df_adm$Consorciados_ativos_nao_contemplados)
  
  med_atv_contemp_df_adm <- mean(maior_segmento_df_adm$Consorciados_ativos_contemplados_por_sorteio)
  
  df_adm_contemplados_trimestre <- maior_segmento_df_adm %>% 
    group_by(Data_base, Administradora, Segmento) %>% 
    summarise(Quantidade_de_consorciados_ativos_contemplados_por_sorteio_no_trimestre =
                sum(Quantidade_de_consorciados_ativos_contemplados_por_sorteio_no_trimestre))
  
  med_contemp_trimestre_df_adm <- mean(df_adm_contemplados_trimestre$Quantidade_de_consorciados_ativos_contemplados_por_sorteio_no_trimestre)
  
  adesoes <- med_ad_periodo_total_df_adm
  atv_contemp <- med_atv_contemp_df_adm
  atv_nao_contemp <- med_atv_nao_contemp_df_adm
  contemp_trimestre <- med_contemp_trimestre_df_adm
  df_resumo <- data.frame(Adesoes=adesoes, 
                          Atv_contemplados=atv_contemp,
                          Atv_nao_contemp=atv_nao_contemp, 
                          Contemp_trimestre=contemp_trimestre)
  return (df_resumo)
}

resumo_porto <- resumo(df, "PORTO SEGURO ADM. CONS. LTDA")
