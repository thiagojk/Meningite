

# Carregando pacotes ------------------------------------------------------

pacman::p_load(tidyverse, rio, dygraphs, xts, ggiraph, forcats, patchwork, kableExtra, ISOweek)


# Carregando dados --------------------------------------------------------

Dados1 <- rio::import("Dados/MENINNET10.DBF")
Dados2 <- rio::import("Dados/MENINNET.DBF")

Dados_Meningite <- rbind(Dados1, Dados2)

# Atualização do dashboard Looker Studio ----------------------------------
# 
# Dados_Meningite_Atualizado <- Dados_Meningite |> 
#   select(-TP_NOT, -NM_CONTATO, -NU_TELEFON, -TEL_CONTAT, 
#          -NU_DDD_TEL, -SOUNDEX, -ID_AGRAVO, -NM_PACIENT, 
#          -FONETICA_N, -END_CONTAT)
# 
# write.csv2(Dados_Meningite_Atualizado, file = "Meningite Atualizado.csv")
# 

attach(Dados_Meningite)


Dados_Meningite <- Dados_Meningite |>
  mutate(
    semana_epi = epiweek(DT_NOTIFIC),
    SEXO = case_when(
      CS_SEXO == "M" ~ "Masculino",
      CS_SEXO == "F" ~ "Feminino",
      TRUE ~ NA_character_),
    GESTANTE = case_when(
      CS_GESTANT == 1 ~ "1° Trimestre",
      CS_GESTANT == 2 ~ "2° Trimestre",
      CS_GESTANT == 3 ~ "3° Trimestre",
      CS_GESTANT == 4 ~ "Idade gestacional ignorada",
      CS_GESTANT == 5 ~ "Não",
      CS_GESTANT == 6 ~ "Não se aplica",
      CS_GESTANT == 9 ~ "Ignorado",
      TRUE ~ NA_character_),
    RACA = case_when(
      CS_RACA == 1 ~ "Branca",
      CS_RACA == 2 ~ "Preta",
      CS_RACA == 3 ~ "Amarela",
      CS_RACA == 4 ~ "Parda",
      CS_RACA == 5 ~ "Indígena",
      CS_RACA == 9 ~ "Ignorado",
      TRUE ~ NA_character_),
    ESCOLARIDADE = case_when(
      CS_ESCOL_N == "01" ~ "1ª a 4ª série incompleta do EF",
      CS_ESCOL_N == "02" ~ "4ª série completa do EF",
      CS_ESCOL_N == "03" ~ "5ª à 8ª série incompleta do EF",
      CS_ESCOL_N == "05" ~ "Ensino médio incompleto",
      CS_ESCOL_N == "04" ~ "Ensino fundamental completo",
      CS_ESCOL_N == "06" ~ "Ensino médio completo",
      CS_ESCOL_N == "07" ~ "Educação superior incompleta",
      CS_ESCOL_N == "08" ~ "Educação superior completa",
      CS_ESCOL_N == "09" ~ "Ignorado",
      CS_ESCOL_N == "10" ~ "Não se aplica",
      TRUE ~ NA_character_),
    CEFALEIA = case_when(CLI_CEFALE == 1 ~ "Sim",
                         CLI_CEFALE == 2 ~ "Não",
                         CLI_CEFALE == 9 ~ "Ignorado",
                         TRUE ~ NA_character_),
    FEBRE = case_when(CLI_FEBRE == 1 ~ "Sim",
                      CLI_FEBRE == 2 ~ "Não",
                      CLI_FEBRE == 9 ~ "Ignorado",
                      TRUE ~ NA_character_),
    VOMITO = case_when(CLI_VOMITO == 1 ~ "Sim",
                      CLI_VOMITO == 2 ~ "Não",
                      CLI_VOMITO == 9 ~ "Ignorado",
                      TRUE ~ NA_character_),
    OCORR_HOSP  = case_when(ATE_HOSPIT == 1 ~ "Sim",
                            ATE_HOSPIT == 2 ~ "Não",
                            ATE_HOSPIT == 9 ~ "Ignorado",
                            TRUE ~ NA_character_),
    CASO_CONFIRMADO = case_when(CLASSI_FIN == 1 ~ "Confirmado",
                                CLASSI_FIN == 2 ~ "Descartado",
                                TRUE ~ NA_character_),
    TIPO_MENINGITE = case_when(
      CON_DIAGES == "01"  ~ "Meningococcemia",
      CON_DIAGES == "02"  ~ "Meningite Meningocócica",
      CON_DIAGES == "03"  ~ "Meningite Meningocócica com Meningococcemia",
      CON_DIAGES == "04"  ~ "Meningite Tuberculosa",
      CON_DIAGES == "05"  ~ "Meningite por outras bactérias",
      CON_DIAGES == "06"  ~ "Meningite não especificada",
      CON_DIAGES == "07"  ~ "Meningite Asséptica",
      CON_DIAGES == "08"  ~ "Meningite por outra etiologia",
      CON_DIAGES == "09"  ~ "Meningite por Hemófilo",
      CON_DIAGES == "10" ~ "Meningite por Pneumococo",
      TRUE ~ NA_character_
  ),
    EVOLUCAO_CASO  = case_when(EVOLUCAO == 1 ~ "Alta",
                               EVOLUCAO == 2 ~ "Óbito por Meningite",
                               EVOLUCAO == 3 ~ "Óbito por outra causa",
                               EVOLUCAO == 9 ~ "Ignorado",
                               TRUE ~ NA_character_),
  investigacao_aberta = case_when(
    !is.na(DT_INVEST) & is.na(DT_ENCERRA) ~ "Sim", 
    !is.na(DT_INVEST) & !is.na(DT_ENCERRA) ~ "Não",
    is.na(DT_INVEST) ~ "Não",                             
    TRUE ~ NA_character_                
  ),
  ano = year(DT_NOTIFIC),
  mes = month(DT_NOTIFIC, label = TRUE, abbr = FALSE)
  )




# Gráficos ----------------------------------------------------------------



notifica_meningite <- Dados_Meningite |> 
  group_by(ano, mes) |> 
  summarise(Casos = n(), 
            Casos_Confirmados = sum(CASO_CONFIRMADO == "Confirmado", na.rm = TRUE),
            .groups = "drop") |> 
  mutate(data = make_date(ano, mes, 1),
         incidencia = (Casos_Confirmados/1350000)*100000)


# semana epidemiologica

casos_semana_epi <- Dados_Meningite |> 
  group_by(ano, semana_epi) |> 
  summarise(Casos = n(), 
            Casos_Confirmados = sum(CASO_CONFIRMADO == "Confirmado", na.rm = TRUE),
            .groups = "drop") |> 
  mutate(incidencia = (Casos_Confirmados/1350000)*100000,
         data = as.Date("2020-01-01") + (semana_epi - 1) * 7)



casos_meningite_xts <- xts(
  notifica_meningite[, c("Casos", "Casos_Confirmados")], # Selecionar colunas de interesse
  order.by = notifica_meningite$data
)


incidencia_xts <- xts(
  notifica_meningite[,c("incidencia", "Casos_Confirmados")], # Selecionar colunas de interesse
  order.by = notifica_meningite$data)



# Criar uma coluna de data para o eixo X (ano-mês)
notifica_meningite$data <- as.Date(paste(notifica_meningite$ano, 
                                         notifica_meningite$mes, "01", sep = "/"), 
                                   format = "%Y/%m/%d")






####################################


# Previsão de casos -------------------------------------------------------
attach(Dados_Meningite)

meningite <- Dados_Meningite |>
  filter(CASO_CONFIRMADO == "Confirmado",
    ano >= 2020) |> 
  mutate(ano_mes = as.Date(paste0(year(DT_NOTIFIC), "-", 
                                  month(DT_NOTIFIC), "-01"))) |> 
  group_by(ano_mes) |> 
  summarise(casos = n())

 

TS_Meningite <- ts(meningite$casos, start = c(2020,1), frequency = 12)

TS_Meningite |> plot()

modelo_meningite <- forecast::auto.arima(TS_Meningite, trace = TRUE)

previsao_meningite <- forecast::forecast(modelo_meningite, h = 6)
previsao_meningite |> plot(main = "Previsão de Casos de Meningite")


ajuste_meningite <- seasonal::seas(TS_Meningite)
ajuste_meningite |> plot()

meningite_df <- data.frame(ajuste_meningite$data)

TS_Meningite_Ajustado <- ts(meningite_df$final, start = c(2020, 1), frequency = 12)

prev <- forecast::forecast(forecast::auto.arima(TS_Meningite_Ajustado), h = 12)

prev |> plot()

# Semana Epidemiologica ---------------------------------------------------

meningite_epi <- Dados_Meningite |> 
  filter(ano >= 2023) |> 
  group_by(semana_epi, ano) |> 
  summarise(soma = n(), .groups = "drop") |> 
  mutate(data_inicio_semana = ISOweek2date(paste0(ano, "-W", sprintf("%02d", semana_epi), "-1"))) |> 
  arrange(data_inicio_semana)



meningite |> 
  ggplot(aes(x = ano_mes, y = casos)) +
  geom_line()




TS_Meningite_EPI <- ts(meningite_epi$soma, 
                       start = c(min(meningite_epi$ano), min(meningite_epi$semana_epi)), 
                       frequency = 52)


TS_Meningite_EPI |> plot()







