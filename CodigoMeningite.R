

# Carregando pacotes ------------------------------------------------------

pacman::p_load(tidyverse, rio, dygraphs, xts)

devtools::install_github("d3plus/d3plus")




# Carregando dados --------------------------------------------------------

Dados_Meningite <- rio::import("Dados/MENINNET.DBF")

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
  )
  )




# Gráficos ----------------------------------------------------------------



notifica_meningite <- Dados_Meningite |> 
  group_by(DT_NOTIFIC) |> 
  summarise(Casos = n())




dygraph(xts(notifica_meningite$Casos, order.by = notifica_meningite$DT_NOTIFIC), 
        main = "Casos de Meningite") |> 
  dyAxis("y", label = "Valores") |> 
  dyAxis("x", label = "Data") 


d3plus(data = data, type = "line", id = "category", x = "date", y = "value")
















