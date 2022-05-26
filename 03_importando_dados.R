# importando dados brutos -------------------------------------------------

df_raw <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1Dx9Za-b1hqeiKso1E067M1BgF6VhdbGyz9FZJC662Sk/edit#gid=2051768494"
)

# df_raw <- janitor::clean_names(df_raw)

# organizando por tempo ---------------------------------------------------

df_people <- df_raw |> 
  dplyr::select(-c(1,2, 24:33)) |> 
  tidyr::pivot_longer(1:21, names_to = "amigo") |> 
  reshape2::dcast(amigo ~ value, fill = 0)

# df_people <- janitor::clean_names(df_people)

df_activities <- df_raw |> 
  dplyr::select(-c(1,2, 24:33)) |> 
  tidyr::pivot_longer(1:21, names_to = "amigo") |> 
  dplyr::group_by(value) |> 
  dplyr::summarise(n = dplyr::n())

df_activities <- df_activities |> 
  dplyr::mutate(
    hr = (n*15) %/% 60,
    min = (n*15) %% 60,
    hora = stringr::str_c(hr, min, sep = ":")
  )

# para ográfico de correlação

df_cor <- cor(df_people[,-1])

df_cor_melt <- data.table::melt(df_cor)

colnames(df_cor_melt) <- c("Atividade1", "Atividade2", "value")

get_upper_tri <- function(cormat){
  cormat[upper.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(df_cor)

melted_cormat <- data.table::melt(upper_tri, na.rm = TRUE)

colnames(melted_cormat) <- c("Atividade1", "Atividade2", "value")

# para o gráfico de dispersão

df_reg <- df_people |> 
  tidyr::pivot_longer(cols = -c(amigo, Dormindo), names_to = "atividade", values_to = "tempo")

# para a tabela ----

df_tabela <- tibble::tibble(`Média de tempo de sono` = NA)

df_tabela <- df_tabela |> 
  dplyr::mutate(
    `Média de tempo de sono` = "8:00",
    `Média de hora de ir dormir` = "23:15",
    `Média de hora de acordar`= "7:15"
  )

# imagens para o gráfico de barras ----

df_activities <- df_activities |> 
  dplyr::mutate(
    img = 
    stringr::str_c(stringr::str_c(here::here("data"), value, sep = "/"), ".png")
  )
