# importando dados brutos -------------------------------------------------

df_raw <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1Dx9Za-b1hqeiKso1E067M1BgF6VhdbGyz9FZJC662Sk/edit#gid=2051768494"
  )[,-2]

atividades <- colnames(df_raw)[23:32]

# inseri uma coluna com cada uma das opções para garantir que todos os nós estarão
# presentes em todos os horários.

## gerando um backup

readr::write_csv(df_raw, "output/df_raw.csv")

# limpando dados brutos ---------------------------------------------------

## correcao de nomes das colunas
colnames(df_raw)[1] <- "hora"
df_raw <- janitor::clean_names(df_raw)

colnames(df_raw)[23:32] <- atividades

# organizando dados para redes --------------------------------------------

individuos_list <- list()

## gerando um df para cada individuo

for (i in 2:ncol(df_raw)){
  df_hora <- df_raw[,1]
  df_amigue <- df_raw[,i]
  individuos_list[[i-1]] <- dplyr::bind_cols(
    df_hora, df_amigue)
  names(individuos_list)[i-1] <- colnames(df_amigue)
}; rm(i, df_hora, df_amigue)

## colocando na forma de source-target

for (i in 1:length(individuos_list)){
  individuos_list[[i]] <- individuos_list[[i]] |> 
    dplyr::mutate(
      source = colnames(individuos_list[[i]])[2]
    ) |> 
    dplyr::rename(target = colnames(individuos_list[[i]])[2]) |> 
    dplyr::relocate(source, .before = target) |> 
    dplyr::relocate(hora, .after = target)
}; rm(i)

## adicionando indice para facilitar no filtro

for (i in 1:length(individuos_list)){
  individuos_list[[i]] <- tibble::rowid_to_column(individuos_list[[i]], "indice")
  individuos_list[[i]] <- individuos_list[[i]] |> 
    dplyr::relocate(indice, .before = hora)
}; rm(i)

## criando dataframe

df_arestas_total <- data.table::rbindlist(individuos_list)

## criando lista por hora

hourly_list <- list()

for (i in 1:96){
  hourly_list[[i]] <- df_arestas_total |> 
    dplyr::filter(indice == i) |> 
    dplyr::select(-indice, -hora)
  names(hourly_list)[i] <- format(df_arestas_total[i,4], format = "%H:%M")
}; rm(i)

# criando df de vértices

df_vertices <- as.data.frame(unique(df_arestas_total$source))

colnames(df_vertices) <- "vertices"

# limpando os objetos do ambiente -----------------------------------------

df_arestas_total <- df_arestas_total |> 
  dplyr::select(-indice)

for (i in 1:length(individuos_list)){
  individuos_list[[i]] <- individuos_list[[i]] |> 
    dplyr::select(-indice)
}; rm(i)

# inserindo informacoes sobre os vertices ---------------------------------

df_vertices <- df_vertices |> 
  dplyr::mutate(
    atividade = ifelse(
    vertices %in% atividades,
    TRUE,
    FALSE
  )); rm(atividades)
