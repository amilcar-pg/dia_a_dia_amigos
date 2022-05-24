# importando dados brutos -------------------------------------------------

df_raw <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1Dx9Za-b1hqeiKso1E067M1BgF6VhdbGyz9FZJC662Sk/edit#gid=2051768494"
)

df_raw <- janitor::clean_names(df_raw)

# organizando por tempo ---------------------------------------------------

df_people <- df_raw |> 
  dplyr::select(-c(1,2, 24:33)) |> 
  tidyr::pivot_longer(1:21, names_to = "amigo") |> 
  reshape2::dcast(amigo ~ value, fill = 0)

df_people <- janitor::clean_names(df_people)

df_activities <- df_raw |> 
  dplyr::select(-c(1,2, 24:33)) |> 
  tidyr::pivot_longer(1:21, names_to = "amigo") |> 
  dplyr::group_by(value) |> 
  dplyr::summarise(n = dplyr::n())