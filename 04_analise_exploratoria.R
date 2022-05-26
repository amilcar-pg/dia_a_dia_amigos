# importando dados --------------------------------------------------------

source("03_importando_dados.R")

# importando pacotes ------------------------------------------------------

library(ggplot2)

# explorando... -----------------------------------------------------------

## regressão do tempo de sono com relação às outras atividades ----

df_reg <- df_people |> 
  tidyr::pivot_longer(cols = -c(amigo, Dormindo), names_to = "atividade", values_to = "tempo")

df_reg |> 
  ggplot(aes(y = Dormindo, x = tempo, color = atividade)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(vars(atividade))

## boxplot das variáveis ----

df_boxplot <- df_people |> 
  tidyr::pivot_longer(-c(amigo),
                      names_to = "atividade",
                      values_to = "tempo")

df_boxplot |> 
  ggplot(aes(x = atividade, y = tempo, fill = atividade)) +
  geom_boxplot()

ggplot(data = melted_cormat, aes(Atividade1, Atividade2, fill = value))+
  geom_tile() +
  scale_colour_distiller(palette = "Oranges")

## corrplot ----

ggplot(melted_cormat, aes(Atividade1, Atividade2, fill = value))+
  geom_tile(color = "white")+
  scale_fill_distiller(palette = "Oranges") +
  labs(title = "Correlação entre as atividades",
       x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5,
                                  size = 16),
        panel.background = element_rect(fill = "#FAFAFA"),
        plot.background = element_rect(fill = "#FAFAFA"),
        panel.grid.major = element_line(colour = "gray70"))

## tabela ----

# recuperando hora de dormir e de acordar do indivíduo
# df_dormir <- df_raw |>
#   dplyr::select(-c(24:33)) |>
#   dplyr::rowwise() |>
#   dplyr::mutate(
#     dormindo = sum(dplyr::c_across(Amilcar:`Breno M.`) == "Dormindo")
#   )
# 
# for (i in 3:(ncol(df_dormir)-1)){
#   df_dormir[i] <- dplyr::case_when(
#     df_dormir[i] == "Dormindo" ~ TRUE
#   )
# }; rm(i)
# 
# df_dormir <- df_dormir |>
#   dplyr::select(-dormindo) |>
#   dplyr::pivot_longer(Amilcar:`Breno M.`,
#                names_to = "Amigo",
#                values_to = "Dormindo")