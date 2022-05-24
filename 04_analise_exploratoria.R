# importando dados --------------------------------------------------------

source("03_importando_dados.R")


# importando pacotes ------------------------------------------------------

library(ggplot2)

# explorando... -----------------------------------------------------------

## regressão do tempo de sono com relação às outras atividades ----

reg_sono <- lm(data = df_people,
   formula = dormindo ~ afazeres_domesticos + atividades_fisicas + comendo + descansando + deslocando + estudando + higiene_pessoal + lazer + trabalhando
)

jtools::summ(reg_sono)

jtools::export_summs(reg_sono)

df_reg <- df_people |> 
  tidyr::pivot_longer(cols = -c(amigo, dormindo), names_to = "atividade", values_to = "tempo")

df_reg |> 
  ggplot(aes(y = dormindo, x = tempo, color = atividade)) +
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