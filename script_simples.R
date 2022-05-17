# carregando pacotes ------------------------------------------------------

library(ggplot2)
library(gganimate)

# importando dados --------------------------------------------------------

edges_df <- readr::read_csv("https://raw.githubusercontent.com/amilcar-pg/dia_a_dia_amigos/master/output/edges_df.csv")
nodes_df <- readr::read_csv("https://raw.githubusercontent.com/amilcar-pg/dia_a_dia_amigos/master/output/nodes_df.csv")

# plotando ----------------------------------------------------------------

p <- ggplot() +
  geom_segment(
    data = edges_df,
    aes(x = x, xend = xend, y = y, yend = yend, group = id, alpha = status), 
    show.legend = FALSE
  ) +
  geom_point(
    data = nodes_df, aes(x, y, group = name, fill = as.factor(atividade)),
    shape = 21, size = 6, show.legend = FALSE
  ) +
  scale_alpha_manual(values = c(0, 1))


# animando ----------------------------------------------------------------

# o título aparece o horário errado.
p +
  labs(title = labs(title = 'Hora: {stringr::str_sub(frame_time, 12, 16)}')) +
  transition_time(frame_time) +
  theme_void()

# funciona certinho, com o índice certinho, mas não tem como colocar o título.
p +
  labs(title = labs(title = 'Hora: {frame_time}')) +
  transition_time(frame) +
  theme_void()