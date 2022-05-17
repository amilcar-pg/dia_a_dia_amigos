# importando dados --------------------------------------------------------



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

p +
  labs(title = labs(title = 'Hora: {stringr::str_sub(frame_time, 12, 16)}')) +
  transition_time(frame_time) +
  theme_void()