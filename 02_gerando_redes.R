# importando dados --------------------------------------------------------

source("01_importando_dados.R")

# gerando rede ------------------------------------------------------------

## como ainda nao domino 100% esse processo, usarei o library no lugar de
## chamar cada funcao a partir do operador ::

## carregando pacotes ----

library(igraph)
library(ggplot2)
library(ggraph)
library(graphlayouts)
library(patchwork)
library(gganimate)

## recuperando a lista ----

# hourly_list

## gerando lista com gráficos individuais ----

### gerando a lista de elementos igraph

igraph_list <- list()

for (i in 1:length(hourly_list)){
  igraph_list[[i]] <- graph_from_data_frame(hourly_list[[i]],
                                            directed = FALSE,
                                            vertices = df_vertices)
  names(igraph_list)[i] <- names(hourly_list)[i]
}; rm(i)

## recuperando as coordenadas

set.seed(123)

xy <- layout_as_dynamic(igraph_list, alpha = 0.2)

for (i in 1:length(xy)){
  names(xy)[i] <- names(hourly_list)[i]
}

## plotando os gráficos

pList <- list()

for (i in 1:length(igraph_list)) {
  pList[[i]] <- ggraph(igraph_list[[i]],
         layout = "manual",
         x = xy[[i]][, 1],
         y = xy[[i]][, 2]) +
    geom_edge_link0(edge_width = 0.6, edge_colour = "grey66") +
    geom_node_point(
      shape = 21,
      aes(fill = as.factor(atividade)),
      size = 6,
      show.legend = FALSE
    ) +
    geom_node_text(
      aes(label = ifelse(atividade == TRUE, name, "")),
      repel = FALSE,
      color = "black",
      size = 4
    ) +
    theme_graph() +
    theme(legend.position = "bottom") +
    labs(title = paste0("Hora: ", names(igraph_list)[i]))
}; rm(i)

## gerando a animação ----

## criando lista de vertices

nodes_lst <- lapply(1:length(igraph_list), function(i) {
  cbind(igraph::as_data_frame(igraph_list[[i]], "vertices"),
        x = xy[[i]][, 1], y = xy[[i]][, 2], frame = i
  )
})

## criando lista de arestas

edges_lst <- lapply(1:length(igraph_list),
                    function(i) cbind(igraph::as_data_frame(igraph_list[[i]], "edges"), frame = i))

edges_lst <- lapply(1:length(igraph_list), function(i) {
  edges_lst[[i]]$x <- nodes_lst[[i]]$x[match(edges_lst[[i]]$from, nodes_lst[[i]]$name)]
  edges_lst[[i]]$y <- nodes_lst[[i]]$y[match(edges_lst[[i]]$from, nodes_lst[[i]]$name)]
  edges_lst[[i]]$xend <- nodes_lst[[i]]$x[match(edges_lst[[i]]$to, nodes_lst[[i]]$name)]
  edges_lst[[i]]$yend <- nodes_lst[[i]]$y[match(edges_lst[[i]]$to, nodes_lst[[i]]$name)]
  edges_lst[[i]]$id <- paste0(edges_lst[[i]]$from, "-", edges_lst[[i]]$to)
  edges_lst[[i]]$status <- TRUE
  edges_lst[[i]]
})

all_edges <- do.call("rbind", lapply(igraph_list, get.edgelist))
all_edges <- all_edges[!duplicated(all_edges), ]
all_edges <- cbind(all_edges, paste0(all_edges[, 1], "-", all_edges[, 2]))

edges_lst <- lapply(1:length(igraph_list), function(i) {
  idx <- which(!all_edges[, 3] %in% edges_lst[[i]]$id)
  if (length(idx != 0)) {
    tmp <- data.frame(from = all_edges[idx, 1], to = all_edges[idx, 2], id = all_edges[idx, 3])
    tmp$x <- nodes_lst[[i]]$x[match(tmp$from, nodes_lst[[i]]$name)]
    tmp$y <- nodes_lst[[i]]$y[match(tmp$from, nodes_lst[[i]]$name)]
    tmp$xend <- nodes_lst[[i]]$x[match(tmp$to, nodes_lst[[i]]$name)]
    tmp$yend <- nodes_lst[[i]]$y[match(tmp$to, nodes_lst[[i]]$name)]
    tmp$frame <- i
    tmp$status <- FALSE
    edges_lst[[i]] <- rbind(edges_lst[[i]], tmp)
  }
  edges_lst[[i]]
})

edges_df <- do.call("rbind", edges_lst)
nodes_df <- do.call("rbind", nodes_lst)
df_time <- data.frame(
  frame = 1:96,
  frame_time = df_raw$hora)

edges_df <- dplyr::left_join(edges_df, df_time, by = "frame")
nodes_df <- dplyr::left_join(nodes_df, df_time, by = "frame")

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

# funciona certo, mas não é fluido
p +
  labs(title = "Hora: {stringr::str_sub(closest_state, 12, 16)}") +
  transition_states(frame_time, state_length = 0) +
  theme_void()

# Começa do frame errado
p +
  labs(title = labs(title = 'Hora: {stringr::str_sub(frame_time, 12, 16)}')) +
  transition_time(frame_time) +
  theme_void()

# Funciona certo, mas não consigo recuperar a label certa para o título
p +
  labs(title = labs(title = 'Hora: {frame_time}')) +
  transition_time(frame) +
  theme_void()

# animate(
#   plot = anim,
#   nframes = 8 * 96,
#   #render = av_renderer("previa_especial.mp4")
#   #render = gifski_renderer("previa_especial.gif")
#   )
