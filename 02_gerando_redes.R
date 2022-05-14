# importando dados --------------------------------------------------------

source("01_importando_dados.R")

# gerando rede ------------------------------------------------------------
## utilizaremos a rede por hora

s50 <- list()

for (i in 1:length(hourly_list)) {
  s50[[i]] <- igraph::graph_from_data_frame(hourly_list[[i]],
                                            directed = FALSE)
}; rm(i)

xy <- layout_as_dynamic(s50, alpha = 0.2)

nodes_lst <- lapply(1:length(s50), function(i) {
  cbind(igraph::as_data_frame(s50[[i]], "vertices"),
        x = xy[[i]][, 1], y = xy[[i]][, 2], frame = i
  )
})