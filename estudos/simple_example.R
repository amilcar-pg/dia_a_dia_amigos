library(tidyverse)
library(igraph)

net.bg <- sample_pa(20) 
V(net.bg)$size <- 8
V(net.bg)$label <- "" 
E(net.bg)$arrow.mode <- 0

net.bg.df <- igraph::as_data_frame(net.bg) 

net.bg.df <- net.bg.df %>%
  mutate(time_frame = 1:n())

l <- layout_randomly(net.bg)

plot(net.bg, layout=l)

library(tidyr)
library(ggraph)
library(gganimate)

df0 <- net.bg.df
df0$time_frame <- as.numeric(df0$time_frame)
for(i in 1:nrow(df0)){
  df0$time_frame[i] <- list(df0$time_frame[i][[1]]:nrow(df0))
}
df <- unnest(df0, time_frame)
g2 <- graph_from_data_frame(df)

l <- as.data.frame(l)  # ggraph only accepts data.frame
colnames(l) <- c("x", "y") # ggraph needs these column names
ggraph(g2, l) +
  geom_node_point(color = "blue", size =3) +
  geom_edge_link0(show.legend = F, width = 1) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  transition_states(time_frame) +
  ggtitle(paste0("time point: ", "{closest_state}"))
