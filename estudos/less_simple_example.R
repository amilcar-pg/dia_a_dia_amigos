library(igraph)
library(dplyr)
library(ggnetwork)
library(gganimate)
library(ggplot2)

### ggnetwork + gganimate


N=120
set.seed(140)
g <- sample_forestfire(N, fw.prob=0.3,bw.factor=.9)
V(g)$Group <- cluster_edge_betweenness(g)$membership
df<-ggnetwork(g,layout = with_fr())

df1 <- df[(N+1):nrow(df),]

results=NULL

for(i in 1:nrow(df1)){
  results[[i]] <- df1[1:i,]
}

df2 <- do.call('rbind', Map(cbind, results, time=1:nrow(df1)))

## need to find which time each node first appears and then add an NA for that node from that time onwards
# this could be tidied up a bit
library(dplyr)
head(df2)
mintimes<-rbind(
  df2 %>% group_by(x,y) %>% summarise(time=min(time)),
  df2 %>% group_by(xend,yend) %>% summarise(time=min(time)) %>% rename(x=xend,y=yend)
) %>% group_by(x,y) %>% summarise(time=min(time)) %>% ungroup() %>% as.data.frame

dfx <- as.data.frame.matrix(df[1:N,]) 
mintimes$x<-as.numeric(mintimes$x)
mintimes$y<-as.numeric(mintimes$y)
nodetimes <- dfx %>% left_join(mintimes)

#expand rows to fill up to end of time
mt<-max(df2$time)
df.expanded <- nodetimes[rep(row.names(nodetimes), (mt-nodetimes$time)), ]

head(df.expanded,20)
df.expanded1 <- df.expanded %>% group_by(x, y, xend, yend) %>% mutate(time = time + row_number() - 1)

## bind back

df3 <- rbind(df.expanded1, df2)

p=ggplot(df3, aes(x = x, y = y, xend = xend, yend = yend, frame=time)) +
  geom_edges(color = "black") +
  geom_nodes(aes(color = factor(Group)), size = 8) + 
  # geom_nodetext(aes(label = vertex.names),color="black", fontface = "bold") +
  theme_blank() +
  theme(legend.position="none") +
  guides(color=guide_legend(keyheight=0.3,default.unit="inch",override.aes = list(size=6)))

animation::ani.options(interval=0.25)

gg_animate(p, 'animation1.mp4', title_frame = FALSE)

p +
  transition_time(time) +
  labs(title = "Time: {frame_time}")
