## 


rm(list=ls())

library (tidyr)
library (dplyr)
library (ggplot2)
library (igraph)
library (sna)
library (sp)

# library ()

#data

dat <- read.csv(file='~/Box Sync/PROYECTO MIGRACIÓN/data/edges.csv', encoding = 'latin1')
str(dat)
summary(dat)
dat <- mutate(dat, w.norm = Weight / max(Weight))

e <- select(dat, Source, Target, Weight)
v <- dat %>% 
    select(Source, Label) %>%
    unique()
    
# network with igraph
net <- graph_from_data_frame(d = dat, directed = TRUE)

# community detection
c <- cluster_louvain(net, weights = E(net)$Weight) # Error  multi-level community detection works for undirected graphs only, Unimplemented function call
c <- cluster_infomap(net, e.weights = E(net)$Weight ) # It identifies 12 clusters

range(dat$w.norm)

# plot
quartz(width=7, height = 7, pointsize = 6)
plot(net, edge.arrow.size = 0.4, vertex.size = 2, 
     edge.width = E(net)$w.norm * 2, 
     vertex.label = NULL,
     vertex.color = 'orange'
     )

membership (c)

# map it
col <- readRDS(file = '~/Box Sync/PROYECTO MIGRACIÓN/data/COL_adm2.rds')
d <- col@data

sort(col@data$NAME_2 [v$Label %in% col@data$NAME_2 == FALSE])
sort(v$Label [col@data$NAME_2 %in% v$Label == F] )
plot(col)

################################  re mapit

t <- membership(c)
tt <- as.data.frame(as.matrix(t))
tt$Municipality <- as.integer(row.names(tt))

muni <- readShapePoly("MAGNA_2012_MPIO_OK.shp")
depto <- readShapePoly("MAGNA_2012_DEPTO.shp")

att <- as.data.frame.matrix(muni@data)
att$MPIO_CCNCT <- as.integer(att$MPIO_CCNCT)

attrib <- left_join(att, tt, by = c('MPIO_CCNCT' = 'Municipality'), copy=T)
muni@data <- attrib

write.table(attrib, file = 'grouping1.csv', sep = ',')
str(attrib)
attach(attrib)

col<- brewer.pal(12,"Paired")
palette(col)

x11()
plot(muni, col = (muni$V1), border = F, main = 'Mapa mediante SNA')
plot(depto, add= T)
box()




