library(ggeasy)
library(tibble)
library(ape)
library(cluster)	
library(ggplot2)
library(extrafont)
font_import()
y
fonttable()
loadfonts(device="win")
fonts()

#Interspecific

data(iris)

#Scaling and	centering	trait	matrix (standardize)

PCOA <- iris[,1:4] %>%  scale() %>%  daisy(metric="euclidean") %>% pcoa(correction="cailliez")
PCOA


Axes<-PCOA$vectors[,1:2] %>% as.data.frame() %>%
  add_column(Species = iris[,5], .before = 1)

names(Axes)[2:3] <- c("PCoA1","PCoA2")

o<-apply(Axes[,2:3],	2,	mean) %>% rbind() %>% as.data.frame()

speS<-Axes[,2:3] %>% apply(1,	function(x)	{	(sum((x-o)^2)	)^0.5}) %>% as.data.frame()

#speS<-speS/max(speS) # final index

A<-ggplot(data = Axes, aes(PCoA1, PCoA2))+  #plot
  geom_segment(data = Axes,
    aes(x = -3.082387e-18, y = 2.333830e-16, xend = PCoA1, yend = PCoA2), colour = "grey", size = 0.3)+
  geom_point(aes(x = -3.082387e-18, y = 2.333830e-16), 
    fill = "#FFFFFF", colour = "grey", size = 3, shape = 21, stroke=0.5)+
  geom_point(data = Axes,aes(group= Species, fill = Species), size=5, shape = 21, stroke=1.1)+
  scale_fill_manual(values = c("#F29783", "#B5F252","#A2A0F0"))+
  labs(title = "Interspecific", x = "PCoA1 (72.9%)", y = "PCoA2 (22.8%)")+
  ggeasy::easy_center_title()+
  coord_fixed()+
  theme(plot.title = element_text(face = "bold",size = 22))+
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16))+
  theme(
    legend.text = element_text(size=16),
    legend.title = element_text(size=20),
    legend.position = c(0.84, 1),
    legend.justification = c("left", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.key.size = )+
  theme(text=element_text(family="Times New Roman"))+
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))
A

#Intraspecific

data(iris)

#Scaling and	centering	trait	matrix (standardize)

PCOA <- iris[,1:4] %>%  scale() %>%  daisy(metric="euclidean") %>% pcoa(correction="cailliez")

Axes<-PCOA$vectors[,1:2] %>% as.data.frame() %>%
  add_column(Species = iris[,5], .before = 1)

names(Axes)[2:3] <- c("PCoA1","PCoA2")

#setosa
set <- Axes[Axes$Species=="setosa", ]

set1 <- apply(set[2:3],	2,	mean) %>% rbind() %>% as.data.frame() #centroid
set1
Spese<-apply(Axes[1:50,2:3], 1,	function(x)	{	(sum((x-set1)^2)	)^0.5}) %>% as.data.frame()

#versicolor

ver <- Axes[Axes$Species=="versicolor", ]

ver1 <- apply(ver[2:3],	2,	mean) %>% rbind() %>% as.data.frame() #centroid
ver1
Spever<-apply(Axes[51:100,2:3], 1,	function(x)	{	(sum((x-ver1)^2)	)^0.5}) %>% as.data.frame()

#virginica

vir <- Axes[Axes$Species=="virginica", ]

vir1 <- apply(vir[2:3],	2,	mean) %>% rbind() %>% as.data.frame() #centroid
vir1
Spevir<-apply(Axes[101:150,2:3], 1,	function(x)	{	(sum((x-vir1)^2)	)^0.5}) %>% as.data.frame()

Intra<-rbind(Spese, Spever, Spevir)
colnames(Intra) <- c("FSpe") #Last modification

#speS<-speS/max(speS) # final index values

B<-ggplot(data = Axes, aes(PCoA1, PCoA2))+  #plot
  geom_segment(data = Axes[1:50,],
               aes(x = -2.217325, y = 0.2879627, xend = PCoA1, yend = PCoA2), colour = "grey", size = 0.3)+
  geom_point(aes(x = -2.217325, y = 0.2879627), 
             fill = "#FFFFFF", colour = "grey", size = 3, shape = 21, stroke=0.5)+
  geom_segment(data = Axes[51:100,],
               aes(x = 0.4947904, y = -0.5483335, xend = PCoA1, yend = PCoA2), colour = "grey", size = 0.3)+
  geom_point(aes(x = 0.4947904, y = -0.5483335), 
             fill = "#FFFFFF", colour = "grey", size = 3, shape = 21, stroke=0.5)+
  geom_segment(data = Axes[101:150,],
               aes(x = 1.722534, y = 0.2603708, xend = PCoA1, yend = PCoA2), colour = "grey", size = 0.3)+
  geom_point(aes(x = 1.722534, y = 0.2603708), 
             fill = "#FFFFFF", colour = "grey", size = 3, shape = 21, stroke=0.5)+
  geom_point(data = Axes,aes(group= Species, fill = Species), size=5, shape = 21, stroke=1.1)+
  scale_fill_manual(values = c("#F29783", "#B5F252","#A2A0F0"))+
  labs(title = "Intraspecific", x = "PCoA1 (72.9%)", y = "PCoA2 (22.8%)")+
  ggeasy::easy_center_title()+
  coord_fixed()+
  theme(plot.title = element_text(face = "bold",size = 22))+
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16))+
  theme(
    legend.text = element_text(size=16),
    legend.title = element_text(size=20),
    legend.position = c(0.84, 1),
    legend.justification = c("left", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.key.size = )+
  theme(text=element_text(family="Times New Roman"))+
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))
B

library(cowplot)

plot_grid(B, A, ncol=2, nrow = 1,
          align = "hv")
