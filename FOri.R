
library(tibble)
library(ape)
library(cluster)

#Interspecific

data(iris)

PCOA <- iris[,1:4] %>%  scale() %>%  daisy(metric="euclidean") %>% pcoa(correction="cailliez")

Axes<-PCOA$vectors[,1:2] %>% as.data.frame() %>%
  add_column(Species = iris[,5], .before = 1)

dist_F<-as.matrix(dist(Axes,method="euclidean"))	;	dist_F[which(dist_F==0)]<-NA

oriS<-apply(dist_F,	1,	min,	na.rm=T	)
oriS

oriS<-oriS/max(oriS)

#Intraspecific

data(iris)

PCOA <- iris[,1:4] %>%  scale() %>%  daisy(metric="euclidean") %>% pcoa(correction="cailliez")

Axes<-PCOA$vectors[,1:2] %>% as.data.frame() %>%
  add_column(Species = iris[,5], .before = 1)

#Setosa
dist_set<-as.matrix(dist(Axes[1:50,],method="euclidean"))	;	dist_set[which(dist_set==0)]<-NA

FOri_set<-apply(dist_set,	1,	min,	na.rm=T	)
FOri_set

FOri_set<-as.matrix(FOri_set/max(FOri_set))

#Versicolor
dist_ver<-as.matrix(dist(Axes[51:100,],method="euclidean"))	;	dist_ver[which(dist_ver==0)]<-NA

FOri_ver<-apply(dist_ver,	1,	min,	na.rm=T	)
FOri_ver

FOri_ver<-as.matrix(FOri_ver/max(FOri_ver))

#Virginica
dist_vir<-as.matrix(dist(Axes[101:150,],method="euclidean"))	;	dist_vir[which(dist_vir==0)]<-NA

FOri_vir<-apply(dist_vir,	1,	min,	na.rm=T	)
FOri_vir

FOri_vir<-as.matrix(FOri_vir/max(FOri_vir))

Intra<-rbind(FOri_set, FOri_ver, FOri_vir)
colnames(Intra) <- c("FOri")



