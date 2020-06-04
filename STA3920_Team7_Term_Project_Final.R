#STA 3920
#Skye Morgan, Neil Balazon, Mei Chang, Andrew Ivanov, Sam Lee
#Term Project Group 7 

library(cluster)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)

#Reading the data

toronto <- read.csv('MCI2014to2018.csv')

#looking at the data

fix(toronto)

#Group data so that individual MCI is shown for each neighbourhood

by_groups <- group_by(toronto, MCI, Neighbourhood)
groups <- summarise(by_groups, n=n())
groups <- groups[c("Neighbourhood", "MCI", "n")]
toronto_wide <- spread(groups, key = MCI, value = n)
fix(toronto_wide)
summary(toronto_wide)

boxplot(toronto_wide)

a=ggplot(toronto_wide) + aes(y=Assault) + geom_boxplot(fill="orange")
b=ggplot(toronto_wide)+ aes(y=`Auto Theft`) + geom_boxplot(fill="orange")
c=ggplot(toronto_wide)+ aes(y=`Break and Enter`) + geom_boxplot(fill="orange")
d=ggplot(toronto_wide)+ aes(y=`Robbery`) + geom_boxplot(fill="orange")
e=ggplot(toronto_wide)+ aes(y=`Theft Over`) + geom_boxplot(fill="orange")

ggarrange(a,b,c,d,e, labels = c("Assault Boxplot", "Auto Theft Boxplot", "Break and Enter Boxplot","Robbery Boxplot",
                                 "Theft Over Boxplot"),ncol = 3, nrow = 2)

#For clustering we have to remove the first column as it is qualitative data
z <- toronto_wide[,-c(1,1)]

#Must remove any missing values
z <- z[complete.cases(z),]

#Data must be scaled appropriately for comparison 
z<-scale(z)

fix(z)

#K-Means Clustering 

#-----------------------------------#

#With k=2

#The first cluster has 126 Neighbourhoods and the second one has 15 
#Cluster 1 has neighbourhood with low assault,low autotheft,low break and enter,low robbery and low theft over
#Cluster 2 has neighbourhood with high assault,high autotheft,high break and enter,high robbery and high theft over

set.seed(2)

km.out=kmeans(z,2,nstart=20)


km.out
km.out$cluster
km.out$tot.withinss
plot(z, col=(km.out$cluster+1), main="K-Means Clustering Results with K=2", xlab="", ylab="", pch=20, cex=2)

z1 <- data.frame(z, km.out$cluster)
clusplot(z1, km.out$cluster, color=TRUE, shade=F, labels=0, lines=0, main='k-Means Cluster Analysis')


#-----------------------------------#

#with k=3

#The first cluster has 10 Neighbourhoods, the second one has 41 and the third cluster has 90 Neighbourhoods
#Cluster 3 has neighbourhood with low assault,low autotheft,low break and enter,low robbery and low theft over
#Cluster 2 has neighbourhood with medium assault,medium autotheft,medium break and enter,medium robbery and medium theft over
#Cluster 1 has neighbourhood with high assault,high autotheft,high break and enter,high robbery and high theft over

set.seed(4)
km.out=kmeans(z,3,nstart=20)
km.out
km.out$cluster
km.out$tot.withinss
plot(z, col=(km.out$cluster+1), main="K-Means Clustering Results with K=3", xlab="", ylab="", pch=20, cex=2)

z2 <- data.frame(z, km.out$cluster)
clusplot(z2, km.out$cluster, color=TRUE, shade=F, labels=0, lines=0, main='k-Means Cluster Analysis')

#From comparing K= 2 and k=3 we see that 
#k=3 maximized within cluster sum of squares 
# k=3 minimizes km.out$tot.withinss 

#We decide to stick to using k=3

#From K-mean clustering we discovered that there are 10 Neighbourhoods with higher than most levels of crime
#operating. This would prove useful to the General Public as well as the Local Police as a starting point 
#in trying to better assist the residents of the area.

#High Crime
i<-toronto_wide[c("24","81","64","7","124","134","4","140","60","126"),]
fix(i)

#Medium Crime
o<-toronto_wide[c("125","33","12","130"),]
fix(o)

#Low Crime
p<-toronto_wide[c("98","133","115","57"),]
fix(p)
         
