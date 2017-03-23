# Include libraries
library(ggplot2) #v 2.1.0

######################################################################
# Example 1 - Scatterplot


# import data 'example1.csv'
data1<-read.csv("~/Documents/BU/ggplot2/data/example1.csv", header=T)

# scatterplot
p1<- ggplot(data1, aes(x=per.fny.reports, y=fny.cdc.cor))+
  geom_point()
p1

# add 2nd variable
p2<- ggplot(data1, aes())+
  geom_point(aes(x=per.fny.reports, y=fny.cdc.cor))+
  geom_point(aes(x=per.athena.reports, y=athena.cdc.cor))
p2

# distinguish colors
# http://html-color-codes.com/
p3<- ggplot(data1, aes())+
  geom_point(aes(x=per.fny.reports, y=fny.cdc.cor), color="#FF3333")+
  geom_point(aes(x=per.athena.reports, y=athena.cdc.cor), color="#3333FF")
p3

# add legend
p4<- ggplot(data1, aes())+
  geom_point(aes(x=per.fny.reports, y=fny.cdc.cor, color="FNY"))+
  geom_point(aes(x=per.athena.reports, y=athena.cdc.cor, color="EHR"))
p4

# add legend and specifiy colors
p5<- ggplot(data1, aes())+
  geom_point(aes(x=per.fny.reports, y=fny.cdc.cor, color="FNY"))+
  geom_point(aes(x=per.athena.reports,y=athena.cdc.cor, color="EHR"))+
  scale_color_manual(values=c(FNY="#FF3333",  EHR="#3333FF"))
p5

# add shapes to distinguish geographical resolutions
p6<- ggplot(data1, aes())+
  geom_point(aes(x=per.fny.reports, y=fny.cdc.cor, color="FNY", shape = factor(scale)))+
  geom_point(aes(x=per.athena.reports, y=athena.cdc.cor, color="EHR", shape = factor(scale)))+
  scale_color_manual(values=c(FNY="#FF3333",  EHR="#3333FF"))
p6

# change shapes 
# http://sape.inf.usi.ch/quick-reference/ggplot2/shape
p7<- ggplot(data1, aes())+
  geom_point(aes(x=per.fny.reports, y=fny.cdc.cor, color="FNY", shape = factor(scale)))+
  geom_point(aes(x=per.athena.reports, y=athena.cdc.cor, color="EHR", shape = factor(scale)))+
  scale_color_manual(values=c(FNY="#FF3333",  EHR="#3333FF"))+
  scale_shape_manual(values = c(8, 17, 16, 15))
p7

# change size 
p8<- ggplot(data1, aes())+
  geom_point(aes(x=per.fny.reports, y=fny.cdc.cor, color="FNY", shape = factor(scale)), size=2)+
  geom_point(aes(x=per.athena.reports, y=athena.cdc.cor, color="EHR", shape = factor(scale)), size=2)+
  scale_color_manual(values=c(FNY="#FF3333",  EHR="#3333FF"))+
  scale_shape_manual(values = c(8, 17, 16, 15))
p8

# remove grey background 
p9<- ggplot(data1, aes())+
  geom_point(aes(x=per.fny.reports, y=fny.cdc.cor, color="FNY", shape = factor(scale)), size=2)+
  geom_point(aes(x=per.athena.reports, y=athena.cdc.cor, color="EHR", shape = factor(scale)), size=2)+
  theme_bw()+
  scale_color_manual(values=c(FNY="#FF3333",  EHR="#3333FF"))+
  scale_shape_manual(values = c(8, 17, 16, 15))
p9

# add axis labels 
p10<- ggplot(data1, aes())+
  geom_point(aes(x=per.fny.reports, y=fny.cdc.cor, color="FNY", shape = factor(scale)), size=2)+
  geom_point(aes(x=per.athena.reports, y=athena.cdc.cor, color="EHR", shape = factor(scale)), size=2)+
  theme_bw()+
  scale_color_manual(values=c(FNY="#FF3333",  EHR="#3333FF"))+
  scale_shape_manual(values = c(8, 17, 16, 15))+
  ylab("Correlation")+xlab("Percent of Reports")+
  ggtitle("Example 1")
p10
  
# adjust legend 
p11<- ggplot(data1, aes())+
  geom_point(aes(x=per.fny.reports, y=fny.cdc.cor, color="FNY", shape = factor(scale)), size=2)+
  geom_point(aes(x=per.athena.reports, y=athena.cdc.cor, color="EHR", shape = factor(scale)), size=2)+
  theme_bw()+
  scale_color_manual(values=c(FNY="#FF3333",  EHR="#3333FF"))+
  scale_shape_manual(values = c(8, 17, 16, 15))+
  ylab("Correlation")+xlab("Percent of Reports")+
  ggtitle("Example 1") +
  theme(legend.title=element_blank(),
        legend.justification=c(1,1), 
        legend.position=c(0.98,0.5),
        legend.key = element_rect(colour = "white"),
        legend.key.size = unit(0.5, "cm"))
p11
  
# adjust y-axis
p12<- ggplot(data1, aes())+
  geom_point(aes(x=per.fny.reports, y=fny.cdc.cor, color="FNY", shape = factor(scale)), size=2)+
  geom_point(aes(x=per.athena.reports, y=athena.cdc.cor, color="EHR", shape = factor(scale)), size=2)+
  theme_bw()+
  scale_color_manual(values=c(FNY="#FF3333",  EHR="#3333FF"))+
  scale_shape_manual(values = c(8, 17, 16, 15))+
  scale_y_continuous( limits = c(0,1), expand = c(0,0) )+
  ylab("Correlation")+xlab("Percent of Reports")+
  ggtitle("Example 1") +
  theme(legend.title=element_blank(),
        legend.justification=c(1,1), 
        legend.position=c(0.98,0.5),
        legend.key = element_rect(colour = "white"),
        legend.key.size = unit(0.5, "cm"))
p12

# adjust x-axis
p13<- ggplot(data1, aes())+
  geom_point(aes(x=per.fny.reports, y=fny.cdc.cor, color="FNY", shape = factor(scale)), size=2)+
  geom_point(aes(x=per.athena.reports, y=athena.cdc.cor, color="EHR", shape = factor(scale)), size=2)+
  theme_bw()+
  scale_color_manual(values=c(FNY="#FF3333",  EHR="#3333FF"))+
  scale_shape_manual(values = c(8, 17, 16, 15))+
  scale_y_continuous( limits = c(0,1), expand = c(0,0) )+
  ylab("Correlation")+
  xlab("Number of EHR Reports")+
  scale_x_continuous( limits = c(-0.1,26.5), expand = c(0,0), position="bottom", breaks = c(5, 10, 15, 20, 25),
                      labels = c(43166, 86336, 129504, 172672, 215840), sec.axis= sec_axis(~.*97, name="Number of FNY Reports"))+
  ggtitle("Example 1") +
  theme(legend.title=element_blank(),
        legend.justification=c(1,1), 
        legend.position=c(0.98,0.5),
        legend.key = element_rect(colour = "white"),
        legend.key.size = unit(0.5, "cm"))
p13

# Save the last plot created
ggsave("~/Documents/BU/BUSCASA/ggplot2/example1.pdf", width = 20, height = 20, units = "cm")

######################################################################
# Example from Aya Mitani
# Density plots and Multiplot function

p1 <- ggplot(data = kappas1, aes(x=value, fill=variable)) + 
  geom_vline(xintercept = truekappam, linetype = "longdash") + 
  geom_density(alpha=0.25) +
  theme(legend.position = c(.175, .62),legend.background = element_rect(fill="transparent"))

p2 <- ggplot(data = kappas2, aes(x=value, fill=variable)) + 
  geom_vline(xintercept = truekappam, linetype = "longdash") + 
  geom_density(alpha=0.25) +
  theme(legend.position="none")

p3 <- ggplot(data = kappas3, aes(x=value, fill=variable)) +
  geom_vline(xintercept = truekappam, linetype = "longdash") + 
  geom_density(alpha=0.25) +
  theme(legend.position="none") 

p4 <- ggplot(data = kappas4, aes(x=value, fill=variable)) + 
  geom_vline(xintercept = truekappam, linetype = "longdash") + 
  geom_density(alpha=0.25) +
  theme(legend.position="none") 


multiplot(
  p1 + ggtitle(paste("Scenario ", scn, "a Normal Random Effects", sep="")),
  p2 + ggtitle(paste("Scenario ", scn, "b Normal Random Effects", sep="")),
  p3 + ggtitle(paste("Scenario ", scn, "a Non-normal Random Effects", sep="")),
  p4 + ggtitle(paste("Scenario ", scn, "b Non-normal Random Effects", sep="")), 
  cols=2
)


######################################################################
# Example from Elise Lim
# Confidence bars and grid arrange

#ctth 
ctth.rm<-c(0.867, 0.905, 0.921, 0.936)
ctth.rl<-c(0.835, 0.873, 0.889, 0.904)
ctth.ru<-c(0.899, 0.938, 0.952, 0.968)

ctth.tm<-c(1.200, 1.257, 1.278, 1.299)
ctth.tl<-c(1.163, 1.219, 1.240, 1.260)
ctth.tu<-c(1.238, 1.296, 1.316, 1.339)

d1<-data.frame(Bone=factor(c("Radius","Radius","Radius","Radius","Tibia","Tibia","Tibia","Tibia"),levels=c("Radius","Tibia")),lsmeans=c(ctth.rm,ctth.tm),Quartile=factor(c("1","2","3","4"),levels=c("1","2","3","4")),low=c(ctth.rl,ctth.tl),high=c(ctth.ru,ctth.tu))

g1<-ggplot(data=d1, aes(x=Quartile, y=lsmeans,group=Bone,colour=Bone)) + 
  geom_point(size=2.5,aes(colour=factor(Bone)),show.legend=F) + 
  geom_line(aes(colour=Bone),size=1) + 
  geom_errorbar(aes(ymin=low, ymax=high,colour=Bone),width=0.1,size=1) + 
  theme(legend.title=element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Cortical Thickness LS-means by VAT Quartile")+
  labs(x="Visceral Fat Quartile",y="Cortical Thickness (mm)") + 
  theme(text = element_text(size=10))+
  geom_text(aes(x=4, y=0.99, label="p=0.0017"), size=4, color="black",show.legend=F)+
  geom_text(aes(x=4, y=1.24, label="p=0.0002"), size=4,color="black",show.legend=F)

#ctbata 
ctbata.rm<-c(0.202, 0.212, 0.213, 0.218)
ctbata.rl<-c(0.193, 0.203, 0.205, 0.209)
ctbata.ru<-c(0.210, 0.220, 0.222, 0.227)

ctbata.tm<-c(0.166, 0.175, 0.178, 0.183)
ctbata.tl<-c(0.160, 0.169, 0.172, 0.177)
ctbata.tu<-c(0.172, 0.181, 0.184, 0.189)

  
d2<-data.frame(Bone=factor(c("Radius","Radius","Radius","Radius","Tibia","Tibia","Tibia","Tibia"),levels=c("Radius","Tibia")),lsmeans=c(ctbata.rm,ctbata.tm),Quartile=factor(c("1","2","3","4"),levels=c("1","2","3","4")),low=c(ctbata.rl,ctbata.tl),high=c(ctbata.ru,ctbata.tu))

g2<-ggplot(data=d2, aes(x=Quartile, y=lsmeans,group=Bone,colour=Bone)) + 
  geom_point(size=2.5,aes(colour=factor(Bone)),show.legend=F) + 
  geom_line(aes(colour=Bone),size=1) + 
  geom_errorbar(aes(ymin=low, ymax=high,colour=Bone),width=0.1,size=1) + 
  theme(legend.title=element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Cortical bone area fraction LS-means by VAT Quartile")+
  labs(x="Visceral Fat Quartile",y="Cortical bone area fraction") + 
  theme(text = element_text(size=10))+
  geom_text(aes(x=4, y=0.206, label="p=0.0073"), size=4, color="black",show.legend=F)+
  geom_text(aes(x=4, y=0.174, label="p<0.0001"), size=4,color="black",show.legend=F)

#tbdens
tbdens.rm<-c(166.527, 170.330, 171.916, 178.002)
tbdens.rl<-c(160.415, 164.099, 165.784, 171.854)
tbdens.ru<-c(172.639, 176.561, 178.048, 184.150)

tbdens.tm<-c(169.501, 180.570, 181.212, 187.129)
tbdens.tl<-c(163.816, 174.824, 175.459, 181.224)
tbdens.tu<-c(175.186, 186.317, 186.964, 193.034)

d3<-data.frame(Bone=factor(c("Radius","Radius","Radius","Radius","Tibia","Tibia","Tibia","Tibia"),levels=c("Radius","Tibia")),lsmeans=c(tbdens.rm,tbdens.tm),Quartile=factor(c("1","2","3","4"),levels=c("1","2","3","4")),low=c(tbdens.rl,tbdens.tl),high=c(tbdens.ru,tbdens.tu))

g3<-ggplot(data=d3, aes(x=Quartile, y=lsmeans,group=Bone,colour=Bone)) + 
  geom_point(size=2.5,aes(colour=factor(Bone)),show.legend=F) + 
  geom_line(aes(colour=Bone),size=1) + 
  geom_errorbar(aes(ymin=low, ymax=high,colour=Bone),width=0.1,size=1) + 
  theme(legend.title=element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Trabecular BMD LS-means by VAT Quartile")+
  labs(x="Visceral Fat Quartile")+ylab(bquote('Trabecular BMD (mgHA/'*cm^3*')')) + 
  theme(text = element_text(size=10)) + 
  geom_text(aes(x=4, y=170, label="p=0.0082"), size=4, color="black",show.legend=F)+
  geom_text(aes(x=4, y=195, label="p<0.0001"), size=4,color="black",show.legend=F)

#tbn
tbn.rm<-c(2.057, 2.083, 2.125, 2.195)
tbn.rl<-c(2.003, 2.027, 2.072, 2.141)
tbn.ru<-c(2.110, 2.137, 2.179, 2.249)

tbn.tm<-c(1.969, 2.123, 2.120, 2.217)
tbn.tl<-c(1.917, 2.070, 2.068, 2.162)
tbn.tu<-c(2.021, 2.176, 2.172, 2.271)


d4<-data.frame(Bone=factor(c("Radius","Radius","Radius","Radius","Tibia","Tibia","Tibia","Tibia"),levels=c("Radius","Tibia")),lsmeans=c(tbn.rm,tbn.tm),Quartile=factor(c("1","2","3","4"),levels=c("1","2","3","4")),low=c(tbn.rl,tbn.tl),high=c(tbn.ru,tbn.tu))

g4<-ggplot(data=d4, aes(x=Quartile, y=lsmeans,group=Bone,colour=Bone)) + 
  geom_point(size=2.5,aes(colour=factor(Bone)),show.legend=F) + 
  geom_line(aes(colour=Bone),size=1) + 
  geom_errorbar(aes(ymin=low, ymax=high,colour=Bone),width=0.1,size=1) + 
  theme(legend.title=element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Trabecular Number LS-means by VAT Quartile")+
  labs(x="Visceral Fat Quartile",y="Trabecular Number (1/mm)") + 
  theme(text = element_text(size=10)) + 
  geom_text(aes(x=1.5, y=1.92, label="p<0.0001"), size=4, color="black",show.legend=F)+
  geom_text(aes(x=1, y=2.13, label="p=0.0001"), size=4,color="black",show.legend=F)

#failload
failload.rm<-c(2533.880, 2701.444, 2634.777, 2706.339)
failload.rl<-c(2459.323, 2625.183, 2560.354, 2630.406)
failload.ru<-c(2608.437, 2777.704, 2709.199, 2782.271)

failload.tm<-c(6443.686, 6725.080, 6732.009, 6823.378)
failload.tl<-c(6288.160, 6567.319, 6574.338, 6658.452)
failload.tu<-c(6599.212, 6882.841, 6889.680, 6988.303)


d5<-data.frame(Bone=factor(c("Radius","Radius","Radius","Radius","Tibia","Tibia","Tibia","Tibia"),levels=c("Radius","Tibia")),lsmeans=c(failload.rm,failload.tm),Quartile=factor(c("1","2","3","4"),levels=c("1","2","3","4")),low=c(failload.rl,failload.tl),high=c(failload.ru,failload.tu))

g5<-ggplot(data=d5, aes(x=Quartile, y=lsmeans,group=Bone,colour=Bone)) + 
  geom_point(size=2.5,aes(colour=factor(Bone)),show.legend=F) + 
  geom_line(aes(colour=Bone),size=1) + 
  geom_errorbar(aes(ymin=low, ymax=high,colour=Bone),width=0.1,size=1) + 
  theme(legend.title=element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Failure load LS-means by VAT Quartile")+
  labs(x="Visceral Fat Quartile",y="Failure Load (N)") + 
  theme(text = element_text(size=10)) + 
  geom_text(aes(x=4, y=3000, label="p=0.0072"), size=4, color="black",show.legend=F)+
  geom_text(aes(x=4, y=6400, label="p=0.0012"), size=4,color="black",show.legend=F)

#ctpo
ctpo.rm<-c(3.811, 3.822, 3.979, 4.384)
ctpo.rl<-c(3.435, 3.437, 3.605, 4.005)
ctpo.ru<-c(4.188, 4.206, 4.353, 4.763)

ctpo.tm<-c(9.681, 9.982, 10.129, 10.248)
ctpo.tl<-c(9.069, 9.360, 9.511, 9.607)
ctpo.tu<-c(10.293, 10.604, 10.746, 10.889)

d6<-data.frame(Bone=factor(c("Radius","Radius","Radius","Radius","Tibia","Tibia","Tibia","Tibia"),levels=c("Radius","Tibia")),lsmeans=c(ctpo.rm,ctpo.tm),Quartile=factor(c("1","2","3","4"),levels=c("1","2","3","4")),low=c(ctpo.rl,ctpo.tl),high=c(ctpo.ru,ctpo.tu))

g6<-ggplot(data=d6, aes(x=Quartile, y=lsmeans,group=Bone,colour=Bone)) + 
  geom_point(size=2.5,aes(colour=factor(Bone)),show.legend=F) + 
  geom_line(aes(colour=Bone),size=1) + 
  geom_errorbar(aes(ymin=low, ymax=high,colour=Bone),width=0.1,size=1) + 
  theme(legend.title=element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Cortical Porosity LS-means by VAT Quartile")+
  labs(x="Visceral Fat Quartile",y="Cortical Porosity (%)") + 
  theme(text = element_text(size=10)) +
  geom_text(aes(x=4, y=3.7, label="p=0.026"), size=4, color="black",show.legend=F)+
  geom_text(aes(x=4, y=9.3, label="p=0.182"), size=4, color="black",show.legend=F)


library(gridExtra)
tiff(filename = "plots.tiff", width=660, height=680)
grid.arrange(g1,g2,g3,g4,g5,g6)
dev.off()


######################################################################
# Example from Sam Lent
# Scatterplot with arrangeGrob

library(ggplot2)
library(ggrepel)
library(grid)
library(gridExtra)
library(gtable)

# Load dataframe with nearest genes (geneannots) and 8 data frames with results: VG50, GG50, VG50, GG75, VB50, GB50, VB50, GB75
load('/Users/lent/Documents/research/hphc/ogct/ggplot_ExData.RData')
# First character in data frame name (V or G) represents cohort, 
# second character (B or G) represents sex, and the number (50 or 75) 
# represents then amount of glucose in the exposure

# We want to compare the effect size estimate of the replication 
# cohort (G) to for both replication exposures (50 and 75) to the
# effect size estimate of the original cohort (V) for the primary
# exposure (50)

# We will have 8 scatter plots and we want 2 figures (one for each sex) with 4 plots each (2x2)

#Function to create individual plot panel (one sex, one replication exposure)
effectsizecomp<-function(vivadf,gen3gdf,title) {
  plotres<-merge(vivadf,gen3gdf,by='probeID')
  colnames(plotres)<-c('probeID','viva.estimate','viva.se','viva.p','gen3g.estimate','gen3g.se','gen3g.p')
  plotres$gen3g.log10p<-(-1)*log10(plotres$gen3g.p)
  plotres$nearest.gene<-ifelse(plotres$gen3g.p>0.05 | sign(plotres$gen3g.estimate)!=sign(plotres$viva.estimate),'',genes[plotres$probeID,'nearestGeneSymbol'])
  p<-ggplot() + geom_point(data=plotres,aes(x=viva.estimate,y=gen3g.estimate,colour=gen3g.log10p)) + 
    scale_colour_gradient(name='Gen3G\n-log10 P', low='black', high='red',limits=c(0,4)) + 
    scale_x_continuous(limits=c(-0.027,0.045)) + 
    scale_y_continuous(limits=c(-0.015,0.02)) + 
    ggtitle(title) + theme_bw() + 
    geom_text_repel(aes(x=viva.estimate,y=gen3g.estimate,label=nearest.gene),data=plotres,force=5,fontface='italic') + 
    geom_abline(slope=1,intercept=0,linetype=3) + 
    theme(axis.title.y = element_blank(),axis.title.x=element_blank(),plot.title=element_text(size=10,face='bold'),panel.background=element_blank())
  p<-p+guides(colour = guide_colorbar(barwidth = 0.75, barheight = 10,title.hjust=0.5))
  return(p)
}


p11<-effectsizecomp(VB50,GB50,'A) Boys Trimester 1 50g replication')
p12<-effectsizecomp(VB50,GB75,'B) Boys Trimester 2 75g replication')
p21<-effectsizecomp(VG50,GG50,'C) Girls Trimester 1 50g replication')
p22<-effectsizecomp(VG50,GG75,'D) Girls Trimester 2 75g replication')


#Extract legend from panel 1
legend = gtable_filter(ggplotGrob(p11), "guide-box") 

# Arrange the elements to be plotted. 
# The inner arrangeGrob() function arranges the four plots, the main title, 
# and the global y-axis title.
# The outer grid.arrange() function arranges and draws the arrangeGrob object and the legend.
jpeg(file='/Users/lent/Documents/research/EffectSizeComparison.jpg',height=500,width=550,quality=100)
grid.arrange(arrangeGrob(p11 + theme(legend.position="none"), 
                         p12 + theme(legend.position="none"),
                         p21 + theme(legend.position="none"),
                         p22 + theme(legend.position="none"), 
                         nrow = 2,
                         bottom = textGrob("Project Viva Effect Size", hjust = 0.5),
                         left = textGrob("Gen3G Effect Size", rot = 90, vjust = 1)),
             legend, widths=unit.c(unit(1, "npc") - legend$width, legend$width), nrow=1)
dev.off()


