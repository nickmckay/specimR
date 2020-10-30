#load in data files

#raw <- brick("Lakes380_NGAHE_LC1U_1B_S2_2019-11-12_00-02-58.raw")
stripe <- brick('stripeRaster.tif')
normalized <- brick('normalizedRaster.tif')
#scaleY <- read.csv("scaleY.csv")
#spectra <- read.csv("spectra.csv")
#allBands <- read.csv("allBands.csv")
indices <- read.csv("indices.csv")
image <- brick("Lakes380_NGAHE_LC1U_1B_S2_2019-11-12_00-02-58 .png")

#NICK -- since I couldnt send raw file, I entered the values here for this specific file
raw@extent@ymax <- 25600
raw@extent@ymin <- 0


ratio <- (stripe@extent@ymax-stripe@extent@ymin)/(raw@extent@ymax-raw@extent@ymin)


#plot RGB image
 plotRGB(image,stretch='hist')
 plotRGB(stripe,stretch='hist',add=TRUE)

#Record
p1_recorded <- recordPlot()
dim(p1_recorded)
#Redraw
t <- ggdraw()+draw_plot(p1_recorded,scale=1)
resizePanels(t,h=1,w=10)

##Rastervis plot - 'trellis' object
a <- rasterVis::levelplot(normalized, layers=1, colorkey=FALSE,add=TRUE, at=seq(0,.25,length.out=15), xlab=NULL, ylab=NULL, scales=list(draw=FALSE))

#two formatting options for plotting levelplot
update(a, aspect=1.8)
c <- resizePanels(a, h=1,w=10)

#Different ways to try and store object
c2 <- ggdraw()+ draw_plot(c)
p <- grid.grab()
p2_recorded <- recordPlot()

##RABD PLOTTING

RABD660 <- ggplot(data = indices)+geom_path(aes(x=RABD660,y=scaleYmm),alpha=.3)+geom_path(aes(y=scaleYmm,x=rollmean(RABD660,10,na.pad=TRUE)))+
  theme_classic()+scale_y_reverse()+geom_vline(xintercept = mean(indices$RABD660))+
  scale_x_discrete(position="top")

RABD845 <- ggplot(data = indices)+geom_path(aes(x=RABD845,y=scaleYmm),alpha=.3)+geom_path(aes(y=scaleYmm,x=rollmean(RABD845,10,na.pad=TRUE)))+
  theme_classic()+scale_y_reverse()+geom_vline(xintercept = 1)+ theme(axis.title.y=element_blank(),
                                                                      axis.text.y=element_blank(),
                                                                      axis.ticks.y=element_blank(),
                                                                      axis.line.y = element_blank())

R570_R630 <- ggplot(data = indices)+geom_path(aes(x=R570_R630,y=scaleYmm),alpha=.3)+geom_path(aes(y=scaleYmm,x=rollmean(R570_R630,10,na.pad=TRUE)))+
  theme_classic()+scale_y_reverse()+geom_vline(xintercept = mean(indices$R570_R630))+ theme(axis.title.y=element_blank(),
                                                                                            axis.text.y=element_blank(),
                                                                                            axis.ticks.y=element_blank(),
                                                                                            axis.line.y = element_blank())

R590_R690 <- ggplot(data = indices)+geom_path(aes(x=R590_R690,y=scaleYmm),alpha=.3)+geom_path(aes(y=scaleYmm,x=rollmean(R590_R690,10,na.pad=TRUE)))+
  theme_classic()+geom_vline(xintercept = mean(indices$R590_R690))+ theme(axis.title.y=element_blank(),
                                                                          axis.text.y=element_blank(),
                                                                          axis.ticks.y=element_blank())+scale_y_reverse(position = "right")

grid.arrange(t,c, RABD660,RABD845,R570_R630,R590_R690,ncol=6, nrow=1)
extent(stripe)
extent()
R660 <- egg::set_panel_size(RABD660, margin = unit(.5, "cm"),
                                             width = unit(4*ratio, "cm"),
                                            height = unit(8*ratio, "cm"))
tX <- egg::set_panel_size(t, margin = unit(.5, "cm"),
                          width = unit(6, "cm"),
                          height=unit(8, "cm"))
cX <- egg::set_panel_size(c2, margin = unit(2, "cm"),
                          width = unit(2, "cm"),
                          height=unit(6, "cm"))

grid.arrange(tX,cX,R660, ncol=3,nrow=1)

lay <- rbind(c(1,1,2))
   #          c(1,2,3),
    #         c(1,2,3))
grid.arrange(t,c2,R660,layout_matrix=lay)
grid.arrange(tX,R660,layout_matrix=lay)



)
