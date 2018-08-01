df<-read.csv("~/FOLDER/SUBFOLDER/FILE.csv") # some file

y <- quantile(df$col_name, c(0.25,0.75)) #col_name in df

x<-qnorm(c(0.25,0.75))

slope<-diff(y)/diff(x)

int<-y[1]-slope*x[1]

df$Select_68<-as.factor(df$Select_68) # The select_68 column as factor

df$Select_68<-df$Select_68+1 #correct to correct scale by adding 1 will be used to colour everything according to this

g<-ggplot() + aes(sample=df$D_VAS_68) + geom_text(label=df$ID,color=df$Select_68, stat="qq", position= position_dodge(width=0.9), size = 5) + geom_abline(intercept=int, slope= slope, size =1) + ylab("Difference in VAS pain score at 68 weeks (mm)") + geom_abline(intercept=-6.75,slope=0, linetype="dotted") + geom_abline(intercept=18.75, slope=0, linetype="dotted")

plot(g+ geom_abline(intercept =5.4, slope = 0) + geom_abline(intercept=6.6, slope=0))

plot1<-plot(g + theme(plot.title=element_text(size=30, face="bold"), 
               axis.text.x=element_text(size=15), 
               axis.text.y=element_text(size=15),
               
               axis.title.x=element_text(size=20),
               axis.title.y=element_text(size=20)))
########################################################################
###### Different colmn plot                                     ########
########################################################################

#ggdensity(df, x= df$D_VAS_16, y="..count..", label=df$ID, label.select=df$ID)
#gd<-ggplot(df, aes(x=D_VAS_68)) + geom_density(color="darkblue", fill="lightblue", size =1) + geom_vline(aes(xintercept=mean(D_VAS_68)), color="blue", linetype="dashed") + xlab("Difference in VAS pain score at 68 weeks (mm)")
#plot1<-plot(gd + theme(plot.title=element_text(size=30, face="bold"), 
 #                     axis.text.x=element_text(size=15), 
#                      axis.text.y=element_text(size=15),
#                      axis.title.x=element_text(size=15),
#                      axis.title.y=element_text(size=15)))

#mu<-ddply(df,"Select_16", summarise, grp.mean=mean(D_VAS_16))

#head(mu)

#ggplot(df,aes(x=D_VAS_16, color=Select_16)) + geom_density()+ geom_vline(data=mu, aes(xintercept=grp.mean, color=Select_16), linetype="dashed")
