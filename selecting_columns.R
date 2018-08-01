##################################################################
#####Selecting those genes that were significantly expressed######
#### A1 is the file of the raw data that was tested with a t-test#
#### tt1 is the file that has the t-test results##################
###identify the ones with p<0.05 and write the new file A3########
##################################################################
A1<-read.csv("~/Documents/PH_SOMA_HIP/HIP_qn_raw_1.csv")

tt1<-read.csv("~/Documents/PH_SOMA_HIP/t_test_hip_qn_adj.csv")

head(tt1)

tt2<-as.data.frame(t(tt1[,-1])) # drop 1st column and transpose tt1

colnames(tt2)<-tt1$X # extract the column X and use it as names for tt2 columns

A2<-rbind.data.frame(A1,tt2[2,]) # combine A1 and tt2

A3<-data.frame(0,0) # initialise df A3

xr<-nrow(A2)

xR<-xr-1

for (i in 1:length(A2)) {

    if (A2[xr,i] <= 0.05)
  
       A3<- cbind(A3,A2[1:xR,i, drop=FALSE])

    }

A3<-A3[,-(1:2),drop=FALSE] # duplicated columns dropped

write.csv(A3, "~/Documents/PH_SOMA_HIP/NEW/HIP_qn_sig_1.csv", col.names = TRUE, row.names = TRUE)
