library("dummies")
library("AER")
library("plotly")
library('RColorBrewer')
library("rgl")
library("data.table")
library("mlogit")
library("gmnl")


rm(list=ls())


setwd("~/Desktop/Pricing Analytics/Project/Project 2")
kiwi <- read.csv('kiwi_bubbles_P2.csv',stringsAsFactors = FALSE, header = TRUE)

################################  Question 3  ###############################################
demand=function(price1,price2,price3,beta01,beta02,beta03,beta1){
    prob=exp(beta01+beta1*price1)/(1+exp(beta01+beta1*price1)+exp(beta02+beta1*price2)+exp(beta03+beta1*price3))
    return(prob)
}


uc=0.5;

kiwi=kiwi[!(kiwi$price.KB==99),]
kiwi=kiwi[!(kiwi$price.KR==99),]
kiwi=kiwi[!(kiwi$price.MB==99),]

avgp_kb <- sum(kiwi$price.KB)/nrow(kiwi)
avgp_kr <- sum(kiwi$price.KR)/nrow(kiwi)
avgp_mb <- sum(kiwi$price.MB)/nrow(kiwi)

mlogitdata=mlogit.data(kiwi,id="id",varying=4:7,choice="choice",shape="wide")

mle = gmnl(choice ~  price, data = mlogitdata)
summary(mle)



##############CHOICE PROBABILITY########################
probKB <- demand(avgp_kb,avgp_kr,avgp_mb,mle$coefficients[1],mle$coefficients[2],mle$coefficients[3],mle$coefficients[4])
probKR <- demand(avgp_kr,avgp_kb,avgp_mb,mle$coefficients[2],mle$coefficients[1],mle$coefficients[3],mle$coefficients[4])
probMB <- demand(avgp_mb,avgp_kb,avgp_kr,mle$coefficients[3],mle$coefficients[1],mle$coefficients[2],mle$coefficients[4])


#############ELASTICITY################################
own_elasticity <- function(beta1,price,prob){
    elasticity <- -beta1*price*(1-prob)
    return(elasticity)
}

E_KB <- own_elasticity(mle$coefficients[4],avgp_kb,probKB)
E_KR <- own_elasticity(mle$coefficients[4],avgp_kr,probKR)
E_MB <- own_elasticity(mle$coefficients[4],avgp_mb,probMB)

cross_elasticity <- function(beta1,price_competitor,prob_competitor){
    cross_elasticity <- -beta1*price_competitor*prob_competitor
    return(cross_elasticity)
}

#KB&KR
CE_KB_KR <- cross_elasticity(mle$coefficients[4],avgp_kr,probKR)
#KB&MB
CE_KB_MB <- cross_elasticity(mle$coefficients[4],avgp_mb,probMB)
#KR&KB
CE_KR_KB <- cross_elasticity(mle$coefficients[4],avgp_kb,probKB)
#KR&MB
CE_KR_MB <- cross_elasticity(mle$coefficients[4],avgp_mb,probMB)
#MB&KB
CE_MB_KB <- cross_elasticity(mle$coefficients[4],avgp_kb,probKB)
#MB&KR
CE_MB_KR <- cross_elasticity(mle$coefficients[4],avgp_mb,probMB)


######################DEMAND & PROFIT################################
demand_all=function(priceKB,priceKR,priceMB,para = mle$coefficients){
    probKB=exp(para[1]+para[4]*priceKB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
    probKR=exp(para[2]+para[4]*priceKR)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
    return(cbind(probKB,probKR))
}

demand_all=function(priceKR,priceMB,para = coef.est3){
    probKR=exp(para[1]+para[3]*priceKR)/(1+exp(para[1]+para[3]*priceKR)+exp(para[2]+para[3]*priceMB))
    return(probKR)
}


profit=function(priceKB,priceKR,priceMB,para = mle$coefficients){
    profitKB=demand_all(priceKB,priceKR,1.43,para)[,1]*(priceKB-uc)*1000
    profitKR=demand_all(priceKB,priceKR,1.43,para)[,2]*(priceKR-uc)*1000
    return(cbind(profitKB,profitKR))
}


####################OPTIMAL PRICE###################################
aux=seq(1,3,0.01)
pricespace=expand.grid(aux,aux)

profitmat=matrix(0L,nrow(pricespace),1)
for (i in 1:nrow(pricespace)){
    profitmat[i]=sum(profit(pricespace[i,1],pricespace[i,2],1.43,para = mle$coefficients))  
}
opti_KB_KR <- pricespace[profitmat==max(profitmat)]

####################PLOT###########################################
xaxis=list(title="P^{KB}")
yaxis=list(autorange = "reversed",title="P^{KR}")
zaxis=list(title="Profit")
p=plot_ly(x=pricespace[,1],y=pricespace[,2],z=as.numeric(profitmat),
          type="scatter3d",mode="markers",
          marker = list(color = as.numeric(profitmat), colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))%>%
    layout(scene=list(xaxis=xaxis,yaxis=yaxis,zaxis=zaxis))%>%
    config(mathjax = 'cdn')

p


################################  Question 4  ###############################################
aux=seq(0.5,1.8,0.01)
pricespace_1=expand.grid(aux,aux)

set.seed(0)
demo=fread("demo_P2.csv",stringsAsFactors = F)
N = 359
coef_noseg=mle$coefficients
demo_cluster = kmeans(x = demo[, 2:18], centers = 6, nstart = 1000)

cluster_id = data.frame(id = demo$id)
cluster_id$cluster = demo_cluster$cluster
kiwi = merge(kiwi, cluster_id, by = "id", all.x = T)

kiwi$cluster[is.na(kiwi$cluster)] = 7



# segment share
seg.share = c( table(demo_cluster$cluster),N - sum(table(demo_cluster$cluster))) / N	

# just store the coefficients (you can store many other things)
coef.est = data.frame(segment = 1:7, intercept.KB = NA, intercept.KR = NA, 
                      intercept.MB = NA, price.coef = NA) 
for (seg in 1:7) {
    # During each loop, pick subset of data of consumers from each segment.
    kiwi.sub = subset(kiwi, cluster == seg)
    
    #Using that data, the rest remains the same.
    mlogitdata=mlogit.data(kiwi.sub,id="id",varying=4:7,choice="choice",shape="wide")
    
    #Run MLE.
    mle= gmnl(choice ~  price, data = mlogitdata)
    mle
    #Store the outcome in the coef.est matrix.
    coef.est[seg, 2:5] = mle$coefficients
}



########################DEMAND FOR EACH PRODUCT##############################

demand_3P <- function(priceKB,priceKR,priceMB,para){
    probKB=exp(para[1]+para[4]*priceKB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
    probKR=exp(para[2]+para[4]*priceKR)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
    probMB=exp(para[3]+para[4]*priceMB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
    return(cbind(probKB,probKR,probMB))
}


uc=0.5

profit_3P=function(priceKB,priceKR,priceMB){
    profitKB=1000*(demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))[,1]*(priceKB-uc)*seg.share[1]+
                       demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))[,1]*(priceKB-uc)*seg.share[2]+
                       demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))[,1]*(priceKB-uc)*seg.share[3]+
                       demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))[,1]*(priceKB-uc)*seg.share[4]+
                       demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))[,1]*(priceKB-uc)*seg.share[5]+
                       demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[6,2:5]))[,1]*(priceKB-uc)*seg.share[6]+
                       demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[7,2:5]))[,1]*(priceKB-uc)*seg.share[7])
    profitKR=1000*(demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))[,2]*(priceKR-uc)*seg.share[1]+
                       demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))[,2]*(priceKR-uc)*seg.share[2]+
                       demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))[,2]*(priceKR-uc)*seg.share[3]+
                       demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))[,2]*(priceKR-uc)*seg.share[4]+
                       demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))[,2]*(priceKR-uc)*seg.share[5]+
                       demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[6,2:5]))[,2]*(priceKR-uc)*seg.share[6]+
                       demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[7,2:5]))[,2]*(priceKR-uc)*seg.share[7])
    profitMB=1000*(demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))[,3]*(priceMB-uc)*seg.share[1]+
                       demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))[,3]*(priceMB-uc)*seg.share[2]+
                       demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))[,3]*(priceMB-uc)*seg.share[3]+
                       demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))[,3]*(priceMB-uc)*seg.share[4]+
                       demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))[,3]*(priceMB-uc)*seg.share[5]+
                       demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[6,2:5]))[,3]*(priceMB-uc)*seg.share[6]+
                       demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[7,2:5]))[,3]*(priceMB-uc)*seg.share[7])
    return(cbind(profitKB,profitKR,profitMB))
}




profitmat_3P=matrix(0L,nrow(pricespace_1),1)
for (i in 1:nrow(pricespace_1)){
    profitmat_3P[i]=sum(profit_3P(pricespace_1[i,1],pricespace_1[i,2],1.43)[,1:2]) 
}
opti_KB <- pricespace_1[,1][profitmat_3P==max(profitmat_3P)]
opti_KR <- pricespace_1[,2][profitmat_3P==max(profitmat_3P)]

###################Segments of KB & KR###############################
plot(coef.est[1,2]-coef.est[1,3],coef.est[1,5],cex=20*seg.share[1],xlim=c(-2,2),ylim=c(-6,-2),
     col = "chocolate",pch=16,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="beta_0^KB-beta_0^KR",ylab=("beta_1"))
points(coef.est[2,2]-coef.est[2,3],coef.est[2,5],cex=20*seg.share[2],col = "chocolate",pch=16)
points(coef.est[3,2]-coef.est[3,3],coef.est[3,5],cex=20*seg.share[3],col = "chocolate",pch=16)
points(coef.est[4,2]-coef.est[4,3],coef.est[4,5],cex=20*seg.share[4],col = "chocolate",pch=16)
points(coef.est[5,2]-coef.est[5,3],coef.est[5,5],cex=20*seg.share[5],col = "chocolate",pch=16)
points(coef.est[6,2]-coef.est[6,3],coef.est[6,5],cex=20*seg.share[6],col = "chocolate",pch=16)
points(coef.est[7,2]-coef.est[7,3],coef.est[7,5],cex=20*seg.share[7],col = "chocolate",pch=16)


###################Segments of KB & MB###############################

plot(coef.est[1,2]-coef.est[1,4],coef.est[1,5],cex=20*seg.share[1],xlim=c(-1,1),ylim=c(-5,-1.5),
     col = "chocolate",pch=16,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="beta_0^KB-beta_0^MB",ylab=("beta_1"))
points(coef.est[2,2]-coef.est[2,4],coef.est[2,5],cex=20*seg.share[2],col = "chocolate",pch=16)
points(coef.est[3,2]-coef.est[3,4],coef.est[3,5],cex=20*seg.share[3],col = "chocolate",pch=16)
points(coef.est[4,2]-coef.est[4,4],coef.est[4,5],cex=20*seg.share[4],col = "chocolate",pch=16)
points(coef.est[5,2]-coef.est[5,4],coef.est[5,5],cex=20*seg.share[5],col = "chocolate",pch=16)
points(coef.est[6,2]-coef.est[6,4],coef.est[6,5],cex=20*seg.share[6],col = "chocolate",pch=16)
points(coef.est[7,2]-coef.est[7,4],coef.est[7,5],cex=20*seg.share[7],col = "chocolate",pch=16)


###################Segments of KR & MB###############################

plot(coef.est[1,3]-coef.est[1,4],coef.est[1,5],cex=20*seg.share[1],xlim=c(-1,1),ylim=c(-6,-1.5),
     col = "chocolate",pch=16,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="beta_0^KR-beta_0^MB",ylab=("beta_1"))
points(coef.est[2,3]-coef.est[2,4],coef.est[2,5],cex=20*seg.share[2],col = "chocolate",pch=16)
points(coef.est[3,3]-coef.est[3,4],coef.est[3,5],cex=20*seg.share[3],col = "chocolate",pch=16)
points(coef.est[4,3]-coef.est[4,4],coef.est[4,5],cex=20*seg.share[4],col = "chocolate",pch=16)
points(coef.est[5,3]-coef.est[5,4],coef.est[5,5],cex=20*seg.share[5],col = "chocolate",pch=16)
points(coef.est[6,3]-coef.est[6,4],coef.est[6,5],cex=20*seg.share[6],col = "chocolate",pch=16)
points(coef.est[7,3]-coef.est[7,4],coef.est[7,5],cex=20*seg.share[7],col = "chocolate",pch=16)

#################Define the aggregat choice probability#############
agg_choice <- function(priceKB,priceKR,priceMB) {
    
    agg_choice_KB=seg.share[1]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))[,1]+
        seg.share[2]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))[,1]+
        seg.share[3]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))[,1]+
        seg.share[4]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))[,1]+
        seg.share[5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))[,1]+
        seg.share[6]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[6,2:5]))[,1]+
        seg.share[7]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[7,2:5]))[,1]
    agg_choice_KR=seg.share[1]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))[,2]+
        seg.share[2]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))[,2]+
        seg.share[3]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))[,2]+
        seg.share[4]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))[,2]+
        seg.share[5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))[,2]+
        seg.share[6]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[6,2:5]))[,2]+
        seg.share[7]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[7,2:5]))[,2]
    agg_choice_MB=seg.share[1]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))[,3]+
        seg.share[2]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))[,3]+
        seg.share[3]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))[,3]+
        seg.share[4]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))[,3]+
        seg.share[5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))[,3]+
        seg.share[6]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[6,2:5]))[,3]+
        seg.share[7]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[7,2:5]))[,3]
        
        return(cbind(agg_choice_KB,agg_choice_KR,agg_choice_MB))
}



agg_choice(avgp_kb,avgp_kr,avgp_mb)
agg_choice(1.15,1.18,1.43)
#################Own elasticity##################################
own_e_seg <- function(priceKB,priceKR,priceMB) {
    
    elasticity_KB=-priceKB/agg_choice(priceKB,priceKR,priceMB)[,1]*(seg.share[1]*coef.est[1,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))[,1]*(1-demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))[,1])+
            seg.share[2]*coef.est[2,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))[,1]*(1-demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))[,1])+
            seg.share[3]*coef.est[3,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))[,1]*(1-demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))[,1])+
            seg.share[4]*coef.est[4,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))[,1]*(1-demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))[,1])+
            seg.share[5]*coef.est[5,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))[,1]*(1-demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))[,1])+
            seg.share[6]*coef.est[6,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[6,2:5]))[,1]*(1-demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[6,2:5]))[,1])+
            seg.share[7]*coef.est[7,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[7,2:5]))[,1]*(1-demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[7,2:5]))[,1]))
    elasticity_KR=-priceKR/agg_choice(priceKB,priceKR,priceMB)[,2]*(seg.share[1]*coef.est[1,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))[,2]*(1-demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))[,2])+
            seg.share[2]*coef.est[2,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))[,2]*(1-demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))[,2])+
            seg.share[3]*coef.est[3,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))[,2]*(1-demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))[,2])+
            seg.share[4]*coef.est[4,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))[,2]*(1-demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))[,2])+
            seg.share[5]*coef.est[5,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))[,2]*(1-demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))[,2])+
            seg.share[6]*coef.est[6,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[6,2:5]))[,2]*(1-demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[6,2:5]))[,2])+
            seg.share[7]*coef.est[7,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[7,2:5]))[,2]*(1-demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[7,2:5]))[,2]))
    elasticity_MB=-priceMB/agg_choice(priceKB,priceKR,priceMB)[,3]*(seg.share[1]*coef.est[1,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))[,3]*(1-demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))[,3])+
            seg.share[2]*coef.est[2,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))[,3]*(1-demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))[,3])+
            seg.share[3]*coef.est[3,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))[,3]*(1-demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))[,3])+
            seg.share[4]*coef.est[4,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))[,3]*(1-demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))[,3])+
            seg.share[5]*coef.est[5,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))[,3]*(1-demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))[,3])+
            seg.share[6]*coef.est[6,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[6,2:5]))[,3]*(1-demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[6,2:5]))[,3])+
            seg.share[7]*coef.est[7,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[7,2:5]))[,3]*(1-demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[7,2:5]))[,3]))
        
    return(cbind(elasticity_KB,elasticity_KR,elasticity_MB))
}

own_e_seg(avgp_kb,avgp_kr,avgp_mb)

#################Cross elasticity###############################

cross_e_seg <- function(priceKB,priceKR,priceMB){
    
    cross_e_KB_KR = -priceKR/agg_choice(priceKB,priceKR,priceMB)[,1]*(seg.share[1]*coef.est[1,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))[,1]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))[,2]+
        seg.share[2]*coef.est[2,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))[,1]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))[,2]+
        seg.share[3]*coef.est[3,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))[,1]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))[,2]+
        seg.share[4]*coef.est[4,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))[,1]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))[,2]+
        seg.share[5]*coef.est[5,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))[,1]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))[,2]+
        seg.share[6]*coef.est[6,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[6,2:5]))[,1]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[6,2:5]))[,2]+
        seg.share[7]*coef.est[7,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[7,2:5]))[,1]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[7,2:5]))[,2])
    
    cross_e_KB_MB = -priceMB/agg_choice(priceKB,priceKR,priceMB)[,1]*(seg.share[1]*coef.est[1,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))[,1]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))[,3]+
        seg.share[2]*coef.est[2,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))[,1]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))[,3]+
        seg.share[3]*coef.est[3,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))[,1]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))[,3]+
        seg.share[4]*coef.est[4,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))[,1]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))[,3]+
        seg.share[5]*coef.est[5,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))[,1]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))[,3]+
        seg.share[6]*coef.est[6,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[6,2:5]))[,1]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[6,2:5]))[,3]+
        seg.share[7]*coef.est[7,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[7,2:5]))[,1]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[7,2:5]))[,3])
    
    cross_e_KR_KB = -priceKB/agg_choice(priceKB,priceKR,priceMB)[,2]*(seg.share[1]*coef.est[1,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))[,2]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))[,1]+
        seg.share[2]*coef.est[2,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))[,2]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))[,1]+
        seg.share[3]*coef.est[3,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))[,2]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))[,1]+
        seg.share[4]*coef.est[4,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))[,2]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))[,1]+
        seg.share[5]*coef.est[5,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))[,2]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))[,1]+
        seg.share[6]*coef.est[6,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[6,2:5]))[,2]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[6,2:5]))[,1]+
        seg.share[7]*coef.est[7,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[7,2:5]))[,2]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[7,2:5]))[,1])                                                                     
    
    cross_e_KR_MB = -priceMB/agg_choice(priceKB,priceKR,priceMB)[,2]*(seg.share[1]*coef.est[1,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))[,2]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))[,3]+
        seg.share[2]*coef.est[2,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))[,2]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))[,3]+
        seg.share[3]*coef.est[3,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))[,2]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))[,3]+
        seg.share[4]*coef.est[4,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))[,2]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))[,3]+
        seg.share[5]*coef.est[5,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))[,2]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))[,3]+
        seg.share[6]*coef.est[6,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[6,2:5]))[,2]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[6,2:5]))[,3]+
        seg.share[7]*coef.est[7,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[7,2:5]))[,2]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[7,2:5]))[,3])
    
    cross_e_MB_KB = -priceKB/agg_choice(priceKB,priceKR,priceMB)[,3]*(seg.share[1]*coef.est[1,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))[,3]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))[,1]+
        seg.share[2]*coef.est[2,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))[,3]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))[,1]+
        seg.share[3]*coef.est[3,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))[,3]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))[,1]+
        seg.share[4]*coef.est[4,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))[,3]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))[,1]+
        seg.share[5]*coef.est[5,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))[,3]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))[,1]+
        seg.share[6]*coef.est[6,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[6,2:5]))[,3]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[6,2:5]))[,1]+
        seg.share[7]*coef.est[7,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[7,2:5]))[,3]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[7,2:5]))[,1])
    
    cross_e_MB_KR = -priceKR/agg_choice(priceKB,priceKR,priceMB)[,3]*(seg.share[1]*coef.est[1,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))[,3]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))[,2]+
        seg.share[2]*coef.est[2,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))[,3]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))[,2]+
        seg.share[3]*coef.est[3,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))[,3]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))[,2]+
        seg.share[4]*coef.est[4,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))[,3]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))[,2]+
        seg.share[5]*coef.est[5,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))[,3]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))[,2]+
        seg.share[6]*coef.est[6,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[6,2:5]))[,3]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[6,2:5]))[,2]+
        seg.share[7]*coef.est[7,5]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[7,2:5]))[,3]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef.est[7,2:5]))[,2])
    
    return(cbind(cross_e_KB_KR,cross_e_KB_MB,cross_e_KR_KB,cross_e_KR_MB,cross_e_MB_KB,cross_e_MB_KR))
    
}

cross_e_seg(avgp_kb,avgp_kr,avgp_mb)

################################  Question 5  ###############################################

############If Launch KB#####################
###############ROUND 1#######################
########### MB decrease price################

profitmat_R1=matrix(0L,length(aux),1)
for (i in 1:length(aux)){
    profitmat_R1[i]=profit_3P(1.15,1.18,aux[i])[,3] 
}
opti_MB_1 <- aux[profitmat_R1==max(profitmat_R1)]# 0.96
max(profitmat_R1)

###############ROUND 2#######################
########### KB&KR decrease price#############

profitmat_R2=matrix(0L,nrow(pricespace_1),1)
for (i in 1:nrow(pricespace_1)){
    profitmat_R2[i]=sum(profit_3P(pricespace_1[i,1],pricespace_1[i,2],0.96)[,1:2]) 
}
opti_KB_2 <- pricespace_1[,1][profitmat_R2==max(profitmat_R2)] #1.03
opti_KR_2 <- pricespace_1[,2][profitmat_R2==max(profitmat_R2)] #1.07
max(profitmat_R2)

###############ROUND 3#######################
########### MB decrease price################

profitmat_R3=matrix(0L,length(aux),1)
for (i in 1:length(aux)){
    profitmat_R3[i]=sum(profit_3P(1.03,1.07,aux[i])[,3]) 
}
opti_MB_3 <- aux[profitmat_R3==max(profitmat_R3)]  #0.92
max(profitmat_R3)


###############ROUND 4#######################
########### KB&KR decrease price#############

profitmat_R4=matrix(0L,nrow(pricespace_1),1)
for (i in 1:nrow(pricespace_1)){
    profitmat_R4[i]=sum(profit_3P(pricespace_1[i,1],pricespace_1[i,2],0.92)[,1:2]) 
}
opti_KB_4 <- pricespace_1[,1][profitmat_R4==max(profitmat_R4)] #1.02
opti_KR_4 <- pricespace_1[,2][profitmat_R4==max(profitmat_R4)] #1.06
max(profitmat_R4)

###############ROUND 5#######################
########### MB decrease price################

profitmat_R5=matrix(0L,length(aux),1)
for (i in 1:length(aux)){
    profitmat_R5[i]=sum(profit_3P(1.02,1.06,aux[i])[,3]) 
}
opti_MB_5 <- aux[profitmat_R5==max(profitmat_R5)]  #0.92
max(profitmat_R5)




############If NOT Launch KB#################
kiwi[kiwi$choice == 'KB',]$choice<-0

coef.est2<- data.frame(segment = 1:7, intercept.KR = NA, 
                   intercept.MB = NA, price.coef = NA) 
for (seg in 1:7) {
    # During each loop, pick subset of data of consumers from each segment.
    kiwi.sub = subset(kiwi, cluster == seg)
    
    #Using that data, the rest remains the same.
    mlogitdata=mlogit.data(kiwi.sub,id="id",varying=c(4,6,7),choice="choice",shape="wide")
    
    #Run MLE.
    mle= gmnl(choice ~  price, data = mlogitdata)
    mle
    #Store the outcome in the coef.est matrix.
    coef.est2[seg, 2:4] = mle$coefficients
}

demand_2P <- function(priceKR,priceMB,para){
    probKR=exp(para[1]+para[3]*priceKR)/(1+exp(para[1]+para[3]*priceKR)+exp(para[2]+para[3]*priceMB))
    probMB=exp(para[2]+para[3]*priceMB)/(1+exp(para[1]+para[3]*priceKR)+exp(para[2]+para[3]*priceMB))
    return(cbind(probKR,probMB))
}

profit_2P=function(priceKR,priceMB){
    profitKR=1000*(demand_2P(priceKR,priceMB,as.numeric(coef.est2[1,2:4]))[,1]*(priceKR-uc)*seg.share[1]+
                       demand_2P(priceKR,priceMB,as.numeric(coef.est2[2,2:4]))[,1]*(priceKR-uc)*seg.share[2]+
                       demand_2P(priceKR,priceMB,as.numeric(coef.est2[3,2:4]))[,1]*(priceKR-uc)*seg.share[3]+
                       demand_2P(priceKR,priceMB,as.numeric(coef.est2[4,2:4]))[,1]*(priceKR-uc)*seg.share[4]+
                       demand_2P(priceKR,priceMB,as.numeric(coef.est2[5,2:4]))[,1]*(priceKR-uc)*seg.share[5]+
                       demand_2P(priceKR,priceMB,as.numeric(coef.est2[6,2:4]))[,1]*(priceKR-uc)*seg.share[6]+
                       demand_2P(priceKR,priceMB,as.numeric(coef.est2[7,2:4]))[,1]*(priceKR-uc)*seg.share[7])
    profitMB=1000*(demand_2P(priceKR,priceMB,as.numeric(coef.est2[1,2:4]))[,2]*(priceMB-uc)*seg.share[1]+
                       demand_2P(priceKR,priceMB,as.numeric(coef.est2[2,2:4]))[,2]*(priceMB-uc)*seg.share[2]+
                       demand_2P(priceKR,priceMB,as.numeric(coef.est2[3,2:4]))[,2]*(priceMB-uc)*seg.share[3]+
                       demand_2P(priceKR,priceMB,as.numeric(coef.est2[4,2:4]))[,2]*(priceMB-uc)*seg.share[4]+
                       demand_2P(priceKR,priceMB,as.numeric(coef.est2[5,2:4]))[,2]*(priceMB-uc)*seg.share[5]+
                       demand_2P(priceKR,priceMB,as.numeric(coef.est2[6,2:4]))[,2]*(priceMB-uc)*seg.share[6]+
                       demand_2P(priceKR,priceMB,as.numeric(coef.est2[7,2:4]))[,2]*(priceMB-uc)*seg.share[7])
    return(cbind(profitKR,profitMB))
}

profitmat=matrix(0L,length(aux),1)
for (i in 1:length(aux)){
    profitmat[i]=profit_2P(aux[i],1.43)[,1] 
}

aux[which.max(profitmat)]

###############ROUND 1#######################
########### MB decrease price################

profitmat_R1_1=matrix(0L,length(aux),1)
for (i in 1:length(aux)){
    profitmat_R1_1[i]=profit_2P(1.02,aux[i])[,2] 
}
opti_MB_2P_1 <- aux[profitmat_R1_1==max(profitmat_R1_1)] # 0.96
max(profitmat_R1_1)

###############ROUND 2#######################
############## KR decrease price#############

profitmat_R2_1=matrix(0L,length(aux),1)
for (i in 1:length(aux)){
    profitmat_R2_1[i]=profit_2P(aux[i],0.96)[,1]
}
opti_KB_2P_2 <- aux[profitmat_R2_1==max(profitmat_R2_1)] #0.96
max(profitmat_R2_1)

###############ROUND 3#######################
########### MB decrease price################

profitmat_R3_1=matrix(0L,length(aux),1)
for (i in 1:length(aux)){
    profitmat_R3_1[i]=profit_2P(0.96,aux[i])[,2] 
}
opti_MB_2P_3 <- aux[profitmat_R3_1==max(profitmat_R3_1)] # 0.94
max(profitmat_R3_1)

###############ROUND 3#######################
############## KR decrease price#############

profitmat_R4_1=matrix(0L,length(aux),1)
for (i in 1:length(aux)){
    profitmat_R4_1[i]=profit_2P(aux[i],0.94)[,1]
}
opti_KB_2P_4 <- aux[profitmat_R4_1==max(profitmat_R4_1)] #0.96
max(profitmat_R4_1)

###############ROUND 5#######################
########### MB decrease price################

profitmat_R5_1=matrix(0L,length(aux),1)
for (i in 1:length(aux)){
    profitmat_R5_1[i]=profit_2P(0.92,aux[i])[,2] 
}
opti_MB_2P_5 <- aux[profitmat_R5_1==max(profitmat_R5_1)] # 0.89
max(profitmat_R5_1)





agg_choice_temp <- function(priceKR,priceMB) {
    agg_choice_KR=seg.share[1]*demand_2P(priceKR,priceMB,as.numeric(coef.est2[1,2:4]))[,1]+
        seg.share[2]*demand_2P(priceKR,priceMB,as.numeric(coef.est2[2,2:4]))[,1]+
        seg.share[3]*demand_2P(priceKR,priceMB,as.numeric(coef.est2[3,2:4]))[,1]+
        seg.share[4]*demand_2P(priceKR,priceMB,as.numeric(coef.est2[4,2:4]))[,1]+
        seg.share[5]*demand_2P(priceKR,priceMB,as.numeric(coef.est2[5,2:4]))[,1]+
        seg.share[6]*demand_2P(priceKR,priceMB,as.numeric(coef.est2[6,2:4]))[,1]+
        seg.share[7]*demand_2P(priceKR,priceMB,as.numeric(coef.est2[7,2:4]))[,1]
    agg_choice_MB=seg.share[1]*demand_2P(priceKR,priceMB,as.numeric(coef.est2[1,2:4]))[,2]+
        seg.share[2]*demand_2P(priceKR,priceMB,as.numeric(coef.est2[2,2:4]))[,2]+
        seg.share[3]*demand_2P(priceKR,priceMB,as.numeric(coef.est2[3,2:4]))[,2]+
        seg.share[4]*demand_2P(priceKR,priceMB,as.numeric(coef.est2[4,2:4]))[,2]+
        seg.share[5]*demand_2P(priceKR,priceMB,as.numeric(coef.est2[5,2:4]))[,2]+
        seg.share[6]*demand_2P(priceKR,priceMB,as.numeric(coef.est2[6,2:4]))[,2]+
        seg.share[7]*demand_2P(priceKR,priceMB,as.numeric(coef.est2[7,2:4]))[,2]
    
    return(cbind(agg_choice_KR,agg_choice_MB))
}    

agg_choice_temp(0.96,0.94)










