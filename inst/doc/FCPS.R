## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  message=FALSE,
  warning = FALSE,
  comment = "#>"
)

## ---- fig.show='hold',fig.width=5,fig.height=5--------------------------------
library(FCPS)
data("Leukemia")
Data=Leukemia$Distance
Cls=Leukemia$Cls
ClusterPlotMDS(Data,Cls,main = 'Leukemia',Plotter3D = 'plotly')

## ---- fig.show='hold',fig.width=5,fig.height=5--------------------------------
library(FCPS)
data('Leukemia')
set.seed(123)
ClusterNo=length(unique(Leukemia$Cls))
CA=AgglomerativeNestingClustering(Leukemia$DistanceMatrix,ClusterNo)
Cls=ClusterRenameDescendingSize(CA$Cls)
sum(match(names(Cls),rownames(Leukemia$DistanceMatrix),nomatch = 0)==0)

## ---- fig.show='hold',fig.width=5,fig.height=5--------------------------------
set.seed(600)
library(FCPS)
DataList=ClusterChallenge("Chainlink",SampleSize = 750)
Data=DataList$Chainlink
Cls=DataList$Cls
ClusterPlotMDS(Data,Cls,Plotter3D = 'plotly',main = "Chainlink")
ClusterCount(Cls)

## ---- fig.show='hold',fig.width=4,fig.height=4--------------------------------
set.seed(600)
library(FCPS)
DataList=ClusterChallenge("Chainlink",SampleSize = 750)
Data=DataList$Chainlink
Cls=DataList$Cls
library(ggplot2)
ClusterabilityMDplot(Data)+theme_bw()

## ---- fig.show='hold',fig.width=7,fig.height=5--------------------------------
library(FCPS)
set.seed(135)
DataList=ClusterChallenge("Chainlink",SampleSize = 900)
Data=DataList$Chainlink
Cls=DataList$Cls
Tree=HierarchicalClustering(Data,1,"SingleL")[[3]]
ClusterDendrogram(Tree,4,main='Single Linkage')
MaximumNunber=7
clsm <- matrix(data = 0, nrow = dim(Data)[1],  ncol = MaximumNunber)
for (i in 2:(MaximumNunber+1)) {
  clsm[,i-1] <- cutree(Tree,i)
}
out=ClusterNoEstimation(Data,ClsMatrix = clsm, max.nc = MaximumNunber,PlotIt = TRUE)

## ---- fig.show='hold'---------------------------------------------------------
library(FCPS)
data("Leukemia")
Distance=Leukemia$DistanceMatrix
Classification=Leukemia$Cls
Cls=HierarchicalClustering(Distance,6,"SingleL")$Cls

#Usual Computation Accuracy per Class
cm=as.matrix(table(Cls,Classification))
diag(cm)/rowSums(cm)
# Usual overall Accuracy
sum(diag(cm)) / sum(cm) 
#e.g.
#MLmetrics::Accuracy(Cls,Classification)
#Correct Computation
ClusterAccuracy(Cls,Classification)
cm

## ---- fig.show='hold'---------------------------------------------------------
library(datasets)
library(FCPS)
Iris=datasets::iris
Data=as.matrix(Iris[,1:4])
SomeFactors=Iris$Species
V=ClusterCreateClassification(SomeFactors)
Cls=V$Cls
V$ClusterNames
ClusterApply(Data,mean,Cls)

## ---- fig.show='hold'---------------------------------------------------------
suppressPackageStartupMessages(library('prabclus',quietly = TRUE))
data(tetragonula)
#Generated Specific Distance Matrix
ta <- alleleconvert(strmatrix=as.matrix(tetragonula[1:236,]))
tai <- alleleinit(allelematrix=ta,distance="none")
Distances=alleledist((unbuild.charmatrix(tai$charmatrix,236,13)),236,13)
Cls=rep(1,nrow(Distances))
DataTrans=ClusterApply(Distances,identity,Cls)$identityPerCluster
dim(DataTrans)
dim(Distances)

