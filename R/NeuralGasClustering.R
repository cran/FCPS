NeuralGasClustering <-function(Data,ClusterNo,PlotIt=FALSE,...){
# Cls=NeuralGas(Data,ClusterNo)
#  
# liefert eine Klassenzuweisung
# INPUT
# Data[1:n,1:d]             Der Datensatz

# ClusterNo  in soviele Cluster werden die daten eingeteilt
  
# OUTPUT
# Cls[1:n]                Clusterung der Daten
# NeuralGasObject         Object of NeuralGas Alorithm
# Author: MT 06/2015
#1.Editor: MT 04/18

  
requireNamespace('cclust')
res=cclust::cclust(x=Data,centers=ClusterNo,method='neuralgas',...)
if(PlotIt){
  requireNamespace('DataVisualizations')
  DataVisualizations::Plot3D(Data,res$cluster)
}
return(list(Cls=res$cluster,Object=res))
}