NetworkClustering <-function(DataOrDistances=NULL,Adjacency=NULL,Type="louvain",Radius=FALSE,PlotIt=FALSE,...){
 
   if(!is.null(DataOrDistances)){
    if (!isSymmetric(unname(DataOrDistances))) {
      #Data = DataOrDistances
      if(Radius==TRUE){
        if(requireNamespace('DataVisualizations')){
          Radius=DataVisualizations::ParetoRadius(DataOrDistances)
        }else{
          stop('DataVisualizations package is missing.')
        }
        #Radius=AdaptGauss::ParetoRadius(Data)
      }
      if(Radius==FALSE){
        if(requireNamespace('parallelDist')){
          Radius=EstimateRadiusByDistance(as.matrix(parallelDist::parallelDist(DataOrDistances)))
        }else{
          stop('parallelDist package is missing.')
        }
      }
      DistanceMatrix = as.matrix(dist(DataOrDistances))
  
    }else{
      DistanceMatrix=DataOrDistances
      if(isFALSE(Radius))
        Radius=EstimateRadiusByDistance(DistanceMatrix)
    }
    
    AnzPunkte = nrow(DistanceMatrix)
    N = ncol(DistanceMatrix)
    Adjacency = matrix(0, ncol = N, nrow = N)
    for (i in 1:AnzPunkte) {
      RInd = which(DistanceMatrix[i, ] <= Radius, arr.ind = T)
      Adjacency[i, RInd] = 1
    }
    #Alternative:
    #result=cccd::nng(dx = d, k = 7, mutual = TRUE, method = NULL)
    #Adjacency=KNNGraphAdjMatrix=igraph::get.adjacency(result,sparse=FALSE,type='both')
    #louvain dauert dann aber ewig...
  }#Adjacency not given
    switch(
      Type,
      'leiden' = {
        if (!requireNamespace('leiden')) {
          message(
            'Subordinate clustering package (leiden) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
          )
          return(
            list(
              Cls = rep(1, nrow(DataOrDistances)),
              Object = "Subordinate clustering package is missing.
                Please install the package which is defined in 'Suggests'."
            )
          )
        }
        if (!requireNamespace('igraph')) {
          message(
            'Subordinate igraph package is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
          )
          return(
            list(
              Cls = rep(1, nrow(DataOrDistances)),
              Object = "Subordinate clustering package is missing.
                Please install the package which is defined in 'Suggests'."
            )
          )
        }
        graph_object <- igraph::graph_from_adjacency_matrix(Adjacency, mode = "directed")
        
        CA=leiden::leiden(object = graph_object,...)
        warning("Cls is not given back and in Object hte clustering vector has to be found manually. Function is under development")
        # s. http://epub.wu.ac.at/1542/1/document.pdf p.5
        # An examination of indices for determining the number of clusters in binary data sets
        # Weingessel, Andreas and Dimitriadou, Evgenia and Dolnicar, Sara (1999)

        #Cls = ClusterRename(Cls, DataOrDistances)
        #if (PlotIt) {
        #  ClusterPlotMDS(DataOrDistances, Cls)
        #}
        return(list(
          Cls = NULL,
          Object = CA
          )
        )
      },
      "louvain" = {
        if (!requireNamespace('NetworkToolbox')) {
          message(
            'Subordinate clustering package (NetworkToolbox) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
          )
          return(
            list(
              Cls = rep(1, nrow(DataOrDistances)),
              Object = "Subordinate clustering package is missing.
                Please install the package which is defined in 'Suggests'."
            )
          )
        }
        
        CA=NetworkToolbox::louvain(A = Adjacency,...)
        Cls= CA$community
        
        Cls = ClusterRename(Cls, DataOrDistances)
        if (PlotIt) {
          ClusterPlotMDS(DataOrDistances, Cls)
        }
        return(list(
          Cls = Cls,
          Object = CA
        ))
      },
      {
        message("Incorrect method selected")
        return(
          list(
            Cls = rep(1, nrow(DataOrDistances)),
            Object = "Incorrect method selected."
          )
        )
      }
    )
}
