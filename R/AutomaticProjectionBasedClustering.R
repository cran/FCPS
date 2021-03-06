AutomaticProjectionBasedClustering=function(DataOrDistances,ClusterNo,Type="NerV",StructureType = TRUE,PlotIt=FALSE,PlotTree=FALSE,PlotMap=FALSE,...){
  #
  # INPUT
  # DataOrDistances[1:n,1:d]    Dataset with n observations and d features or distance matrix with size n
  # ClusterNo                   Number of clusters to search for
  # Type                        Type of Projection method. Choose: NerV, Pswarm, MDS, ICA, CCA, Sammon
  # StructureType               Boolean. Either compact (TRUE) or connected (FALSE), see discussion in [Thrun, 2018] 
  # PlotIt                      Boolean. Decision to plot or not
  # PlotTree                    Boolean. Plots the dendrogram
  # PlotMap                     Boolean. Plots the topographic map [Thrun et al., 2016].
  # 
  # OUTPUT
  # Cls[1:n]    Clustering of data
  # Object      List of projection and visualization
  #
  # Author: MT, 04/2020
  
  if (!requireNamespace('ProjectionBasedClustering',quietly = TRUE)) {
    message(
      'Subordinate clustering package (ProjectionBasedClustering) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      list(
        Cls = rep(1, nrow(DataOrDistances)),
        Object = "Subordinate clustering package (ProjectionBasedClustering) is missing.
                Please install the package which is defined in 'Suggests'."
      )
    )
  }
  
  if(isSymmetric(unname(DataOrDistances))){
    #Type %in% c('Sammon','Pswarm','MDS')
    if((Type %in% c('NerV'))){
    #if(!(Type %in% c('Sammon','Pswarm','MDS'))){
      Type='MDS'
      warning('Distances matrix is given but the type',Type,'is selected, which does not work with distances. Switching to MDS.')
    }
   
    if(isTRUE(PlotMap)){
      message('Given a distance matrix instead of data is experimental. MDS transformation is used to generate a data matrix.')
    
      Data=ProjectionBasedClustering::MDS(DataOrDistances = DataOrDistances,OutputDimension = dim(DataOrDistances)[1]-2)$ProjectedPoints
    }
    
  }else{
    #if(isTRUE(PlotMap))
      Data=DataOrDistances#for plotting
  }
  n=dim(DataOrDistances)[1]
    switch(Type,
      'NerV'={
        out=list()
        out$ProjectedPoints=ProjectionBasedClustering::NeRV(Data = Data,OutputDimension = 2,PlotIt = FALSE,...)
        },
      'Pswarm'={
          out=ProjectionBasedClustering::PolarSwarm(DataOrDistances = DataOrDistances,PlotIt = FALSE,...)
        },
      'MDS'={out=ProjectionBasedClustering::MDS(DataOrDistances = DataOrDistances,OutputDimension = 2,PlotIt = FALSE,...)
      
      },
      'CCA'={
        out=ProjectionBasedClustering::CCA(DataOrDistances = DataOrDistances,OutputDimension = 2,PlotIt = FALSE,...)
      },
      'Sammon'={
        out=ProjectionBasedClustering::SammonsMapping(DataOrDistances = DataOrDistances,OutputDimension = 2,PlotIt = FALSE,...)
      },
      't-SNE'={
        out=ProjectionBasedClustering::tSNE(DataOrDistances = DataOrDistances,OutputDimension = 2,PlotIt = FALSE,...)
      },
      'Uwot'={
        out=ProjectionBasedClustering::UniformManifoldApproximationProjection(DataOrDistances = DataOrDistances,OutputDimension = 2,PlotIt = FALSE,...)
      },{
        warning('Incorrect type selected')
        return('Incorrect type selected')
      }
    )
  #  out=out
  out$Type=Type
#ToDo, vorgang abschalten, da fuer clusterung unnoetig----
  # Computation of GeneralizedUmatrix
  if (n > 4096/8) 
    minNeurons = n * 8
  else minNeurons = 4096
  
  visualization="PlotMap=TRUEE computes the GeneralizedUmatrix"
  
  coordsres = GeneralizedUmatrix::XYcoords2LinesColumns(X = out$ProjectedPoints[, 1], 
                                    Y = out$ProjectedPoints[, 2], PlotIt = F, minNeurons = minNeurons)
  LC = coordsres$LC
  Bestmatches = coordsres$GridConvertedPoints
  


  # Automatic Clustering
  if(Type!='Pswarm'){
    # Number of cluster from dendrogram or visualization (PlotIt=T)
    Cls=ProjectionBasedClustering::ProjectionBasedClustering(k=ClusterNo, DataOrDistances, BestMatches = Bestmatches, LC = LC,StructureType = StructureType,PlotIt=PlotTree)
  }else{
    if(requireNamespace('DatabionicSwarm',quietly = TRUE)){
      Cls=DatabionicSwarm::DBSclustering(k = ClusterNo,DataOrDistance = DataOrDistances,BestMatches = Bestmatches,LC = LC,StructureType = StructureType,PlotIt = PlotTree)
    }else{
      warning('DatabionicSwarm package is not installed.')
      return('DatabionicSwarm package is not installed.')
    }
  }
  # Verification
  if(isTRUE(PlotMap)){
	if(requireNamespace('GeneralizedUmatrix',quietly = TRUE)){
	  if(Type!='Pswarm')
	    if(requireNamespace("GeneralizedUmatrix",quietly = TRUE)){
	      visualization=GeneralizedUmatrix::GeneralizedUmatrix(Data = Data,out$ProjectedPoints,PlotIt = FALSE)
	    }
	  else{
	    stop('GeneralizedUmatrix package not loaded or installed.')
	  }
	  else{
	    if(requireNamespace("DatabionicSwarm",quietly = TRUE)){
	      # Visualization of GenerelizedUmatrix
	      visualization=DatabionicSwarm::GeneratePswarmVisualization(Data = Data,out$ProjectedPoints,out$LC,PlotIt = FALSE)
	    }
	    else{
	      stop('DatabionicSwarm package not loaded or installed.')
	    }
	  }
	  GeneralizedUmatrix::plotTopographicMap(visualization$Umatrix,visualization$Bestmatches,Cls)
	}
	
  }
  if(isTRUE(PlotIt)){
    ClusterPlotMDS(DataOrDistances,Cls)
  }
  Cls=ClusterRename(Cls,DataOrDistances)
    return(list(Cls=Cls,Object=list(Projection=out,Bestmatches=coordsres,GeneralizedUmatrix=visualization)))
  }