ClusterabilityMDplot=function(Data,Method="pca"){
  #author MT
  requireNamespace('clusterability')
  requireNamespace('ggplot2')
  requireNamespace('signal')
  requireNamespace('reshape2')
  
  #requireNamespace('DataVisualizations')
   
  ## real code
  if(!is.list(Data)){
  pvalm=clusterability::clusterabilitytest(Data,reduction = Method,test = 'dip',pca_scale=FALSE,pca_center=FALSE)
  print(pvalm$pvalue)
  pvalue=round(pvalm$pvalue,2)
  if(pvalue==0) 
    pvalue='p < 0.01'
  else
    pvalue=paste('p =',pvalue)
  
  res <- prcomp(x=Data,retx=T,scale. =FALSE,tol = 0,center=FALSE)
  TransData=as.matrix(res$x)
  ProjectedPoints=TransData[,1]
  main=paste('MDplot of Clusterability')

  plot=MDplot(as.matrix(ProjectedPoints),Names = pvalue,Ordering = 'Columnwise')+ggplot2::ggtitle(main)+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))+
    ggplot2::xlab('Probability that data has no cluster structure')+
    ggplot2::ylab('PDE of 1st Principal Component')
  }else{
    n=length(Data)
    pvalsL=lapply(Data, function(x) return(clusterability::clusterabilitytest(x,reduction = Method,test = 'dip')$pvalue),Method)
    Names=names(Data)
    vals=unlist(pvalsL)
    vals=round(vals,2)
    ind=which(vals==0)
     ind2=which(vals!=0)
     vals[ind]='p < 0.01'
     vals[ind2]=paste("p =",vals[ind2])
	 #modes depricated
    #if(is.null(Names)){
      Ordering = 'Columnwise'
    #  Names=as.character(vals)
    #}else{
    #  Names=paste0(Names,', ',vals)
    #  Ordering = 'Bimodal'
    #}
    
    pcas=lapply(Data, function(x) return((res <- prcomp(x=x,retx=T,scale=FALSE,tol = 0,center=FALSE)$x)[,1]))
 
    names(pcas)=Names

    plot=DataVisualizations::MDplot4multiplevectors(pcas,Gaussian_lwd=0.5,Names = Names,Ordering = Ordering,Scaling = 'Robust')+
      ggplot2::ggtitle('MDplot of Clusterability for Multiple Datasets')+
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))+
      ggplot2::xlab('Probability that data has no cluster structure')+
      ggplot2::ylab('PDE of 1st Principal Component')
    
  }
  return(plot)
}
## internal functions ----

stat_pde_density <- function(mapping = NULL,
                             data = NULL,
                             geom = "violin",
                             position = "dodge",
                             ...,
                             trim = TRUE, #enden des violins werden korrekt angezeigt und nicht ueber den wertebereich fortgesetzt
                             scale = "area",
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE) {
  
  scale <- match.arg(scale, c("area", "count", "width"))
  
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatPDEdensity,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      na.rm = na.rm,
      ...
    )
  )
}


compute_pdedensity <- function(x) {
  nx <- length(x)
  
  # if less than 2 points return data frame of NAs and a warning
  if (nx < 2) {
    warning("stat_pde_density: Groups with fewer than two data points have been dropped.",
            call. = FALSE)
    return(
      data.frame(
        x = NA_real_,
        density = NA_real_,
        scaled = NA_real_,
        count = NA_real_,
        n = NA_integer_
      )
    )
  }
  
  ##MT: chatch error of one unique value
  Flag <- FALSE
  if (length(unique(x)) ==1) {
    warning('stat_pde_density: Only one unique value in Data.')
    if(unique(x)!=0)
      x <- c(unique(x), head(x, 1) * runif(1, 0.999, 1.001))
    else
      x <- c(unique(x), head(x, 1) + runif(1, 0.999, 1.001))
    
    Flag <- TRUE
  }
  requireNamespace('DataVisualizations')
  dens <- DataVisualizations::ParetoDensityEstimation(Data = x)
  
  # Density cannot be estiamted, set density to value equal 1
  if (Flag) {
    # scatter kernels a little to visualize several features if given
    dens$kernels <- dens$kernels * runif(length(dens$kernels), 0.998, 1.002)
    x <- max(dens$kernels) - min(dens$kernels)
    dens$paretoDensity[1:length(dens$paretoDensity)] <- 1 / x # integral over pdf should be 1
  }
  data.frame(
    x = dens$kernels,
    density = dens$paretoDensity,
    scaled =  dens$paretoDensity / max(dens$paretoDensity, na.rm = TRUE),
    count =   dens$paretoDensity * nx,
    n = nx
  )
  
}

StatPDEdensity <- ggproto("StatPDEdensity",
                          Stat,
                          required_aes = c("x", "y"),
                          
                          compute_group = function(data,
                                                   scales,
                                                   width = NULL,
                                                   trim = TRUE,
                                                   na.rm = FALSE) {
                            if (nrow(data) < 3)
                              return(data.frame())
                            range <- range(data$y, na.rm = TRUE)
                            modifier <- if (trim) 0 else 3
                            dens <- compute_pdedensity(data$y)
                            
                            dens$y <- dens$x
                            dens$x <- mean(range(data$x))
                            
                            # Compute width if x has multiple values
                            if (length(unique(data$x)) > 1) {
                              width <- diff(range(data$x)) * 0.9
                            }
                            dens$width <- width
                            
                            dens
                          },
                          
                          compute_panel = function(self,
                                                   data,
                                                   scales,
                                                   width = NULL,
                                                   trim = TRUE,
                                                   na.rm = FALSE,
                                                   scale = "area") {
                            data <- ggproto_parent(Stat, self)$compute_panel(
                              data,
                              scales,
                              width = width,
                              trim = trim,
                              na.rm = na.rm
                            )
                            
                            # choose how violins are scaled relative to each other
                            data$violinwidth <- switch(
                              scale,
                              # area : keep the original densities but scale them to a max width of 1
                              #        for plotting purposes only
                              area = data$density / max(data$density),
                              # count: use the original densities scaled to a maximum of 1 (as above)
                              #        and then scale them according to the number of observations
                              count = data$density / max(data$density) * data$n / max(data$n),
                              # width: constant width (density scaled to a maximum of 1)
                              width = data$scaled
                            )
                            data
                          }
                          
)