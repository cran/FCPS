cluster_analysis_fun = function(i, fun, DataOrDistances, ClusterNo = NULL,
                                 SetSeed = TRUE, ...) {
  # cluster_analysis_fun(i,fun,DataOrDistances,ClusterNo=2)
  #
  # INTERNAL WORKER
  #
  # Executes one clustering trial for a generic data or distance input. Validates
  # the trial identifier, optionally sets a deterministic seed, calls FUN, measures
  # elapsed time, and extracts the unique result element named Cls.
  #
  # INPUT
  # i                Positive integer trial identifier.
  # fun              Function or character string naming the clustering function.
  # DataOrDistances  Data or distance object supplied to fun.
  #
  # OPTIONAL
  # ClusterNo        Number of clusters. NULL omits this argument from the call.
  # SetSeed          Logical. If TRUE, uses seed 1000 + i. Default: TRUE.
  # ...              Further arguments forwarded to fun.
  #
  # OUTPUT
  # List with:
  # Cls              Clustering vector, or NULL when no unique Cls element exists.
  # ComputationTime  Named elapsed time in seconds.
  # Seed             Integer seed, or NULL when SetSeed = FALSE.
  # CAs              Complete object returned by fun.
  #
  # INTERNAL
  # This function is used by parApplyClusterAnalysis() and is not intended as the
  # primary user interface.
  
  if (!is.logical(SetSeed) || length(SetSeed) != 1L || is.na(SetSeed)) {
    stop("'SetSeed' must be exactly TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(i) || length(i) != 1L || is.na(i) || !is.finite(i) ||
      i < 1 || i != floor(i) || i > (.Machine$integer.max - 1000L)) {
    stop("'i' must be a positive integer trial ID.", call. = FALSE)
  }

  if (isTRUE(SetSeed)) {
    seedno = 1000L + as.integer(i)
    set.seed(seedno)
    delta_name = paste0("Seed_", seedno)
  } else {
    seedno = NULL
    set.seed(NULL)
    delta_name = as.character(as.integer(i))
  }

  fun_object = match.fun(fun)
  formal_names = names(formals(fun_object))
  if (is.null(formal_names)) {
    stop("'fun' must be an R function with inspectable formal arguments.", call. = FALSE)
  }

  non_dots = formal_names[formal_names != "..."]
  preferred_names = c(
    "DataOrDistances", "Data", "Distances", "Distance", "Dissimilarities"
  )

  if (length(non_dots) > 0L && non_dots[[1L]] %in% preferred_names) {
    input_name = non_dots[[1L]]
  } else {
    available = preferred_names[preferred_names %in% formal_names]
    if (length(available) > 0L) {
      input_name = available[[1L]]
    } else if (length(non_dots) > 0L) {
      input_name = non_dots[[1L]]
    } else if ("..." %in% formal_names) {
      input_name = "DataOrDistances"
    } else {
      stop("Could not identify an input argument in 'fun'.", call. = FALSE)
    }
  }

  call_args = list()
  call_args[[input_name]] = DataOrDistances
  if (!is.null(ClusterNo)) {
    call_args$ClusterNo = ClusterNo
  }

  dots = list(...)
  if (length(dots) > 0L) {
    dot_names = names(dots)
    if (is.null(dot_names)) {
      dot_names = rep("", length(dots))
    }
    duplicate_names = nzchar(dot_names) & dot_names %in% names(call_args)
    if (any(duplicate_names)) {
      stop(
        sprintf(
          "Arguments in '...' duplicate wrapper-supplied arguments: %s.",
          paste(unique(dot_names[duplicate_names]), collapse = ", ")
        ),
        call. = FALSE
      )
    }
    call_args = c(call_args, dots)
  }

  if (!("..." %in% formal_names)) {
    arg_names = names(call_args)
    if (is.null(arg_names)) {
      arg_names = rep("", length(call_args))
    }
    keep = !nzchar(arg_names) | arg_names %in% formal_names
    call_args = call_args[keep]
  }

  prior = proc.time()[["elapsed"]]
  object = do.call(fun_object, call_args)
  delta = as.numeric(proc.time()[["elapsed"]] - prior)
  names(delta) = delta_name

  cls = NULL
  object_names = names(object)
  if (!is.null(object_names)) {
    cls_index = which(object_names == "Cls")
    if (length(cls_index) == 1L) {
      cls = object[[cls_index]]
    }
  }

  list(
    Cls = cls,
    ComputationTime = delta,
    Seed = seedno,
    CAs = object
  )
}
