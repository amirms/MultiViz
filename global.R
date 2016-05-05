# library(GeLaToLab)
# library(tm)
library(Rcpp)
# library(inline)
# library(ITM)

MultiViewState <- function(session=NULL) {
  state = list (
    
    #Current Project in benchmark folder
    prname= NULL, 
    
    vocab = NULL,
    
    doc.names = NULL,
    D = 0,
    
    cfg=NULL,
    cfg_kernel_beta=5,
    
    #transaction frequency
    freq = NULL,
    freq_kernel_alpha = 0.01,
    
    
    
    kernels = NULL,
    NCuts =0,
    
    #State of Interactive LDA
    curIterations = 0, #LDA iterations run so far (gets reset when 'Reset Topics' is clicked)
    nextIterations = 0, #LDA iterations to run in the next LDA command
    dirty = FALSE, #Dirty flag: true if we've changed the constraints in any way
    #without running LDA to refine the topics.
    session = session #Current Session
    
  )
  
  
  state <- list2env(state)
  class(state) <- "MultiViewState"
  return(state)
}
  

combine.views <- function(mystate, views = c(T,T,T), choice="MQ")
  {
  
  require(Rcpp)
  require(inline)
  require(GeLaToLab)

  
  if (is.null(mystate$prname))
    return()
  
  mystate$docnames <- load_docnames(mystate$prname)
  
  
  #always load cfg
  mystate$cfg <- load_cfg(mystate$prname, mystate$docnames)
  if (views[1]){
    # make symmetric
    cfg <- make.symmetric(mystate$cfg)
    cfg.laplacian = laplacian(cfg, TRUE)   
    cfg.kernel <- calc.diffusion.kernel(cfg.laplacian, beta = mystate$cfg_kernel_beta)   
  } 

  if (views[2]){
    bow <- load_bow(mystate$prname, mystate$docnames)
    lexsim.kernel <- compute_cosine_kernel(bow)
  }
  if (views[3]){
    freq <- load_freq(mystate$prname, mystate$docnames)
       
    freq.kernel = gaussian.kernel(freq, 0.01)
  }
  
  mystate$kernels <- #list(
      switch(choice,
                         MQ = cfg.kernel,
                         CQ = lexsim.kernel,
                         FQ = freq.kernel,
                         MKL.Add = add.kernels(list(cfg.kernel, lexsim.kernel, freq.kernel)),
                         MKL.Prod = product.kernels(list(cfg.kernel, lexsim.kernel, freq.kernel)),
                         CT = compute.cotraining(list(cfg.kernel, lexsim.kernel, freq.kernel), k, iter=50, priori.decomp, prname),
                         MO = generalized_pareto_multiview(laplacian(cfg.kernel, TRUE), laplacian(lexsim.kernel, TRUE), laplacian(freq.kernel, TRUE)))
  #)
  
  print(dim(mystate$kernels))
  
  return(mystate)
}


load_docnames <- function(prname){
  classnames <- unlist(read.table(paste("data", prname , "classnames.txt" , sep="/")) )
  
  return (classnames)
  
}

load_cfg <- function(prname, docnames){
  
  extensions= c("java/", "org/xml/", "javax/")
  cfg <- import.bunch.matrix(paste("data", prname ,"dep_graph.txt", sep="/"), exclude.ext=extensions)
  cfg <- cfg[which(rownames(cfg) %in% docnames), which(colnames(cfg) %in% docnames)]
  cfg <- cfg[order(rownames(cfg)), order(colnames(cfg))]
  # no self-references
  diag(cfg) <- 0 

  return(cfg)
}


load_bow <- function(prname, docnames){
  bow <- read.table(paste("data", prname , "BoW.csv", sep="/"), sep=",", row.names = 1, header = TRUE, check.names = FALSE)  
  bow <- as.matrix(bow)
  bow <- bow[which(rownames(bow) %in% docnames),]
  bow <- bow[order(rownames(bow)),]
  
  vocab <- colnames(bow)
  #only lexical items of longer than 4 characters
  vocab <- vocab[which(unlist(lapply(vocab, nchar))>4)]
  
  bow <- bow[,vocab]
  
  
  doc.freq <- colSums(bow>0)
  doc.freq[doc.freq == 0] <- 1
  w <- 1/log(nrow(bow)/doc.freq)
  R <- diag(w)
  
  
  #Compute cosine similarity
  bow <- bow %*% R
  
  return(bow)
} 


load_freq <- function(prname, docnames){
  freq <- read.table(paste("data", prname , "mydata-change-freq-matrix.csv", sep="/"), sep=",", row.names = 1, header = TRUE, check.names = FALSE)  
  freq <- as.matrix(freq)
  freq <- freq[which(rownames(freq) %in% docnames),]
  freq <- freq[order(rownames(freq)),]
  #Process the transaction frequency
  no_transactions <- colSums(freq)
  freq <- freq[, which(no_transactions <= mystate$freq_threshold)]
  
  return(freq)
}

prepare.global.state <- function(session) {
  
  mystate <- MultiViewState(session)
  
  mystate$prname= "jedit-5.1.0"
  
  return(mystate)
}
