source('mht.r')

# Generyczne funkcje dla przeszukiwania wgłąb

dfs.cost<-function(x){
  stop('Brak implementacji funkcji kosztu')
}

dfs.init<-function(UG){
	stop('Brak implementacji funkcji inicjującej')
}

dfs.model_init<-function(UG){
  return(
	list(lifo=list(),all_done=FALSE)
	)
}

dfs.model_update<-function(X,M){



  XS <- dfs.neighbours(X)
  XS <- setdiff(XS,pop())
  len <- length(M$lifo)
  
  if(length(XS) > 0){    
    M$lifo <- c(M$lifo,XS)
  }else{
    if(len==0){
      M$all_done <- TRUE
    }
  }

  return(M)
}

dfs.generate<-function(XS,M,UG){  	
  last <- length(M$lifo)
  
  X <- M$lifo[[last]]
  M$lifo[[last]] <- NULL

  return(X)
}

dfs.select<-function(XS,M,UG){
  last <- length(XS)
  
  return(XS[[last]])
}

dfs.stop<-function(XS,M)
{
  return(dfs.cost(XS[[length(XS)]])==0 || M$all_done==TRUE)
}

dfs.search<-function(){
  search(dfs.model_init, dfs.model_update, dfs.init, dfs.select, dfs.generate, dfs.stop, UG)
  
	return(pop(1)[[1]])
}

