
####
# Wyszukiwanie wgłąb dla problemu sortowania
source('dfs.r')
source('sort.r')
dfs.sort <-function( task){
  sort.task<<-task
  
	
	dfs.init <<- function(UG){
		x<-1:length(sort.task)
		
		return(x)
	}
	
	dfs.neighbours <<- function(X){         
    return(sort.neighbours(list(X)))
	}
	dfs.cost <<- sort.cost
	
  sol <- dfs.search()
  
	return(sort.task[sol])
}

# Przykład użycia
x<-c(-14, 20, -50)
print('Sortujemy liczby')
x

#sorted <- list()
sorted <- dfs.sort(x)
print('Wynik')
sorted
