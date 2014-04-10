sort.task <- c() # wektor z liczbami do posortowania, definiuje przestrzeń zadania

# generacja wszystkich posortowań możliwych do osiągnięcia
# z dotychczasowo sprawdzonych poprzez zastosowanie jednej
# zamiany kolejności elementów
sort.neighbours <- function(XS){
	res<-list()
	
	for(i in 1:length(XS)){
		x<-XS[[i]]
		
		for(j in 1:(length(x)-1)){
			for(k in (j+1):length(x)){
				res[[length(res)+1]] <- sort.swap(x,j,k)
			}
		}
	}	
  
  return(res)
}

# generuje nowy punkt na podstawie danego zamieniając
# kolejnością i-tą i j-tą pozycję
sort.swap <- function(x,i,j){
	xn<-x
	xn[i]<-x[j]
	xn[j]<-x[i]
	
	return(xn)
}


# Implementacja funkcji kosztu dla zadania sortowania rosnącego
# x - jest wektorem z przestrzeni przeszukiwań określającym kolejność liczb
# Funkcja zwraca oszacowanie kosztu posortowania wektora z ustawieniem x, 
# 0 - oznacza, że wektor sort.task[x] jes posortowany rosnąco.
sort.cost <- function(x){
	s <- sort.task[x]
	len <- length(s)

	q = 0
	
	for(i in 1:len){
		for(j in i:len){
			if(s[i] > s[j]){
				q = q + 1
			}
		}
	}
  
	return(q)
}