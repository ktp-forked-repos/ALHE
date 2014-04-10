source('mht.r')

# Z wejscia gdzie otrzymuje liste wierzcholkow z wartosciami generuje tylko prawidlowe krawedzie i zapisuje zaleznosci miedzy przecinajacymi sie

# !!!KONSTRUKCJE!!!
#
# wyspa - list(x,y,v) :: wspolrzedne wyspy i jej liczba
#

# przyjmuje dwie wyspy jako argumenty
# zwraca wartosc logiczna - czy moga one byc polaczone mostem
# tzn czy maja taka sama wspolrzedna rzedna lub odcieta

metody.areEdgesConnected <- function(edge1, edge2){ # spr czy odcinki tworzone przez krawedzie stykaja sie wierzcholkami
	if(	edge1$x1 == edge2$x1 && edge1$y1 == edge2$y1 ||
		edge1$x1 == edge2$x2 && edge1$y1 == edge2$y2 ||
		edge1$x2 == edge2$x1 && edge1$y2 == edge2$y1 ||
		edge1$x2 == edge2$x2 && edge1$y2 == edge2$y2
	){
		return(TRUE)
	}
	else{
		return(FALSE)
	}
}	

metody.isConsistent <- function(x){ #spr, czy graf jest spojny
	total = length(x)
	nothingChanged <- FALSE
	negative=0
	tmpSet <- list()
	for(i in 1:length(x)){
		if(x[[i]]$edgeNo<=0){
			negative = negative + 1
		}
	}
	
	for(i in 1:length(x)){ #musi byc tak, bo inaczej petla ponizej sie nie wykona
		if(x[[i]]$edgeNo>0){
			tmpSet[[length(tmpSet)+1]] <- list(x1=x[[i]]$x1, y1=x[[i]]$y1, v1=x[[i]]$v1, x2=x[[i]]$x2, y2=x[[i]]$y2, v2=x[[i]]$v2, dep=x[[i]]$dep, edgeNo=x[[i]]$edgeNo)
			x[i] <- NULL
			break
		}
	}
	while(nothingChanged==FALSE){
		nothingChanged <- TRUE	
		for(i in 1:length(x)){
			for(k in 1:length(tmpSet)){
				if(x[[i]]$edgeNo>0){
					if( metody.areEdgesConnected(tmpSet[[k]], x[[i]]) ){
						tmpSet[[length(tmpSet)+1]] <- list(x1=x[[i]]$x1, y1=x[[i]]$y1, v1=x[[i]]$v1, x2=x[[i]]$x2, y2=x[[i]]$y2, v2=x[[i]]$v2, dep=x[[i]]$dep, edgeNo=x[[i]]$edgeNo)
						x[i] <- NULL
						if(length(x)==0){
							return(TRUE)
						}
						nothingChanged <- FALSE
						break
					} 
				}
			}
			if(nothingChanged==FALSE){
				break
			}
		}
	}	
	if(length(tmpSet)!=0 && length(tmpSet) == total - negative){
		return(TRUE)
	}
	return(FALSE)
}

metody.cost <- function(x){ # funkcja celu (spojnosc + zle krawedzie) - minimalizowana: 0 - najlepsza
	result <- 0
	if(metody.isConsistent(x)==FALSE){
		result <- 15
	}
	for(i in 1:length(x)){
		result <- result + abs( x[[i]]$v1 - x[[i]]$edgeNo )
		result <- result + abs( x[[i]]$v2 - x[[i]]$edgeNo )
	}
	return(result)
}

metody.canBeConnected <- function(islandA,islandB) # spr, czy wyspy teoretycznie moga byc polaczone
{
	return( (islandA$x == islandB$x) || (islandA$y == islandB$y) )
}

metody.genEdgeNo <- function(){ # generuje losowa liczbe krawedzi
	x <- UG()
	if(x<=0.33){
		return(0)
	}
	else{
		if(x<=0.66){
			return(1)
		}
		else{
			return(2)
		}
	}
}

metody.mutate <- function(r, ML, UG){ #dla zbioru (r) i zadanego stopnia mutacji (ML) mutuje 1 lub 2 krawedzie
	if(ML<=1.5){
		tmp <- sample(1:length(r), 1)
		result <- metody.mutateInner(r, tmp)
	}
	else{
		tmp <- metody.mutateInner(r, sample(1:length(r), 1))
		result <- metody.mutateInner(tmp, sample(1:length(r), 1))
	}
print(result)
	return(result)
}

metody.mutateInner <- function(r, x) #mutuje wskazana krawedz (numer x) ze zbioru (r)
{	v1<- metody.genEdgeNo()
	tmp=0
	if(r[[x]]$edgeNo<0){ #gdy wylaczona, wlacz i spr inne
		if(r[[x]]$edgeNo==0){
			r[[x]]$edgeNo <- v1
		}
		else{
			r[[x]]$edgeNo <- (-1)*r[[x]]$edgeNo
		}
		if(!is.null(r[[x]]$dep)){
			for(i in 1:length(r[[x]]$dep)){
				tmp=r[[x]]$dep[i]
				if(r[[tmp]]$edgeNo>=0){
					r[[tmp]]$edgeNo <- (-1)*r[[tmp]]$edgeNo
				}
				}
			}
		} 
	}
	else{ #gdy wlaczona, wylacz
		r[[x]]$edgeNo <- (-1) * v1
	}
	return(r)
}


metody.findEdges <- function(n){ #wyszukuje w zbiorze wysp wszystkie poprawne krawedzie
	result <- list()
	flag <- TRUE
	count <- length(n)
	
	for(i in 1:(length(n)-1))
	{
		for(j in (i+1):length(n))
		{
			# dla kazdej pary wysp i,j
			if( metody.canBeConnected(n[[i]], n[[j]]) ) # jesli mozna postawic most miedzy wyspami i oraz j
			{
			
				flag <- TRUE
				for(k in 1:count)
				{
					# czy pomiedzy wyspami jest jeszcze inna wyspa
					if
					(
						n[[i]]$x==n[[j]]$x && n[[j]]$x==n[[k]]$x && (n[[i]]$y<n[[k]]$y && n[[k]]$y<n[[j]]$y || n[[j]]$y<n[[k]]$y && n[[k]]$y<n[[i]]$y)
						||
						n[[i]]$y==n[[j]]$y && n[[j]]$y==n[[k]]$y && (n[[i]]$x<n[[k]]$x && n[[k]]$x<n[[j]]$x || n[[j]]$x<n[[k]]$x && n[[k]]$x<n[[i]]$x)
					)
					{
						flag <- FALSE
						break
					}
				}
				if(flag)
				{
					if(n[[i]]$x <= n[[j]]$x)
					{	#te warunki potrzebne, zeby pozniej dobrze i latwo sprawdzac przeciecia
						if(n[[i]]$x != n[[j]]$x)
						{
							tmp_x1 <- n[[i]]$x
							tmp_x2 <- n[[j]]$x
							tmp_y1 <- n[[i]]$y
							tmp_y2 <- n[[j]]$y
							tmp_v1 <- n[[i]]$v
							tmp_v2 <- n[[j]]$v
						}
						else
						{
							if(n[[i]]$y <= n[[j]]$y)
							{
								tmp_x1 <- n[[i]]$x
								tmp_x2 <- n[[j]]$x
								tmp_y1 <- n[[i]]$y
								tmp_y2 <- n[[j]]$y
								tmp_v1 <- n[[i]]$v
								tmp_v2 <- n[[j]]$v
							}
							else
							{
								tmp_x2 <- n[[i]]$x
								tmp_x1 <- n[[j]]$x
								tmp_y2 <- n[[i]]$y
								tmp_y1 <- n[[j]]$y
								tmp_v1 <- n[[j]]$v
								tmp_v2 <- n[[i]]$v
							}
						}
					}
					else
					{
						tmp_x2 <- n[[i]]$x
						tmp_x1 <- n[[j]]$x
						tmp_y2 <- n[[i]]$y
						tmp_y1 <- n[[j]]$y
						tmp_v1 <- n[[j]]$v
						tmp_v2 <- n[[i]]$v
					}

					result[[length(result)+1]] <- list(x1=tmp_x1, y1=tmp_y1, v1=(n[[i]]$v), x2=tmp_x2, y2=tmp_y2, v2=(n[[j]]$v), dep=c(), edgeNo=0)
				}
			}
		}
	}
	return(result)
}

metody.areCrossing  <- function(n){	# spr, czy sie krzyzuja i dodaje zaleznosci do mostow
	for(i in 1:(length(n)-1)){
		for(j in (i+1):length(n)){
			if(
				n[[i]]$x1<n[[j]]$x1 && n[[i]]$x2>n[[j]]$x1 && n[[i]]$y1>n[[j]]$y1 && n[[i]]$y1<n[[j]]$y2
				||
				n[[j]]$x1<n[[i]]$x1 && n[[j]]$x2>n[[i]]$x1 && n[[j]]$y1>n[[i]]$y1 && n[[j]]$y1<n[[i]]$y2
				){
				n[[i]]$dep[length(n[[j]]$dep)+1] <- c(j)
				n[[j]]$dep[length(n[[j]]$dep)+1] <- c(i)
			}
		}
	}
	return(n)
}

metody.init <- function(n){ #z listy wierzcholkow generuje kompletny graf
	result <- list()
	result <- metody.findEdges(n)
	result <- metody.areCrossing(result)
	return(result)
}

#node = list(x,y,val)
n1 = list(x=1,y=2,v=2)
n2 = list(x=1,y=3,v=2)
n3 = list(x=2,y=3,v=4)
n4 = list(x=2,y=1,v=3)
n5 = list(x=3,y=1,v=3)
n6 = list(x=3,y=2,v=1)

#n7 = list(x=10,y=10,v=1) #do spr, ale dziala
#n8 = list(x=10,y=20,v=1)
#n9 = list(x=10,y=30,v=1)

#nodes = list(n1,n2,n3,n4,n5,n6,n7,n8,n9)
nodes = list(n1,n2,n3,n4,n5,n6)
#nodes = list(n1,n2,n3)
edges <- metody.init(nodes)
edges <- metody.mutate(edges, 2, UG)
edges
#metody.cost(edges)