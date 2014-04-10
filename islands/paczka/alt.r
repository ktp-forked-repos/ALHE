
# wczytywanie tablicy wysp z pliku
bridges.loadIslandsFromFile <- function(filename)
{
	return(read.csv(filename, header = TRUE, sep = " ", quote="\"", dec=".", fill = TRUE, comment.char=""))
}

# a,b - indeksy wysp
# islands - tablica wysp
bridges.canBeConnected <- function(a,b,islands)
{

	coords <- c(NA,NA)
	
	# ktore wspolrzedne sa takie same
	if(islands[a,"x"] == islands[b,"x"])
		coords <- c(same="x", notsame="y")
	if(islands[a,"y"] == islands[b,"y"])
		coords <- c(same="y", notsame="x")
	
	# jesli zadne to od razu nie
	if(is.na(coords["same"]))
		return(FALSE)
		
	csame <- coords["same"]			# wspolrzedna jednakowa
	cnsame <- coords["notsame"]		# wspolrzedna niejednakowa
		
	# wspolrzedne tylko tych 2 wysp
	abisl <- islands[c(a,b),c("x","y")]
	
	common <- abisl[1,csame] # wspolna wspolrzedna
	ncommon <- c( abisl[1,cnsame], abisl[2,cnsame]) # wspolrzedne roznicae sie
	
	bvec <- 
	(islands[,csame] == common) & 		# tylko wyspy na tej samej wspolnej wspolrzednej
	islands[,cnsame] > min(ncommon) & 	# a teraz wybieramy te ze "srodka"
	islands[,cnsame] < max(ncommon)
	
	if(sum(bvec) > 0)
		return(FALSE) # jest wyspa pomiedzy tamtymi dwoma
	
	return(TRUE)
		
}

# generuje liste wszystkich mozliwych do stworzenia mostow
# dla kazdego mostu poczatkowa jego wartoscia jest 0
bridges.generateBridgesList <- function(islands)
{

	n <- nrow(islands)
	bridges <- c()
	
	for(i in 1:(n-1))
	{
		for(j in (i+1):n)
		{
			if(bridges.canBeConnected(i,j,islands))
				bridges <- rbind(bridges,c(i,j,0))
		}
	}
	
	colnames(bridges) <- c("a","b","val")
	
	return(bridges)
	
}

# zwraca wektor dlugosci liczby wysp
# dla kazdej wyspy okresla liczbe "zapotrzebowania" na polaczenia
# 0 	- wyspa ma dokladnie tyle polaczen ile potrzebuje
# -N 	- wyspa ma o N polaczen za duzo
# +N 	- wyspa potrzebuje jeszcze N polaczen
bridges.currentIslandsConnectionsDemand <- function(bridges, islands)
{
	curConSum <- bridges.currentIslandsConnectionsSum(bridges, islands)
	demandCon <- islands[,"val"]
	return (demandCon - curConSum)
}


bridges.between <- function(i,a,b)
{
	return(i>min(a,b) && i<max(a,b))
}

# returns island i as point :: list(x,y)
bridges.getIslandAsPoint <- function(i,islands)
{
	vx <- islands[i,"x"]
	vy <- islands[i,"y"]
	return(list(x=vx,y=vy))
}

# returns bridge i as segment (point-to-point, island-to-island)
# segment = list(sbegin=list(x,y), send=list(x,y))
bridges.getBridgeAsSegment <- function(i,bridges,islands)
{
	startIsland <- bridges[i,"a"]
	endIsland <- bridges[i,"b"]
	ps <- bridges.getIslandAsPoint(startIsland,islands)
	pe <- bridges.getIslandAsPoint(endIsland,islands)
	return(list(sbegin=ps, send=pe))
}


# zwraca wartosc logiczna - czy mosty i,j sie przecinaja
bridges.areBridgesCrossing <- function(i,j,bridges,islands)
{

	isHorizontal <- function(seg)
	{
		return(seg$sbegin$y == seg$send$y)
	}
	
	isBetween <- function(a,b,c)
	{
		return(a>min(b,c) && a<max(b,c))
	}

	first <- bridges.getBridgeAsSegment(i,bridges,islands)
	second <- bridges.getBridgeAsSegment(j,bridges,islands)

	if(isHorizontal(first) == isHorizontal(second))
		return(FALSE)
	
	if(isHorizontal(first))
	{
		# first jest poziomy
		return(isBetween(first$sbegin$y, second$sbegin$y, second$send$y) && isBetween(second$sbegin$x, first$sbegin$x, first$send$x))
	}
	else
	{
		return(isBetween(second$sbegin$y, first$sbegin$y, first$send$y) && isBetween(first$sbegin$x, second$sbegin$x, second$send$x))
	}

	stop('blad')
	
}

# zwraca macierz potencjalnych przeciec mostow
bridges.bridgesPottentialCrossingMatrix <- function(bridges, islands)
{

	count <- nrow(islands)		# liczba wysp
	bcount <- nrow(bridges)		# liczba mostow
	
	crossMtx <- matrix(rep(FALSE,times=bcount*bcount), nrow=bcount)
	
	# lecimy po mostach
	for(i in 1:(bcount-1))
	{
		for(j in (i+1):bcount)
		{
			if(bridges.areBridgesCrossing(i,j,bridges,islands))
			{
				crossMtx[i,j] <- TRUE
				crossMtx[j,i] <- TRUE
			}
		}
	}
	
	return(crossMtx)

}


# zwraca liczbe przeciec mostow
bridges.currentBridgesCrossingsCount <- function(bridges, islands)
{

	count <- nrow(islands)		# liczba wysp
	bcount <- nrow(bridges)		# liczba mostow
	
	cross <- 0
	
	# lecimy po mostach
	for(i in 1:(bcount-1))
	{
	
		if(bridges[i,"val"] <= 0)
			next
	
		for(j in (i+1):bcount)
		{
		
			if(bridges[j,"val"] <= 0)
				next

			if(bridges.areBridgesCrossing(i,j,bridges,islands))
					cross <- cross + 1
					
		}
	}
	
	return(cross)

}



# zwraca wektor o dlugosci - liczba wysp
# dla kazdej wyspy zawiera liczbe polaczen do niej dochodzacych (odchodzacych)
#
# bridges - uklad mostow dla ktorych maja byc policzone statystyki
# islands - wyspy
bridges.currentIslandsConnectionsSum <- function(bridges, islands)
{

	count <- nrow(islands)		# liczba wysp
	bcount <- nrow(bridges)		# liczba mostow
	
	# wstepna macierz, TRUE na przekatnej, FALSE gdzie indziej
	summat <- rep(0,times=count)
	
	# lecimy po mostach
	for(i in 1:bcount)
	{
	
		a <- bridges[i,"a"]
		b <- bridges[i,"b"]
		val <- bridges[i,"val"]

		# dla kazdego istniejacego mostu dodajemy jego wartosc do obu stron
		if(val > 0)
		{
			summat[a] <- summat[a] + val
			summat[b] <- summat[b] + val
		}
		
	}
	
	return(summat)
	
}


# zwraca macierz N na N (N - liczba wysp)
# kazda komorka jest wartoscia logiczna - czy da sie przejsc
# pomiedzy wyspami
bridges.currentReachabilityMatrix <- function(bridges, islands)
{

	count <- nrow(islands)		# liczba wysp
	bcount <- nrow(bridges)		# liczba mostow
	
	# wstepna macierz, TRUE na przekatnej, FALSE gdzie indziej
	rmat <- matrix(rep(FALSE,times=count*count), nrow=count) | (diag(count)==1)
	
	# lecimy po mostach
	for(i in 1:bcount)
	{
	
		a <- bridges[i,"a"]
		b <- bridges[i,"b"]
		val <- bridges[i,"val"]

		
		# dla kazdego istniejacego mostu robimy 'or' na rzedach macierzy przejsc
		# odpowiadajacych obu koncom mostu
		
		if(val > 0)
		{
			tmp <- rmat[a,] | rmat[b,]
			tmp2 <- rmat[,a] | rmat[,b]
			rmat[a,] <- tmp
			rmat[b,] <- tmp
			rmat[,a] <- tmp2
			rmat[,b] <- tmp2		
		}
		
	}
	
	return(rmat)

}



####################################################################################################


# zwraca liczbe par niepolaczonych wysp (takich ze z jednej
# nie da sie przejsc do drugiej)
bridges.currentConsistencyLack <- function(bridges, islands)
{
	rmat <- bridges.currentReachabilityMatrix(bridges, islands)
	return(sum(!rmat) / 2)
}


# zwraca wartosc logiczna - czy graf jest spojny czy nie
bridges.isConsistent <- function(bridges, islands)
{
	return(bridges.currentConsistencyLack(bridges,islands)==0)
}




