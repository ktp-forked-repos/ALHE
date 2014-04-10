source('evol.r')
source('metody.r')

evol.islands <-function(task){


	evol.mutationLevelTuneSettings <<- function()
	{
		return(
			list(
				rate = 10,			# co tyle prob nastapi dostrojenie stopnia mutacji
				upRatio = 1.2,		# mnoznik uzywany do zwiekszania stopnia mutacji
				downRatio = 0.8,	# jw. do zmniejszania
				minLevel = 1,		# minimalny stopien mutacji (nizej nie zejdzie)
				maxLevel = 2,		# jw. max
				startLevel = 2		# poczatkowy stopien mutacji
			)
		)
	}

	
	# wejscie - osobnik do ocenienia
	evol.cost <<- metody.cost

	evol.getInitialCreature <<- function(UG)
	{
		return(task) # nie zmieniac!
	}

	evol.isCostLowEnough <<- function(Cost)
	{
		return(Cost == 0) # szuka rozwiazania idealnego, nieprzyblizonego
	}	
	
	# X - wejsciowy osobnik
	# MutateLevel - zadany stopien mutacji
	# ret: zmutowany osobnik (brat X'a)
	evol.mutate <<- metody.mutate
	
	sol <- evol.search()
  
	return(sol)
	
}

# Przyk³ad u¿ycia

# !!!
# x <- osobnik do rozwiazania
#node = list(x,y,val)
n1 = list(x=1,y=2,v=2)
n2 = list(x=1,y=3,v=4)
n3 = list(x=2,y=3,v=2)
x <- list(n1,n2,n3)
edges <- metody.init(x)
edges
solved <- metody.cost(edges)
#solved <- evol.islands(edges)
print('Wynik')
solved