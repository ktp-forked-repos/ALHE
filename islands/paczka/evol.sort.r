

source('evol.r')
source('sort.r')

evol.sort <-function( task){

	sort.task <<- task

	evol.cost <<- sort.cost

	evol.getInitialCreature <<- function(UG)
	{
		return(1 : length(sort.task))
	}

	evol.isCostLowEnough <<- function(Cost)
	{
		return(Cost == 0)
	}	
	
	# X - wejsciowy osobnik
	# MutateLevel - zadany stopien mutacji
	# ret: zmutowany osobnik
	
	evol.mutate <<- function(X, MutateLevel, UG)
	{
	
		mutated <- X
		
		for(i in 1:MutateLevel)
		{
			indexes <- sample(1:length(X), 2)
			mutated <- sort.swap(X, indexes[1], indexes[2])
		}
		
		return(mutated)
		
	}
	
	evol.mutationLevelTuneSettings <<- function()
	{
		return(
			list(
				rate = 10,			# co tyle prob nastapi dostrojenie stopnia mutacji
				upRatio = 1.2,		# mnoznik uzywany do zwiekszania stopnia mutacji
				downRatio = 0.8,	# jw. do zmniejszania
				minLevel = 1,		# minimalny stopien mutacji (nizej nie zejdzie)
				maxLevel = 5,		# jw. max
				startLevel = 3		# poczatkowy stopien mutacji
			)
		)
	}	
	
	
	sol <- evol.search()
  
	return(sort.task[sol])
	
}


plec <- function()
{
	generationCount <- evol.history[,1]
	bestCost <- evol.history[,2]
	plot(generationCount, bestCost, type="o")
}

# Przyk³ad u¿ycia
x<-c(1,3,2,2,6,2,6,4,2,3,4,5,2,4,5,4,2,6,364,54,4,4,356,4,56,1)

#x <- sample(1:100, 100)
print('Sortujemy liczby')
x

sorted <- evol.sort(x)
print('Wynik')
sorted