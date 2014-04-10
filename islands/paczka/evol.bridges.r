

source('evol.r')
source('alt.r')

evol.bridges <-function(task){

	bridges.islands <<- task
	bridges.brlist <<- bridges.generateBridgesList(task)
	bridges.count <<- nrow(bridges.brlist)
	
	# optymalizacja - cache przeciec
	bridges.crossingsCache <- bridges.bridgesPottentialCrossingMatrix(bridges.brlist,bridges.islands)
	
	# metody

	evol.optimizedCrossingsCount <- function(bridges, islands)
	{
		count <- nrow(islands)		# liczba wysp
		bcount <- nrow(bridges)		# liczba mostow
		cross <- 0
		# lecimy po mostach
		for(i in 1:(bcount-1)){
			for(j in (i+1):bcount){
				if(bridges.crossingsCache[i,j]){
					if(bridges[j,"val"] > 0 && bridges[i,"val"] > 0)
						cross <- cross + 1
				}
			}
		}
		return(cross)
	}
	
	evol.mutationLevelTuneSettings <<- function()
	{
		return(
			list(
				rate = 10,			# co tyle prob nastapi dostrojenie stopnia mutacji
				upRatio = 1.2,		# mnoznik uzywany do zwiekszania stopnia mutacji
				downRatio = 0.8,	# jw. do zmniejszania
				minLevel = 1,		# minimalny stopien mutacji (nizej nie zejdzie)
				maxLevel = 40,		# jw. max
				startLevel = 5		# poczatkowy stopien mutacji
			)
		)
	}
	
	evol.cost <<- function(X)
	{
	
		bridges.brlist[,"val"] <- X
		
		consLack <- bridges.currentConsistencyLack(bridges.brlist, bridges.islands)
		conDemand <- bridges.currentIslandsConnectionsDemand(bridges.brlist, bridges.islands)
		#brCross <- bridges.currentBridgesCrossingsCount(bridges.brlist, bridges.islands)
		brCross <- evol.optimizedCrossingsCount(bridges.brlist, bridges.islands)
		
		#print("cost!")
		#print(consLack)
		#print(sum(abs(conDemand)))
		
		#cost <- consLack*1000+brCross*100+sum(abs(conDemand))
		cost <- consLack+brCross+sum(abs(conDemand))
		
		return(cost)
		
	}

	evol.getInitialCreature <<- function(UG)
	{
	
		initial <- rep(0, times=nrow(bridges.brlist))
		
		print('Initial creature:')
		print(initial)
	
		return(initial)
		
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
			if(sample(1:2, 1) == 1)
			{
				# dodajemy
				ind <- sample(1:length(mutated),1)
				if(mutated[ind] < 2) mutated[ind] <- mutated[ind]+1
			}
			else
			{
				# usuwamy
				ind <- sample(1:length(mutated),1)
				if(mutated[ind] > 0) mutated[ind] <- mutated[ind]-1
			}
		}

		#print('Zmutowany')
		#print(mutated)
		
		return(mutated)
		
	}
	
	sol <- evol.search()
  
	bridges.brlist[,"val"] <- sol
  
	return(bridges.brlist)
	
}

runTest <- function(filename)
{

	is <- bridges.loadIslandsFromFile(filename)
	br <- bridges.generateBridgesList(is)

	print(is)
	print(br)
	
	print('Wyszukiwanie rozwiazania...')

	solved <- evol.bridges(is)

	print('Rozwiazane!')
	print(solved)

}

plec <- function()
{
	generationCount <- evol.history[,1]
	bestCost <- evol.history[,2]
	plot(generationCount, bestCost, type="o")
}

runTest("tests/0.txt")




