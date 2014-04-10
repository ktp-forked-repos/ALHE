

source('evol2.r')
source('alt.r')

evol2.bridges <-function(task){

	bridges.islands <<- task
	bridges.brlist <<- bridges.generateBridgesList(task)
	bridges.count <<- nrow(bridges.brlist)
	
	# optymalizacja - cache przeciec
	bridges.crossingsCache <- bridges.bridgesPottentialCrossingMatrix(bridges.brlist,bridges.islands)
	
	# metody

	evol2.optimizedCrossingsCount <- function(bridges, islands)
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
	
	evol2.getSettings <<- function()
	{
		return(
			list(
				generationCount = 2,
				mutationCount = 2,
				crossingCount = 10			
			)
		)
	}	
	
	
	evol2.cost <<- function(X)
	{
	
		print(X)
	
		bridges.brlist[,"val"] <- X
		
		consLack <- bridges.currentConsistencyLack(bridges.brlist, bridges.islands)
		conDemand <- bridges.currentIslandsConnectionsDemand(bridges.brlist, bridges.islands)
		#brCross <- bridges.currentBridgesCrossingsCount(bridges.brlist, bridges.islands)
		brCross <- evol2.optimizedCrossingsCount(bridges.brlist, bridges.islands)
		
		#print("cost!")
		#print(consLack)
		#print(sum(abs(conDemand)))
		
		#cost <- consLack*1000+brCross*100+sum(abs(conDemand))
		cost <- consLack+brCross+sum(abs(conDemand))
		
		return(cost)
		
	}

	
	# X - wejsciowy osobnik
	# Y - wejsciowy osobnik
	# UG - generator liczb losowych (0,1)
	# ret: krzyzowka X oraz Y
	evol2.cross <<- function(X, Y, UG)
	{
		parents <- list(X,Y)
		ret <- rep(0, times=bridges.count)
		rnd <- sample(1:2, bridges.count, replace=T)
		for(i in 1:bridges.count)
			ret[i] <- parents[[rnd[i]]][i]
		print(ret)
		return(ret)
	}

	
	# UG - generator liczb losowych (0,1)
	# ret: losowy osobnik
	evol2.getRandomCreature <<- function(UG)
	{
		return(sample(0:2, bridges.count, replace=T))
	}

	
	evol2.isCostLowEnough <<- function(Cost)
	{
		return(Cost == 0)
	}	
	
	# X - wejsciowy osobnik
	# MutateLevel - zadany stopien mutacji
	# ret: zmutowany osobnik
	
	evol2.mutate <<- function(X, UG)
	{
	
		mutated <- X
		
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

		#print('Zmutowany')
		#print(mutated)
		
		return(mutated)
		
	}
	
	sol <- evol2.search()
	costs <- apply(sol, 1, evol2.cost) 
	ret <- sol[costs == min(costs)]
  
	bridges.brlist[,"val"] <- ret
  
	return(bridges.brlist)
	
}

runTest <- function(filename)
{

	is <- bridges.loadIslandsFromFile(filename)
	br <- bridges.generateBridgesList(is)

	print(is)
	print(br)
	
	print('Wyszukiwanie rozwiazania...')

	solved <- evol2.bridges(is)

	print('Rozwiazane!')
	print(solved)

}

plec <- function()
{
	generationCount <- evol2.history[,1]
	bestCost <- evol2.history[,2]
	plot(generationCount, bestCost, type="o")
}

#runTest("tests/0.txt")
#runTest("tests/2.txt")
#runTest("tests/3.txt")
#runTest("tests/4.txt")
#runTest("tests/5.txt")
#runTest("tests/6.txt")
#runTest("tests/7.txt")
#runTest("tests/1.txt")



