source('mht.r')

evol2.counter <- 0 # licznik generacji

# X - osobnik
# ret: koszt osobnika (mniejszy = lepszy)
evol2.cost<-function(x)
{
  stop('Brak implementacji funkcji cost')
}

# UG - generator liczb losowych (0,1)
# ret: losowy osobnik
evol2.getRandomCreature <- function(UG)
{
	stop('Brak implementacji funkcji getInitialCreature')
}

# Cost - koszt znalezionego osobnika
# ret: wartosc logiczna - czy dany koszt satysfakcjonuje wywolujacego
#	czy kontynuowac poszukiwania
evol2.isCostLowEnough <- function(Cost)
{
	stop('Brak implementacji funkcji isCostLowEnough')
}

# X - wejsciowy osobnik
# UG - generator liczb losowych (0,1)
# ret: zmutowany osobnik
evol2.mutate <- function(X, UG)
{
	stop('Brak funkcji mutate!')
}

# X - wejsciowy osobnik
# Y - wejsciowy osobnik
# UG - generator liczb losowych (0,1)
# ret: krzyzowka X oraz Y
evol2.cross <- function(X, Y, UG)
{
	stop('Brak funkcji mutate!')
}

# ustawienia wywolania, mozna przeladowac, mozna zostawic
evol2.getSettings <- function()
{
	return(
		list(
			generationCount = 20,
			mutationCount = 3,
			crossingCount = 50			
		)
	)
}


#########################################################################################
#########################################################################################
#########################################################################################

evol2.history <- c()	#history
evol2.counter <- 0 		#generations counter

# zwraca settings$generationCount
# inicjujacych osobnikow (w postaci kolejnych rzedow macierzy)
evol2.init<-function(UG)
{

#	print('evol2.init')

	evol2.counter <<- 0
	evol2.history <<- c()
	
	set <- evol2.getSettings() #opcje
	
	initMtx <- evol2.getRandomCreature(UG)
	
	for(i in (2:(set$generationCount)))
	{
		tmp <- evol2.getRandomCreature(UG)
		initMtx <- rbind(initMtx, tmp)
	}
	
	return(initMtx)
	
}

evol2.model_init<-function(UG)
{

	return
	(
		list(
			bestCost=Inf,
			sett = evol2.getSettings() # opcje
		)
	)
	
}

# X - wyniki selekcji
# M - model
# ret: zaktualizowany model
evol2.model_update<-function(X,M)
{
	costs <- apply(X, 1, evol2.cost) 
	minCost <- min(costs)
	if(minCost < M$bestCost)
	{
		print('Cost:')
		print(minCost)
		M$bestCost <- minCost
		
		evol2.history <<- rbind(evol2.history, c(evol2.counter, minCost))
		
	}
	return(M)
}

# zwraca kolejne pokolenie
# XS - wyniki selekcji
evol2.generate<-function(XS,M,UG)
{

#	print('evol2.generate')

	evol2.counter <<- evol2.counter + 1
	
	if(evol2.counter %% 100 == 0)
		print(evol2.counter)

	selected <- XS
	selectedcount <- nrow(selected)
	
	generated <- c()

#	print('evol2.generate.mutacje')

	# mutacje
	for(i in 1:M$sett$mutationCount)
	{
		toMutate <- selected[sample(1:selectedcount, 1) ,]
		mutated <- evol2.mutate(toMutate, UG)
		generated <- rbind(generated, mutated)
	}

#	print('evol2.generate.krzyzowanie')
	
	# krzyzowanie
	for(i in 1:M$sett$crossingCount)
	{
		crossA <- selected[sample(1:selectedcount, 1) ,]
		crossB <- selected[sample(1:selectedcount, 1) ,]
		crossed <- evol2.cross(crossA,crossB,UG)
		generated <- rbind(generated, crossed)
	}
	
	return(generated)
	
}

# z ostatniej generacji wybiera M$sett$generationCount najlepszych
evol2.select<-function(XS,M,UG)
{

#	print('evol2.select')

	ind <- length(XS)
	generated <- XS[[ind]] # generated lately
	costs <- apply(generated, 1, evol2.cost) 
	
	selected <- generated[order(costs) <= M$sett$generationCount , ]
	
	#print(selected)
	
	return(selected)
	
}

# zatrzymuje jesli ktorykolwiek osobnik z ostatniej generacji spelnia wymagania
evol2.stop<-function(XS,M)
{
	ind <- length(XS)
	generated <- XS[[ind]] # generated lately
	costs <- apply(generated, 1, evol2.cost) 
	minCost <- min(costs)
	success <- sapply(costs,evol2.isCostLowEnough)
	
	if(sum(success) > 0)
	{
		evol2.history <<- rbind(evol2.history, c(evol2.counter, minCost))
		return(TRUE)
	}
	
	return(FALSE)
	
}

evol2.search<-function()
{
	search(evol2.model_init, evol2.model_update, evol2.init, evol2.select, evol2.generate, evol2.stop, UG)
	return(pop(1)[[1]])
}







