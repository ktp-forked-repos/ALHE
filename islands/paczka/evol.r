source('mht.r')

evol.counter <- 0 # licznik generacji

# X - osobnik
# ret: koszt osobnika (mniejszy = lepszy)
evol.cost<-function(x)
{
  stop('Brak implementacji funkcji cost')
}

# UG - generator liczb losowych (0,1)
# ret: osobnik od ktorego rozpocznie sie wyszukiwanie
evol.getInitialCreature <- function(UG)
{
	stop('Brak implementacji funkcji getInitialCreature')
}

# Cost - koszt znalezionego osobnika
# ret: wartosc logiczna - czy dany koszt satysfakcjonuje wywolujacego
#	czy kontynuowac poszukiwania
evol.isCostLowEnough <- function(Cost)
{
	stop('Brak implementacji funkcji isCostLowEnough')
}

# X - wejsciowy osobnik
# MutateLevel - zadany stopien mutacji (zakres do ustawienia w opcjach wywolania metody)
# UG - generator liczb losowych (0,1)
#
# ret: zmutowany osobnik (im wyzszy stopein mutacji tym bardziej powinien byc zmutowany)
#
evol.mutate <- function(X, MutateLevel, UG)
{
	stop('Brak funkcji mutate!')
}

# ustawienia wywolania, mozna przeladowac, mozna zostawic
evol.mutationLevelTuneSettings <- function()
{
	return(
		list(
			rate = 10,			# co tyle prob nastapi dostrojenie stopnia mutacji
			upRatio = 1.2,		# mnoznik uzywany do zwiekszania stopnia mutacji
			downRatio = 0.8,	# jw. do zmniejszania
			minLevel = 1,		# minimalny stopien mutacji (nizej nie zejdzie)
			maxLevel = 10,		# jw. max
			startLevel = 5		# poczatkowy stopien mutacji
		)
	)
}


#########################################################################################
#########################################################################################
#########################################################################################

evol.counter <- 0
evol.history <- c()

evol.mutationLevelTuneSettingsChecked <- function()
{
	tmp <- evol.mutationLevelTuneSettings()

	if(tmp$minLevel > tmp$maxLevel) stop("Invalid mutationLevelTuneSettings :: minlevel > maxlevel");
	if(tmp$rate < 10) stop("Invalid mutationLevelTuneSettings :: rate < 10");
	if(tmp$startLevel > tmp$maxLevel) stop("Invalid mutationLevelTuneSettings :: startLevel > maxlevel");
	if(tmp$startLevel < tmp$minLevel) stop("Invalid mutationLevelTuneSettings :: startLevel < minlevel");
	
	return(tmp)
}



evol.init<-function(UG)
{
	evol.counter <<- 0
	evol.history <<- c()
	return(evol.getInitialCreature(UG))
}

evol.model_init<-function(UG)
{

	tuneSettings <- evol.mutationLevelTuneSettingsChecked()

	return
	(
		list(
		
			best = NULL,									# najlepszy dotad znaleziony osobnik
			mutationLevel = tuneSettings$startLevel,	# stopien mutacji
			
			tried = 0,			# liczba prob, licznik do wykonywanie tuningu
			succeed = 0,		# liczba sukcesow od ostatniego tuningu stopnia mutacji
			
			consts = list	(
								tuneRate = tuneSettings$rate,
								tuneUpRatio = tuneSettings$upRatio,
								tuneDownRatio = tuneSettings$downRatio,
								minMutationLevel = tuneSettings$minLevel,
								maxMutationLevel =  tuneSettings$maxLevel
							)
			
		)
	)
	
}

# X - wyniki selekcji
# M - model
# ret: zaktualizowany model
evol.model_update<-function(X,M)
{

	oldCost <- Inf
	
	if(!(is.null(M$best))) oldCost <- evol.cost(M$best)
	
	newCost <- evol.cost(X)
	
	M$tried <- M$tried + 1
	
	if(newCost < oldCost)
	{
	
		evol.history <<- rbind(evol.history, c(evol.counter, newCost))
	
		print(newCost)
		M$succeed <- M$succeed + 1
		M$best <- X
		
		
	}
	
	if(M$tried == M$consts$tuneRate)
	{
	
		sucratio <- M$succeed / M$tried
		
		# za duzo sukcesow, zbyt powoli sie przemieszczamy
		# trzeba zwiekszyc kroki
		
		if(sucratio > 0.2)
		{
			M$mutationLevel <- min(M$mutationLevel * M$consts$tuneUpRatio, M$consts$maxMutationLevel)
		}
			
		# za malo sukcesow, musimy zmniejszyc kroki
		if(sucratio < 0.2)
		{
			M$mutationLevel <- max(M$mutationLevel * M$consts$tuneDownRatio, M$consts$minMutationLevel)
		}
	
		M$tried <- 0
		M$succeed <- 0
		
		
	}

	return(M)
  
}


# zwraca kolejne pokolenie - tj dziecko osobnika najlepszego
evol.generate<-function(XS,M,UG)
{

	evol.counter <<- evol.counter + 1
	
	if(evol.counter %% 1000 == 0)
		print(evol.counter)

	best <- M$best
	child <- evol.mutate(best, M$mutationLevel, UG)
	return(child)
}

evol.select<-function(XS,M,UG)
{
  last <- length(XS)
  return(XS[[last]])
}

evol.stop<-function(XS,M)
{

	cost <- evol.cost(XS[[length(XS)]])

	if(evol.isCostLowEnough(cost))
	{
		evol.history <<- rbind(evol.history, c(evol.counter, cost))
		return(TRUE)
	}
	
	return(FALSE)
	
}

evol.search<-function()
{
	search(evol.model_init, evol.model_update, evol.init, evol.select, evol.generate, evol.stop, UG)
	return(pop(1)[[1]])
}







