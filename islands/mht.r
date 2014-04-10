# Konwencja nazewnicza:
# X - przestrzen przeszukiwan
# XS - sekwencja punktow (moze byc pusta)
# U - liczba "losowa"
# US - sekwencja liczb losowych (moze byc pusta)
# M - model (moze byc pusty)

# zmienne globalne:
xtrace <<- list() #slad punktow

# zapis do sladu punktow
push<-function(XS)
{
  xtrace[[length(xtrace)+1]] <<- XS
}

# odczyt ze sladu punktow co najwyzej n elementow
# Inf oznacza cala historie
pop<-function(n=Inf)
{
  len <- length(xtrace)
  return(xtrace[max(len-n+1,1):len])
}

# inicjacja sladu
trace_init<-function(){
  xtrace <<- list()
}

# UG - generator liczb losowych - odczyt kolejnej wartosci 
# ze zbioru liczb losowych lub generacja
UG<-function()
{
  return(runif(1,min=0,max=1))
}

# inicjacja początkowego modelu
model_init<-function(UG){
  return(list())
}

# aktualizacja modelu - aktualizuje model na podstawie 
# sekwencji punkt?w
model_update<-function(XS,M)
{
  return(M)
}

# inicjacja - generuje sekwencje punktow na podstawie 
# liczb losowych
op_init<-function(UG)
{
  return(XS)
}

# selekcja - generuje podsekwencje argumentu, posilkujac sie modelem i 
# sekwencja liczb losowych
op_select<-function(XS,M, UG)
{
  return(XS)
}

# generacja - generuje sekwencje punktow na podstawie 
# innej sekwencji punktow, modelu oraz liczb losowych
op_generate<-function(XS,M,UG)
{
  return(XS)
}


# kryterium zatrzymania obserwuje punkty z historii, moze przyjmowac
# argumenty ktorych nie sposob przewidziec z gory
# zwraca wartosc logiczna
stop_criterion<-function(XS, M)
{
  return(T)
}

#algorytm przeszukiwania 
search<-function(model_init,model_update, op_init, op_select,op_generate, stop_criterion, UG)
{

  trace_init()
  
  push(op_init(UG))
  M<-model_init(UG)
  
  while (!stop_criterion(pop(),M))
  {
    XS <- op_select(pop(),M,UG)
    M <- model_update(XS,M)
    push(op_generate(XS,M, UG))
  }
  
  
  
}
