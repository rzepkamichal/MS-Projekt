##########################################         DANE         ###########################################
czerwone <- c(37.342,
              37.713,
              37.986,
              38.148,
              38.581,
              38.629,
              38.971,
              39.429,
              39.712,
              39.722,
              39.87,
              39.876,
              40.268,
              40.617,
              41.071,
              41.528,
              41.732,
              42.33,
              42.36,
              42.379,
              42.588,
              43.037,
              43.117,
              43.192,
              43.758
)

czarne <- c(35.145,
            36.591,
            36.898,
            37.469,
            37.56,
            37.562,
            38.332,
            38.51,
            40.063,
            40.159,
            40.469,
            40.621,
            40.743,
            40.755,
            40.786,
            40.931,
            41.173,
            41.327,
            41.417,
            41.466,
            41.567,
            42.057,
            43.27,
            42.675,
            44.17
)

##########################################      ZADANIE 1       ##########################################

#MIARY POLOZENIA
#srednia
average <- function(x) {
    suma <- 0
    for (n in x) {
        suma <- suma + n
    }
    return (suma/length(x))
}

#kwantyl - x wektor, p wartosc w procentach (0 - 1.0)
quantile <- function(x, p) {
    x <- sort(x, decreasing = FALSE)
    return (x[ceiling(p * length(x))])
}

#dominanta
dominant <- function(x) {
    #elementy z pierwszej kolumny - wartwartosc zliczanego elementu wektora x
    #elementy z drugiej kolumny - ilosc wystapien
    wystapienia <- matrix(nrow = 0, ncol = 2)
    for(n in x){
        y <- which(wystapienia[,1] == n)
        #funkcja dla sprawdzenia czy y jest integer(0)
        is.integer0 <- function(x) {
            is.integer(x) && length(x) == 0L
        }
        #jezeli n jest ju¿ w pierwszej kolumnie wystapienia
        if(!is.integer0(y)){
            #zwiêkszksz liczbê wyst¹pieñ o jeden
            wystapienia[y, 2] <- wystapienia[y, 2] + 1
        }
        #jezeli n jeszcze nie ma
        else{
            #ilosc wyst¹pieñ n - 1
            wystapienia <- rbind(wystapienia, c(n, 1))
        }
    }
  #nr wiersza maksymalnego elementu z drugiej kolumny, wskazuje dominante wektora x
    sorted <- sort(wystapienia[,2], decreasing = TRUE)
    nrWiersza <- which(wystapienia[,2] == sorted[1])
    max <- wystapienia[nrWiersza[1],1]
    return (max)
    
}

#MIARY ZROZNICOWANIA
#rozstep
distance <- function(x) {
    sorted <- sort(x)
    return (sorted[length(sorted)] - sorted[1])
}
#rozstep miêddzykwartylowy
distanceBtwQuantile <- function(x) {
    q3 <- quantile(x, 0.75)
    q1 <- quantile(x, 0.25)
    return (q3 - q1)
}
#moment centralny - funkcja pomocnicza
#x - wektor danych, k - stopien momentu centralnego
centralMoment <- function(x, k) {
    suma <- 0
    for (n in x){
        suma <- suma + (n - average(x)) ^ k
    }
    return (suma/length(x))
}
#wariancja
variance <- function(x) {
    return (centralMoment(x, 2))
}
#odchylenie standardowe
standardDeviation <- function(x) {
    return (sqrt(variance(x)))
}
#wspolczynnik zmiennosc
variationCoef <- function(x) {
    return (standardDeviation(x)/average(x))
}

#MIARY ASYMETRII
#skosnosc
slant <- function(x) {
    momCentk3 <- centralMoment(x, 3)
    odchStanDo3 <- standardDeviation(x)^3
    return (momCentk3/odchStanDo3)
}

#MIARY SKUPIENIA
#kurtoza
kurtoza <- function(x) {
    momCentk4 <- centralMoment(x, 4)
    odchStanDo4 <- standardDeviation(x)^4
    return (momCentk4/odchStanDo4)
}
#wspolczynnik Giniego
gini <- function(x) {
    sorted <- sort(x)
    suma <- 0
    for(i in 1:length(x)){
        dodaj <- (2*i - length(x) - 1) * sorted[i]
        suma <- suma + dodaj
    }
    podziel <- length(x)^2 * average(x)
    return (suma/podziel)
}

#HISTOGRAM
#histogram
histFun <- function(vektor, nazwa) {
    hist(
    vektor,
    main=nazwa,
    xlab="Zawartosc witaminy C",
    freq=TRUE,
    col="green",
    xlim=c(35, 45), #przedzial X
    ylim=c(0, 8),
    breaks=c(35, seq(35, 45, 1)) #przedzial x - zaczyna sie w 34, do 46, co 1
    )
    print(paste("Wygenerowano histogram dla: ", nazwa))
}

funkcjaZadania1 <- function(vec) {
    name <- deparse(substitute(vec)) #pobranie nazwy zmiennej
    print("Miary polozenia: ")
    print(paste("Srednia: ", average(vec)))
    print(paste("Pierwszy kwartyl: ", quantile(vec, 0.25)))
    print(paste("Mediana: ", quantile(vec, 0.5)))
    print(paste("Trzeci kwartyl: ", quantile(vec, 0.75)))
    print(paste("Dominanta: ", dominant(vec)))
    print("")
    print("Miary zroznicowania: ")
    print(paste("Rozstep: ", distance(vec)))
    print(paste("Rozstep miedzykwartylowy: ", distanceBtwQuantile(vec)))
    print(paste("Wariancja: ", variance(vec)))
    print(paste("Odchylenie standardowe: ", standardDeviation(vec)))
    print(paste("Wspolczynnik zmiennosci: ", variationCoef(vec)))
    print("")
    print("Miary asymetrii: ")
    print(paste("Skosnosc: ", slant(vec)))
    print("")
    print("Miary skupienia: ")
    print(paste("Kurtoza: ", kurtoza(vec)))
    print(paste("Wspolczynnik Giniego: ", gini(vec)))
    histFun(vec, name)
}

wypiszDane <- function (vec) {
    name <- deparse(substitute(vec))
    print(paste(name, ":"))
    print(vec)
    print(paste(name, ", posortowane:"))
    print(sort(vec))
    print("")
}

#wypisanie danych
drukujCzerwone = TRUE

if (drukujCzerwone) {
    wypiszDane(czerwone)
    funkcjaZadania1(czerwone)
} else {
    wypiszDane(czarne)
    funkcjaZadania1(czarne)
}


##########################################      ZADANIE 2       ##########################################

#Szereg rozdzielczy punktowy ----
diffMinMax <- function(table){
  return(max(table) - min(table))
}

diffMinMaxczerwone = diffMinMax(czerwone)
diffMinMaxczarne = diffMinMax(czarne)
k = sqrt(length(czerwone))  
#dlugosc taka sama dla obu tablic

h <- function(val, k){
  return(val/k)  
}

boundCzerwone = h(diffMinMaxczerwone, k)
boundCzarne = h(diffMinMaxczarne, k)


#Granice przedzialow ----
lowerBoundaryczerwone = czerwone[1]-(0.5*boundCzerwone)
lowerBoundaryczarne = czarne[1]-(0.5*boundCzarne)

boundary <- function(lowerBoundary, bound, color){
  count = 1
  boundaryTable <- c(lowerBoundary)
  while(boundaryTable[count]<max(color)){
    count = count + 1
    boundaryTable[count] = boundaryTable[count - 1] + bound
  }
  return(boundaryTable)
}

boundaryczerwone = boundary(lowerBoundaryczerwone, boundCzerwone, czerwone)
boundaryczarne = boundary(lowerBoundaryczarne, boundCzarne, czarne)
boundaryczerwone


emptyTable <- function(){
  count = 1
  myTable <- c()
  while(count<length(boundaryczerwone)){
    myTable[count] = ''
    count = count + 1
  }
  #zwracana tablica bedzie o 1 rozmiar mniejsza od boundaryczerwone
  return(myTable)
}

#Numer klasy szeregu rozdzielczego ----
vecIFunc = function(){
  count = 1
  myTable <- c()
  while(count<length(boundaryczerwone)){
    myTable[count] =  count
    count = count + 1
  }
  return(myTable)
} 

vecI = vecIFunc()


#Gorna granica przedzialu klasowego szeregu rozdzielczego ----
vecAiFunc = function(color, bound){
  count = 1
  upperCompartments <- c()
  for(eachOne in 1:length(color)-1){
    if(color[eachOne+1] > bound[count] & color[eachOne+1] < bound[count+1]){
    next
    }
    else{
    upperCompartments[count] = color[eachOne]  #gdyby np. byla tylko jedna wartosc w przedziale
    count = count + 1
    }
  }
  upperCompartments[count] = color[length(color)]
  #jesli w ostatnim przedziale jest tylko jedna wartosc
  return(upperCompartments)
}

vecAiCzerwone = vecAiFunc(czerwone, boundaryczerwone)
vecAiCzarne = vecAiFunc(czarne, boundaryczarne)


#Liczebnosc i-tej klasy szeregu rozdzielczego ----
vecNiFunc = function(vecAi, color, bound){
  howManyElements <- c()
  for(i in 1:length(vecAi)){
    howManyElements[i] = 0
  }
  howManyElements[1] = howManyElements[1] - 1
  howManyElements[length(vecAi)] = howManyElements[length(vecAi)] + 1

  count = 1
  upperCompartments <- c()
  for(eachOne in 1:length(color)-1){
    if(color[eachOne+1] > bound[count] & color[eachOne+1] < bound[count+1]){
      howManyElements[count] = howManyElements[count] + 1
    }
    else{
      howManyElements[count] = howManyElements[count] + 1
      count = count + 1
    }
  }
  return(howManyElements)
}

vecNiCzerwone = vecNiFunc(vecAiCzerwone, czerwone, boundaryczerwone)
vecNiCzarne = vecNiFunc(vecAiCzarne, czarne, boundaryczarne)


#Liczebnosc skumulowana i-tej klasy szeregu rozdzielczego ----
vecNisFunc <- function(vecNi){
  sumOfElements <- c(vecNi[1])
  for(eachOne in 2:length(vecNi)){
   sumOfElements[eachOne] = sumOfElements[eachOne - 1] + vecNi[eachOne]
  }
  return(sumOfElements)
}

vecNisczerwone = vecNisFunc(vecNiCzerwone)
vecNisczarne = vecNisFunc(vecNiCzarne)

#Dystrybuanta skumulowana ----
vecSnFunc <- function(vecNis, color){
  tableVecOfSn <- c()
  for(eachOne in 1:length(vecNis)){
    tableVecOfSn[eachOne] = vecNis[eachOne]/length(color)
  }
  return(tableVecOfSn)
}

vecSnCzerwone = vecSnFunc(vecNisczerwone, czerwone)
vecSnCzarne = vecSnFunc(vecNisczarne, czarne)

averageCzerwone = average(czerwone)
averageCzarne = average(czarne)


varianceCzerwone = variance(czerwone)
varianceCzarne = variance(czarne)


#Odchylenie standardowe ----
standardDeviationCzerwone = sqrt(varianceCzerwone)
standardDeviationCzarne = sqrt(varianceCzarne)

#Zestandaryzowana wartosc zmiennej w przedziale, liczona wg wzoru u = ( ai - srednia ) / odch.std. ----
vecU <- function(vecAi, average, standardDeviation){
  tableOfU <- c()
  for(eachOne in 1:length(vecAi)){
    tableOfU[eachOne] = (vecAi[eachOne] - average)/standardDeviation
  }
  return(tableOfU)
}

vecUCzerwone = vecU(vecAiCzerwone, averageCzerwone, standardDeviationCzerwone)
vecUCzarne = vecU(vecAiCzarne, averageCzarne, standardDeviationCzarne)

#Skumulowana dystrybuanta dla wartosci zestandaryzowanej ----
vecFxFunc <- function(vecU){
  tableOfFx <- c()
  for(eachOne in 1:length(vecU)){
    tableOfFx[eachOne] = pnorm(vecU[eachOne])
  }
  return(tableOfFx)
}

vecFxCzerwone = vecFxFunc(vecUCzerwone)
vecFxCzarne = vecFxFunc(vecUCzarne)


#Roznica miedzy dystrybuanta skumulowana a dystrybuanta dla rozkladu normalnego ----
vecSnFnFunc <- function(vecSn, vecFx){
  tableSnFn <- c()
  for(eachOne in 1:length(vecSn)){
    tableSnFn[eachOne] = abs(vecSn[eachOne]-vecFx[eachOne])
  }
  return(tableSnFn)
}

vecSnFnCzerwone = vecSnFnFunc(vecSnCzerwone, vecFxCzerwone)
vecSnFnCzarne = vecSnFnFunc(vecSnCzarne, vecFxCzarne)

row.names <- emptyTable()
column.names <- c('i', 'ai', 'ni', 'nis', 'Sn(x)', 'u', 'F(x)', '|Sn(x)-F(x)|')

matrix.names <- c('\nTest Kolmogorowa-Smirnowa dla pozycji czerwonych')
result <- array(c(vecI, vecAiCzerwone, vecNiCzerwone, vecNisczerwone, vecSnCzerwone, vecUCzerwone, vecFxCzerwone, vecSnFnCzerwone), dim = c((length(boundaryczerwone)-1), 8, 1), dimnames = list(row.names, column.names, matrix.names))
print(result)

matrix.names <- c('\nTest Kolmogorowa-Smirnowa dla pozycji czarnych')
result <- array(c(vecI, vecAiCzarne, vecNiCzarne, vecNisczarne, vecSnCzarne, vecUCzarne, vecFxCzarne, vecSnFnCzarne), dim = c((length(boundaryczerwone)-1), 8, 1), dimnames = list(row.names, column.names, matrix.names))
print(result)

dnFunc <- function(vecSnFn){
  return(max(vecSnFn))
}

dnCzerwone = dnFunc(vecSnFnCzerwone)
dnCzarne = dnFunc(vecSnFnCzarne)

#Poziom istotnosci alfa (0.05) ----
dnAlfa = 0.26404

result <- function(dn){
  if(dn<dnAlfa){
    print('Rozklad jest normalny')
  }
  else{
    print('Rozklad nie jest normalny')
  }
}

resultCzerwone = result(dnCzerwone)
resultCzarne = result(dnCzarne)

##########################################      ZADANIE 3       ##########################################

# Wspolczynnik ufnosci 1 - alfa = 0.98
# alfa = 0.02

# Wspolczynnik T-Studenta
TStudentCoefficient <- function(conf, n) {
  return(qt((1 - conf) / 2, n - 1, lower.tail = FALSE, log.p = FALSE))
}

# Przedzial ufnosci dla sredniej zawartosci witaminy C w owocach czerwonej porzeczki
trustIntervalLowerEndpoint <- function(av, Tcoe, stdDev, n) {
  lowerEndpoint = av - Tcoe * (stdDev / sqrt(n - 1))
  return(lowerEndpoint)
}

trustIntervalUpperEndpoint <- function(av, Tcoe, stdDev, n) {
  upperEndpoint = av + Tcoe * (stdDev / sqrt(n - 1))
  return(upperEndpoint)
}

# Wzgledna precyzja oszacowania
estimationPrecision <- function(ue, le, av){
  relativePrecision = 0.5 * (ue - le) / av
  return(relativePrecision)
}

# Przedzial‚ ufnosci dla zawartosci witaminy C w czerwonej porzeczce
lowerEndpointRed <- trustIntervalLowerEndpoint(average(czerwone), TStudentCoefficient(0.98, 25), standardDeviation(czerwone), 25)
upperEndpointRed <- trustIntervalUpperEndpoint(average(czerwone), TStudentCoefficient(0.98, 25), standardDeviation(czerwone), 25)

# Precyzja oszacowania dla zawartosci witaminy C w czerwonej porzeczce
precisionOfEstimation <- estimationPrecision(upperEndpointRed, lowerEndpointRed, average(czerwone))

##########################################      ZADANIE 4       ##########################################

amountCzarne <- length(czarne)

# Wspoczynnik ufnosci 1 - alfa = 0.98
alfa <- 0.02
# Odczytanie wartosci kwantyli rozkladu chi^2 dla danej ufnosci przy n-probek

chiSqCoef <- function(ufn, n) {
    return(qchisq(ufn, n - 1))
}
#Granice przedzialu ufnosci dla wariancji zawartosci wit. C w Czarnej porzeczce
valBoundary <- function(n, war, wspChi) {
    return((n * war) / wspChi)
}
#Wzgledna precyzja oszacowania
relValPrecision <- function(dolnaGranica, gornaGranica, war) {
    return(((0.5 * (gornaGranica - dolnaGranica)) / war)*100)
}

#Wsplczynniki chiKwadrat dla: chi(0.01,24), chi(0.99,24)

chiKwadrat1 <- chiSqCoef(alfa / 2, amountCzarne) 
chiKwadrat2 <- chiSqCoef(1 - (alfa / 2), amountCzarne)

#Przedzial ufnosci dla zawartosci wit. C w Czarnej porzeczce
upperValBoundary <- valBoundary(amountCzarne, variance(czarne), chiKwadrat1) #obliczana dla ufnoï¿½ci 0.01
lowerValBoundary <- valBoundary(amountCzarne, variance(czarne), chiKwadrat2) #obliczana dla ufnoï¿½ci 0.99

#Precyzja oszacowania dla zawartosci wit. C w Czarnej porzeczce
precyzjaCzarna <- relValPrecision(lowerValBoundary, upperValBoundary, variance(czarne))

##########################################      ZADANIE 5       ##########################################

#liczebnosc probek
amount <- 25

"wariancje obu rozkladow nie sa znane,
nie wiemy tez czy sa rowne
- zatem, aby wybrac dobra statystyke testowa nalezy najpierw wykonac
test hipotezy, ze sa one rowne"

# badanie H_0: sigma1^2 = sigma2^2, alpha = 0.05

#srednie probkowe
avgCzerwone <- average(czerwone)
avgCzarne <- average(czarne)

print(avgCzerwone)
print(avgCzarne)

#wariancje probkowe
varianceCzerwone <- variance(czerwone)
varianceCzarne <- variance(czarne)

print(varianceCzerwone)
print(varianceCzarne)

#wyznaczenie nieobciozonych estymatorow wariancji
unbiasedVarianceEstCzerwone <- amount * varianceCzerwone/(amount - 1)
unbiasedVarianceEstCzarne <- amount * varianceCzarne/(amount - 1)

print(unbiasedVarianceEstCzerwone)
print(unbiasedVarianceEstCzarne)

# f- stosunek wartosci estymatorow
"nie ma podstaw do odrzucenia hipotezy,
jesli wartosc f nie nalezy do obszaru krytycznego "

if (unbiasedVarianceEstCzerwone >= unbiasedVarianceEstCzarne){
  f <- unbiasedVarianceEstCzerwone / unbiasedVarianceEstCzarne
}else{
  f <- unbiasedVarianceEstCzarne / unbiasedVarianceEstCzerwone
}

print(f)

"mozliwe jest skorzystanie z testu t-Studenta"

#wyznaczenie statystyki testowej
#nalezy sprawdzic, czy nalezy do obszaru krytycznego
T <- (avgCzerwone - avgCzarne)/sqrt((amount*(varianceCzerwone+varianceCzarne)/(2*amount-2))*(2/amount))

print(T)

