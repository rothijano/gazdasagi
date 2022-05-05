# Putnóczki Fanni zh, neptun kód: GCFA4K	


#----------------------------------------------1. feladat

# a sorozat indexelése 0-ról indul
get_series = function(n) {
	if(n==0) {return(0)}
	if(n==1) {return(1)}
	if(n==2) {return(2)}
	x = c(0,1,2)
	for(i in 3:n) {
		new_x = x[i-2] + x[i-1] + x[i]
		x = c(x, new_x)
	}
	return(tail(x, n=1))
}

get_series(9)

#----------------------------------------------2. feladat

#--------a rész
u = runif(100) #100 számot generál
p1 = 0
p2 = 1
random_cauchy = qcauchy(u, location=p1, scale=p2)

#---------c rész
filtered = random_cauchy[random_cauchy>-10 & random_cauchy<10]
hist(filtered, main="generált cauchy eloszláss", freq=F) #a piros a tényleges
points(seq(-10,10,0.1), dcauchy(seq(-10,10,0.1)), type="l", col="red")

#---------b rész

log_lik = function(y, params) {
    l = sum(dcauchy(y, location=params[1], scale=params[2], log=TRUE))
    return (-l)
}
opt = nlm(f=log_lik, p=c(0,1), y=random_cauchy)
opt$estimate  # a becsült értékek, az 1. a p1, a 2. a p2



#---------------------------------------------3. feladat

x = c(21,25,29,31,33,35,42,45,51,53)
y = c(175,182,190,206,242,265,280,356,380,450)

# lineáris modell vizsgálata
model_lin = lm(y~x); summary(model_lin)
# következmény: b0-ra nincs szükség, mert 0 csillagot kapott
model_lin2 = lm(y~-1+x); summary(model_lin2)

#exponenciális modell vizsgálata
model_exp = lm(y~-1+x+I(exp(x)));summary(model_exp)
# következmény: e^x csak egy csillagot kapott, ezért nem sokat módosít

# a modellben az x és y közötti (életkor és havi jövedelem) kapcsolat
# erõsségét az R-squared érték jelöli, minél közelebb áll az 1-hez, annál jobb
# -> az exponenciálisban az R-squared értéke jobb

#ábrázolás
xline = seq(15,60,0.1)
par(mfrow=c(1,2))

plot(x,y, main="a lineáris modell")
points(xline, predict(model_lin,newdata=data.frame(x=xline)), type="l", col="red")

plot(x,y, main="az exponenciális modell")
points(xline, predict(model_exp,newdata=data.frame(x=xline)), type="l", col="red")

# a hibák négyzetösszege
ex_hiba = sum((y-predict(model_exp))^2); ex_hiba;
lin_hiba = sum((y-predict(model_lin))^2); lin_hiba;
# a 2 modell közül az exponenciális jobban illeszkedik, 
# mert kisebb a hibája, és az R-square értéke is közelebb áll az 1-hez
# és a p-value is kisebb

# 30 évre becslés exponenciálissal:
x30 = predict(model_exp,newdata=data.frame(x=30), level=0.99)
#az átlagos jövedelem:
x30


#--------------------------------------------4. feladat

hozamok = c(0.30,0.20,0.09,0.12,0.17)
szorasok = c(0.45,0.43,0.10,0.20,0.32)

get_min_risk_weights = function(hozamok, szorasok, r) {
    n = length(hozamok)
    Dmat = diag(szorasok)
    dvec = rep(0, n)
    Amat = cbind(hozamok, rep(1, n), diag(n))
    bvec = c(r, 1, rep(0,n))
    meq = 2
    solution = solve.QP(Dmat, dvec, Amat, bvec, meq)
    weights = round(solution$solution, digits=4)
    #weights=solution$solution
    return( list(weights = weights, risk = solution$value) )
}

dat = get_min_risk_weights(hozamok, szorasok, 0.20)
#a legjobb súlyok 0.20 hozamhoz
dat$weights
# a súlyok összege tényleg 1 (kb a kerekítés miatt)
sum(dat$weights)
# a súlyokhoz tartozó kockázat
risk = sqrt(dat$weights%*%diag(szorasok)%*%dat$weight); risk

#ábrázolás
par(mfrow=c(1,1))
labels = c("A", "B", "C", "D", "E")
pie(dat$weights, labels, main="A befektetések megoszlása")

