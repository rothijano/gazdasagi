# Putn�czki Fanni zh, neptun k�d: GCFA4K	


#----------------------------------------------1. feladat

# a sorozat indexel�se 0-r�l indul
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

#--------a r�sz
u = runif(100) #100 sz�mot gener�l
p1 = 0
p2 = 1
random_cauchy = qcauchy(u, location=p1, scale=p2)

#---------c r�sz
filtered = random_cauchy[random_cauchy>-10 & random_cauchy<10]
hist(filtered, main="gener�lt cauchy eloszl�ss", freq=F) #a piros a t�nyleges
points(seq(-10,10,0.1), dcauchy(seq(-10,10,0.1)), type="l", col="red")

#---------b r�sz

log_lik = function(y, params) {
    l = sum(dcauchy(y, location=params[1], scale=params[2], log=TRUE))
    return (-l)
}
opt = nlm(f=log_lik, p=c(0,1), y=random_cauchy)
opt$estimate  # a becs�lt �rt�kek, az 1. a p1, a 2. a p2



#---------------------------------------------3. feladat

x = c(21,25,29,31,33,35,42,45,51,53)
y = c(175,182,190,206,242,265,280,356,380,450)

# line�ris modell vizsg�lata
model_lin = lm(y~x); summary(model_lin)
# k�vetkezm�ny: b0-ra nincs sz�ks�g, mert 0 csillagot kapott
model_lin2 = lm(y~-1+x); summary(model_lin2)

#exponenci�lis modell vizsg�lata
model_exp = lm(y~-1+x+I(exp(x)));summary(model_exp)
# k�vetkezm�ny: e^x csak egy csillagot kapott, ez�rt nem sokat m�dos�t

# a modellben az x �s y k�z�tti (�letkor �s havi j�vedelem) kapcsolat
# er�ss�g�t az R-squared �rt�k jel�li, min�l k�zelebb �ll az 1-hez, ann�l jobb
# -> az exponenci�lisban az R-squared �rt�ke jobb

#�br�zol�s
xline = seq(15,60,0.1)
par(mfrow=c(1,2))

plot(x,y, main="a line�ris modell")
points(xline, predict(model_lin,newdata=data.frame(x=xline)), type="l", col="red")

plot(x,y, main="az exponenci�lis modell")
points(xline, predict(model_exp,newdata=data.frame(x=xline)), type="l", col="red")

# a hib�k n�gyzet�sszege
ex_hiba = sum((y-predict(model_exp))^2); ex_hiba;
lin_hiba = sum((y-predict(model_lin))^2); lin_hiba;
# a 2 modell k�z�l az exponenci�lis jobban illeszkedik, 
# mert kisebb a hib�ja, �s az R-square �rt�ke is k�zelebb �ll az 1-hez
# �s a p-value is kisebb

# 30 �vre becsl�s exponenci�lissal:
x30 = predict(model_exp,newdata=data.frame(x=30), level=0.99)
#az �tlagos j�vedelem:
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
#a legjobb s�lyok 0.20 hozamhoz
dat$weights
# a s�lyok �sszege t�nyleg 1 (kb a kerek�t�s miatt)
sum(dat$weights)
# a s�lyokhoz tartoz� kock�zat
risk = sqrt(dat$weights%*%diag(szorasok)%*%dat$weight); risk

#�br�zol�s
par(mfrow=c(1,1))
labels = c("A", "B", "C", "D", "E")
pie(dat$weights, labels, main="A befektet�sek megoszl�sa")

