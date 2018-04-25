  library("tsDyn")
library("vars")
data(Canada)
data<-data.frame(Canada)
#Fit a VAR
exogen = data[,4]
exogen<-colnames("name")
#VAR<-lineVar(data[,1:3], lag=1, exogen = exogen)

VAR<-VAR(data[,1:3], exogen=cbind(bla=data[,4]))
summary(VAR)
#compare results with package vars:
if(require(vars)) {
  a<-VAR(data, p=1)
  coef_vars <- t(sapply(coef(a), function(x) x[c(3,1,2),1]))
  all.equal(coef(VAR),coef_vars, check.attributes=FALSE)
}
###VECM
VECM.EG<-lineVar(data, lag=2, model="VECM")
summary(VECM.EG)
VECM.ML<-lineVar(data, lag=2, model="VECM", estim="ML")
VECM.ML
summary(VECM.ML)
###Check Johansen MLE
myVECM<-lineVar(data, lag=1, include="const", model="VECM", estim="ML")
summary(myVECM, digits=7)
#comparing with vars package
if(require(vars)){
  a<-ca.jo(data, spec="trans")
  summary(a)
  #same answer also!
}
##export to Latex
toLatex(VECM.EG)
toLatex(summary(VECM.EG))
options("show.signif.stars"=FALSE)
toLatex(summary(VECM.EG), parenthese="Pvalue")
options("show.signif.stars"=TRUE)


require(graphics)

f <- function (x, a) (x - a)^2
xmin <- optimize(f, c(0, 1), tol = 0.0001, a = 1/3)
xmin

## See where the function is evaluated:
optimize(function(x) x^2*(print(x)-1), lower = 0, upper = 10)

## "wrong" solution with unlucky interval and piecewise constant f():
f  <- function(x) ifelse(x > -1, ifelse(x < 4, exp(-1/abs(x - 1)), 10), 10)
fp <- function(x) { print(x); f(x) }

plot(f, -2,5, ylim = 0:1, col = 2)
optimize(fp, c(-4, 20))   # doesn't see the minimum
optimize(fp, c(-7, 20))   # ok

f <- function(x,y) 2*x*(y**2)+2*(x**2)*y+x*y
x<- seq(-0.5,0.5, len=200)
y<- seq(-0.5,0.5, len=200)
z <- outer(x,y,f)
persp(x,y,z, theta=-30,phi=15,ticktype="detailed")
fx <- function(x,y,h=0.001) (f(x+h,y)-f(x,y))/h
fy <- function(x,y,h=0.001) (f(x,y+h)-f(x,y))/h
zfx <- outer(x,y,fx)
zfy <- outer(x,y,fy)
contour(x,y,zfx,level=0)
contour(x,y,zfy,level=0, add=T, col=”red”)
x <- seq(-0.2,0,len=400)
y <- seq(-0.2,0,len=400)
z<- outer(x,y,f)
image(x,y,z)
contour(x,y,z,add=T)
fbb<-function(x) f(x[1],x[2])
optim(c(0.5,0.5),fbb,control=list(fnscale=-1)) 
