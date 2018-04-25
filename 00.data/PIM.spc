	#C:\Users\vinicius.botelho\Desktop\Dessaz\PIM\PIM_BR.spc was created on 16/09/2013 10:22:50
#Created using X-12 

series{ 
    file= 'PIM.txt'
    format= 'Datevalue'
    period= 12
    span= (2002.1,)
    savelog= peaks
    decimals=2
} 
transform{ function= log} 
regression{ 
    variables= ( td easter[1] )
    file= 'Carnaval1.txt'
    format= Datevalue
    user= (user1 )
    usertype= (holiday )
} 

outlier{ 
    types=all
} 

arima{ model= (0 1 1)(0 2 2)} 
forecast{ 
    maxlead= 12
    print= none
} 
estimate{
    print= (roots regcmatrix acm)
    savelog= (aicc aic bic hq afc)
} 
check{print= all savelog= (lbq nrm)} 
x11{ 
    seasonalma= MSR
    savelog= all
    save=d11
} 
slidingspans{ 
    savelog= percent
    additivesa= percent
} 
history{ 
    estimates= (fcst aic sadj sadjchng trend trendchng)
    savelog= (asa ach atr atc)
} 
