ger <- data.frame(DATE=as.Date(c("1991-01-01", "1991-04-01", "1991-07-01", "1991-10-01", "1992-01-01" )),
                                     VALUE= c(470780, 468834, 466332, 472949, 480359))

DateSeq <- seq(ger$DATE[1],ger$DATE[length(ger$DATE)],by="1 month")



gerMonthly <- data.frame(DATE=DateSeq, Interp.Value=spline(ger, method="natural", xout=DateSeq)$y)

merge(ger, gerMonthly, by='DATE', all.y = T)


# LUCAS AQUIIIII
library(readxl)
dados <- read_excel("../Cópia de Setor privado FBCF.xlsx")
ger <- na.omit(dados[,c(1,3)])
colnames(ger) <- c("DATE","VALUE")
ger$DATE <- as.Date(ger$DATE, format = "%Y%mm%dd")
data <- c(ger[1,1],ger[nrow(ger),1])
DateSeq <- seq(data[1],data[2],by="1 month")
gerMonthly <- data.frame(DATE=DateSeq, Interp.Value=spline(ger, method="natural", xout=DateSeq)$y)
merge(ger, gerMonthly, by='DATE', all.y = T)
write.csv2(gerMonthly, "interpolação.csv")
# d <- data.frame(DATE = DateSeq)
# d
# 
# for(i in 1:nrow(ger)){
#   d[which(ger[i,1]  == DateSeq),"VALUE"] <- ger[i,2]
# }
# 
# library(zoo)
# d$interpolacao <- na.approx(d[,2])
