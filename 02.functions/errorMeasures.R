# precisa ser série temporal ou estar no mesmo tamanho

MAPE <- function(y,x){ # MEAN ABSOLUT PERCENTUAL ERROR
  data <- na.omit(cbind(y,x))
  mean(abs((data[,1] - data[,2])/data[,1]))*100
}

MAE <- function(y,x){ # MEAN ABSOLUT ERROR
  data <- na.omit(cbind(y,x))
  mean(abs(data[,1] - data[,2]))
}

sMAPE <- function(y,yhat){ # MEAN ABSOLUT PERCENTUAL ERROR (usar quando tem 0)
  data <- na.omit(data.frame(cbind(y,yhat)))
  num <- sum(abs(data[,1] - data[,2]))
  dem <- sum(abs(data[,1] + data[,2])[-1])
  (1*nrow(data))*num/dem
}

RMSE <- function(y,yhat){ # ROOT MEAN SQUARE ERROR
  data <- na.omit(data.frame(cbind(y,yhat)))
  sqrt(mean((data[,1] - data[,2])^2))
}
