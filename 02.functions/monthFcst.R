last <- tryCatch(readRDS("00.data/monthFcst.rds"), error = function(e) NULL)
new <- readRDS("01.models/05.combination/comb_model.rds")$OUT
new <- data.frame(data = Sys.Date(), new, ref = tail(as.Date(pim) + months(1),1))

if(new$data %in% last$data){
  last[which(new$data == last$data),] <- new
  juntar <- unique(last)
}else{
  juntar <- unique(rbind(last,new))
}
rownames(juntar) <- NULL
saveRDS(juntar, "00.data/monthFcst.rds")
