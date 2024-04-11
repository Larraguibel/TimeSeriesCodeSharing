ccfplot <- function(fit, data){
  if(class(fit)[1] == "lm"){
    res <- rep(NA,nrow(data))
    res[as.integer(names(fit$residuals))] <- fit$residuals
  }else if(class(fit)[1] == "marima"){
    res <- fit$residuals[1, ]
  }
  par(mfrow=c(1,2), mar=c(3,3,3,1), mgp=c(2, 0.7,0))
  (i <- as.integer(names(fit$residuals)))
  ccf(fit$residuals[i], X$Pinner[i], na.action=na.pass, main="")
  title(main="CCF(residuals,Pinner)", line=0.2)
  ccf(fit$residuals[i], X$Touter[i], na.action=na.pass, main="")
  title(main="CCF(residuals,Touter)", line=0.2)
}
