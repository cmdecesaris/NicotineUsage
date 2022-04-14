library(tidyverse)
library(MASS)
library(nnet)
library(lmtest)
library(AER)
source("preprocessing.R")
head(drug)

dim(drug)

drug= drop_na(drug)


drug2 = drug %>%mutate(Nicotine_nu = case_when(NicotineL=='Never Used'~1,
                                               TRUE ~ 0))

drug2 = drug2 %>%mutate(Nicotine_pu = case_when(NicotineL=='Past User'~1,
                                                TRUE ~ 0))

drug2 = drug2 %>%mutate(Nicotine_ru = case_when(NicotineL=='Recent User'~1,
                                                TRUE ~ 0))




drug2=drug2[,-c(14,15)]










empty_bo = multinom(cbind(Nicotine_nu,Nicotine_pu,Nicotine_ru) ~ 1, data=drug2)


drug.bo <- multinom(cbind(Nicotine_nu,Nicotine_pu,Nicotine_ru) ~ ., data=drug2)
summary(drug.bo)
drug.bo_bic = stepAIC(drug.bo, k = log(dim(drug)[1]), trace=F)
summary(drug.bo_bic)


drug.lrempty = glm(cbind(Nicotine_ru, Nicotine_nu)~1, data = drug2, family=binomial("logit"))
drug.2rempty = glm(cbind(Nicotine_pu, Nicotine_nu)~1, data = drug2, family=binomial("logit"))

drug.lr1 = glm(cbind(Nicotine_ru, Nicotine_nu)~ Gender + Education + Oscore + Cscore + Impulsive, data = drug2, family=binomial("logit"))
plot(drug.lr1)

anova(drug.lrempty,drug.lr1,test ="Chi")


drug.lr2 = glm(cbind(Nicotine_pu, Nicotine_nu)~ Gender + Education + Oscore + Cscore + Impulsive, data = drug2, family=binomial("logit"))
plot(drug.lr2)

anova(drug.2rempty,drug.lr2, test="Chi")

anova(drug.bo_bic,empty_bo, test="Chi")

confint(drug.bo_bic, level=0.9)

coeftest(drug.bo_bic)
empty_bo$edf




plot(drug.lr2,which=4,caption = "", main  = "Never User vs Past User")
plot(drug.lr1,which=4,main	
     =  "Never User vs Recent User" ,caption = "")
plot(drug.lr2,which=5,main	
     =  "Never User vs Past User" ,caption = "")
plot(drug.lr1,which=5,main	
     =  "Never User vs Recent User" ,caption = "",cook.legendChanges	
     =NULL)
library(kableExtra)
#809, 1103, 1004
drug[c(809, 1103, 1004),]%>%kable%>% kable_classic(full_width =F,lightable_options   =c("striped","bordered") )%>% 
  column_spec (1:16,
               border_left = T, 
               border_right = T)

#drug3=drug2[-c(809, 1103, 1004),]





#outlier removed

drug.bo_bicE = multinom(formula = cbind(Nicotine_nu, Nicotine_pu, Nicotine_ru) ~ 
                          1, data = drug3)

drug.bo_bic2 = multinom(formula = cbind(Nicotine_nu, Nicotine_pu, Nicotine_ru) ~ 
                          Gender + Education + Oscore + Cscore + Impulsive, data = drug3)
summary(drug.bo_bic2)
coeftest(drug.bo_bic2)
coeftest(drug.bo_bic)





#submodel leverage and cooks


leverage = hatvalues(drug.lr1)

p <- length(coef(drug.lr1))
n <- nrow(drug2)
plot(names(leverage), leverage, xlab="Index", type="h")
points(names(leverage), leverage, pch=16, cex=0.6)
infPts <- which(leverage>2*p/n)
susPts <- as.numeric(names(sort(cooks[infPts], decreasing=TRUE)[1:3]))
text(susPts, leverage[susPts], susPts, adj=1, cex=0.7, col=4)


abline(h=2*p/n,col=2,lwd=2,lty=2)


# ** Cook's Distance ----------------

# high Cook's distance => influential points/outliers
# leverage points with high Cook's distance => suspicious influential points & outliers
#                    may need to be deleted -> check scatterplots

cooks = cooks.distance(drug.lr1)

plot(cooks, ylab="Cook's Distance", pch=16, cex=0.6)

susPts <- as.numeric(names(sort(cooks[infPts], decreasing=TRUE)[1:3]))
text(susPts, cooks[susPts], susPts, adj=2, cex=0.7, col=4)
susPts






cats= c("Gender" , "Education")
generalhoslem::lipsitz.test(drug.po_bic)
generalhoslem::pulkrob.chisq(drug.po_bic, catvars = cats)

generalhoslem::pulkrob.deviance(drug.po_bic, catvars = cats)#fails at significane level 0.01





prd_prob_bo = predict(drug.bo_bic, type = 'prob')
prd_prob_bo = fitted(drug.bo_bic)
head(prd_prob_bo)
prd_labl_bo = predict(drug.bo_bic)
head(prd_labl_bo)

prd_prob_bo2 <- fitted(drug.bo_bic)


obslabel <- t(apply(drug2[,14:16], 1, function(x) {
  res <- numeric(3)
  res[which.max(x)] <- 1
  res
}))




resP.bo <- sapply(2:ncol(obslabel), function(m) {
  # baseline is column 1 here 
  # otherwise you should replace "1" with the corresponding index and adjust the range of "m" accordingly
  obs_m <- obslabel[rowSums(obslabel[,c(1,m)]) > 0, m]
  fit_m <- prd_prob_bo2[rowSums(obslabel[,c(1,m)]) > 0,c(1,m)]
  fit_m <- fit_m[,2] / rowSums(fit_m)
  (obs_m - fit_m) / sqrt(fit_m * (1 - fit_m))
})


# m= 3 says, set u

par(mfrow=c(1,2))
m=2
fit_m <- prd_prob_bo2[rowSums(obslabel[,c(1,m)]) > 0,c(1,m)]
fit_m <- fit_m[,2] / rowSums(fit_m)

plot(fit_m,resP.bo[[1]], main = "Never Used vs Past User",ylab='Deviance Residuals', xlab='Fitted Values')
lines(smooth.spline(fit_m,resP.bo[[1]], spar=1.3), col=2,lwd =1.5)
abline(h=0, lty=2, col='grey')

m=3
fit_m <- prd_prob_bo2[rowSums(obslabel[,c(1,m)]) > 0,c(1,m)]
fit_m <- fit_m[,2] / rowSums(fit_m)

plot(fit_m,resP.bo[[2]], main = "Never Used vs Recent User",ylab='Deviance Residuals', xlab='Fitted Values')
lines(smooth.spline(fit_m,resP.bo[[2]], spar=1.3), col=2,lwd = 1.5)
abline(h=0, lty=2, col='grey')


boxplot(resP.bo[[2]])
boxplot(resP.bo[[1]])



drug%>%ggplot(aes(y=NicotineL))+
  geom_bar(stat="identity")+
  coord_polar()







drug = drug[-c(14)]
drug$NicotineL = as.factor(drug$NicotineL)

drug.po <- polr(NicotineL ~ .,Hess =T , data=drug)
summary(drug.po)
drug.po_bic = stepAIC(drug.po, k = log(dim(drug)[1]), trace=F)
summary(drug.po_bic, digits=4)

empty.po <- polr(NicotineL ~ 1,Hess =T , data=drug)

anova(empty.po,drug.po_bic, test="Chi")
#baseline odds
coeftest(drug.po_bic)

cbind((drug.po_bic$coefficients), confint(drug.po_bic,level=0.9))





prd_prob_po = predict(drug.po_bic, type='prob')
# equivalently
prd_prob_po = fitted(drug.po_bic)
# vector of predicted labels:
prd_labl_po = predict(drug.po_bic)

head(data.frame(prd_labl_po, prd_prob_po))

obslabel <- t(apply(drug2[,14:16], 1, function(x) {
  res <- numeric(3)
  res[which.max(x)] <- 1
  res
}))

resP.plr <- sapply(1:(ncol(obslabel)-1), function(m) {
  obs_m <- rowSums(as.matrix(obslabel[,1:m]))
  fit_m <- rowSums(as.matrix(prd_prob_po[seq_len(nrow(drug2)),1:m]))
  (obs_m - fit_m) / sqrt(fit_m * (1 - fit_m))
})


#health_m_aug <- inner_join(nhanes_adult, pred_probs) #add probs
#health_m_aug <- inner_join(health_m_aug, residuals) #add resid

residuals
pred_probs
plot(pred_probs$Nicotine_nu,residuals$resid.Nicotine_nu)

plot(pred_probs$Nicotine_nu,residuals$resid.Nicotine_nu)


plot(pred_probs$Nicotine_pu,residuals$resid.Nicotine_pu)

plot(pred_probs$Nicotine_ru,residuals$resid.Nicotine_ru)

lines(smooth.spline(pred_probs$Nicotine_ru,residuals$resid.Nicotine_ru, spar=0.9), col=2)


plot(drug.bo_bic$fitted.values[,3],residuals$resid.Nicotine_ru)

lines(smooth.spline(drug.bo_bic$fitted.values[,3],residuals$resid.Nicotine_ru, spar=0.9), col=2)




