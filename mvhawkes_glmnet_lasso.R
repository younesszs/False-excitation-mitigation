library(glmnet)
print(getwd())
#Substitute PATH by the corresponding path directory
setwd("PATH")
Nsamp=as.matrix(read.csv("PATH/Nsamp.csv",header=F))
X=as.matrix(read.csv("PATH/X.csv",header=F))
y=as.vector(read.csv("PATH/y.csv",header=F)$V1)
pfac=as.vector(read.csv("PATH/pfac.csv",header=F)$V1)

M=(-1+(1+4*length(pfac))^.5)/2

plim=rep(0,length(pfac))
pfit <- glmnet(X, y,penalty.factor = pfac,positive=TRUE,
               lower.limits=plim,alpha=.2,intercept=FALSE)
coeffs <- coef(pfit, s = 10)[2:(M*(M+1)+1)]

t(matrix(coeffs[(M+1):(M*(M+1))],M))

cv <- cv.glmnet(X,
                y,
                nfolds = 5,
                penalty.factor = pfac,positive=TRUE,
                lower.limits=plim,alpha=.2,intercept=FALSE)

glmnet.coef <- as.matrix(coef(cv,s ="lambda.min"))

coeffs_cv <- glmnet.coef[2:(M*(M+1)+1)]

t(matrix(coeffs_cv[(M+1):(M*(M+1))],M))

write.csv(coeffs_cv,"PATH/glm_coefs.csv",row.names=F)

