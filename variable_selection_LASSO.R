###########################################
# Variable selection using LASSO
###########################################


fit_lasso <- glmnet(x=as.matrix(dataset[,-1]), 
                    y=factor(dataset$diagnosis),
                    family="binomial",
                    standardize=TRUE,
                    alpha=1)
plot(fit_lasso,xvar="lambda",label=TRUE)

# Variable selection through cross validation
cv_lasso <- cv.glmnet(x=as.matrix(dataset[,-1]), 
                      y=factor(dataset$diagnosis),
                      family="binomial",
                      standardize=TRUE,
                      alpha=1,
                      nfolds=15,
                      type.measure="class",
                      parallel= TRUE)

plot(cv_lasso)
coef<-coef(cvfit,s='lambda.min',exact=TRUE)
index<-which(coef!=0)
optimum_subset<-row.names(coef)[index]
optimum_subset<-c(optimum_subset[-1])
dataset_lasso <- dataset[,c("diagnosis",optimum_subset)]