### model
N = c(100)
T = list(1,100,sample(5:10,100,replace=TRUE))
J = c(10:2)
P_f = c(10:0)
P_r = c(10:0)
C = c(10:0)

### parm
alpha = c("rnorm(model$P_f)")
s = c("rdirichlet(rep(1,model$C))")
b = c("matrix(rnorm(model$P_r*model$C),nrow=model$P_r,ncol=model$C)")
Omega = c("matrix(rnorm(model$P_r*model$P_r*model$C),nrow=model$P_r*model$P_r,ncol=model$C)")
Sigma = c("matrix(rnorm((model$J-1)*(model$J-1)),nrow=model$J-1,ncol=model$J-1)")

### lcus
do_lcus = c(TRUE,FALSE)
C0 = c(-1:10)
Cmax = c(10:2)
buffer = c(50)
epsmin = c(0.5)
epsmax = c(0.5)
distmin = c(0.1,2)

### init
at_true = c(TRUE,FALSE)
alpha0 = c("alpha","rnorm(model$P_f)")
b0 = c("b","matrix(rnorm(model$P_r*model$C),nrow=model$P_r,ncol=model$C)")
Omega0 = c("Omega","matrix(rnorm(model$P_r*model$P_r*model$C),nrow=model$P_r*model$P_r,ncol=model$C)")
Sigma0 = c("Sigma","matrix(rnorm((model$J-1)*(model$J-1)),nrow=model$J-1,ncol=model$J-1)")
U0 = c("matrix(rnorm((model$J-1)*model$N*max(model$T)),nrow=model$J-1,ncol=model$N*max(model$T))")
beta0 = c("matrix(rnorm(model$P_r*model$N),nrow=model$P_r,ncol=model$N)")
m0 = c("rdirichlet(rnorm(ifelse(lcus$do_lcus,lcus$C0,model$C)))*model$N")

### prior 
eta = c("rnorm(model$P_f)")
Psi = c("matrix(rnorm(model$P_f*model$P_f),nrow=model$P_f,ncol=model$P_f)")
delta = c("rnorm(1)")
xi = c("rnorm(model$P_r)")
D = c("matrix(rnorm(model$P_r*model$P_r),nrow=model$P_r,ncol=model$P_r)")
nu = c("rnorm(1)")
Theta = c("matrix(rnorm(model$P_r*model$P_r),nrow=model$P_r,ncol=model$P_r)")
kappa = c("rnorm(1)")
E = c("matrix(rnorm((model$J-1)*(model$J-1)),nrow=model$J-1,ncol=model$J-1)")

### mcmc
R = c(100,1000)
B = c(50,200)
Q = c(10,300)
nprint = c(0,1,50)

### norm 
parameter = c(1,"'s'","'a'")
index = c(5:1)
value = c(-1,0,2)

### out 
pp = c(TRUE,FALSE)
results = c(TRUE,FALSE)

### function to pick random or default element
pick = function(vector){
  if(deparse(substitute(vector)) %in% c("N","T","J","P_f","P_r","C") || runif(1)<0.9){
    if(length(vector)<=1){
      text = vector
    } else {
      text = sample(vector,1,prob=(1/(length(vector):1))/sum(1/(length(vector):1)))
    }
    out = try(eval(parse(text=text)),silent=TRUE)
    if(any(class(out) == "try-error")) out = NULL
    return(out)
  } else {
    return(NULL)
  }
}

### function to perform one validation run
valid = function(id,rdir){
  if(runif(1)<0.9){
    model = list("N" = pick(N),
                 "T" = pick(T),
                 "J" = pick(J),
                 "P_f" = pick(P_f),
                 "P_r" = pick(P_r),
                 "C" = pick(C))
  } else {
    model = NULL
  }
  
  if(runif(1)<0.9){
    parm = list("alpha" = pick(alpha),
                "s" = pick(s),
                "b" = pick(b),
                "Omega" = pick(Omega),
                "Sigma" = pick(Sigma))
  } else {
    parm = NULL
  }
  
  if(runif(1)<0.9){
    lcus = list("do_lcus" = pick(do_lcus),
                "C0" = pick(C0),
                "Cmax" = pick(Cmax),
                "buffer" = pick(buffer),
                "epsmin" = pick(epsmin),
                "epsmax" = pick(epsmax),
                "distmin" = pick(distmin))
  } else {
    lcus = NULL
  }
  
  if(runif(1)<0.9){
    init = list("at_true" = pick(at_true),
                "alpha0" = pick(alpha0),
                "b0" = pick(b0),
                "Omega0" = pick(Omega0),
                "Sigma0" = pick(Sigma0),
                "U0" = pick(U0),
                "beta0" = pick(beta0),
                "m0" = pick(m0))
  } else {
    init = NULL
  }
  
  if(runif(1)<0.9){
    prior = list("eta" = pick(eta),
                 "Psi" = pick(Psi),
                 "delta" = pick(delta),
                 "xi" = pick(xi),
                 "D" = pick(D),
                 "nu" = pick(nu),
                 "Theta" = pick(Theta),
                 "kappa" = pick(kappa),
                 "E" = pick(E))
  } else {
    prior = NULL
  }
  
  if(runif(1)<0.9){
    mcmc = list("R" = pick(R),
                "B" = pick(B),
                "Q" = pick(Q),
                "nprint" = pick(nprint))
  } else {
    mcmc = NULL
  }
  
  
  if(runif(1)<0.9){
    norm = list("parameter" = pick(parameter),
                "index" = pick(index),
                "value" = pick(value))
  } else {
    norm = NULL
  }
  
  if(runif(1)<0.9){
    out = list("pp" = pick(pp),
               "results" = pick(results))
  } else {
    out = NULL
  }
  
  out$id = id
  out$rdir = rdir
  
  dir.create(paste0(folder,"/",id,"_par"))
  saveRDS(model,paste0(folder,"/",id,"_par/model.rds"))
  saveRDS(parm,paste0(folder,"/",id,"_par/parm.rds"))
  saveRDS(lcus,paste0(folder,"/",id,"_par/lcus.rds"))
  saveRDS(init,paste0(folder,"/",id,"_par/init.rds"))
  saveRDS(prior,paste0(folder,"/",id,"_par/prior.rds"))
  saveRDS(mcmc,paste0(folder,"/",id,"_par/mcmc.rds"))
  saveRDS(norm,paste0(folder,"/",id,"_par/norm.rds"))
  saveRDS(out,paste0(folder,"/",id,"_par/out.rds"))
  
  rpb("model" = model, 
      "parm" = parm,
      "lcus" = lcus,
      "init" = init,
      "prior" = prior,
      "mcmc" = mcmc, 
      "norm" = norm,
      "out" = out)
}

