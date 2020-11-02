#requireNamespace(reticulate)
post_process <- function(theta, alpha = 0.95, level = 3, B1=1000, B2=500, B3=50, B10=10){
  require(reticulate)
  a0 = (1-alpha)/2
  cond_B1 = length(intersect(ls(envir=.GlobalEnv),"B1"))>0
  cond_B2 = length(intersect(ls(envir=.GlobalEnv),"B2"))>0
  cond_B3 = length(intersect(ls(envir=.GlobalEnv),"B3"))>0
  cond_B10 = length(intersect(ls(envir=.GlobalEnv),"B10"))>0
  cond_level = length(intersect(ls(envir=.GlobalEnv),"level"))>0
  #level = 3
  if(cond_B1 | cond_B2 | cond_B3 | cond_B10){
    print("B1, B2, B3, B10, and level are imported to the global environment!")
  }
  assign("B1", B1, envir = .GlobalEnv)
  assign("B2", B2, envir = .GlobalEnv)
  assign("B3", B3, envir = .GlobalEnv)
  assign("B10", B10, envir = .GlobalEnv)
  assign("level", level, envir = .GlobalEnv)

  T_s = matrix(0, B1, p)
  T0_s = matrix(0, B1, p)
  d = matrix(0, B1, p)
  d0 = matrix(0, B1, p)
  CI1 = CI2 = CI3 = matrix(0,p,2)
  CI_perc = matrix(0,p,2)
  pmt =proc.time()[3]
  code_Sampling <- paste(system.file(package="GBS"), "GBS_Triple_Sampling.py", sep="/")
  code_single = paste(system.file(package="GBS"), "GBS_Single_Sampling.py", sep="/")
  #if(level == 3){code_Sampling <- paste(system.file(package="GBS"), "GBS_Triple_Sampling.py", sep="/")}
  #if(level == 2){code_Sampling <- paste(system.file(package="GBS"), "GBS_Double_Sampling.py", sep="/")}
  #if(level == 1){code_Sampling <- paste(system.file(package="GBS"), "GBS_Single_Sampling.py", sep="/")}
  #total = B1*B2*B3
  #if(level == 3) total = B1*B2*B3
  #if(level == 2) total = B1*B2
  #if(level == 1) total = B1
  print("Generation Done!")
  print("Post Process Evaluation Starts!")
  cat(paste("Confidence Level: ", alpha,sep=""), fill=T)
  p0 = 1
  A1 = B1/B10
  tr.mean = function(x) mean(x, trim = 0.1)
  reticulate::source_python(code_single, envir = NULL,convert = FALSE)
  Theta1 = py$Theta1
  sd1 = apply(Theta1,2,sd)
  MEAN1 = apply(Theta1,2, mean)
  theta_bc1 = 2*theta - MEAN1
  pmt = proc.time()[3]
  MEAN2 = rep(0,p)
  MEAN3 = rep(0,p)
  for(h in 1:A1){
    reticulate::source_python(code_Sampling, envir = NULL,convert = FALSE)
    #reticulate::source_python('~/Dropbox/Paper work/GBS/GBS-Double/R_package/GBS/inst/GBS_Triple_Sampling.py')
    #time_samp = proc.time()[3]-pmt
    Theta1 = py$Theta1
    Theta2 = py$Theta2
    Theta3 = py$Theta3
    mean_s = apply(Theta1, 2, mean)
    if(level>1){
      mean_ss = apply(Theta2, 3, mean)
    }else{
      mean_ss = 0
    }
    if(level>2){
      mean_sss = apply(Theta3, 4, mean)
    }else{
      mean_sss = 0
    }
    MEAN2 = MEAN2 + mean_ss/A1
    MEAN3 = MEAN3 + mean_sss/A1
    i0 = (h-1)*B10
    for(i in 1:B10){
      #if(i %% round(B1/10) == 0){
      #  cat(paste(p0*10/A1,"%"," | ",sep=""));
      #  p0 = p0 + 1
      #}
      out = GBS::calib(Theta1[i,], Theta2[,i,], Theta3[,,i,], theta, n, p, B1, B2, B3, B1, B2, B3)
      T_s[i0+i,] = out$T_s
      T0_s[i0+i,] = out$T0_s
      d[i0+i,] = out$d
      d0[i0+i,] = out$d0
    }
    if( h %% round(A1/10) == 0 ){
      cat(paste(100*h/A1,"%"," | ",sep=""))
    }
  }

  for(j in 1:p){
    q_L = quantile(d[,j],a0)
    q_U = quantile(d[,j],1-a0)
    if(q_L<0.0001) q_L=0.0001
    if(q_U<0.0001) q_U=0.0001
    if(q_L>0.9999) q_L=1-0.0001
    if(q_U>0.9999) q_U=1-0.0001
    CI2[j,1] = theta[j] - quantile(T_s[,j],q_U)#*SD[j]
    CI2[j,2] = theta[j] - quantile(T_s[,j],q_L)#*SD[j]
    q_L = quantile(d0[,j],a0)
    q_U = quantile(d0[,j],1-a0)
    if(q_L<0.0001) q_L=0.0001
    if(q_U<0.0001) q_U=0.0001
    if(q_L>0.9999) q_L=1-0.0001
    if(q_U>0.9999) q_U=1-0.0001
    CI3[j,1] = theta[j]  - quantile(T0_s[,j],q_U)*sd1[j]
    CI3[j,2] = theta[j]  - quantile(T0_s[,j],q_L)*sd1[j]
    CI1[j,1] = 2*theta[j] - quantile(Theta1[,j],1-a0)
    CI1[j,2] = 2*theta[j] - quantile(Theta1[,j],a0)
    CI_perc[j,1] = quantile(Theta1[,j], a0)
    CI_perc[j,2] = quantile(Theta1[,j], 1-a0)
  }

  ################### bias-correction
  #LL = 200
  #print("###################")
  #print("Bias Calculation...")
  #for(k in 1:LL){
  #  reticulate::source_python(code_Sampling, envir = NULL,convert = FALSE)
  #  #reticulate::source_python('~/Dropbox/Paper work/GBS/GBS-Double/R_package/GBS/inst/GBS_Triple_Sampling.py')
  #  Theta10 = py$Theta1
  #  Theta2 = py$Theta2
  #  Theta3 = py$Theta3
  #  mean_s = apply(Theta10, 2, mean)
  #  if(level>1){
  #    mean_ss = apply(Theta2, 3, mean)
  #  }else{
  #    mean_ss = 0
  #  }
  #  if(level>2){
  #    mean_sss = apply(Theta3, 4, mean)
  #  }else{
  #    mean_sss = 0
  #  }
  #  MEAN2 = MEAN2 + mean_ss/LL
  #  MEAN3 = MEAN3 + mean_sss/LL
  #  if( k %% round(LL/10) == 0 ){
  #    cat(paste(100*k/LL,"%"," | ",sep=""))
  #  }
    #theta_bc2 = theta_bc2 + (3*theta - 3*mean_s + mean_ss)/10
    #theta_bc3 = theta_bc3 + (4*theta - 6*mean_s + 4*mean_ss - mean_sss)/10
  #}
  #print(MEAN2)
  #print(MEAN3)
  theta_bc2 = 3*theta - 3*MEAN1 + MEAN2
  theta_bc3 = 4*theta - 6*MEAN1 + 4*MEAN2 - MEAN3
  reticulate::source_python(code_single, envir = NULL,convert = FALSE)
  Theta1 = py$Theta1
  time_post = proc.time()[3] - pmt
  cat("Done!", fill=T)
  total = round(B1*B2*B3)
  print("######################################################")
  print(paste("The first level: ", B1," bootstrap samples are generated.",sep=""))
  print(paste("The second level: ", B2," bootstrap samples are generated.",sep=""))
  print(paste("The third level: ", B3," bootstrap samples are generated.",sep=""))
  print("------------------------------------------------------")
  print(paste("Total: ", total," bootstrap samples are generated.",sep=""))

  return(list(Theta1 = Theta1, CI1 = CI1, CI2 = CI2, CI3 = CI3, CI_perc = CI_perc,
              T_s=T_s, T0_s=T0_s, d=d, d0=d0, mean1 = MEAN1, mean2 = MEAN2, mean3 = MEAN3, theta_bc1=theta_bc1,
              theta_bc2=theta_bc2, theta_bc3=theta_bc3, alpha=alpha, time_post = time_post))
}





