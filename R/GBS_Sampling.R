#requireNamespace(reticulate)
GBS_Sampling <- function(B1=1000,B2=500,B3=50, B10=50, level = 3, S=100){
  #require(reticulate)
  cond_B1 = length(intersect(ls(envir=.GlobalEnv),"B1"))>0
  cond_B2 = length(intersect(ls(envir=.GlobalEnv),"B2"))>0
  cond_B3 = length(intersect(ls(envir=.GlobalEnv),"B3"))>0
  cond_B10 = length(intersect(ls(envir=.GlobalEnv),"B10"))>0
  cond_S = length(intersect(ls(envir=.GlobalEnv),"S"))>0
  if(cond_B1 | cond_B2 | cond_B3 | cond_B10 | cond_S
  ){print("B1, B2, B3, B10, and S are imported to the global environment!")}
  assign("B1", B1, envir = .GlobalEnv)
  assign("B2", B2, envir = .GlobalEnv)
  assign("B3", B3, envir = .GlobalEnv)
  assign("B10", B10, envir = .GlobalEnv)
  assign("S", S, envir = .GlobalEnv)
  have_torch <- reticulate::py_module_available("torch")
  have_random <- reticulate::py_module_available("random")
  have_numpy <- reticulate::py_module_available("numpy")
  if (!have_torch)
    print("Pytorch is not installed!")
  if (!have_random)
    print("random is not installed!")
  if (!have_numpy)
    print("numpy is not installed!")
  #code_Logit <- paste(system.file(package="GBS"), "GBS_Triple_Sampling.py", sep="/")
  if(level == 3){code_Sampling <- paste(system.file(package="GBS"), "GBS_Triple_Sampling.py", sep="/")}
  if(level == 2){code_Sampling <- paste(system.file(package="GBS"), "GBS_Double_Sampling.py", sep="/")}
  if(level == 1){code_Sampling <- paste(system.file(package="GBS"), "GBS_Single_Sampling.py", sep="/")}
  #pmt =proc.time()[3]
  #reticulate::source_python(code_Sampling, envir = NULL,convert = FALSE)
  #reticulate::py_run_file(code_Logit, local = T, convert = F)
  #time_tr = proc.time()[3]-pmt
  #print("Training Done!")
  Theta1_GBS = matrix(0,B1,p)
  Theta2_GBS = array(0,dim=c(B2,B1,p))
  print("Generation Starts!")
  A1 = B1/B10
  pmt =proc.time()[3]
  if(level == 3){
    Theta3_GBS = array(0,dim=c(B3,B2,B1,p))
    for(h in 1:A1){
      reticulate::source_python(code_Sampling, envir = NULL,convert = FALSE)
      ind = ((h-1)*B10+1):(h*B10)
      Theta1 = py$Theta1
      Theta2 = py$Theta2
      Theta3 = py$Theta3
      Theta1_GBS[ind,] = Theta1
      Theta2_GBS[,ind,] = Theta2
      Theta3_GBS[,,ind,] = Theta3
      if( h %% round(A1/10) == 0 ){
        cat(paste(100*h/A1,"%"," | ",sep=""))
      }
    }
  }
  if(level == 2){
    reticulate::source_python(code_Sampling, envir = NULL,convert = FALSE)
    Theta1_GBS = py$Theta1
    Theta2_GBS = py$Theta2
    Theta3_GBS = 0
  }
  if(level == 1){
    reticulate::source_python(code_Sampling, envir = NULL,convert = FALSE)
    Theta1_GBS = py$Theta1
    Theta2_GBS = 0
    Theta3_GBS = 0
  }
  time_samp = proc.time()[3] - pmt
  hidden_size = py$hidden_size
  L = py$L
  total = dim(Theta1_GBS)[1]*dim(Theta2_GBS)[1]*dim(Theta3_GBS)[1]
  print("Generation Done!")
  print("######################################################")
  print(paste("The first level: ", dim(Theta1_GBS)[1]," bootstrap samples are generated.",sep=""))
  print(paste("The second level: ", dim(Theta2_GBS)[1]," bootstrap samples are generated.",sep=""))
  print(paste("The third level: ", dim(Theta3_GBS)[1]," bootstrap samples are generated.",sep=""))
  print("------------------------------------------------------")
  print(paste("Total: ", total," bootstrap samples are generated.",sep=""))
  print("######################################################")
  #print(paste("Time for training: ",round(time_tr,2), " seconds"))
  print(paste("Time for generation: ",round(time_samp,2), " seconds"))
  print("######################################################")
  return(list( Theta1 = Theta1_GBS, Theta2 = Theta2_GBS, Theta3 = Theta3_GBS,
                time_samp = time_samp, level = level, S = S, hidden_size = hidden_size, L = L))
}
