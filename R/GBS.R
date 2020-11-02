#requireNamespace(reticulate)
GBS <- function(X, y, model, S=100, M=100, V=20, lr_U=0.001,
                      lr_L = 0.0001, lr_power = 0.3, num_it = 5000, hidden_size = 300,
                      L = 3, gpu_ind = 0){
  require(reticulate)
  n = nrow(X)
  p = ncol(X)
  #cond_B1 = length(intersect(ls(envir=.GlobalEnv),"B1"))>0
  #cond_B2 = length(intersect(ls(envir=.GlobalEnv),"B2"))>0
  #cond_B3 = length(intersect(ls(envir=.GlobalEnv),"B3"))>0
  cond_S = length(intersect(ls(envir=.GlobalEnv),"S"))>0
  cond_M = length(intersect(ls(envir=.GlobalEnv),"M"))>0
  cond_V = length(intersect(ls(envir=.GlobalEnv),"V"))>0
  cond_lr_U = length(intersect(ls(envir=.GlobalEnv),"lr_U"))>0
  cond_lr_L = length(intersect(ls(envir=.GlobalEnv),"lr_L"))>0
  cond_lr_power = length(intersect(ls(envir=.GlobalEnv),"lr_power"))>0
  cond_num_it = length(intersect(ls(envir=.GlobalEnv),"num_it"))>0
  cond_hidden_size = length(intersect(ls(envir=.GlobalEnv),"hidden_size"))>0
  cond_L = length(intersect(ls(envir=.GlobalEnv),"L"))>0
  #cond_fac = length(intersect(ls(envir=.GlobalEnv),"fac"))>0
  cond_gpu_ind = length(intersect(ls(envir=.GlobalEnv),"gpu_ind"))>0
  #if(cond_B1 | cond_B2 | cond_B3 | cond_S | cond_M | cond_V | cond_lr_U | cond_lr_L |
  #  cond_lr_power | cond_num_it | cond_hidden_size | cond_L | cond_fac | cond_gpu_ind
  #){print("B1, B2, B3, lr_U, lr_L, S, M, V, lr_power, num_it, hidden_size, L, fac, gpu_ind are imported to the global environment!")}
  if(cond_S | cond_M | cond_V | cond_lr_U | cond_lr_L |
     cond_lr_power | cond_num_it | cond_hidden_size | cond_L | cond_gpu_ind
  ){print("lr_U, lr_L, S, M, V, lr_power, num_it, hidden_size, L, and gpu_ind are imported to the global environment!")}

  #switch(menu(c("Import the input parameters to the global environment.", "Do not import them.")) + 1,
  #       cat("Nothing done\n"), {assign("B1", B1, envir = .GlobalEnv)
  #assign("B1", B1, envir = .GlobalEnv)
  #assign("B2", B2, envir = .GlobalEnv)
  #assign("B3", B3, envir = .GlobalEnv)
  assign("lr_U", lr_U, envir = .GlobalEnv)
  assign("lr_L", lr_L, envir = .GlobalEnv)
  assign("S", S, envir = .GlobalEnv)
  assign("M", M, envir = .GlobalEnv)
  assign("V", V, envir = .GlobalEnv)
  assign("lr_power", lr_power, envir = .GlobalEnv)
  assign("num_it", num_it, envir = .GlobalEnv)
  assign("hidden_size", hidden_size, envir = .GlobalEnv)
  assign("L", L, envir = .GlobalEnv)
  #assign("fac", fac, envir = .GlobalEnv)
  assign("gpu_ind", gpu_ind, envir = .GlobalEnv)
  have_torch <- reticulate::py_module_available("torch")
  have_random <- reticulate::py_module_available("random")
  have_numpy <- reticulate::py_module_available("numpy")
  if (!have_torch)
    print("Pytorch is not installed!")
  if (!have_random)
    print("random is not installed!")
  if (!have_numpy)
    print("numpy is not installed!")
  if(model == "linear"){code_train <- paste(system.file(package="GBS"), "GBS_Triple_Linear.py", sep="/")}
  if(model == "logistic"){code_train <- paste(system.file(package="GBS"), "GBS_Triple_Logit.py", sep="/")}
  if(model == "LAD"){code_train <- paste(system.file(package="GBS"), "GBS_Triple_LAD.py", sep="/")}
  if(model == "cox"){code_train <- paste(system.file(package="GBS"), "GBS_Triple_cox.py", sep="/")}
  #if(level == 3){code_Sampling <- paste(system.file(package="GBS"), "GBS_Triple_Sampling.py", sep="/")}
  #if(level == 2){code_Sampling <- paste(system.file(package="GBS"), "GBS_Double_Sampling.py", sep="/")}
  #if(level == 1){code_Sampling <- paste(system.file(package="GBS"), "GBS_Single_Sampling.py", sep="/")}
  pmt =proc.time()[3]
  reticulate::source_python(code_train, envir = NULL,convert = FALSE)
  #reticulate::py_run_file(code_Logit, local = T, convert = F)
  time_tr = proc.time()[3]-pmt
  print("Training Done!")
  #print("Generation Starts!")
  #pmt =proc.time()[3]
  #reticulate::source_python(code_Sampling, envir = NULL,convert = FALSE)
  #time_samp = proc.time()[3]-pmt
  #Theta1_GBS = py$Theta1
  #Theta2_GBS = py$Theta2
  #Theta3_GBS = py$Theta3

  #total = dim(Theta1_GBS)[1]*dim(Theta2_GBS)[1]*dim(Theta3_GBS)[1]
  #if(Theta3_GBS[1]==0) total = dim(Theta1_GBS)[1]*dim(Theta2_GBS)[1]
  #if(Theta2_GBS[1]==0) total = dim(Theta1_GBS)[1]
  #print("Generation Done!")
  #print("######################################################")
  #print(paste("The first level: ", dim(Theta1_GBS)[1]," bootstrap samples are generated.",sep=""))
  #print(paste("The second level: ", dim(Theta2_GBS)[1]," bootstrap samples are generated.",sep=""))
  #print(paste("The third level: ", dim(Theta3_GBS)[1]," bootstrap samples are generated.",sep=""))
  #print("------------------------------------------------------")
  #print(paste("Total: ", total," bootstrap samples are generated.",sep=""))
  print("######################################################")
  print(paste("Time for training of G: ",round(time_tr,2), " seconds"))
  #print(paste("Time for generation: ",round(time_samp,2), " seconds"))
  print("######################################################")
  return( list(time_tr = time_tr, S = S, V = V, L = L) )
}
