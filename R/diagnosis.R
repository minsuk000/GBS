diagnosis <- function(alpha_cand, theta_alpha){
  cond1 = length(intersect(ls(envir=.GlobalEnv),"alpha_cand"))>0
  cond2 = length(intersect(ls(envir=.GlobalEnv),"theta_alpha"))>0
  code_diagnosis <- paste(system.file(package="GBS"), "diagnosis_Sampling.py", sep="/")

  if(cond1 | cond2){print("alpha_cand and theta_alpha are imported to the global environment!")}
  reticulate::source_python(code_diagnosis, envir = NULL,convert = FALSE)
  Theta_diag = py$Theta_diag
  l2 = (Theta_diag - theta_alpha)^2
  l2 = apply(l2,1,sum)
  l2 = sqrt(l2)
  l1 = abs(Theta_diag - theta_alpha)
  l1 = apply(l1,1,sum)
  return(list(alpha_cand = alpha_cand, theta_alpha = theta_alpha, G_alpha = Theta_diag, L2 = l2, L1 = l1))
}





