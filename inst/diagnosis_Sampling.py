alpha_cand = r.alpha_cand
#theta_alpha = r.theta_alpha
alpha_cand = torch.from_numpy(alpha_cand)
#theta_alpha = torch.from_numpy(theta_alpha)
alpha_cand = alpha_cand.to(device, dtype = torch.float)
#theta_alpha = theta_alpha.to(device, dtype = torch.float)

##################################
G.eval()  
###########################################################
with torch.no_grad():
  Theta_diag = G(alpha_cand)

Theta_diag = Theta_diag.cpu().detach().numpy()
