##################################
B1 = int(r.B1)
B2 = int(r.B2)
B3 = int(r.B3)
#fac = int(r.fac)
G.eval()  
###########################################################
a1_many_sample = torch.distributions.exponential.Exponential(torch.ones(B1,n_a))
one2 = torch.ones(B2,n_a).to(device)
one3 = torch.ones(B3,n_a).to(device)
with torch.no_grad():
  alpha1 = a1_many_sample.sample().to(device)
  m = torch.mean(alpha1,1).to(device)
  m = m.reshape(B1,1)
  alpha1 = alpha1/m
  Theta1 = G(alpha1)
  alpha2 = torch.distributions.gamma.Gamma(torch.ones(B2,B1,n_a).to(device)*alpha1, 1).sample()
  m = torch.mean(alpha2,2).to(device)
  alpha2 = alpha2/(torch.ones(B2,B1,n_a).to(device)*m.reshape(B2,B1,1))
  Theta2 = G(alpha2.reshape(B2*B1,n_a))
  Theta2 = Theta2.reshape(B2,B1,p)

Theta1 = Theta1.cpu().detach().numpy()
Theta2 = Theta2.cpu().detach().numpy()
Theta3 = 0.0
#del(Theta2)
