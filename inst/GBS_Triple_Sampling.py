##################################
B10 = int(r.B10)
B2 = int(r.B2)
B3 = int(r.B3)
#fac = int(r.fac)
G.eval()  
###########################################################
a1_many_sample = torch.distributions.exponential.Exponential(torch.ones(B10,n_a))
Theta3 = torch.zeros(B3,B2,B10,p).to('cpu')
one2 = torch.ones(B2,n_a).to(device)
one3 = torch.ones(B3,n_a).to(device)
#N30 = int(B3/fac)
with torch.no_grad():
  alpha1 = a1_many_sample.sample().to(device)
  m = torch.mean(alpha1,1).to(device)
  m = m.reshape(B10,1)
  alpha1 = alpha1/m
  Theta1 = G(alpha1)
  alpha2 = torch.distributions.gamma.Gamma(torch.ones(B2,B10,n_a).to(device)*alpha1, 1).sample()
  m = torch.mean(alpha2,2).to(device)
  alpha2 = alpha2/(torch.ones(B2,B10,n_a).to(device)*m.reshape(B2,B10,1))
  Theta20 = G(alpha2.reshape(B2*B10,n_a))
  Theta2 = Theta20.reshape(B2,B10,p)
  #ind = range((k*N30),((k+1)*N30))
  alpha3 = torch.distributions.gamma.Gamma(torch.ones(B3,B2,B10,n_a).to(device)*alpha2, 1).sample()
  m = torch.mean(alpha3,3).to(device)
  alpha3 = alpha3/(torch.ones(B3,B2,B10,n_a).to(device)*m.reshape(B3,B2,B10,1))
  Theta30 = G(alpha3.reshape(B3*B2*B10,n_a))
  Theta3 = Theta30.reshape(B3,B2,B10,p)
  #Theta3[ind,:,:,:] = Theta30.to('cpu')
#  m.size()
#for i in range(N1):
  #  a = alpha1[i,:].reshape(1,n_a)
  #  parm = torch.matmul(torch.ones(N2,1).to(device), a)
  #  alpha2 = torch.distributions.gamma.Gamma(parm, one2).sample()
  #  m = torch.mean(alpha2,1).to(device)
  #  m = m.reshape(N2,1)
  #  alpha2 = alpha2/m
  #  Theta2[i,:,:] = generator_CNN(alpha2)
    
    #for k in range(N2):
    #  a = alpha2[k,:].reshape(1,n_a)
    #  parm = torch.matmul(torch.ones(N3,1).to(device), a)
    #  alpha3 = torch.distributions.gamma.Gamma(parm, one3).sample()
    #  m = torch.mean(alpha3,1).to(device)
    #  m = m.reshape(N3,1)
    #  alpha3 = alpha3/m
    #  Theta3[i,k,:,:] = generator_CNN(alpha3)
    #if i % 100 == 0:
    #  print(i)
    #sys.stdout.flush()
    
Theta1 = Theta1.cpu().detach().numpy()
Theta2 = Theta2.cpu().detach().numpy()
Theta3 = Theta3.cpu().detach().numpy()

