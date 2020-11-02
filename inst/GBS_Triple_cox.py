import torch
import torch.nn as nn
import numpy as np
from random import sample
# Device configuration
gpu_ind = int(r.gpu_ind)
print("###########################################")
if torch.cuda.is_available():
  device = torch.device('cuda', gpu_ind)
  print("Training G via GPU computing starts!")
else:
  device = torch.device('cpu')
  print("Training G via CPU computing starts!")
  print("WARNING: CPU computing is not efficient in training.")
print("###########################################")
n = int(r.n)
p = int(r.p)
lr_U = r.lr_U
lr_L = r.lr_L
lr_power = r.lr_power
n_a = int(r.S)
n_b = int(n/n_a)
sub_size = n
hidden_size = int(r.hidden_size)
L = int(r.L)
V = int(r.V)
num_it = int(r.num_it)
K0 = int(r.M)
X = r.X
y = r.y
X = torch.from_numpy(X)
y = torch.from_numpy(y)
delta0 = r.delta
delta0 = torch.from_numpy(delta0)
delta0 = delta0.to(dtype = torch.float)
X = X.to(device, dtype = torch.float)
y = y.to(device, dtype = torch.float)

class Net(nn.Module):
  def __init__(self, n_a,  hidden_size, L):
    super(Net, self).__init__()
    self.fc0 = nn.Linear(2*n_a, hidden_size)
    self.relu = nn.ReLU()
    self.bn1 = nn.BatchNorm1d(hidden_size)
    self.fc_out = nn.Linear(hidden_size+2*n_a, p)
    self.layers = nn.ModuleList()
    for j in range(L - 1):
      self.layers.append( nn.Linear(hidden_size+2*n_a, hidden_size) )
      self.layers.append( nn.ReLU() )
      self.layers.append( nn.BatchNorm1d(hidden_size) )
      
  def forward(self, a):
    out0 = torch.exp(-1.0*a)
    out1 = torch.cat([out0, 1.0 - out0],dim=1)
    out = self.relu(self.fc0(out1))
    out = self.bn1(out)
    out = torch.cat([out1, out],dim=1)
    for i in range(L-1):
      out = self.layers[3*i](out)
      out = self.layers[3*i+1](out)
      out = self.layers[3*i+2](out)
      out = torch.cat([out1, out],dim=1)  
    out = self.fc_out(out)
    return out 

layers = nn.ModuleList()
for j in range(L - 1):
  layers.append( nn.Linear(hidden_size+2*n_a, hidden_size) )
  layers.append( nn.ReLU() )
  layers.append( nn.BatchNorm1d(hidden_size) )
  
#def D(y,X, Theta):
#  c = torch.matmul(X1,Theta.t())
#  out =  -1.0*torch.abs(y1 - c)
#  return out
def D(y, X, Theta, delta0):
    c = torch.matmul(X,Theta.t())
    #dset = torch.unique(y1[delta1 == 1.0])
    dset = y[delta0[:,0] == 1.0,:]
    rset = torch.ge(y.repeat(dset.size(0),1).view(-1,y.size(0)).t(),dset).type(torch.float)
    out = c[(delta0[:,0] == 1.0).nonzero()[:,0],:] - torch.log( torch.matmul(rset.t(),torch.exp(c)) )
    return out


def Schedule(it0, lag1, lag):
    s = (1.0 + np.cos(3.14*(it0 - lag1*np.floor(it0/lag))/lag ))
    return s

#############################################
nsub = int(sub_size*n_b)
ones = torch.ones(2,2).to(device)
n1 = float(n)
G = Net(n_a, hidden_size, L).to(device)
#optimizer = torch.optim.Adam(generator_CNN.parameters(), lr= lr0)
#optimizer = torch.optim.SGD(generator_CNN.parameters(), lr= 0.0001, momentum=0.9)
optimizer = torch.optim.RMSprop(G.parameters(), lr= 0.0005, alpha=0.99, eps=1e-08)
scheduler = torch.optim.lr_scheduler.CyclicLR(optimizer, base_lr=lr_L, max_lr=lr_U, step_size_up = 1000)
a_sample = torch.distributions.exponential.Exponential(torch.ones(V))
J = 1
it = 0
it0 = 0.0
LOSS = 10000.0*torch.zeros(num_it).to(device)
#alpha = (torch.randn(K0,n_a)**2).to(device)
alpha = torch.ones(K0,n_a).to(device)
A = torch.zeros(n, n_a)
for i in range(n_a):
  ind = range(i*n_b,(i+1)*n_b)
  A[ind,i] = 1
A = A.t().to(device) 

loss0 = 0.0
t0 = 0
T0 = 0
loss2 = torch.zeros(1).to(device)
while J == 1:
    for param_group in optimizer.param_groups:
        param_group["lr"] = param_group["lr"]/((it0+1.0)**lr_power)
    ind_a = sample(range(15,K0), 20)
    for k in range(20):
      ind_b = sample(range(n_a), V)
      weight1 = a_sample.sample().to(device)
      weight2 = torch.distributions.gamma.Gamma(weight1, torch.ones(V).to(device)).sample()
      weight3= torch.distributions.gamma.Gamma(weight2, torch.ones(V).to(device)).sample()
      alpha[ind_a[k], ind_b] = 1.1*weight3
    for h in range(3):
      ind_b = sample(range(n_a), V)  
      weight1 = a_sample.sample().to(device)
      k = sample(range(5),1)
      alpha[k,ind_b] = 1.1*weight1
    for h in range(5):
      ind_b = sample(range(n_a), V)  
      weight1 = a_sample.sample().to(device)
      weight2 = torch.distributions.gamma.Gamma(weight1, torch.ones(V).to(device)).sample()
      k = sample(range(10,15),1)
      alpha[k, ind_b] = 1.1*weight2

    w1 = torch.matmul(alpha, A).t()
    Theta = G(alpha)
    Theta[Theta != Theta] = 0.0
    loss1 = D(y, X, Theta, delta0).to(device)
    loss_log = loss1*(w1.t()[(delta0[:,0] == 1.0).nonzero()[:,0],:])

#    loss_log = loss1*w1
    loss = torch.mean(-1.0*loss_log)/n 
    optimizer.zero_grad()
    loss.backward() 
    optimizer.step()
    LOSS[it] = loss.item()
    loss0 += loss.item()
    it0 = it0 + 1.0
    it += 1
    if it > (num_it-1):
      J = 0
    if(it+1) % 100==0:
      ##########################################
      for param_group in optimizer.param_groups:
        lr1 = param_group["lr"] 
      print('Logit [{}/{}], Loss: {:.4f},  lr: {:.7f}, device: {}'
          .format(it+1, num_it, n1*loss0/100, lr1, device))
      print('n: {}, p: {},  hidden_size: {}, V:{},  M: {}, S: {}'
          .format(n, p,  hidden_size, V, K0, n_a))
      loss0 = 0.0
      sys.stdout.flush()
    scheduler.step()

