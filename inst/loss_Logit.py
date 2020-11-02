import os
import torch
import numpy as np
import sys
# Device configuration
gpu_ind = int(r.gpu_ind)
if torch.cuda.is_available():
  device = torch.device('cuda', gpu_ind)
else:
  device = torch.device('cpu')
"r.cpu_ind" in dir(os)
try:
   r.cpu_ind
except:
    print("No cpu_ind")
else:
    cpu_ind = int(r.cpu_ind)
    if cpu_ind == 1:
      device = torch.device('cpu')
n = int(r.n)
p = int(r.p)

S = int(r.S)
n_b = int(n/S)
dat = r.dat
dat = dat.values
dat = torch.from_numpy(dat)
dat = dat.to(device, dtype = torch.float)

def D(dat, Theta):
  c = torch.matmul(dat[:,0:p],Theta.t())
  c = torch.clamp(c, -20, 20) 
  out =  -1.0*(1-dat[:,p].reshape(n,1))*c - torch.log(1.0+torch.exp(-c))
  return out
  
