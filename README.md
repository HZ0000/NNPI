# Neural Prediction Intervals for Response Surface in Simulation Metamodeling

This repository contains code related to the paper
"Neural Prediction Intervals for Response Surface in Simulation Metamodeling" 
by Henry Lam and Haofeng Zhang. 

Paper link: https://arxiv.org/abs/2204.01904

# Citation
If you find this repository or the ideas presented in our paper useful, please consider citing the following:
```
@article{lam2022prediction,
  title={Neural Prediction Intervals for Response Surface in Simulation Metamodeling},
  author={Lam, Henry and Zhang, Haofeng},
  journal={arXiv preprint arXiv:2204.01904},
  year={2022}
}
```

# Implementation

For running experiments on the example of "Computer Communication Network" with four-dimensional input: <br />
1. To obtain the training data in our experiment design, run <br />
experiment - 4input_tr1.R/experiment - 4input_tr2.R <br />
to obtain data that are similar to the ones in <br />
truth_4input_tr1.csv/truth_4input_tr2.csv <br />
2. To obtain the test data, run <br />
experiment - 4input_te.R <br />
to obtain data that are similar to the ones in <br />
truth_4input_te.csv  <br />
3. To implement NN-based algorithms (including NNVA and our proposals NNGN and NNGU) to generate prediction intervals on the data, run <br />
PI_computer.py <br />
