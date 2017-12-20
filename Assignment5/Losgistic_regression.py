import numpy as np
from scipy import linalg
def mylogistic(_x, _y):

	x = _x.copy()
	y = _y.copy()
	r, c = x.shape
	beta = np.zeros((c, 1))
	epsilon = 1e-6
	while True:
		eta = np.dot(x, beta)
		pr = exp_it(eta)
		w = pr * (1 - pr)
		z = eta + (y - pr) / w
		sw = np.sqrt(w)
		mw = np.repeat(sw, c, axis=1)
		x_work = mw * x
		y_work = sw * z
		beta_new, _, _, _ = np.linalg.lstsq(x_work, y_work)
		err = np.sum(np.abs(beta_new - beta))
		beta = beta_new
		if err < epsilon:
			break

	return beta
def exp_it(_x):
	x = _x.copy()
	y = 1 / (1 + np.exp(-x))
	return y
if __name__ == '__main__':
	n = 1000
	p = 5
X = np.random.normal(0, 1, (n, p))
#beta = np.arange(p) + 1
beta = np.ones((p, 1))
print beta
Y = np.random.uniform(0, 1, (n, 1)) < exp_it(np.dot(X, beta)).reshape((n, 1))
logistic_beta = mylogistic(X, Y)
print logistic_beta