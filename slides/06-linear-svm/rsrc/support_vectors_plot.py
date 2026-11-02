import os.path as osp

import numpy as np
import matplotlib.pyplot as plt
from sklearn.svm import SVC


def main():
	np.random.seed(100)

	slope = -1
	pos_margin_bias = 1
	neg_margin_bias = -1
	num_points_per_class = 16
	num_margin_points_per_class = 8

	x_1_scale = 2
	shift_scale = 2

	pos_x_1 = np.random.randn(num_points_per_class, 1) * x_1_scale
	pos_x_2 = slope * pos_x_1 + pos_margin_bias
	pos_points = np.concatenate([pos_x_1, pos_x_2], axis=1)

	pos_on_margin = np.ones((num_points_per_class,)).astype(bool)
	pos_on_margin[:num_margin_points_per_class] = False

	pos_shift = np.random.rand(num_margin_points_per_class, 1) * shift_scale * np.array([1, 1])
	pos_points[~pos_on_margin] += pos_shift


	neg_x_1 = np.random.randn(num_points_per_class, 1) * x_1_scale
	neg_x_2 = slope * neg_x_1 + neg_margin_bias
	neg_points = np.concatenate([neg_x_1, neg_x_2], axis=1)

	neg_on_margin = np.ones((num_points_per_class,)).astype(bool)
	neg_on_margin[:num_margin_points_per_class] = False

	neg_shift = np.random.rand(num_margin_points_per_class, 1) * shift_scale * np.array([-1, -1])
	neg_points[~neg_on_margin] += neg_shift

	whole_x = np.concatenate([pos_points, neg_points], axis=0)
	whole_y = np.concatenate([np.ones(pos_points.shape[0], dtype=int), np.full(neg_points.shape[0], -1, dtype=int)])
	model = SVC(kernel='linear')
	model.fit(whole_x, whole_y)


	is_support = np.zeros_like(whole_y, dtype=bool)
	is_support[model.support_] = True
	pos_is_support = is_support[:num_points_per_class]
	neg_is_support = is_support[num_points_per_class:]

	coef = model.coef_.reshape(-1)
	hyperplane_slope = - coef[0] / coef[1]
	hyperplane_bias = - model.intercept_ / coef[1]
	hyperplane_x_1 = np.linspace(-3, 3, 50)
	hyperplane_x_2 = hyperplane_slope * hyperplane_x_1 + hyperplane_bias


	# visualize orignal data
	fig, ax = plt.subplots(1, 1, figsize=(5, 5))
	ax.scatter(pos_points[:, 0], pos_points[:, 1], s=50, label='Class: +1', facecolors='none', edgecolors='r')
	ax.scatter(neg_points[:, 0], neg_points[:, 1], s=50, label='Class: -1', facecolors='none', edgecolors='b')
	ax.set_title('Scatterplot of Data')
	plt.legend()
	plt.tight_layout()
	plt.savefig(osp.join('../figure', 'linear_svm_support_vectors_1.png'), bbox_inches='tight')
	plt.close(fig)

	# visualize support vectors and hyperplane
	fig, ax = plt.subplots(1, 1, figsize=(5, 5))
	ax.scatter(
	    pos_points[~pos_is_support][:, 0], 
	    pos_points[~pos_is_support][:, 1], 
	    s=50, 
	    label='Class: +1', 
	    facecolors='none', 
	    edgecolors='r')
	ax.scatter(
	    pos_points[pos_is_support][:, 0], 
	    pos_points[pos_is_support][:, 1], 
	    marker='v',
	    s=50, 
	    label='Class: +1 Support Vectors', 
	    c='r'
	)

	ax.scatter(
	    neg_points[~neg_is_support][:, 0], 
	    neg_points[~neg_is_support][:, 1], 
	    s=50, 
	    label='Class: -1', 
	    facecolors='none', 
	    edgecolors='b'
	)
	ax.scatter(
	    neg_points[neg_is_support][:, 0], 
	    neg_points[neg_is_support][:, 1], 
	    s=50, 
	    label='Class: -1 Support Vectors',  
	    c='b'
	)

	ax.plot(hyperplane_x_1, hyperplane_x_2, color='g')
	ax.plot(hyperplane_x_1,  slope * hyperplane_x_1 + pos_margin_bias, linestyle='dotted', color='g')
	ax.plot(hyperplane_x_1,  slope * hyperplane_x_1 + neg_margin_bias, linestyle='dotted', color='g')
	ax.set_title('Linear SVM and Support Vectors')
	plt.legend()
	plt.tight_layout()
	plt.savefig(osp.join('../figure/linear_svm_support_vectors_2.png'), bbox_inches='tight')


if __name__ == '__main__':
	main()
