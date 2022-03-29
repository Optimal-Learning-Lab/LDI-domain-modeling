from utils import *
import numpy as np
from numpy import linalg as LA
import time
from sklearn.metrics import mean_squared_error, mean_absolute_error, roc_auc_score
import warnings
import sys
import pandas as pd
from scipy.special import expit

warnings.filterwarnings("error")


class FDTF(object):

    def __init__(self, config):
        """
        :param config:
        :var
        """

        np.random.seed(1)
        #self.log_file = config['log_file']
        #if self.log_file:
        #    self.logger = create_logger(self.log_file)
        #for key in config():
        #    print(key)
        
        self.views = config['views']
        
        self.train_data = config['train']
        self.test_data=config['test']

        self.num_users = int(config['num_users'])
        self.num_attempts = int(config['num_attempts'])
        self.num_concepts = int(config['num_concepts'])
        self.num_questions = int(config['num_questions'])
        self.lambda_t = config['lambda_t']
        self.lambda_q = config['lambda_q']
        self.lambda_bias = config['lambda_bias']

        self.lr = config['lr']
        self.max_iter = config['max_iter']
        self.tol = config['tol']
        self.slr = config['slr']

        self.metrics = config['metrics']

        self.use_bias_t = True
        self.use_global_bias = True

        self.binarized_question = True

        self.current_test_attempt = None
        self.test_obs_list = []
        self.test_pred_list = []

        self.T = np.random.random_sample((self.num_users, self.num_attempts,
                                          self.num_concepts))
        self.Q = np.random.random_sample((self.num_concepts, self.num_questions))
        
        self.Y=[0]
        #self.Q_matrix=[0]
        self.bias_s = np.zeros(self.num_users)
        self.bias_t = np.zeros(self.num_attempts)
        self.bias_q = np.zeros(self.num_questions)
        
        self.global_bias = np.mean(self.train_data, axis=0)[3]
    def __getstate__(self):
        """
        since the logger cannot be pickled, to avoid the pickle error, we should add this
        :return:
        """
        d = dict(self.__dict__)
        del d['logger']
        return d

    def __setstate__(self, d):
        self.__dict__.update(d)


    def _get_question_prediction(self, student, attempt, question):
        """
        predict value at tensor Y[attempt, student, question]
        :param attempt: attempt index
        :param student: student index
        :param question: question index
        :return: predicted value of tensor Y[attempt, student, question]
        """
        pred = np.dot(self.T[student, attempt, :], self.Q[:, question])

        if self.use_bias_t:
            if self.use_global_bias:
                pred += self.bias_s[student] + self.bias_t[attempt] + self.bias_q[question] + \
                        self.global_bias
            else:
                pred += self.bias_s[student] + self.bias_t[attempt] + self.bias_q[question]
        else:
            if self.use_global_bias:
                pred += self.bias_s[student] + self.bias_q[question] + self.global_bias
            else:
                pred += self.bias_s[student] + self.bias_q[question]

        if self.binarized_question:
            pred = sigmoid(pred) # Sigmoid functions most often show a return value (y axis) in the range 0 to 1.

        return pred


    def _get_loss(self):
        """
        override the function in super class
        compute the loss, which is RMSE of observed records
        :return: loss
        """
        loss, square_loss, reg_bias = 0., 0., 0.
        square_loss_q = 0.
        q_count = 0.
        
        train_data_new=[]
        train_data_new=self.train_data.tolist()
        
        for (student, attempt, question, obs, resource) in train_data_new:

            pred = self._get_question_prediction(int(student), int(attempt), int(question))
            square_loss_q += (obs - pred) ** 2
            q_count += 1

        #print("square loss {}".format(square_loss_q))

        reg_T = LA.norm(self.T) ** 2  # regularization on tensor T
        reg_Q = LA.norm(self.Q) ** 2  # regularization on matrix Q

        reg_features = self.lambda_q * reg_Q + self.lambda_t * reg_T

        q_rmse = np.sqrt(square_loss_q / q_count) if q_count != 0 else 0.

        if self.lambda_bias:
            if self.use_bias_t:
                reg_bias = self.lambda_bias * (
                        LA.norm(self.bias_s) ** 2 + LA.norm(self.bias_t) ** 2 +
                        LA.norm(self.bias_q) ** 2)
            else:
                reg_bias = self.lambda_bias * (
                        LA.norm(self.bias_s) ** 2 + LA.norm(self.bias_q) ** 2)
 
        loss = square_loss_q + reg_features + reg_bias
        print("Overall Loss {}".format(loss))
        return loss, q_count, q_rmse, reg_features, reg_bias


    def _grad_T_ij(self, student, attempt, index, obs, resource=None):
        """
        compute the gradient of loss w.r.t a specific student j's knowledge at
        a specific attempt i: T_{i,j,:},
        :param attempt: index
        :param student: index
        :param obs: observation
        :return:
        """
        #print("_grad_T_ij")

        grad = np.zeros_like(self.T[student, attempt, :])

        if obs is not None:
            pred = self._get_question_prediction(student, attempt, index)
            if self.binarized_question:
                grad = -2. * (obs - pred) * pred * (1. - pred) * self.Q[:, index] +\
                       2. * self.lambda_t * self.T[student, attempt, :]
            else:
                grad = -2. * (obs - pred) * self.Q[:, index] + \
                       2. * self.lambda_t * self.T[student, attempt, :]

        return grad


    def _grad_Q_k(self, student, attempt, question, obs):
        """
        compute the gradient of loss w.r.t a specific concept-question association
        of a question in Q-matrix,
        :param attempt: index
        :param student:  index
        :param question:  index
        :param obs: the value at Y[attempt, student, question]
        :return:
        """

        grad = np.zeros_like(self.Q[:, question])
        if obs is not None:
            pred = self._get_question_prediction(student, attempt, question)
            if self.binarized_question:
                grad = -2. * (obs - pred) * pred * (1. - pred) * self.T[student, attempt, :] + \
                       2. * self.lambda_q * self.Q[:, question]
            else:
                grad = -2. * (obs - pred) * self.T[student, attempt, :] + \
                       2. * self.lambda_q * self.Q[:, question]

        return grad

    def _grad_bias_s(self, student, attempt, material, obs, resource=None):
        """
        compute the gradient of loss w.r.t a specific bias_s
        :param attempt:
        :param student:
        :param material: material material of that resource, here is the question
        :param obs:
        :return:
        """
        grad = 0.
        if obs is not None:
            pred = self._get_question_prediction(student, attempt, material)
            if self.binarized_question:
                grad -= 2. * (obs - pred) * pred * (1. - pred) + \
                        2.0 * self.lambda_bias * self.bias_s[student]
            else:
                grad -= 2. * (obs - pred) + 2.0 * self.lambda_bias * self.bias_s[student]

        return grad


    def _grad_bias_t(self, student, attempt, material, obs, resource=None):
        """
        compute the gradient of loss w.r.t a specific bias_a
        :param attempt:
        :param student:
        :param material: material material of that resource
        :return:
        """
        #print("_grad_bias_t")
        grad = 0.
        if obs is not None:
            pred = self._get_question_prediction(student, attempt, material)
            if self.binarized_question:
                grad -= 2. * (obs - pred) * pred * (1. - pred) + \
                        2.0 * self.lambda_bias * self.bias_t[attempt]
            else:
                grad -= 2. * (obs - pred) + 2.0 * self.lambda_bias * self.bias_t[attempt]
        return grad

    def _grad_bias_q(self, student, attempt, question, obs):
        """
        compute the gradient of loss w.r.t a specific bias_q
        :param attempt:
        :param student:
        :param question:
        :param obs:
        :return:
        """
        #print("_grad_bias_q")
        grad = 0.
        if obs is not None:
            pred = self._get_question_prediction(student, attempt, question)
            if self.binarized_question:
                grad -= 2. * (obs - pred) * pred * (1. - pred) + \
                        2. * self.lambda_bias * self.bias_q[question]
            else:
                grad -= 2. * (obs - pred) + 2. * self.lambda_bias * self.bias_q[question]

        return grad

    def _grad_global_bias(self, student, attempt, question, obs):
        """
        compute the gradient of loss w.r.t a specific bias_q
        :param attempt:
        :param student:
        :param question:
        :param obs:
        :return:
        """
        #print("_grad_global_bias")
        grad = 0.
        if obs is not None:
            pred = self._get_question_prediction(student, attempt, question)
            if self.binarized_question:
                grad -= 2. * (obs - pred) * pred * (1. - pred) + \
                        2. * self.lambda_bias * self.global_bias
            else:
                grad -= 2. * (obs - pred) + 2. * self.lambda_bias * self.global_bias

        return grad

    def _optimize_sgd(self, student, attempt, material, obs, resource=None):
        """
        train the T and Q with stochastic gradient descent
        :param attempt:
        :param student:
        :param material: material material of that resource, it's question here
        :return:
        """
        #print("_optimize_sgd")

        # optimize Q
        grad_q = self._grad_Q_k(student, attempt, material, obs)
        self.Q[:, material] -= self.lr * grad_q
        self.Q[:, material][self.Q[:, material] < 0.] = 0.
        if self.lambda_q == 0.:
            sum_val = np.sum(self.Q[:, material])
            if sum_val != 0:
                self.Q[:, material] /= sum_val  # normalization

        # the updated Q will be used for computing gradient of T
        grad_t = self._grad_T_ij(student, attempt, material, obs, resource)
        self.T[student, attempt, :] -= self.lr * grad_t

        # optimize the bias(es)
        self.bias_q[material] -= self.lr * self._grad_bias_q(student, attempt, material, obs)
        self.bias_s[student] -= self.lr * self._grad_bias_s(student, attempt, material, obs)

        if self.use_bias_t:
            self.bias_t[attempt] -= self.lr * self._grad_bias_t(student, attempt, material, obs)

        if self.use_global_bias:
            self.global_bias -= self.lr * self._grad_global_bias(student, attempt, material, obs)



    def training(self):
        """
        minimize the loss until converged or reach the maximum iterations
        with stochastic gradient descent
        :return:
        """
        #self.logger.info(strBlue("*"*100))
        #self.logger.info(strBlue('test attempt: {}, train size: {}'.format(
        #    self.current_test_attempt, len(self.train_data)))
        #)

        loss, q_count, q_rmse, reg_features, reg_bias = self._get_loss()
        #self.logger.info(strBlue("initial: lr: {:.4f}, loss: {:.2f}, q_count: {}, q_rmse: {:.5f}, "
        #                         "reg_features: {:.2f}, reg_bias: {:.3f}".format(
        #    self.lr, loss, q_count, q_rmse, reg_features, reg_bias))
        #)
        loss_list = [loss]
        #self.logger.info(strBlue("*" * 40 + "[ Training Results ]" + "*" * 40))

        train_perf = []
        start_time = time.time()
        converge = False
        iter_num = 0
        min_iter = 10
        best_T, best_Q = [0] * 2
        best_bias_s, best_bias_t, best_bias_q, best_global_bias = [0] * 4

        while not converge:
            self.train_data=self.train_data.copy()
            np.random.shuffle(self.train_data)
            best_T = np.copy(self.T)
            best_Q = np.copy(self.Q)
            best_bias_s = np.copy(self.bias_s)
            best_bias_t = np.copy(self.bias_t)
            best_bias_q = np.copy(self.bias_q)

            for (student, attempt, index, obs, resource) in self.train_data:
                self._optimize_sgd(int(student), int(attempt), int(index), int(obs), resource=resource)
            
            sorted_train_data = sorted(self.train_data, key = lambda x:[x[0], x[1]])
            for (student, attempt, index, obs, resource) in sorted_train_data:
                student=int(student)
                attempt=int(attempt)
                index=int(index)
                obs=int(obs)
                if attempt < self.num_attempts - 1:
                   self.T[student, attempt + 1, :] = 2 * self.T[student, attempt, :] +\
                       np.true_divide(2 * (1 - self.T[student, attempt, :]), 1 + np.exp(- self.slr * self.Q[:, index])) - 1

            loss, q_count, q_rmse, reg_features, reg_bias = self._get_loss()
            train_perf.append([q_count, q_rmse])

            run_time = time.time() - start_time
            #self.logger.debug("iter: {}, lr: {:.4f}, total loss: {:.2f}, q_count: {}, "
            #                  "q_rmse: {:.5f}".format(iter_num, self.lr, loss, q_count, q_rmse))
            #self.logger.debug("reg_features: {:.2f}, reg_bias: {:.3f}, "
            #                  "run time so far: {:.2f}".format(reg_features, reg_bias, run_time))

            if iter_num == self.max_iter:
                #self.logger.info("=" * 50)
                #self.logger.info("** converged **, condition: 0, iter: {}".format(iter_num))
                loss_list.append(loss)
                converge = True
                #self.logger.info("training loss: {:.5f}".format(loss))
                #print("training loss"+loss)
                #sys.stdout.flush()
                #self.logger.info("q_rmse: {:.5f}".format(q_rmse))
                #self.logger.info("regularization on parameters: {:.5f}".format(reg_features))

            elif iter_num >= min_iter and loss >= np.mean(loss_list[-5:]):
                #self.logger.info("=" * 40)
                #self.logger.info("** converged **, condition: 1, iter: {}".format(iter_num))
                converge = True
                #self.logger.info("training loss: {:.5f}".format(loss))
                #self.logger.info("q_rmse: {:.5f}".format(q_rmse))
                #self.logger.info("regularization on parameters: {:.5f}".format(reg_features))
            elif loss == np.nan:
                self.lr *= 0.1

            elif loss > loss_list[-1]:
                loss_list.append(loss)
                self.lr *= 0.5
                iter_num += 1
                print('iter_num: '+str(iter_num))

            else:
                loss_list.append(loss)
                iter_num += 1
                print('iter_num: '+str(iter_num))

        # reset to previous T, Q
        self.T = best_T
        self.Q = best_Q
        sys.stdout.flush()
        self.bias_s = best_bias_s
        self.bias_t = best_bias_t
        self.bias_q = best_bias_q
        Y=np.dot(self.T,self.Q)+self.global_bias
        for i in range(0,self.num_users):
            Y[i,:,:]=Y[i,:,:]+self.bias_s[i]
        for j in range(0,self.num_attempts):
            Y[:,j,:]=Y[:,j,:]+self.bias_t[j]
        for k in range(0,self.num_questions):
            Y[:,:,k]=Y[:,:,k]+self.bias_q[k]
            
        
        Y=np.where(Y > 100, 1, Y)
        Y=np.where(Y < -100, 0, Y)
        
        #self.Q_matrix=np.mean(Y,axis=1)
        
        self.Y=expit(Y)
        
        return train_perf[-1]

    def testing(self, test_data, validation=False):
        """
        :return: performance metrics mean squared error, RMSE, and mean absolute error
        """
        #if not validation:
        #    self.logger.info(strGreen("*" * 40 + "[ Testing Results ]" + "*" * 40))
        #    self.logger.info(strGreen("Current testing attempt: {}, Test size: {}".format(
        #        self.current_test_attempt, len(test_data))))

        curr_pred_list = []
        curr_obs_list = []
        test_data_new=[]
        test_data_new=test_data.values.tolist()
        #sys.stdout.flush()
        for (student, attempt, question, obs, resource) in test_data_new:
            if resource == 0:
                curr_obs_list.append(obs)
                #sys.stdout.flush()
                pred = self._get_question_prediction(int(student), int(attempt), int(question))
                curr_pred_list.append(pred)
                self.test_obs_list.append(obs)
                self.test_pred_list.append(pred)
                #self.logger.debug(strCyan("true: {:.5f}, pred: {:.5f}\n".format(obs, pred)))

        return self.eval(curr_obs_list, curr_pred_list)

    def eval(self, obs_list, pred_list):
        """
        evaluate the prediction performance
        :param obs_list:
        :param pred_list:
        :return:
        """
        assert len(pred_list) == len(obs_list)

        count = len(obs_list)
        perf_dict = {}
        if len(pred_list) == 0:
            return perf_dict
        else:
            # self.logger.info("Test Size: {}".format(count))
            perf_dict["count"] = count

        for metric in self.metrics:
            if metric == "rmse":
                rmse = mean_squared_error(obs_list, pred_list, squared=False)
                perf_dict[metric] = rmse
                #self.logger.info(strGreen("RMSE: {:.5f}".format(rmse)))
            elif metric == 'mae':
                mae = mean_absolute_error(obs_list, pred_list)
                perf_dict[metric] = mae
                #self.logger.info(strGreen("MAE: {:.5f}".format(mae)))
            elif metric == "auc":
                if np.sum(obs_list) == count:
                    #self.logger.info(strGreen("AUC: None (all ones in true y)"))
                    perf_dict[metric] = None
                else:
                    auc = roc_auc_score(obs_list, pred_list)
                    perf_dict[metric] = auc
                    #self.logger.info(strGreen("AUC: {:.5f}".format(auc)))
        #self.logger.info("\n")
        return perf_dict
