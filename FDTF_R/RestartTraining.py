import numpy as np
def restart_training(model):
    # initialize the bias for each attempt, student, question, lecture, or discussion
    if int(model.views[0]) == 1:
        model.T = np.random.random_sample((model.num_users, model.num_attempts,
                                           model.num_concepts))
        model.Q = np.random.random_sample((model.num_concepts, model.num_questions))
        model.bias_s = np.zeros(model.num_users)
        model.bias_t = np.zeros(model.num_attempts)
        model.bias_q = np.zeros(model.num_questions)
        model.global_bias = np.mean(model.train_data, axis=0)[3]
    else:
        raise AttributeError

    return model