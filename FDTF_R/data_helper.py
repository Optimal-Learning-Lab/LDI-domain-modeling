# some helper functions for generating suitable data or configuration on running experiments
from utils import *
import gensim
import nltk
import csv
import base64
import pandas as pd
import xlsxwriter
import openpyxl
import warnings

def fdtf_config(data_str, course_str, views, concept_dim, fold, lambda_t, lambda_q, lambda_bias, slr, lr, max_iter,
                metrics, validation, validation_limit=30):

    """
    generate model configurations for training and testing
    such as initialization of each parameters and hyperparameters
    :return: config dict
    """

    warnings.filterwarnings('ignore', category=UserWarning, module='openpyxl')
    testFile = pd.read_excel('data/{}/{}/{}_tfData_test.xlsx'.format(data_str, course_str, fold), engine="openpyxl")
    trainFile= pd.read_excel('data/{}/{}/{}_tfData_training.xlsx'.format(data_str, course_str, fold), engine="openpyxl")
    validFile = pd.read_excel('data/{}/{}/{}_tfData_validation.xlsx'.format(data_str, course_str, fold), engine="openpyxl")

    testData=testFile.values.tolist()
    trainData=trainFile.values.tolist()
    validData=validFile.values.tolist()

    #print(testData)
    #print(trainData)
    #print(validData)

    numStudent=18  #17
    numAttempt=12  #11
    numQuestion=74 #72

    data={'num_users':numStudent,'num_quizzes':numQuestion,'num_lectures':0,'num_discussions':0,'num_attempts': numAttempt,'train':trainData,'test':testData,'val':validData}
    #print(data)

    config = {
        'views': views,
        'num_users': data['num_users'],
        'num_attempts': data['num_attempts'],
        'num_questions': data['num_quizzes'],
        'num_concepts': concept_dim,
        'lambda_t': lambda_t,
        'lambda_q': lambda_q,
        'lambda_bias': lambda_bias,
        'lr': lr,
        'max_iter': max_iter,
        'tol': 1e-3,
        'slr': slr,
        'metrics': metrics,
        'log_file': log_file,
        'validation':True
    }

    # output the config
    model_str = 'fdtf'
    output_path = "results/{}/{}/{}/test_fold_config_{}.txt".format(data_str, course_str, model_str, fold)
    f = open(output_path, "w+")
    f.write('{}{}'.format('views: ', views))
    f.write("\n")
    f.write('{}{}'.format('num_users: ', data['num_users']))
    f.write("\n")
    f.write('{}{}'.format('num_quizzes: ', data['num_quizzes']))
    f.write("\n")
    f.write('{}{}'.format('num_lectures: ', data['num_lectures']))
    f.write("\n")
    f.write('{}{}'.format('num_discussions: ', data['num_discussions']))
    f.write("\n")
    f.write('{}{}'.format('num_attempts: ', data['num_attempts']))
    f.write("\n")
    f.write('{}{}'.format('num_questions: ', data['num_quizzes']))
    f.write("\n")

    f.write('{}{}'.format('config: ', config))
    f.close()

    # generate config, train_set, test_set for general train and test
    train_data = []
    for (student, attempt, question, score, resource) in data['train']:
        # if it is for validation, we only use the first 30 attempts for cross validation to
        # do the hyperparameter tuning
        if validation:
            if attempt < validation_limit:
                train_data.append(
                    (int(student), int(attempt), int(question), float(score), int(resource))
                )
        else:
            train_data.append(
                (int(student), int(attempt), int(question), float(score), int(resource))
            )
    config['train'] = train_data

    val_data = []
    for (student, attempt, question, score, resource) in data['val']:
        if validation:
            if attempt < validation_limit:
                val_data.append(
                    (int(student), int(attempt), int(question), float(score), int(resource))
                )
        else:
            val_data.append(
                (int(student), int(attempt), int(question), float(score), int(resource))
            )
    config['val'] = val_data

    test_set = []
    test_users = {}
    for (student, attempt, question, score, resource) in data['test']:
        if validation:
            if attempt < validation_limit:
                test_set.append(
                    (int(student), int(attempt), int(question), float(score), int(resource))
                )
        else:
            test_set.append(
                (int(student), int(attempt), int(question), float(score), int(resource))
            )
        if student not in test_users:
            test_users[student] = 1
    config['test'] = test_set
    if validation:
        config['num_attempts'] = validation_limit

    return config




