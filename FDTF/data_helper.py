# some helper functions for generating suitable data or configuration on running experiments
from utils import *
import gensim
import nltk
import csv
import base64
import pandas as pd
import xlsxwriter

def fdtf_config(data_str, course_str, views, concept_dim, fold, lambda_t, lambda_q, lambda_bias, slr, lr, max_iter,
                metrics, log_file, validation, validation_limit=30):

    """
    generate model configurations for training and testing
    such as initialization of each parameters and hyperparameters
    :return: config dict
    """

    with open('data/{}/{}/{}_train_val_test.pkl'.format(data_str, course_str, fold), 'rb') as f:
        data = pickle.load(f)
        print(data)
        # convert your original dataset into .csv, add by Liang
        path_train_csv = 'data/{}/{}/{}_train.xlsx'.format(data_str, course_str, fold)
        train_data=data['train']
        header_data=['Student','Attempt','Question','Score','Resource']
        df = pd.DataFrame(train_data)
        writer = pd.ExcelWriter(path_train_csv, engine='xlsxwriter')
        df.to_excel(writer, sheet_name='train', index=False)
        workbook = writer.book
        worksheet = writer.sheets['train']
        # Add a header format.
        header_format = workbook.add_format({'bold': True,
                                             'bottom': 2,
                                             })
        # Write the column headers with the defined format.
        for col_num, value in enumerate(header_data):
            worksheet.write(0, col_num, value,header_format)
        writer.save()

        path_test_csv = 'data/{}/{}/{}_test.xlsx'.format(data_str, course_str, fold)
        test_data=data['test']
        header_data1 = ['Student', 'Attempt', 'Question', 'Score', 'Resource']
        df1 = pd.DataFrame(test_data)
        writer1 = pd.ExcelWriter(path_test_csv, engine='xlsxwriter')
        df1.to_excel(writer1, sheet_name='test', index=False)
        workbook1 = writer1.book
        worksheet1 = writer1.sheets['test']
        header_format1 = workbook1.add_format({'bold': True,
                                             'bottom': 2,
                                             })
        for col_num, value in enumerate(header_data1):
            worksheet1.write(0, col_num, value,header_format1)
        writer1.save()

        path_test_csv = 'data/{}/{}/{}_val.xlsx'.format(data_str, course_str, fold)
        val_data=data['val']
        header_data2 = ['Student', 'Attempt', 'Question', 'Score', 'Resource']
        df2 = pd.DataFrame(val_data)
        writer2 = pd.ExcelWriter(path_test_csv, engine='xlsxwriter')
        df2.to_excel(writer2, sheet_name='val', index=False)
        workbook2 = writer2.book
        worksheet2 = writer2.sheets['val']
        header_format2 = workbook2.add_format({'bold': True,
                                             'bottom': 2,
                                             })
        for col_num, value in enumerate(header_data2):
            worksheet2.write(0, col_num, value,header_format2)
        writer2.save()

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
        'log_file': log_file
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




