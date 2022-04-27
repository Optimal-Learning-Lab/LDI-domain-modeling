import pandas as pd
import sys
from collections import defaultdict
from Representation.rnn import DKTnet
import numpy as np

class DKT:

    def __init__(self, input_data, repr_type="rnn", dkt_type="rnn"):

        self.repr_type = repr_type
        self.dkt_type = dkt_type
        data = input_data
        d_skill = dict(zip(data["problem_id"].map(str), data["skill_name"].map(str)))
        data = data[["user_id", "problem_id", "correct"]]
        problems = sorted(list(set(data["problem_id"].map(str))))
        problem_correct = []
        p = {}
        up = {}
        for j, i in enumerate(problems):
            problem_correct.extend([i+"0", i+"1"])
            p[i+"0"] = 2*j
            p[i+"1"] = 2*j + 1
            up[i] = j
        self.p = p
        self.up = up
        g = list(data.groupby(["user_id"]))
        responses = []
        for i in range(len(g)):
            responses.append(len(g[i][1]))
        self.max_responses = max(responses)-1

    def dkt_data(self, data):

        users = len(set(data["user_id"]))
        g = list(data.groupby(["user_id"]))
        p = self.p
        up = self.up
        max_responses = self.max_responses
        input_shape = (users, max_responses, len(p))
        x_train = np.zeros((users, max_responses, len(p)), dtype=np.uint8)
        y_train = -np.ones((users, max_responses, 1), dtype=np.int8)
        y_train_order = np.zeros((users, max_responses, len(up)), dtype=np.int8)
        from datetime import datetime
        st = datetime.now()
        for i in range(len(g)):
            temp_data = g[i][1]
            counter = 0
            responses = max_responses# min(max_responses, len(g[i][1])-1)
            x1 = np.zeros((responses, len(p)))
            y1 = np.zeros((responses, len(up)))
            yy1 = np.zeros((responses, len(up)))
            for j in range(len(temp_data)-1):
                # x1[j, p[str(temp_data.iloc[j]["problem_id"])+str(temp_data.iloc[j]["correct"])] ] = 1
                x_train[i, j, p[str(temp_data.iloc[j]["problem_id"])+str(temp_data.iloc[j]["correct"])] ] = 1
                y_train_order[i, j, up[str(temp_data.iloc[j+1]["problem_id"])] ] = 1
                y_train[i, j, 0] = int(temp_data.iloc[j+1]["correct"])
                counter += 1
        return x_train, y_train, y_train_order

    def build_model(self, data, val_data, activation, hidden_layer_size=200):

        x_train, y_train, y_train_order = self.dkt_data(data)
        x_train_val, y_train_val, y_train_order_val = self.dkt_data(val_data)
        print("dkt line 6555555555")

        #My own code fix
        p=self.p
        up=self.up

        input_dim = len(p)
        input_dim_order = len(up)
        model = DKTnet(input_dim, input_dim_order, hidden_layer_size, activation, x_train, y_train, y_train_order, x_train_val, y_train_val, y_train_order_val)
        model = model.build()
        return model

    def dkt_representation(self, data, activation, hidden_layer_size):

        repr_type = self.repr_type
        d_skill = dict(zip(data["problem_id"].map(str), data["skill_name"].map(str)))
        print("=============dkt_representation=================")
        data = data[["user_id", "problem_id", "correct"]]
        problems = sorted(list(set(data["problem_id"].map(str))))
        print(problems)
        print(len(problems))
        problem_correct = []
        p = {}
        up = {}
        for j, i in enumerate(problems):
            problem_correct.extend([i+"0", i+"1"])
            p[i+"0"] = 2*j
            p[i+"1"] = 2*j + 1
            up[i] = j
        #quatity of problems is doubled, p
        #grand count of students is 478, users
        #the max number of response is 119, max_response, how many times of reponse for each student.
        g = list(data.groupby(["user_id"]))
        print("length of p is "+str(len(p)))
        print(p)
        print("length of up is " + str(len(up)))
        print(up)
        print("length of g is "+str(len(g)))

        responses = []
        for i in range(len(g)):
            responses.append(len(g[i][1]))
        max_responses = max(responses)-1

        print("max_response is: "+str(max_responses))
        print("len(up) is "+str(len(up)))

        users = len(g)
        input_shape = (users, max_responses, len(p))
        x_train = np.zeros((users, max_responses, len(p)), dtype=np.bool) # tensor structure
        y_train = np.zeros((users, max_responses, 1), dtype=np.float32)  # ??????????????????why set 1, the datatype should be 'float32' instead of the 'uint8'
        y_train_order = np.zeros((users, max_responses, len(up)), dtype=np.float32)
        from datetime import datetime
        st = datetime.now()
        for i in range(len(g)):    # len(g) is the total student number
            temp_data = g[i][1]
            counter = 0
            responses = min(max_responses, len(g[i][1])-1)
            x1 = np.zeros((responses, len(p)))
            y1 = np.zeros((responses, len(up)))
            yy1 = np.zeros((responses, len(up)))
            for j in range(responses):
                x1[j, p[str(temp_data.iloc[j]["problem_id"])+str(temp_data.iloc[j]["correct"])] ] = 1
                y_train_order[i, j, up[str(temp_data.iloc[j+1]["problem_id"])] ] = 1
                y_train[i, j, 0] = int(temp_data.iloc[j+1]["correct"])
                counter += 1
            if max_responses >=  len(temp_data):
                x2 = np.zeros((max_responses-len(temp_data)+1, len(p)))-np.ones((max_responses-len(temp_data)+1, len(p)))
                x_train[i] = np.concatenate((x1, x2))
            else:
                x_train[i] = x1

        print ("Shapes of x_train, y_train, order for dkt:", np.shape(x_train), np.shape(y_train), np.shape(y_train_order))

        en = datetime.now()
        input_dim = len(p)
        input_dim_order = len(up)
        model = DKTnet(input_dim, input_dim_order, hidden_layer_size, activation, x_train, y_train, y_train_order)
        model = model.build()
        repr_matrix = 0

        print("repr_type is {}".format(repr_type))

        for i in model.layers:
            for j in ((i.get_weights())):
                if repr_type=="dense" and list(np.shape(j)) == [input_dim_order, hidden_layer_size]:
                    repr_matrix = j
                    break
                if (not repr_type=="dense") and list(np.shape(j)) == [input_dim, hidden_layer_size]:
                    repr_matrix = j
                    break
        vector, problem_ids = [], []

        print("repr_matrix is {}".format(repr_matrix))
        print("repr_matrix shape is {}".format(repr_matrix.shape))

        for i, j in up.items():
            problem_ids.append(i)
            if repr_type=="correct-incorrect":
                vector.append(list(repr_matrix[p[i+"0"]])+list(repr_matrix[p[i+"1"]]))
            elif repr_type=="correct":
                vector.append(list(repr_matrix[p[i+"1"]]))
            elif repr_type=="incorrect":
                vector.append(list(repr_matrix[p[i+"0"]]))
            elif repr_type=="dense":
                print("====dense=====")
                print('i is {}'.format(i))
                print('up is {}'.format(up))
                vector.append(list(repr_matrix[up[str(i)]]))
            else:
                print ("Error")
        skill_vec = list(map(lambda x:d_skill[x], problem_ids))
        X = pd.DataFrame({"problem_id":problem_ids, "vector":vector, "skill_name":skill_vec})
        print("length of the vector is {}".format(len(vector)))
        print("skill_vec is {}".format(skill_vec))
        print ('Representation Done for dkt')
        return X