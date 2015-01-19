import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import os
import cPickle as pickle
from dateutil.parser import parse

os.chdir('/home/lj/ML/NuBank')


app_data = pd.read_csv('NuBank_Application.csv')
loan_data = pd.read_csv('NuBank_LoanPerformance.csv')

loan_ids = [x.lower()[:10] for x in loan_data.idLoan]

known_id = []

for customer_id in app_data.customer_id:
    if customer_id in loan_ids:
        known_id.append(customer_id)
        print customer_id

for loan_id in loan_ids:
    print loan_id in app_data.customer_id

loan_flag_dict = dict(zip(loan_ids, loan_data.flgGood))


bad_ids = []
for customer_id in app_data.customer_id:
    try:
        print customer_id, loan_flag_dict[customer_id]
    except:
        bad_ids.append(customer_id)


y = []
for customer_id in app_data.customer_id:
    try:
        y.append(loan_flag_dict[customer_id])
    except:
        y.append('')



app_data['y'] = y

def flags2binary(x):
    if x == 'Good':
        return 1        
    elif x == 'Bad':
        return -1
    else:
        return x


app_data['y'] = app_data['y'].apply(flags2binary)
app_data.to_csv('data.csv')

def timedelta_to_years(td):
    x = float(str(td).split()[0])
    return x/(1e9*3600*24*365.25)


data = pd.read_csv('data.csv')
data['Credit_Line_approved_pct'] = data['Credit_Line_approved'] / data['Credit_Line_requested']
data['application_when'] = data['application_when'].apply(parse)
data['birth_date'] = data['birth_date'].apply(parse)
data['applicant_age'] = data['application_when'] - data['birth_date']
data['applicant_age'] = data['applicant_age'].apply(timedelta_to_years)

data.dropna(axis=0, subset=['y'], inplace=True)

data.to_csv('data.csv')




