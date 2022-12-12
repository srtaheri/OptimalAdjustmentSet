from tabgan.sampler import OriginalGenerator, GANGenerator
import pandas as pd
import numpy as np
from sklearn import datasets, ensemble, model_selection, metrics
import seaborn as sns
import matplotlib.pyplot as plt


data = pd.read_csv('../obs_data_ecoli (1).csv')
#data = data.drop(0,axis=0)

# data200 = data.sample(n=200, random_state=1)
# data100 = data.sample(n=100, random_state=1)
# data50 = data.sample(n=50,  random_state=1)
# data20 = data.sample(n=20, random_state=1)
# data10 = data.sample(n=10,  random_state=1)

target = pd.DataFrame(data['ATRN_HUMAN'])
train = pd.DataFrame(data.drop(['ATRN_HUMAN'], axis=1))

test = train

# generate data
# new_train1, new_target1 = OriginalGenerator().generate_data_pipe(train, target, test, )

new_train, new_target = GANGenerator(gen_x_times=1).generate_data_pipe(train, target, train, only_generated_data=True)
new_train['ATRN_HUMAN'] = new_target

def writefile(df,filename):
    df.to_csv(filename, index=False)

# new_train3, new_target3 = GANGenerator(gen_x_times=1.1, cat_cols=None,
#            bot_filter_quantile=0.001, top_filter_quantile=0.999, is_post_process=True,
#            adversarial_model_params={
#                "metrics": "AUC", "max_depth": 10, "max_bin": 100,"num_leaves":25,
#                "learning_rate": 0.01, "random_state": 42, "n_estimators": 1000,
#            }, pregeneration_frac=2, only_generated_data=True,
#            gan_params = {"batch_size": 1000, "patience": 50, "epochs" : 500,}).generate_data_pipe(train, target,
#                                           test, deep_copy=True, only_adversarial=False, use_adversarial=True)
new_train = data.merge(data2, on='ATRN_HUMAN')
writefile(new_train,'ov_gan/newtrain4ov.csv')

############# analysis plots #################

for (columnName, columnData) in a.iteritems():
    fig, axes = plt.subplots(1, 2)

    axes[0].set_title('gan'+columnName)
    axes[1].set_title('original'+columnName)
    new_train[columnName].hist(ax=axes[0],bins =50,range=(a[columnName].min()-1, a[columnName].max()+1))
    data[columnName].hist(ax=axes[1],bins =50,range=(a[columnName].min()-1, a[columnName].max()+1))
    plt.savefig('hist_compare_50'+columnName+'.png')
    plt.show()


sns_plot = sns.pairplot(new_train.iloc[:, :10 ], height=2.5)

cormat = new_train.corr()
round(cormat,4)

cormat = data.corr()
round(cormat,4)