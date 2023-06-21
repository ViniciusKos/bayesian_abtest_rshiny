import numpy as np
import pandas as pd
from scipy import stats

def get_prob_and_loss( data , sample_size = None):

    vals = {}
    prob_test_better_control = []
    expected_loss_control = []
    expected_loss_test = []


    for day in range( len ( data ) ):

        for v in ["control", "test"]:

            #Generate Beta distribution params
            vals[f"u_{v}"] , vals[f"var_{v}"] = stats.beta.stats( a = 1 + data.loc[day, f'acc_{v}_clicks'], \
                    b = 1 + (data.loc[day, f'acc_{v}_visits'] - data.loc[day, f'acc_{v}_clicks']), moments='mv')

            
            #Generate Normal distribution
            vals[f"x_{v}"] = np.random.normal( loc = vals[f"u_{v}"], scale = 1.25*np.sqrt( vals[f"var_{v}"] ) ,size = sample_size)

            #Generate Beta PDF
            vals[f"f{v}"] = stats.beta.pdf ( vals[f"x_{v}"], 
                            a = 1 + data.loc[day, f'acc_{v}_clicks'], 
                            b = 1 + (data.loc[day, f'acc_{v}_visits'] - data.loc[day, f'acc_{v}_clicks']) )
            
            #Generate Normal PDF
            vals[f"g{v}"] = stats.norm.pdf( vals[f"x_{v}"], loc = vals[f"u_{v}"], scale = 1.25*np.sqrt( vals[f"var_{v}"] ) )


        # Beta / Normal
        y = ( vals['fcontrol'] * vals['ftest'] ) / ( vals['gcontrol'] * vals['gtest'] )
        yb = y[ vals['x_test'] >= vals['x_control'] ]
            
        #Calculate probabilities
        p = ( 1/ sample_size ) * np.sum(yb)


        loss_control = (1 / sample_size ) * np.sum ( ( ( vals['x_test'] - vals['x_control'] )*y) [vals['x_test'] >= vals['x_control']] )
        loss_test = (1 / sample_size ) * np.sum ( ( ( vals['x_control'] - vals['x_test'] )*y) [vals['x_control'] >= vals['x_test']] )

        prob_test_better_control.append(p)
        expected_loss_control.append( loss_control )
        expected_loss_test.append( loss_test )


    return prob_test_better_control, expected_loss_control, expected_loss_test


df = pd.read_csv("data_experiment.csv")
df1 = df.copy()
df1['group'] = df1['group'].replace( {"treatment":"test"})
for i in ['visits','clicks']:
    df1[i] = df1[i].astype(int)
df1 = df1.reset_index().rename( columns={'index':'day'} )
df1 = df1.pivot_table( index='day', columns='group', values=df1.select_dtypes(exclude='object').columns ,aggfunc='sum')
df1 = df1.swaplevel( axis=1 )
df1 = df1.reindex(sorted(df1.columns), axis=1).fillna(0)
df1.columns = ["_".join(i) for i in df1.columns]
for i in df1.columns:
    df1[f"acc_{i}"] = df1[i].cumsum()

proba_b_better_a, expected_loss_A, expected_loss_B = get_prob_and_loss(df1, sample_size=1000)

x1 = np.arange( len(proba_b_better_a) )

df2 = pd.DataFrame( list(zip(x1, proba_b_better_a, expected_loss_A, expected_loss_B)), columns = ["x1", "proba_b_better_a", "expected_loss_A", "expected_loss_B"])
df2.to_csv("data_experiment_probas.csv", index=False)