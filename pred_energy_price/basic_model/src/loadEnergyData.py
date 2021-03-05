import tensorflow as tf

# define data file paths
data_dir = 'C:/Projects/ISCTE/ADI/adi-energy-cost-analysis/pred_energy_price/basic_model/data'
df_train = data_dir + '/date_hour_price_2020_train.csv'
df_eval = data_dir + '/date_hour_price_2020_eval.csv'

# define columns
COLUMNS = ['hour', 'year', 'month', 'day', 'price']
RECORDS_ALL = [[0], [0], [0], [0], [0.0]]

# define de input function
def input_fn(data_file, batch_size, num_epoch = None):
    # import the data				
    def parse_csv(value):        
        columns = tf.io.decode_csv(value, record_defaults = RECORDS_ALL)        
        features = dict(zip(COLUMNS, columns))				
        labels =  features.pop('price')        
        return features, labels							
          
    # create the iterator
    dataset = (tf.compat.v1.data.TextLineDataset(data_file) # read text file       
        .skip(1) # skip header row       
        .map(parse_csv))			   
    dataset = dataset.repeat(num_epoch)    
    dataset = dataset.batch(batch_size) 
        				
    # consume the data   
    iterator = dataset.make_one_shot_iterator()    
    features, labels = iterator.get_next()    
    return features, labels	

next_batch = input_fn(df_train, batch_size = 1, num_epoch = None)
print(next_batch)
#with tf.compat.v1.Session() as sess:    
#    first_batch = sess.run(next_batch)    
#    print(first_batch)	

# define feature columns
X1= tf.feature_column.numeric_column('hour')
X2= tf.feature_column.numeric_column('year')
X3= tf.feature_column.numeric_column('month')
X4= tf.feature_column.numeric_column('day')
base_columns = [X1, X2, X3, X4]

# build the model
model = tf.estimator.LinearRegressor(feature_columns=base_columns, model_dir = './zz_train000')

# train the estimator
model.train(steps = 1000, input_fn = lambda : input_fn(df_train, batch_size = 128, num_epoch = None))

# evaluate the model
results = model.evaluate(steps = None, input_fn = lambda : input_fn(df_eval, batch_size = 128, num_epoch = 1))
for key in results:   
    print ('    {}, was: {}'.format(key, results[key]))

# prepare data for prediction
prediction_input = {				
          'hour': [0,0,0,12,12,12],				
          'year': [2021,2021,2021,2021,2021,2021],				
          'month': [1,1,1,1,1,1],				
          'day': [1,15,31,1,15,31]
        }

def test_input_fn():    
    dataset = tf.data.Dataset.from_tensors(prediction_input)    
    return dataset

def print_pred_input(pred_input, i, v):
    return '{:d}/{:d}/{:d} {:d}h -> {:f}€/MWh'.format(pred_input['year'][i], pred_input['month'][i], 
                                    pred_input['day'][i], pred_input['hour'][i], v['predictions'][0])

# predict     
pred_results = model.predict(input_fn=test_input_fn)	

for count, value in enumerate(pred_results):  
    print(print_pred_input(prediction_input, count, value))
