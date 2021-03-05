import tensorflow as tf

# define data file paths
data_dir = "C:/Projects/ISCTE/ADI/adi-energy-cost-analysis/pred_energy_price/basic_model/data"
df_train = data_dir + "/date_hour_price_2020_train.csv"
df_eval = data_dir + "date_hour_price_2020_eval.csv"

# define columns
COLUMNS = ["hour", "date", "price"]
RECORDS_ALL = [[0], [0], [0.0]]

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
X2= tf.feature_column.numeric_column('date')
base_columns = [X1, X2]

# build the model
model = tf.estimator.LinearRegressor(feature_columns=base_columns, model_dir = '../zz_train000')

# train the estimator
model.train(steps = 1000, input_fn = lambda : input_fn(df_train, batch_size = 128, num_epoch = None))

# evaluate the model
results = model.evaluate(steps = None, input_fn = lambda : input_fn(df_eval, batch_size = 128, num_epoch = 1))
for key in results:   
    print ("    {}, was: {}".format(key, results[key]))
