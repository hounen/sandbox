# encoding=UTF-8

import os
os.environ['KMP_DUPLICATE_LIB_OK']='True'

import numpy as np
from keras.models import Sequential
from keras.layers import Dense

heights = [150, 160, 170, 180, 190]
weights = [50, 60, 70, 80, 90]

X_train = np.array(heights).reshape(-1, 1)
y_train = np.array(weights)

model = Sequential()
model.add(Dense(1, input_shape=(1,)))
model.compile(optimizer='adam', loss='mean_squared_error')

model.fit(X_train, y_train, epochs=1000)

new_height = np.array([170]).reshape(-1, 1)
predicted_weight = model.predict(new_height)
print("Predicted weight:", predicted_weight)

