# encoding=UTF-8

import os
os.environ['KMP_DUPLICATE_LIB_OK']='True'

# from tensorflow.keras.models import Sequential
# from tensorflow.keras.layers import Dense, Activation
# from tensorflow.keras.utils import to_categorical
# import matplotlib.pyplot as plt
# from tensorflow.keras.models import load_model
# 
# import numpy as np

import tensorflow as tf
import numpy as np
import matplotlib.pyplot as plt

features = np.array([1.0, 2.0, 3.0, 4.0, 5.0])
labels = np.array([2.0, 4.0, 6.0, 8.0, 10.0])

model = tf.keras.Sequential()
model.add(tf.keras.layers.Dense(1, input_shape=(1,)))

loss_fn = tf.keras.losses.mean_squared_error

optimizer = tf.keras.optimizers.SGD(learning_rate=0.01)

model.compile(optimizer, loss_fn)
history = model.fit(features, labels, epochs=100)

new_data = np.array([6.0, 7.0, 8.0])
predictions = model.predict(new_data)
print(predictions)

plt.plot(history.history["loss"], label="loss", ls="-", marker="o")
plt.ylabel("loss")
plt.xlabel("epoch")
plt.legend(loc="best")
plt.savefig("brain.png")
plt.show()
