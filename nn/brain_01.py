# encoding=UTF-8

import os
os.environ['KMP_DUPLICATE_LIB_OK']='True'

from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, Activation
from tensorflow.keras.utils import to_categorical
import matplotlib.pyplot as plt
from tensorflow.keras.models import load_model

import numpy as np

# ƒf[ƒ^‚Ì€”õ
X_data = np.arange(1, 10001).reshape(-1, 1) # np.array([[1],[2],[3]]) # np.random.randint(0, 256, size=(5000, 2))
y_data = np.arange(2, 20002, 2).reshape(-1, 1) # np.array([[2],[4],[6]]) # np.random.randint(0, 256, size=(5000, 1))
 
# ƒf[ƒ^‚Ì•ªŠ„
split_point = int(0.7 * X_data.shape[0])
X_train, X_test = np.split(X_data, [split_point])
y_train, y_test = np.split(y_data, [split_point])

# ”]‚Ì\’z
model = Sequential()
model.add(Dense(1, input_dim=1, activation="linear"))
# model.compile(optimizer="sgd", loss="categorical_crossentropy", metrics=["accuracy"])
model.compile(optimizer="sgd", loss="mean_squared_error", metrics=[])

# ”]‚ÌŒP—û
# history = model.fit(X_train, y_train, verbose=1, epochs=3)
history = model.fit(X_train, y_train, epochs=100, batch_size=32)

# ”]‚Ì•Û‘¶
model.save("brain.h5")

# ”]‚Ì“Ç
model = load_model("brain.h5")

# ”]‚Ì•]‰¿
score = model.evaluate(X_test, y_test, verbose=1)

# ”]‚Ì—\Œ¾
# pred = model.predict(np.array([[125,125],[0,0],[255,255]]))
pred = model.predict(np.array([[4]]))
print(pred)

# # ƒOƒ‰ƒt‚Ì•\¦
# plt.plot(history.history["accuracy"], label="accuracy", ls="-", marker="o")
# plt.ylabel("accuracy")
# plt.xlabel("epoch")
# plt.legend(loc="best")
# plt.savefig("brain.png")
# plt.show()

