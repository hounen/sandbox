# encoding=UTF-8

import os
os.environ['KMP_DUPLICATE_LIB_OK']='True'

import numpy as np
from keras.models import Sequential
from keras.layers import Dense

# 学習データ（身長と体重のリスト）
heights = [150, 160, 170, 180, 190]
weights = [50, 60, 70, 80, 90]

# データの前処理
X_train = np.array(heights).reshape(-1, 1)  # 入力データを2次元配列に変換
y_train = np.array(weights)

# モデルの構築
model = Sequential()
model.add(Dense(1, input_shape=(1,)))  # 入力層として1つのニューロンを追加
model.compile(optimizer='adam', loss='mean_squared_error')

# モデルのトレーニング
model.fit(X_train, y_train, epochs=1000)

# 新しい生徒の身長から体重を予測
new_height = np.array([170]).reshape(-1, 1)
predicted_weight = model.predict(new_height)
print("Predicted weight:", predicted_weight)

