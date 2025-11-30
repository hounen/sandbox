import importlib
import glob

import matplotlib.pyplot as plt
import numpy as np

def run():

    # experimental data
    # x: 高さ (m)
    # y: 時間 (s)

    x = [0, 0.36, 0.49, 0.56, 0.64, 0.71]
    y = [0, 0.73, 1.0, 1.71, 2.0, 1.8]

    cf1 = ["最小2乗法（1次式）", lambda x, y: np.polyfit(x, y, 1)]
    cf2 = ["最小2乗法（2次式）", lambda x, y: np.polyfit(x, y, 2)]
    cf3 = ["最小2乗法（3次式）", lambda x, y: np.polyfit(x, y, 3)]
    cf4 = ["最小2乗法（4次式）", lambda x, y: np.polyfit(x, y, 4)]
    cf5 = ["最小2乗法（5次式）", lambda x, y: np.polyfit(x, y, 5)]
    cf6 = ["最小2乗法（6次式）", lambda x, y: np.polyfit(x, y, 6)]
    cf7 = ["最小2乗法（7次式）", lambda x, y: np.polyfit(x, y, 7)]
    cf8 = ["最小2乗法（8次式）", lambda x, y: np.polyfit(x, y, 8)]
    cf9 = ["最小2乗法（9次式）", lambda x, y: np.polyfit(x, y, 9)]


    plt.plot(x, y, marker='o', linewidth=0)   # 折れ線グラフを描く
    plt.show()       # グラフを表示




