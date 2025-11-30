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

    coeffs = np.polyfit(x, y, 2)
    print(coeffs)
    y_fit = np.polyval(coeffs, x)

    plt.xlabel("time (s)")
    plt.ylabel("height (m)")
    plt.plot(x, y, marker='o', linewidth=0)   # 折れ線グラフを描く
    plt.plot(x, y_fit, linestyle='-', color='black', label="最小2乗法（1次式）")  
    plt.savefig("./images/calc.png")  # グラフを画像ファイルとして保存