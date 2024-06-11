from datetime import datetime

now = datetime.now()
current_time = now.strftime("%Y-%m-%d %H:%M:%S")

file_path = "../output/iris_py.txt"

with open(file_path, 'a') as file:
    file.write(current_time + "\n")