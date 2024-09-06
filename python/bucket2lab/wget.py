import subprocess

url = "http://localhost:8081/root/test/_attached/1725631935090YsLGq7UGEG"
out = "1725631935090YsLGq7UGEG.png"
command = f"Invoke-WebRequest -Uri {url} -OutFile {out}"

result = subprocess.run(["powershell", "-Command", command], capture_output=True, text=True)

# 実行結果を表示
if result.returncode == 0:
    print("Download successful.")
    print(result.stdout)
else:
    print("Error occurred:")
    print(result.stderr)

# wget  -OutFile 1725631935090YsLGq7UGEG.png