# import subprocess
# 
# url = "http://localhost:8081/root/test/_attached/1725631935090YsLGq7UGEG"
# out = "1725631935090YsLGq7UGEG.png"
# command = f"Invoke-WebRequest -Uri {url} -OutFile {out}"
# 
# result = subprocess.run(["powershell", "-Command", command], capture_output=True, text=True)
# 
# # 実行結果を表示
# if result.returncode == 0:
#     print("Download successful.")
#     print(result.stdout)
# else:
#     print("Error occurred:")
#     print(result.stderr)

import subprocess

# 1. ログインしてクッキーを取得するPowerShellコマンド
login_url = "http://localhost:8081/signin?redirect=%2F"
username = "root"
password = "root"
login_command = f"""
$loginUrl = '{login_url}';
$loginForm = @{{'userName' = '{username}'; 'password' = '{password}'}};
$response = Invoke-WebRequest -Uri $loginUrl -Method Post -Body $loginForm -SessionVariable session;
$session;
"""

# 2. 画像ファイルをダウンロードするPowerShellコマンド
image_url = "http://localhost:8081/root/test/_attached/1725631935090YsLGq7UGEG"
output_file = "1725631935090YsLGq7UGEG.png"
download_command = f"""
$url = '{image_url}';
$out = '{output_file}';
Invoke-WebRequest -Uri $url -OutFile $out -WebSession $session;
"""

# 3. PowerShellコマンドをPythonから実行する
powershell_command = f"{login_command} {download_command}"
result = subprocess.run(["powershell", "-Command", powershell_command], capture_output=True, text=True)

# 実行結果を確認
if result.returncode == 0:
    print("Download successful.")
    print(result.stdout)
else:
    print("Error occurred:")
    print(result.stderr)