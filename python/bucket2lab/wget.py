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

import gitlab

# GitLabインスタンスに接続
gl = gitlab.Gitlab('https://gitlab.example.com', private_token='YOUR_PRIVATE_TOKEN')

# プロジェクトを取得
project = gl.projects.get('your_project_id')

# ファイルをアップロード
with open('your_file.txt', 'rb') as f:
    upload_data = project.upload(f)

# アップロードされたファイルのURL
file_url = upload_data['url']
print(f"Uploaded file URL: {file_url}")

# Issueを作成
issue = project.issues.create({
    'title': 'Issue with file upload',
    'description': f'The uploaded file is available [here]({file_url})',
})

# 既存のIssueを取得
issue = project.issues.get(issue_id)

# Noteを追加
note = issue.notes.create({
    'body': f'The uploaded file is available [here]({file_url})',
})