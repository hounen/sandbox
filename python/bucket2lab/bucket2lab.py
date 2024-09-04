

# ローカルのGitBucketをGitHubに移行する（その２）
# https://kwatanabe.hatenablog.jp/entry/2020/10/11/130324#GitBucket-%E3%81%8B%E3%82%89-Issue-%E3%82%92%E5%8F%96%E3%82%8A%E5%87%BA%E3%81%99
# ローカルのGitBucketをGitHubに移行する（その３）
# https://kwatanabe.hatenablog.jp/entry/2020/10/22/191134

# from GitBucket

# issueのリストを取得する
# issueを登録する
# commentを登録する

# APIにリクエストを送信
response = requests.get(url)

# レスポンスが正常か確認
if response.status_code == 200:
    # JSONデータをパース
    data = response.json()
    # パースしたデータを表示
    print(data)
else:
    print(f"エラー: {response.status_code}")

# 名前にアクセス
print(data['name'])  # John
# 年齢にアクセス
print(data['age'])   # 30

# to GitLab
issues = project.issues.list()

# delete all issue
for issue in issues:
    issue.delete()

for issue in issues:
    print("-------------------")
    print("【タイトル】", issue.title)
    print("【Description】", issue.description)
    print("【状態】", issue.state)
    print("【Assignee】", issue.assignee["name"])
    print("【Due date】", issue.due_date)
    print("【Labels】", issue.labels)

# tags

# labels

# issues
new_issue = project.issues.create({ \
    "title":"title"
    ,"description":"description"
    ,"due_date":"2020-10-20"
})

# issue.title =
# issue.description =
# issue.due_date =
issue.labels = ['label1', 'label2']
issue.assignee_ids = username
issue.save()

# comment
issue.notes.create({"body": "some comment"})

print(new_issue.id)

