
new_issue = project.issues.create({ \
    "title":"title"
    ,"description":"description"
    ,"due_date":"2020-10-20"
})

print(new_issue.id)