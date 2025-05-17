' ' Excelファイルの保存先パス
' Const ExcelFilePath = "C:\Users\tateishi\OneDrive\00_TaskManegement\output.xlsx"
' 
' ' 読み込むファイルパス
' Const InputFilePath = ".\getTaskFolders.txt"
' 
' ' 1日の列番号
' Const FirstDayColumn = 2
' 
' ' シート名
' Const SheetName = "Calendar"
' 
' ' 日付の書式
' Const DateFormat = "yyyy/mm/dd"
' 
' ' Excelオブジェクトの生成
' Dim excelApp : Set excelApp = CreateObject("Excel.Application")
' excelApp.Visible = True
' 
' ' ワークブックの生成
' Dim workbook : Set workbook = excelApp.Workbooks.Add()
' 
' ' ワークシートの生成
' Dim worksheet : Set worksheet = workbook.Worksheets.Add()
' worksheet.Name = SheetName
' 
' ' ファイルの読み込み
' Dim fs : Set fs = CreateObject("Scripting.FileSystemObject")
' Dim file : Set file = fs.OpenTextFile(InputFilePath, 1)
' 
' ' カレンダーに書き込む行の初期化
' Dim row : row = 2
' 
' ' 行の読み込みとカレンダーへの書き込み
' Do Until file.AtEndOfStream
'     Dim line : line = file.ReadLine()
'     Dim dateStr : dateStr = Left(line, 8)
'     Dim memo : memo = Mid(line, 9)
' 
'     ' 日付の取得
'     Dim year : year = Left(dateStr, 4)
'     Dim month : month = Mid(dateStr, 5, 2)
'     Dim day : day = Right(dateStr, 2)
' 
'     ' カレンダーに書き込むセルの列番号の計算
'     Dim column : column = FirstDayColumn + (day - 1)
' 
'     ' セルに日付とメモを書き込む
'     worksheet.Cells(row, column).Value = DateSerial(year, month, day)
'     worksheet.Cells(row, column + 1).Value = memo
' 
'     ' 次の行へ移動
'     row = row + 1
' Loop
' 
' ' ファイルのクローズ
' file.Close()
' 
' ' 日付の書式設定
' Dim lastColumn : lastColumn = worksheet.UsedRange.Columns.Count
' Dim dateRange : Set dateRange = worksheet.Range(worksheet.Cells(2, FirstDayColumn), worksheet.Cells(row - 1, lastColumn - 1))
' dateRange.NumberFormat = DateFormat
' 
' ' Excelファイルの保存
' workbook.SaveAs ExcelFilePath
' 
' ' 終了処理
' workbook.Close
' excelApp.Quit
' Set excelApp = Nothing

Set objExcel = CreateObject("Excel.Application")
objExcel.Visible = True
Set objWorkbook = objExcel.Workbooks.Add()
Set objWorksheet = objWorkbook.Worksheets(1)

intRow = 1
intColumn = 1

Set objFSO = CreateObject("Scripting.FileSystemObject")
Set objTextFile = objFSO.OpenTextFile(".\getTaskFolders.txt", 1)

Do Until objTextFile.AtEndOfStream
    strLine = objTextFile.ReadLine
		' strLine = Left(strLine, 8)
		' MsgBox(Left(strLine,4))
    ' MsgBox(Mid(strLine, 5, 2))
		' MsgBox(Right(strLine, 2))
    dtDate = CDate(Left(strLine, 4) & "/" & Mid(strLine, 5, 2) & "/" & Mid(strLine, 7, 2))
    objWorksheet.Cells(intRow, intColumn) = dtDate
    intColumn = intColumn + 1
    If Weekday(dtDate) = 2 And intColumn > 1 Then intColumn = 1: intRow = intRow + 1
Loop

objTextFile.Close
objExcel.Quit

