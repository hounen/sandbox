' ' Excel�t�@�C���̕ۑ���p�X
' Const ExcelFilePath = "C:\Users\tateishi\OneDrive\00_TaskManegement\output.xlsx"
' 
' ' �ǂݍ��ރt�@�C���p�X
' Const InputFilePath = ".\getTaskFolders.txt"
' 
' ' 1���̗�ԍ�
' Const FirstDayColumn = 2
' 
' ' �V�[�g��
' Const SheetName = "Calendar"
' 
' ' ���t�̏���
' Const DateFormat = "yyyy/mm/dd"
' 
' ' Excel�I�u�W�F�N�g�̐���
' Dim excelApp : Set excelApp = CreateObject("Excel.Application")
' excelApp.Visible = True
' 
' ' ���[�N�u�b�N�̐���
' Dim workbook : Set workbook = excelApp.Workbooks.Add()
' 
' ' ���[�N�V�[�g�̐���
' Dim worksheet : Set worksheet = workbook.Worksheets.Add()
' worksheet.Name = SheetName
' 
' ' �t�@�C���̓ǂݍ���
' Dim fs : Set fs = CreateObject("Scripting.FileSystemObject")
' Dim file : Set file = fs.OpenTextFile(InputFilePath, 1)
' 
' ' �J�����_�[�ɏ������ލs�̏�����
' Dim row : row = 2
' 
' ' �s�̓ǂݍ��݂ƃJ�����_�[�ւ̏�������
' Do Until file.AtEndOfStream
'     Dim line : line = file.ReadLine()
'     Dim dateStr : dateStr = Left(line, 8)
'     Dim memo : memo = Mid(line, 9)
' 
'     ' ���t�̎擾
'     Dim year : year = Left(dateStr, 4)
'     Dim month : month = Mid(dateStr, 5, 2)
'     Dim day : day = Right(dateStr, 2)
' 
'     ' �J�����_�[�ɏ������ރZ���̗�ԍ��̌v�Z
'     Dim column : column = FirstDayColumn + (day - 1)
' 
'     ' �Z���ɓ��t�ƃ�������������
'     worksheet.Cells(row, column).Value = DateSerial(year, month, day)
'     worksheet.Cells(row, column + 1).Value = memo
' 
'     ' ���̍s�ֈړ�
'     row = row + 1
' Loop
' 
' ' �t�@�C���̃N���[�Y
' file.Close()
' 
' ' ���t�̏����ݒ�
' Dim lastColumn : lastColumn = worksheet.UsedRange.Columns.Count
' Dim dateRange : Set dateRange = worksheet.Range(worksheet.Cells(2, FirstDayColumn), worksheet.Cells(row - 1, lastColumn - 1))
' dateRange.NumberFormat = DateFormat
' 
' ' Excel�t�@�C���̕ۑ�
' workbook.SaveAs ExcelFilePath
' 
' ' �I������
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

