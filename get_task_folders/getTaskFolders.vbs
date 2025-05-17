Dim FSObj
Dim OutFileObj
Dim NetWorkObj
Dim VmxFileObj

Set NetworkObj = CreateObject("WScript.Network")
HostNameStr = NetworkObj.ComputerName

Set FSObj = WScript.CreateObject("Scripting.FileSystemObject")
TopFolder = FSObj.GetParentFolderName(WScript.ScriptFullName)

OutFilePath = TopFolder & "\" & FSObj.GetBaseName(WScript.ScriptFullName) & ".txt"
If FSObj.FileExists(OutFilePath) = 0 Then
  FSObj.CreateTextFile(OutFilePath)
Else
  FSObj.DeleteFile(OutFilePath)
  FSObj.CreateTextFile(OutFilePath)
End If

Set OutFileObj = FSObj.OpenTextFile(OutFilePath, 8)

For d = 0 To 4
	strFormattedDate = DateAdd("d", d, Now())
	strFormattedDate = Left(strFormattedDate, 10)
	strFormattedDate = Replace(strFormattedDate, "/", "")
	OutFileObj.WriteLine strFormattedDate & "-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------"
Next

For w = 1 To 3
	strFormattedDate = GetMonDate(w)
	strFormattedDate = Replace(strFormattedDate, "/", "")
	OutFileObj.WriteLine strFormattedDate & "-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------"
Next

For m = 1 To 3
	strFormattedDate = DateSerial(Year(Date()), Month(Date()) + m, 1)
	strFormattedDate = Replace(strFormattedDate, "/", "")
	OutFileObj.WriteLine strFormattedDate & "-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------"
Next

For y = 1 To 3
	strFormattedDate = DateSerial(Year(Date()) + y, 1, 1)
	strFormattedDate = Replace(strFormattedDate, "/", "")
	OutFileObj.WriteLine strFormattedDate & "___ " & Year(Date()) - 1986 + y & "-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------"
Next

For y = 1 To 3
	strFormattedDate = DateSerial(Year(Date()) + y*10, 1, 1)
	strFormattedDate = Replace(strFormattedDate, "/", "")
	OutFileObj.WriteLine strFormattedDate & "___ " & Year(Date()) - 1986 + y*10 & "-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------"
Next

OutVmFoldersInfo FSObj.GetFolder(TopFolder)

VmxFileObj.Close
OutFileObj.Close

Dim WshShellObj
Set WshShellObj = WScript.CreateObject("WScript.Shell")
WshShellObj.Run "sort" & " /r /o " & OutFilePath & " " & OutFilePath

' ' Reload
' EdtFilePath = TopFolder & "\" & FSObj.GetBaseName(WScript.ScriptFullName) & "_tmp.txt"
' OutFilePath = TopFolder & "\" & FSObj.GetBaseName(WScript.ScriptFullName) & ".txt"
' 
' If FSObj.FileExists(OutFilePath) = 0 Then
'   FSObj.CreateTextFile(OutFilePath)
' Else
'   FSObj.DeleteFile(OutFilePath)
'   FSObj.CreateTextFile(OutFilePath)
' End If
' 
' Set OutFileObj = FSObj.OpenTextFile(OutFilePath, 8)
' 
' Set File = FSObj.OpenTextFile(EdtFilePath, 1)
' Do While Not File.AtEndOfStream
'   line = File.ReadLine
'   ' ここでlineを処理する
' 	strYMD = Left(line, 8)
' 	If IsDateString(strYMD) Then
' 	  Dim weekNum
' 	  weekNum = GetWeekdayFromDate(strYMD)
' 	  OutFileObj.WriteLine String(weekNum, strYMD) & line
' 	Else
' 	  OutFileObj.WriteLine line
' 	End If
' Loop

Set WshShellObj = Nothing
Set OutFileObj = Nothing
Set NetworkObj = Nothing
Set FSObj = Nothing

'---

Sub OutVmFoldersInfo(Folder)
  For Each File in Folder.Files
    Fname = File.name
    Extension = LCase(FSObj.GetExtensionName(Fname))
    Select Case Extension

      Case "progress"
        Set VmxFileObj = FSObj.OpenTextFile(Folder & "\" &Fname, 1, 0)
        Do Until VmxFileObj.AtEndOfStream
          Dim LineStr
          LineStr = VmxFileObj.ReadLine
          Dim DispNameItemStr
          DispNameItemStr = "displayName = "
          If Instr(LineStr, DispNameItemStr) <> 0 Then
            Dim DispNameStr
            DispNameStr = Replace(Mid(LineStr, Len(DispNameItemStr) + 1), """", "")
            Exit Do
          End If
        Loop

				ListOut Folder,NULL

    End Select
  Next

  For Each Vmfolder in Folder.SubFolders
    OutVmFoldersInfo Vmfolder
  Next
End Sub

'---

Sub ListOut(TargetFolder, OutStrA)

	OutFileObj.WriteLine FSObj.GetFileName(TargetFolder) & " <<< " & FSObj.GetParentFolderName(TargetFolder)

'   "|<." & HostNameStr &_
'   "|<." & OutStrA &_
'   "|<." & TargetFolder &_
'   "|>." & FormatNumber(TargetFolder.Size, 0) &_
'   "|>." & Left(TargetFolder.DateCreated, 10) &_
'   "|>." & Left(TargetFolder.DateLastModified, 10) &_
'   "|"
'   "|>." & TargetFolder.DateLastAccessed &_
End Sub

Function GetMonDate(WeekCount)
  Dim myMon

  Select Case Weekday(Now())
    Case 1
    	myMon = DateAdd( "d", 7*WeekCount, Date)  - 6
    Case 2
    	myMon = DateAdd( "d", 7*WeekCount, Date)
		Case Else
    	myMon = DateAdd( "d", 7*WeekCount, Date) - Weekday(DateAdd( "d", 7*WeekCount, Date)) + 2
  End Select

	GetMonDate = myMon
End Function

Function GetFirstDay(MyDate)
    Dim strDate
    
    strDate = DateAdd("m", 1, MyDate)
    
    'strDate = Year(strDate) & "/" & Right("0" & Month(strDate), 2) & "/01"

		strDate = DateSerial(Year(strDate), Month(strDate), 1)
    
    GetLastDay = strDate
End Function

Function IsDateString(dateString)
  If Len(dateString) <> 8 Then
    IsDateString = False
    Exit Function
  End If
  If Not IsNumeric(dateString) Then
    IsDateString = False
    Exit Function
  End If
  Dim year, month, day
  year = Left(dateString, 4)
  month = Mid(dateString, 5, 2)
  day = Right(dateString, 2)
  If Not IsDate(year & "-" & month & "-" & day) Then
    IsDateString = False
    Exit Function
  End If
  IsDateString = True
End Function

Function GetWeekdayFromDate(dateString)
  ' dateStringはyyyymmdd形式の8桁の日付文字列
  
  ' 年、月、日を切り出す
	Dim year, month, day
  year = Left(dateString, 4)
  month = Mid(dateString, 5, 2)
  day = Right(dateString, 2)
  
  ' DateSerial関数で日付オブジェクトを作成
  dateObj = DateSerial(year, month, day)
  
  ' Weekday関数で曜日を数値で取得（月曜を0とする）
  weekdayNum = Weekday(dateObj, vbMonday)
  
  GetWeekdayFromDate = weekdayNum - 1
End Function
