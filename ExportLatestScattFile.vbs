on error resume next


Public Function FormNum(value)
	res = FormatNumber(value, 3, -1, 0, 0)
    FormNum = Replace(res, ",", ".")
End Function

Sub QuitWithError(msg, retval)
	WScript.Echo msg
	WScript.Quit retval
End Sub

Function GetTheBase(filespec)
   Dim fso
   Set fso = CreateObject("Scripting.FileSystemObject")
   GetTheBase = fso.GetBaseName(filespec)
End Function

Function GetTheExtension(filespec)
   Dim fso
   Set fso = CreateObject("Scripting.FileSystemObject")
   GetTheExtension = fso.GetExtensionName(filespec)
End Function

Function GetSortedFileList(sDir)
	Dim fso		: Set fso = CreateObject("Scripting.FileSystemObject")
	Dim oShell	: Set oShell = CreateObject("WScript.Shell")
	Dim oExec	: Set oExec = oShell.Exec("%comspec% /c dir /A:-D /B /O:-D /T:W /S """ & sDir & """")
	Dim dicTmp	: Set dicTmp = CreateObject("Scripting.Dictionary")

	Do Until oExec.Stdout.AtEndOfStream
		dicTmp(fso.GetFile(oExec.Stdout.ReadLine())) = Empty
		'dicTmp(fso.GetFile(fso.BuildPath(sDir, oExec.Stdout.ReadLine()))) = Empty
	Loop
	If Not oExec.Stderr.AtEndOfStream Then
		WScript.Echo "Error:", oExec.Stderr.ReadAll()
	End If
	GetSortedFileList = dicTmp.Keys()

End Function

 
'Check Arguments
Dim DirSource
Dim DirTarget
Set objArgs = WScript.Arguments
if objArgs.Count < 1 then 
	DirSource = "C:\Scatt catalog"
	DirTarget = "."
elseIf objArgs.Count > 0 then 
	DirSource = objArgs(0)
	DirTarget = objArgs(1)
else
	QuitWithError "Usage: ScattCatalog_directory TargetDirectory", 1
end if

	

'open Filesystem folder
'set fso = createobject("scripting.filesystemobject") 
'set folder = fso.getfolder(objArgs(0))  


liste = GetSortedFileList(DirSource)

' process all files
For Each scatt in liste
	if GetTheExtension(scatt) = "scatt" then

		Dim doc
		Set doc = CreateObject("ScattDoc.ScattDocument")

		' Creating instance of the ScattDoc object, through which we will access
		' data. Scatt Professional application has to be installed.
		doc.FileName = scatt
		if doc Is Nothing then QuitWithError "Error: Scatt Professional was not installed properly.", 1
		doc.Load
		if Not doc.Valid then QuitWithError "Error: File not found or invalid file format.", 1

		Dim AllShots
		Set AllShots = doc.Aimings.Match.Shots
		if AllShots.Count = 0 then QuitWithError "Error: No match shots"

		' Open Output file
		'
		Dim outputFileName
		'outputFileName = scatt & ".csv"
		outputFileName = DirTarget & "\" & GetTheBase(scatt) & ".csv"


		Dim fso, outputFile
		Set fso = CreateObject("Scripting.FileSystemObject")
		Set outputFile = fso.CreateTextFile(outputFileName, True)
		if Err.Number <> 0 then QuitWithError "Error: Can't create output file " & outputFileName

		' Store all match shots in a variable AllShots
		'

		' Writing information about file
		'
		outputFile.WriteLine "Project Title,Group,Ammunition,Distance,Aim X,Aim Y,Center X,Center Y,Point X,Point Y,Velocity"
		dim ProjTitle, GroupNo, Ammu, Distance, AimX, AimY, CenterX, CenterY, PointX, PointY, Velocity
		ProjTitle = "Scatt"
		GroupNo = 1 'will be overridden
		Ammu = 4.5 'will be overridden
		Distance = 10 'will be overridden
		AimX = 0
		AimY = 0
		CenterX = 0
		CenterY = 0
		PointX = 0
		PointY = 0
		Velocity = 0

		' Iterate through all shots in a collection.


		Dim i
		For i = 1 To AllShots.Count
			Dim CurrentShot
			Set CurrentShot = AllShots(i)
			Ammu = CurrentShot.Attr.Event.Caliber
			Distance = CurrentShot.Attr.Event.Distance

			PointX = CurrentShot.BreachX
			PointY = CurrentShot.BreachY
			if(PointX <> 0 and PointY <> 0) then
				outputFile.WriteLine ProjTitle & "," & GroupNo & "," & FormNum(Ammu) & "," & FormNum(Distance) & "," & FormNum(AimX) & "," & FormNum(AimY) & "," & FormNum(CenterX) & "," & FormNum(CenterY) & "," & 	FormNum(PointX) & "," & FormNum(PointY) & "," & FormNum(Velocity)
			end if
			if (i mod 5) = 0 then GroupNo = GroupNo + 1
		Next
	end if
	Exit For
Next


