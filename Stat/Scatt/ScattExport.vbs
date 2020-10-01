' This sample demonstrates how to extract x and y coordinates
' from Scatt file
' Copyright (c) 2002 ZAO Scatt

on error resume next


Public Function FormNum(value)
	res = FormatNumber(value, 3, -1, 0, 0)
    FormNum = Replace(res, ",", ".")
End Function

Sub QuitWithError(msg, retval)
	WScript.Echo msg
	WScript.Quit retval
End Sub



' Did user specified file name?'
' first argument SCATT-Filename 
' 2nd argument f-coefficient
Set objArgs = WScript.Arguments
if objArgs.Count < 1 then QuitWithError "Usage: SAMPLES filename.scatt output.txt", 1



' Creating instance of the ScattDoc object, through which we will access
' data. Scatt Professional application has to be installed.
Dim doc
Set doc = CreateObject("ScattDoc.ScattDocument")
doc.FileName = objArgs(0)
if doc Is Nothing then QuitWithError "Error: Scatt Professional was not installed properly.", 1
doc.Load
if Not doc.Valid then QuitWithError "Error: File not found or invalid file format.", 1
Dim AllShots
Set AllShots = doc.Aimings.Match.Shots
if AllShots.Count = 0 then QuitWithError "Error: No match shots"

' Open Output file
'
Dim outputFileName
if objArgs.Count = 1 then outputFileName = objArgs(0) & ".csv" else outputFileName = objArgs(1)


Dim fso, outputFile
Set fso = CreateObject("Scripting.FileSystemObject")
Set outputFile = fso.CreateTextFile(outputFileName, True)
if Err.Number <> 0 then QuitWithError "Error: Can't create output file " & objArgs(1)

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


	outputFile.WriteLine ProjTitle & "," & GroupNo & "," & FormNum(Ammu) & "," & FormNum(Distance) & "," & FormNum(AimX) & "," & FormNum(AimY) & "," & FormNum(CenterX) & "," & FormNum(CenterY) & "," & FormNum(PointX) & "," & FormNum(PointY) & "," & FormNum(Velocity)
	if (i mod 5) = 0 then GroupNo = GroupNo + 1
Next

			