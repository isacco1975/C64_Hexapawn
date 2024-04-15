'
' **************************************************************
' * HEXAPAWN 1.0 - C. 2024 IGP (ISAAC GARCIA PEVERI) TECH BLOG *
' * DATE WRITTEN:  04/04/2024                                  *
' * DATE COMPILED: 12/04/2024                                  *
' * LAST EDITED:   12/04/2024                                  *
' * COMPILER:      XC BASIC 3 FOR WINDOWS X64                  *
' **************************************************************
' *                                                            *
' *  changes: 12/04/2024 many bug fixes on some moves.         *
' *                                                            *
' **************************************************************
' * this software is based upon HER created by Roberto Rampini *
' * (C3B3 MOVE IS NOT PERMITTED AS FIRST)                      *
' *                                                            *
' * I have a new 2.00 version with all the permitted moves     *
' * with also c3b3 at start. Unfortunately I have to find a    *
' * way to rewrite the software, because runs out of 50KB!     *
' *                                                            *
' * MAX POSSIBILITIES FOR PLAYER TO WIN IN THIS VERSION: 7     *
' * (after 6 player wins, the machine becomes unbeatable!)     *
' **************************************************************
' * Unfortunately XC BASIC is still under development, so there*
' * are still some bugs calling routines and it is not still   *
' * possible to create lists of arrays.                        *
' **************************************************************

' **** WORKING-STORAGE
Const maxBoxes = 24
Const maxCells = 9

Dim gameGrid(9) As Byte
Dim pawn(6) As String*1 ' A PAWN TO DRAW
Dim playerWins As Byte
Dim cpuWins As Byte
Dim boxExtracted As Byte
Dim winner As Byte
Dim numMove As Byte

' THE MATCHBOXES
Dim box1(9)  As Byte
Dim box2(9)  As Byte
Dim box3(9)  As Byte
Dim box4(9)  As Byte
Dim box5(9)  As Byte
Dim box6(9)  As Byte
Dim box7(9)  As Byte
Dim box8(9)  As Byte
Dim box9(9)  As Byte
Dim box10(9) As Byte
Dim box11(9) As Byte
Dim box12(9) As Byte
Dim box13(9) As Byte
Dim box14(9) As Byte
Dim box15(9) As Byte
Dim box16(9) As Byte
Dim box17(9) As Byte
Dim box18(9) As Byte
Dim box19(9) As Byte
Dim box20(9) As Byte
Dim box21(9) As Byte
Dim box22(9) As Byte
Dim box23(9) As Byte
Dim box24(9) As Byte

' THE SPHERE BOXES
Dim sbx1(5)  As String*1
Dim sbx2(5)  As String*1
Dim sbx3(5)  As String*1
Dim sbx4(5)  As String*1
Dim sbx5(5)  As String*1
Dim sbx6(5)  As String*1
Dim sbx7(5)  As String*1
Dim sbx8(5)  As String*1
Dim sbx9(5)  As String*1
Dim sbx10(5) As String*1
Dim sbx11(5) As String*1
Dim sbx12(5) As String*1
Dim sbx13(5) As String*1
Dim sbx14(5) As String*1
Dim sbx15(5) As String*1
Dim sbx16(5) As String*1
Dim sbx17(5) As String*1
Dim sbx18(5) As String*1
Dim sbx19(5) As String*1
Dim sbx20(5) As String*1
Dim sbx21(5) As String*1
Dim sbx22(5) As String*1
Dim sbx23(5) As String*1
Dim sbx24(5) As String*1
' -------------------------------------------------------------

' **** INITIALIZE SINGLE PAWN CHARACTER BUFFER AND MATCH BOXES
Sub Init() Static
   playerWins = 0: cpuWins = 0: winner = 0: numMove = 0

   ' PAWN SHAPE
   pawn(0) = " "
   pawn(1) = CHR$(230)
   pawn(2) = " "
   pawn(3) = CHR$(230)
   pawn(4) = CHR$(230)
   pawn(5) = CHR$(230)

   ' MATCHBOXES INITIALIZE
   box1(0) = 2 :  box2(0) = 2 :  box3(0) = 2 :  box4(0) = 0 :  box5(0) = 2
   box1(1) = 2 :  box2(1) = 2 :  box3(1) = 0 :  box4(1) = 2 :  box5(1) = 0
   box1(2) = 2 :  box2(2) = 2 :  box3(2) = 2 :  box4(2) = 2 :  box5(2) = 2
   box1(3) = 1 :  box2(3) = 0 :  box3(3) = 2 :  box4(3) = 1 :  box5(3) = 1
   box1(4) = 0 :  box2(4) = 1 :  box3(4) = 1 :  box4(4) = 2 :  box5(4) = 1
   box1(5) = 0 :  box2(5) = 0 :  box3(5) = 0 :  box4(5) = 0 :  box5(5) = 0
   box1(6) = 0 :  box2(6) = 1 :  box3(6) = 0 :  box4(6) = 0 :  box5(6) = 0
   box1(7) = 1 :  box2(7) = 0 :  box3(7) = 0 :  box4(7) = 0 :  box5(7) = 1
   box1(8) = 1 :  box2(8) = 1 :  box3(8) = 1 :  box4(8) = 1 :  box5(8) = 0

   sbx1(0) = "c": sbx2(0) = "c": sbx3(0) = "c": sbx4(0) = "c": sbx5(0) ="c"
   sbx1(1) = "y": sbx2(1) = "y": sbx3(1) = "y": sbx4(1) = "y": sbx5(1) ="y"
   sbx1(2) = "m": sbx2(2) = " ": sbx3(2) = "m": sbx4(2) = "m": sbx5(2) ="m"
   sbx1(3) = " ": sbx2(3) = " ": sbx3(3) = "k": sbx4(3) = "k": sbx5(3) =" "
   sbx1(4) = " ": sbx2(4) = " ": sbx3(4) = " ": sbx4(4) = " ": sbx5(4) =" "

   box6(0) = 2 :  box7(0) = 0 :  box8(0) = 0 :  box9(0) =  2:  box10(0) = 2
   box6(1) = 2 :  box7(1) = 2 :  box8(1) = 2 :  box9(1) =  0:  box10(1) = 2
   box6(2) = 0 :  box7(2) = 2 :  box8(2) = 2 :  box9(2) =  2:  box10(2) = 0
   box6(3) = 1 :  box7(3) = 0 :  box8(3) = 2 :  box9(3) =  2:  box10(3) = 1
   box6(4) = 0 :  box7(4) = 2 :  box8(4) = 1 :  box9(4) =  0:  box10(4) = 1
   box6(5) = 1 :  box7(5) = 1 :  box8(5) = 1 :  box9(5) =  1:  box10(5) = 2
   box6(6) = 0 :  box7(6) = 1 :  box8(6) = 1 :  box9(6) =  0:  box10(6) = 0
   box6(7) = 0 :  box7(7) = 0 :  box8(7) = 0 :  box9(7) =  1:  box10(7) = 0
   box6(8) = 1 :  box7(8) = 0 :  box8(8) = 0 :  box9(8) =  0:  box10(8) = 1

   sbx6(0) = "c": sbx7(0) = "c": sbx8(0) = "c": sbx9(0) = "c": sbx10(0) ="c"
   sbx6(1) = "y": sbx7(1) = "y": sbx8(1) = "y": sbx9(1) = "y": sbx10(1) ="y"
   sbx6(2) = "m": sbx7(2) = "m": sbx8(2) = " ": sbx9(2) = " ": sbx10(2) =" "
   sbx6(3) = " ": sbx7(3) = " ": sbx8(3) = " ": sbx9(3) = " ": sbx10(3) =" "
   sbx6(4) = " ": sbx7(4) = " ": sbx8(4) = " ": sbx9(4) = " ": sbx10(4) =" "

   box11(0) = 0 :  box12(0) = 0 :  box13(0) = 2 :  box14(0) = 0 :  box15(0) = 2
   box11(1) = 2 :  box12(1) = 2 :  box13(1) = 0 :  box14(1) = 0 :  box15(1) = 0
   box11(2) = 2 :  box12(2) = 2 :  box13(2) = 2 :  box14(2) = 2 :  box15(2) = 0
   box11(3) = 0 :  box12(3) = 0 :  box13(3) = 1 :  box14(3) = 2 :  box15(3) = 1
   box11(4) = 1 :  box12(4) = 1 :  box13(4) = 0 :  box14(4) = 2 :  box15(4) = 1
   box11(5) = 0 :  box12(5) = 0 :  box13(5) = 0 :  box14(5) = 1 :  box15(5) = 1
   box11(6) = 0 :  box12(6) = 1 :  box13(6) = 0 :  box14(6) = 0 :  box15(6) = 0
   box11(7) = 0 :  box12(7) = 0 :  box13(7) = 0 :  box14(7) = 0 :  box15(7) = 0
   box11(8) = 1 :  box12(8) = 0 :  box13(8) = 1 :  box14(8) = 0 :  box15(8) = 0

   sbx11(0) = "c": sbx12(0) = "c": sbx13(0) = "c": sbx14(0) = "c": sbx15(0) ="c"
   sbx11(1) = "y": sbx12(1) = "y": sbx13(1) = " ": sbx14(1) = "y": sbx15(1) =" "
   sbx11(2) = " ": sbx12(2) = " ": sbx13(2) = " ": sbx14(2) = " ": sbx15(2) =" "
   sbx11(3) = " ": sbx12(3) = " ": sbx13(3) = " ": sbx14(3) = " ": sbx15(3) =" "
   sbx11(4) = " ": sbx12(4) = " ": sbx13(4) = " ": sbx14(4) = " ": sbx15(4) =" "

   box16(0) = 0 :  box17(0) = 0 :  box18(0) = 2 :  box19(0) = 0 :  box20(0) = 0
   box16(1) = 2 :  box17(1) = 2 :  box18(1) = 0 :  box19(1) = 0 :  box20(1) = 0
   box16(2) = 0 :  box17(2) = 0 :  box18(2) = 0 :  box19(2) = 2 :  box20(2) = 2
   box16(3) = 2 :  box17(3) = 1 :  box18(3) = 2 :  box19(3) = 1 :  box20(3) = 2
   box16(4) = 1 :  box17(4) = 1 :  box18(4) = 2 :  box19(4) = 2 :  box20(4) = 1
   box16(5) = 1 :  box17(5) = 2 :  box18(5) = 1 :  box19(5) = 2 :  box20(5) = 0
   box16(6) = 0 :  box17(6) = 0 :  box18(6) = 0 :  box19(6) = 0 :  box20(6) = 0
   box16(7) = 0 :  box17(7) = 0 :  box18(7) = 0 :  box19(7) = 0 :  box20(7) = 0
   box16(8) = 0 :  box17(8) = 0 :  box18(8) = 0 :  box19(8) = 0 :  box20(8) = 0

   sbx16(0) = "c": sbx17(0) = "c": sbx18(0) = "c": sbx19(0) = "c": sbx20(0) ="c"
   sbx16(1) = "y": sbx17(1) = "y": sbx18(1) = "y": sbx19(1) = "y": sbx20(1) ="y"
   sbx16(2) = " ": sbx17(2) = " ": sbx18(2) = " ": sbx19(2) = " ": sbx20(2) ="m"
   sbx16(3) = " ": sbx17(3) = " ": sbx18(3) = " ": sbx19(3) = " ": sbx20(3) =" "
   sbx16(4) = " ": sbx17(4) = " ": sbx18(4) = " ": sbx19(4) = " ": sbx20(4) =" "

   box21(0) = 0 :  box22(0) = 0 :  box23(0) = 2 :  box24(0) = 0
   box21(1) = 2 :  box22(1) = 2 :  box23(1) = 0 :  box24(1) = 0
   box21(2) = 0 :  box22(2) = 0 :  box23(2) = 0 :  box24(2) = 2
   box21(3) = 1 :  box22(3) = 0 :  box23(3) = 2 :  box24(3) = 0
   box21(4) = 2 :  box22(4) = 1 :  box23(4) = 1 :  box24(4) = 1
   box21(5) = 0 :  box22(5) = 2 :  box23(5) = 0 :  box24(5) = 2
   box21(6) = 0 :  box22(6) = 0 :  box23(6) = 0 :  box24(6) = 0
   box21(7) = 0 :  box22(7) = 0 :  box23(7) = 0 :  box24(7) = 0
   box21(8) = 0 :  box22(8) = 0 :  box23(8) = 0 :  box24(8) = 0

   sbx21(0) = "c": sbx22(0) = "c": sbx23(0) = "c": sbx24(0) = "c"
   sbx21(1) = "y": sbx22(1) = "y": sbx23(1) = "y": sbx24(1) = "y"
   sbx21(2) = " ": sbx22(2) = " ": sbx23(2) = " ": sbx24(2) = " "
   sbx21(3) = " ": sbx22(3) = " ": sbx23(3) = " ": sbx24(3) = " "
   sbx21(4) = " ": sbx22(4) = " ": sbx23(4) = " ": sbx24(4) = " "
End Sub
' -------------------------------------------------------------

' *** IF PLAYER WINS, CLEARS SELETCED SPHERE FROM BUFFER
Sub ClearSelectedSpheres() Static
   sbx1(4) = " "
   sbx2(4) = " "
   sbx3(4) = " "
   sbx4(4) = " "
   sbx5(4) = " "
   sbx6(4) = " "
   sbx7(4) = " "
   sbx8(4) = " "
   sbx9(4) = " "
   sbx10(4) = " "
   sbx11(4) = " "
   sbx12(4) = " "
   sbx13(4) = " "
   sbx14(4) = " "
   sbx15(4) = " "
   sbx16(4) = " "
   sbx17(4) = " "
   sbx18(4) = " "
   sbx19(4) = " "
   sbx20(4) = " "
   sbx21(4) = " "
   sbx22(4) = " "
   sbx23(4) = " "
   sbx24(4) = " "
End Sub
' -------------------------------------------------------------

' *** CLEAR THE SCREEN
Sub ClearScreen(pBorderColor As Byte, pBackColor As Byte) Static
   Print CHR$(147)
   Border pBorderColor: Background pBackColor: POKE 646,1
End Sub
' -------------------------------------------------------------

' *** If PLAYER WINS, THE ONE THAT CPU USED TO LOSE THE GAME IS REMOVED
Sub RemoveSphere(pBox As Byte) Static
   Select Case pBox
       Case 1
           If sbx1(4) = "c" Then sbx1(0) = " "
           If sbx1(4) = "y" Then sbx1(1) = " "
           If sbx1(4) = "m" Then sbx1(2) = " "

       Case 2
           If sbx2(4) = "c" Then sbx2(0) = " "
           If sbx2(4) = "y" Then sbx2(1) = " "

       Case 3
           If sbx3(4) = "c" Then sbx3(0) = " "
           If sbx3(4) = "y" Then sbx3(1) = " "
           If sbx3(4) = "m" Then sbx3(2) = " "
           If sbx3(4) = "k" Then sbx3(3) = " "

       Case 4
           If sbx4(4) = "c" Then sbx4(0) = " "
           If sbx4(4) = "y" Then sbx4(1) = " "
           If sbx4(4) = "m" Then sbx4(2) = " "
           If sbx4(4) = "k" Then sbx4(3) = " "

       Case 5
           If sbx5(4) = "c" Then sbx5(0) = " "
           If sbx5(4) = "y" Then sbx5(1) = " "
           If sbx5(4) = "m" Then sbx5(2) = " "

       Case 6
           If sbx6(4) = "c" Then sbx6(0) = " "
           If sbx6(4) = "y" Then sbx6(1) = " "
           If sbx6(4) = "m" Then sbx6(2) = " "

       Case 7
           If sbx7(4) = "c" Then sbx7(0) = " "
           If sbx7(4) = "y" Then sbx7(1) = " "
           If sbx7(4) = "m" Then sbx7(2) = " "

       Case 8
           If sbx8(4) = "c" Then sbx8(0) = " "
           If sbx8(4) = "y" Then sbx8(1) = " "

       Case 9
           If sbx9(4) = "c" Then sbx9(0) = " "
           If sbx9(4) = "y" Then sbx9(1) = " "

       Case 10
           If sbx10(4) = "c" Then sbx10(0) = " "
           If sbx10(4) = "y" Then sbx10(1) = " "

       Case 11
           If sbx11(4) = "c" Then sbx11(0) = " "
           If sbx11(4) = "y" Then sbx11(1) = " "

       Case 12
           If sbx12(4) = "c" Then sbx12(0) = " "
           If sbx12(4) = "y" Then sbx12(1) = " "

       Case 13
           If sbx13(4) = "c" Then sbx13(0) = " "

       Case 14
           If sbx14(4) = "c" Then sbx14(0) = " "
           If sbx14(4) = "y" Then sbx14(1) = " "

       Case 15
           If sbx15(4) = "c" Then sbx15(0) = " "

       Case 16
           If sbx16(4) = "c" Then sbx16(0) = " "
           If sbx16(4) = "y" Then sbx16(1) = " "

       Case 17
           If sbx17(4) = "c" Then sbx17(0) = " "
           If sbx17(4) = "y" Then sbx17(1) = " "

       Case 18
           If sbx18(4) = "c" Then sbx18(0) = " "
           If sbx18(4) = "y" Then sbx18(1) = " "

       Case 19
           If sbx19(4) = "c" Then sbx19(0) = " "
           If sbx19(4) = "y" Then sbx19(1) = " "

       Case 20
           If sbx20(4) = "c" Then sbx20(0) = " "
           If sbx20(4) = "y" Then sbx20(1) = " "
           If sbx20(4) = "m" Then sbx20(2) = " "

       Case 21
           If sbx21(4) = "c" Then sbx21(0) = " "
           If sbx21(4) = "y" Then sbx21(1) = " "

       Case 22
           If sbx22(4) = "c" Then sbx22(0) = " "
           If sbx22(4) = "y" Then sbx22(1) = " "

       Case 19
           If sbx23(4) = "c" Then sbx23(0) = " "
           If sbx23(4) = "y" Then sbx23(1) = " "

       Case 19
           If sbx24(4) = "c" Then sbx24(0) = " "
           If sbx24(4) = "y" Then sbx24(1) = " "
   End Select

   Call ClearSelectedSpheres()
End Sub
' -------------------------------------------------------------

' *** PRINTS THE GRID. THIS ROUTINE CAN BE OPTIMIZED MORE.
Sub ShowGrid(pColor As Byte) Static
   Dim wRow As Byte
   Dim wCol As Byte
   Poke 646,pColor

   wRow = 8: wCol = 1
   Locate 13,wRow: Print CHR$(176)

   For column As Byte = 0 To 10
      Locate 13 + wCol,wRow: Print CHR$(197)
      wCol = wCol + 1
   Next

   wCol = 1
   Locate 13 + (column + 1),wRow: Print CHR$(174)

   For idx As Byte = 0 To 1
      wRow = wRow + 1
      Locate 13,wRow
      Print CHR$(116)
      Locate 17,wRow
      Print CHR$(104)
      Locate 21,wRow
      Print CHR$(104)
      Locate 25,wRow
      Print CHR$(121)
   Next

   wRow = wRow + 1
   Locate 13,wRow: Print CHR$(116)

   For column As Byte = 0 To 10
      Locate 13 + wCol,wRow: Print CHR$(195)
      wCol = wCol + 1
   Next

   wCol = 1
   Locate 13 + (column + 1),wRow: Print CHR$(121)

   For idx As Byte = 0 To 1
      wRow = wRow + 1
      Locate 13,wRow
      Print CHR$(116)
      Locate 17,wRow
      Print CHR$(104)
      Locate 21,wRow
      Print CHR$(104)
      Locate 25,wRow
      Print CHR$(121)
   Next

   wRow = wRow + 1
   Locate 13,wRow: Print CHR$(116)

   For column As Byte = 0 To 10
      Locate 13 + wCol,wRow: Print CHR$(195)
      wCol = wCol + 1
   Next

   wCol = 1
   Locate 13 + column + 1,wRow: Print CHR$(121)

   For idx As Byte = 0 To 1
      wRow = wRow + 1
      Locate 13,wRow
      Print CHR$(116)
      Locate 17,wRow
      Print CHR$(104)
      Locate 21,wRow
      Print CHR$(104)
      Locate 25,wRow
      Print CHR$(121)
   Next

   wRow = wRow + 1
   Locate 13,wRow: Print CHR$(173)

   For column As Byte = 0 To 10
      Locate 13 + wCol,wRow: Print CHR$(102)
      wCol = wCol + 1
   Next

   wCol = 1
   Locate 13 + column + 1,wRow: Print CHR$(189)
End Sub
' -------------------------------------------------------------

' *** GAME TITLE ON THE SCREEN
Sub ShowTitle(pColor As Byte) Static
   Poke 646, pColor

   Locate 1, 3
   Print "esapedone 1.8 - by isaac garcia peveri"

   Locate 1, 5
   Print "realizzato per leonardo del canale yt:"

   Locate 6, 6
   Print "<<< mille e una avventura >>>    "
End Sub
' -------------------------------------------------------------

' *** A SINGLE PAWN: PLACE ON THE GRID OR REMOVE IT
Sub PawnOnGrid(pAction As String*6, pType As String*3, pPosition As Byte) Static
   Dim wCol As Byte, wRow As Byte

   If pType = "CPU" Then
      Poke 646,7
      gameGrid(pPosition) = 2
   End If
   If pType = "PLY" Then
      Poke 646,1
      gameGrid(pPosition) = 1
   End If
   If pType = "   " Then
      Poke 646,0
      gameGrid(pPosition) = 0
   End If

   Select Case pPosition 'CALCULATES COORDINATES AUTOMATICALLY FROM IDX
       Case 0
            wCol = 14: wRow = 9
       Case 1
            wCol = 18: wRow = 9
       Case 2
            wCol = 22: wRow = 9
       Case 6
            wCol = 14: wRow = 15
       Case 7
            wCol = 18: wRow = 15
       Case 8
            wCol = 22: wRow = 15
       Case 3
            wCol = 14: wRow = 12
       Case 4
            wCol = 18: wRow = 12
       Case 5
            wCol = 22: wRow = 12
   End Select

   Dim startCol As Byte
   startCol = wCol

   For idx As Byte = 0 To 2
      If pAction = "PLACE " Then
         Locate wCol,wRow: Print pawn(idx)
      Else
         Locate wCol,wRow: Print " "
      End If
      wCol = wCol + 1
   Next

   wRow = wRow + 1: wCol = startCol

   For idx As Byte = 3 To 5
      If pAction = "PLACE " Then
         Locate wCol,wRow: Print pawn(idx)
      Else
         Locate wCol,wRow: Print " "
      End If
      wCol = wCol + 1
   Next
End Sub
' -------------------------------------------------------------

' *** PLACING INITIAL PAWNS ON THE BOARD
Sub PlaceInitialPawns() Static
   Call PawnOnGrid("PLACE ", "CPU", 0)
   Call PawnOnGrid("PLACE ", "CPU", 1)
   Call PawnOnGrid("PLACE ", "CPU", 2)

   Call PawnOnGrid("REMOVE", "   ", 3)
   Call PawnOnGrid("REMOVE", "   ", 4)
   Call PawnOnGrid("REMOVE", "   ", 5)

   Call PawnOnGrid("PLACE ", "PLY", 6)
   Call PawnOnGrid("PLACE ", "PLY", 7)
   Call PawnOnGrid("PLACE ", "PLY", 8)
End Sub
' -------------------------------------------------------------

'*** LETTERS AND NUMBERS ON THE GRID
Sub BoardNumbers(color As Byte) Static
   Poke 646,color

   Locate 12,10:  Print "a"
   Locate 12,13:  Print "b"
   Locate 12,16:  Print "c"

   Locate 15,18:  Print "1"
   Locate 19,18:  Print "2"
   Locate 23,18:  Print "3"
End Sub
' -------------------------------------------------------------

' *** CHECK WHO WINS
Sub CheckWinner() Static
    winner = 0

    If gameGrid(0) = 1 _
    Or gameGrid(1) = 1 _
    Or gameGrid(2) = 1 Then
       winner = 1
    Else
       If gameGrid(6) = 2 _
       Or gameGrid(7) = 2 _
       Or gameGrid(8) = 2 Then
          winner = 2
       Else
          Dim isWhitePawnFound As Byte: isWhitePawnFound = 0

          For idx As Byte = 0 To maxCells - 1
             If gameGrid(idx) = 1 Then
                isWhitePawnFound = 1
                Exit For
             End If
          Next

          If isWhitePawnFound = 0 Then
             winner = 2
          End If
       End If
    End If

    If  gameGrid(0) = 2 And gameGrid(1) = 0 And gameGrid(2) = 0 _
    And gameGrid(3) = 1 And gameGrid(4) = 0 And gameGrid(5) = 2 _
    And gameGrid(6) = 0 And gameGrid(7) = 0 And gameGrid(8) = 1 Then
        winner = 2
    End If

    If  gameGrid(0) = 0 And gameGrid(1) = 2 And gameGrid(2) = 0 _
    And gameGrid(3) = 0 And gameGrid(4) = 1 And gameGrid(5) = 0 _
    And gameGrid(6) = 0 And gameGrid(7) = 0 And gameGrid(8) = 0 Then
        winner = 1
    End If

    If  gameGrid(0) = 0 And gameGrid(1) = 2 And gameGrid(2) = 0 _
    And gameGrid(3) = 0 And gameGrid(4) = 1 And gameGrid(5) = 2 _
    And gameGrid(6) = 0 And gameGrid(7) = 0 And gameGrid(8) = 1 Then
        winner = 2
    End If

    If  gameGrid(0) = 0 And gameGrid(1) = 2 And gameGrid(2) = 0 _
    And gameGrid(3) = 2 And gameGrid(4) = 1 And gameGrid(5) = 0 _
    And gameGrid(6) = 1 And gameGrid(7) = 0 And gameGrid(8) = 0 Then
        winner = 2
    End If

    If  gameGrid(0) = 2 And gameGrid(1) = 0 And gameGrid(2) = 0 _
    And gameGrid(3) = 1 And gameGrid(4) = 2 And gameGrid(5) = 0 _
    And gameGrid(6) = 0 And gameGrid(7) = 1 And gameGrid(8) = 0 Then
        winner = 2
    End If

    If  gameGrid(0) = 2 And gameGrid(1) = 0 And gameGrid(2) = 0 _
    And gameGrid(3) = 1 And gameGrid(4) = 2 And gameGrid(5) = 0 _
    And gameGrid(6) = 0 And gameGrid(7) = 1 And gameGrid(8) = 0 Then
        winner = 2
    End If

    If  gameGrid(0) = 0 And gameGrid(1) = 0 And gameGrid(2) = 2 _
    And gameGrid(3) = 0 And gameGrid(4) = 2 And gameGrid(5) = 1 _
    And gameGrid(6) = 0 And gameGrid(7) = 1 And gameGrid(8) = 0 Then
        winner = 2
    End If

    If  gameGrid(0) = 2 And gameGrid(1) = 0 And gameGrid(2) = 2 _
    And gameGrid(3) = 1 And gameGrid(4) = 2 And gameGrid(5) = 1 _
    And gameGrid(6) = 0 And gameGrid(7) = 1 And gameGrid(8) = 0 Then
        winner = 1
    End If
End Sub
' -------------------------------------------------------------

' CPU MOVE: EXTRACTS A SPHERE FROM THE SELECTED MATCHBOX SCHEME
Sub ExtractSphere(pBox As Byte) Static
    Dim sphereNum As Int
    sphereNum = 0
    Randomize TI()

    Select Case pBox
       Case 1
            sphereNum = CInt(Rnd()*3) + 1

            If sphereNum = 1 And sbx1(0) <> " " Then
               If gameGrid(3) = 1 And gameGrid(1) = 2 Then
                  Call PawnOnGrid("REMOVE", "   ", 1)
                  Call PawnOnGrid("PLACE ", "CPU", 3)
                  sbx1(4) = sbx1(0)
               Else
                  sphereNum = sphereNum + 1
               End If
            End If

            If sphereNum = 2 And sbx1(1) <> " " Then
               If gameGrid(4) = 0 And gameGrid(1) = 2 Then
                  Call PawnOnGrid("REMOVE", "   ", 1)
                  Call PawnOnGrid("PLACE ", "CPU", 4)
                  sbx1(4) = sbx1(1)
               Else
                  sphereNum = sphereNum + 1
               End if
            End If

            If sphereNum = 3 And sbx1(2) <> " " Then
               If gameGrid(5) = 0 And gameGrid(2) = 2 Then
                  Call PawnOnGrid("REMOVE", "   ", 2)
                  Call PawnOnGrid("PLACE ", "CPU", 5)
                  sbx1(4) = sbx1(2)
               End If
            End if

       Case 2
            sphereNum = CInt(Rnd()*2) + 1

            If sphereNum = 1 And sbx2(0) <> " " Then
               If gameGrid(3) = 0 And gameGrid(0) = 2 Then
                  Call PawnOnGrid("REMOVE", "   ", 0)
                  Call PawnOnGrid("PLACE ", "CPU", 3)
                  sbx2(4) = sbx2(0)
               Else
                  sphereNum = sphereNum + 1
               End If
            End If

            If sphereNum = 2 And sbx2(1) <> " " Then
               If gameGrid(4) = 1 And gameGrid(0) = 2 Then
                  Call PawnOnGrid("REMOVE", "   ", 0)
                  Call PawnOnGrid("PLACE ", "CPU", 4)
                  sbx2(4) = sbx2(1)
               End If
            End If

       Case 3
            sphereNum = CInt(Rnd()*4) + 1

            If sphereNum = 1 And sbx3(0) <> " " Then
               If gameGrid(6) = 0 And gameGrid(3) = 2 Then
                  Call PawnOnGrid("REMOVE", "   ", 3)
                  Call PawnOnGrid("PLACE ", "CPU", 6)
                  sbx3(4) = sbx3(0)
               Else
                  sphereNum = sphereNum + 1
               End If
            End If

            If sphereNum = 2 And sbx3(1) <> " " Then
               If gameGrid(4) = 1 And gameGrid(0) = 2 Then
                  Call PawnOnGrid("REMOVE", "   ", 0)
                  Call PawnOnGrid("PLACE ", "CPU", 4)
                  sbx3(4) = sbx3(1)
               Else
                  sphereNum = sphereNum + 1
               End if
            End If

            If sphereNum = 3 And sbx3(2) <> " " Then
               If gameGrid(4) = 1 And gameGrid(2) = 2 Then
                  Call PawnOnGrid("REMOVE", "   ", 2)
                  Call PawnOnGrid("PLACE ", "CPU", 4)
                  sbx3(4) = sbx3(2)
               Else
                  sphereNum = sphereNum + 1
               End If
            End if

            If sphereNum = 4 And sbx3(3) <> " " Then
               If gameGrid(5) = 1 And gameGrid(2) = 2 Then
                  Call PawnOnGrid("REMOVE", "   ", 2)
                  Call PawnOnGrid("PLACE ", "CPU", 5)
                  sbx3(4) = sbx3(3)
               End If
            End if

       Case 4
            sphereNum = CInt(Rnd()*4) + 1

            If sphereNum = 1 And sbx4(0) <> " " Then
               If gameGrid(3) = 1 And gameGrid(1) = 2 Then
                  Call PawnOnGrid("REMOVE", "   ", 1)
                  Call PawnOnGrid("PLACE ", "CPU", 3)
                  sbx4(4) = sbx4(0)
               Else
                  sphereNum = sphereNum + 1
               End If
            End If

            If sphereNum = 2 And sbx4(1) <> " "  Then
               If gameGrid(7) = 0 And gameGrid(4) = 2 Then
                  Call PawnOnGrid("REMOVE", "   ", 4)
                  Call PawnOnGrid("PLACE ", "CPU", 7)
                  sbx4(4) = sbx4(1)
               Else
                  sphereNum = sphereNum + 1
               End If
            End If

            If sphereNum = 3 And sbx4(2) <> " " Then
               If gameGrid(8) = 1 And gameGrid(4) = 2 Then
                  Call PawnOnGrid("REMOVE", "   ", 4)
                  Call PawnOnGrid("PLACE ", "CPU", 8)
                  sbx4(4) = sbx4(2)
               Else
                  sphereNum = sphereNum + 1
               End If
            End If

            If sphereNum = 4 And sbx4(3) <> " " Then
               If gameGrid(5) = 0 And gameGrid(2) = 2 Then
                  Call PawnOnGrid("REMOVE", "   ", 2)
                  Call PawnOnGrid("PLACE ", "CPU", 5)
                  sbx4(4) = sbx4(3)
               Else
                  sphereNum = sphereNum + 1
               End If
            End If

       Case 5
            sphereNum = CInt(Rnd()*3) + 1

            If sphereNum = 1 And sbx5(0) <> " " Then
               If gameGrid(4) = 1 And gameGrid(0) = 2 Then
                  Call PawnOnGrid("REMOVE", "   ", 0)
                  Call PawnOnGrid("PLACE ", "CPU", 4)
                  sbx5(4) = sbx5(0)
               Else
                  sphereNum = sphereNum + 1
               End If
            End If

            If sphereNum = 2 And sbx5(1) <> " "  Then
               If gameGrid(4) = 1 And gameGrid(2) = 2 Then
                  Call PawnOnGrid("REMOVE", "   ", 2)
                  Call PawnOnGrid("PLACE ", "CPU", 4)
                  sbx5(4) = sbx5(1)
               Else
                  sphereNum = sphereNum + 1
               End If
            End If

            If sphereNum = 2 And sbx5(2) <> " "  Then
               If gameGrid(5) = 0 And gameGrid(2) = 2 Then
                  Call PawnOnGrid("REMOVE", "   ", 2)
                  Call PawnOnGrid("PLACE ", "CPU", 5)
                  sbx5(4) = sbx5(2)
               End If
            End If

       Case 6
            sphereNum = CInt(Rnd()*3) + 1

            If sphereNum = 1 And sbx6(0) <> " "  Then
               If gameGrid(1) = 2 And gameGrid(3) = 1 Then
                  Call PawnOnGrid("REMOVE", "   ", 1)
                  Call PawnOnGrid("PLACE ", "CPU", 3)
                  sbx6(4) = sbx6(0)
               Else
                  sphereNum = sphereNum + 1
               End If
            End If

            If sphereNum = 2 And sbx6(1) <> " "   Then
               If gameGrid(1) = 2 And gameGrid(5) = 0 Then
                  Call PawnOnGrid("REMOVE", "   ", 1)
                  Call PawnOnGrid("PLACE ", "CPU", 5)
                  sbx6(4) = sbx6(1)
               Else
                  sphereNum = sphereNum + 1
               End If
            End If

            If sphereNum = 3 And sbx6(2) <> " "   Then
               If gameGrid(1) = 2 And gameGrid(4) = 1 Then
                  Call PawnOnGrid("REMOVE", "   ", 1)
                  Call PawnOnGrid("PLACE ", "CPU", 4)
                  sbx6(4) = sbx6(2)
               End If
            End If

       Case 7
            sphereNum = CInt(Rnd()*3) + 1

            If sphereNum = 1 And sbx7(0) <> " "   Then
               If gameGrid(4) = 2 And gameGrid(6) = 1 Then
                  Call PawnOnGrid("REMOVE", "   ", 4)
                  Call PawnOnGrid("PLACE ", "CPU", 6)
                  sbx7(4) = sbx7(0)
               Else
                  sphereNum = sphereNum + 1
               End If
            End If

            If sphereNum = 2 And sbx7(1) <> " "  Then
               If gameGrid(4) = 2 And gameGrid(7) = 0 Then
                  Call PawnOnGrid("REMOVE", "   ", 4)
                  Call PawnOnGrid("PLACE ", "CPU", 7)
                  sbx7(4) = sbx7(1)
               Else
                  sphereNum = sphereNum + 1
               End If
            End If

            If sphereNum = 3 And sbx7(2) <> " "  Then
               If gameGrid(1) = 2 And gameGrid(5) = 1 Then
                  Call PawnOnGrid("REMOVE", "   ", 1)
                  Call PawnOnGrid("PLACE ", "CPU", 5)
                  sbx7(4) = sbx7(2)
               End If
            End If

       Case 8
            sphereNum = CInt(Rnd()*2) + 1

            If sphereNum = 1 And sbx8(0) <> " "   Then
               If gameGrid(1) = 2 And gameGrid(5) = 1 Then
                  Call PawnOnGrid("REMOVE", "   ", 1)
                  Call PawnOnGrid("PLACE ", "CPU", 5)
                  sbx8(4) = sbx8(0)
               Else
                  sphereNum = sphereNum + 1
               End If
            End If

            If sphereNum = 2 And sbx8(1) <> " "   Then
               If gameGrid(2) = 2 And gameGrid(4) = 1 Then
                  Call PawnOnGrid("REMOVE", "   ", 2)
                  Call PawnOnGrid("PLACE ", "CPU", 4)
                  sbx8(4) = sbx8(1)
               End If
            End If

       Case 9
            sphereNum = CInt(Rnd()*2) + 1

            If sphereNum = 1 And sbx9(0) <> " "   Then
               If gameGrid(3) = 2 And gameGrid(6) = 0 Then
                  Call PawnOnGrid("REMOVE", "   ", 3)
                  Call PawnOnGrid("PLACE ", "CPU", 6)
                  sbx9(4) = sbx9(0)
               Else
                  sphereNum = sphereNum + 1
               End If
            End If

            If sphereNum = 2 And sbx9(1) <> " "   Then
               If gameGrid(3) = 2 And gameGrid(7) = 1 Then
                  Call PawnOnGrid("REMOVE", "   ", 3)
                  Call PawnOnGrid("PLACE ", "CPU", 7)
                  sbx9(4) = sbx9(1)
               End If
            End If

       Case 10
            sphereNum = CInt(Rnd()*2) + 1

            If sphereNum = 1 And sbx10(0) <> " " Then
               If gameGrid(4) = 1 And gameGrid(0) = 2 Then
                  Call PawnOnGrid("REMOVE", "   ", 0)
                  Call PawnOnGrid("PLACE ", "CPU", 4)
                  sbx10(4) = sbx10(0)
               Else
                  sphereNum = sphereNum + 1
               End If
            End If

            If sphereNum = 2 And sbx10(1) <> " " Then
               If gameGrid(1) = 2 And gameGrid(3) = 1 Then
                  Call PawnOnGrid("REMOVE", "   ", 1)
                  Call PawnOnGrid("PLACE ", "CPU", 3)
                  sbx10(4) = sbx10(1)
               End If
            End If

       Case 11
            sphereNum = CInt(Rnd()*2) + 1

            If sphereNum = 1 And sbx11(0) <> " " Then
               If gameGrid(2) = 2 And gameGrid(4) = 1 Then
                  Call PawnOnGrid("REMOVE", "   ", 2)
                  Call PawnOnGrid("PLACE ", "CPU", 4)
                  sbx11(4) = sbx11(0)
               Else
                  sphereNum = sphereNum + 1
               End If
            End If

            If sphereNum = 2 And sbx11(1) <> " " Then
               If gameGrid(2) = 2 And gameGrid(5) = 0 Then
                  Call PawnOnGrid("REMOVE", "   ", 2)
                  Call PawnOnGrid("PLACE ", "CPU", 5)
                  sbx11(4) = sbx11(1)
               End If
            End If

       Case 12
            sphereNum = CInt(Rnd()*2) + 1

            If sphereNum = 1 And sbx12(0) <> " " Then
               If gameGrid(2) = 2 And gameGrid(4) = 1 Then
                  Call PawnOnGrid("REMOVE", "   ", 2)
                  Call PawnOnGrid("PLACE ", "CPU", 4)
                  sbx12(4) = sbx12(0)
               Else
                  sphereNum = sphereNum + 1
               End If
            End If

            If sphereNum = 2 And sbx12(1) <> " " Then
               If gameGrid(2) = 2 And gameGrid(5) = 0 Then
                  Call PawnOnGrid("REMOVE", "   ", 2)
                  Call PawnOnGrid("PLACE ", "CPU", 5)
                  sbx12(4) = sbx12(1)
               End If
            End If

       Case 13
            If sbx13(0) <> " " Then
               If gameGrid(2) = 2 And gameGrid(5) = 0 Then
                  Call PawnOnGrid("REMOVE", "   ", 2)
                  Call PawnOnGrid("PLACE ", "CPU", 5)
                  sbx13(4) = sbx13(0)
               End If
            End If

       Case 14
            sphereNum = CInt(Rnd()*2) + 1

            If sphereNum = 1 And sbx14(0) <> " " Then
               If gameGrid(3) = 2 And gameGrid(6) = 0 Then
                  Call PawnOnGrid("REMOVE", "   ", 3)
                  Call PawnOnGrid("PLACE ", "CPU", 6)
                  sbx14(4) = sbx14(0)
               Else
                  sphereNum = sphereNum + 1
               End If
            End If

            If sphereNum = 2 And sbx14(1) <> " " Then
               If gameGrid(4) = 2 And gameGrid(7) = 0 Then
                  Call PawnOnGrid("REMOVE", "   ", 4)
                  Call PawnOnGrid("PLACE ", "CPU", 7)
                  sbx14(4) = sbx14(1)
               End If
            End If

       Case 15
            If sbx15(0) <> " " Then
               If gameGrid(0) = 2 And gameGrid(4) = 1 Then
                  Call PawnOnGrid("REMOVE", "   ", 0)
                  Call PawnOnGrid("PLACE ", "CPU", 4)
                  sbx15(4) = sbx15(0)
               End If
            End If

       Case 16
            sphereNum = CInt(Rnd()*2) + 1

            If sphereNum = 1 And sbx16(0) <> " " Then
               If gameGrid(3) = 2 And gameGrid(6) = 0 Then
                  Call PawnOnGrid("REMOVE", "   ", 3)
                  Call PawnOnGrid("PLACE ", "CPU", 6)
                  sbx16(4) = sbx16(0)
               Else
                  sphereNum = sphereNum + 1
               End If
            End If

            If sphereNum = 2 And sbx16(1) <> " " Then
               If gameGrid(1) = 2 And gameGrid(5) = 0 Then
                  Call PawnOnGrid("REMOVE", "   ", 1)
                  Call PawnOnGrid("PLACE ", "CPU", 5)
                  sbx16(4) = sbx16(1)
               End If
            End If

       Case 17
            sphereNum = CInt(Rnd()*2) + 1

            If sphereNum = 1 And sbx17(0) <> " " Then
               If gameGrid(1) = 2 And gameGrid(3) = 1 Then
                  Call PawnOnGrid("REMOVE", "   ", 1)
                  Call PawnOnGrid("PLACE ", "CPU", 3)
                  sbx17(4) = sbx17(0)
               Else
                  sphereNum = sphereNum + 1
               End If
            End If

            If sphereNum = 2 And sbx17(1) <> " " Then
               If gameGrid(5) = 2 And gameGrid(8) = 0 Then
                  Call PawnOnGrid("REMOVE", "   ", 5)
                  Call PawnOnGrid("PLACE ", "CPU", 8)
                  sbx17(4) = sbx17(1)
               End If
            End If

       Case 18
            sphereNum = CInt(Rnd()*2) + 1

            If sphereNum = 1 And sbx18(0) <> " " Then
               If gameGrid(3) = 2 And gameGrid(6) = 0 Then
                  Call PawnOnGrid("REMOVE", "   ", 3)
                  Call PawnOnGrid("PLACE ", "CPU", 6)
                  sbx18(4) = sbx18(0)
               Else
                  sphereNum = sphereNum + 1
               End If
            End If

            If sphereNum = 2 And sbx18(1) <> " " Then
               If gameGrid(4) = 2 And gameGrid(7) = 0 Then
                  Call PawnOnGrid("REMOVE", "   ", 4)
                  Call PawnOnGrid("PLACE ", "CPU", 7)
                  sbx18(4) = sbx18(1)
               End If
            End If

       Case 19
            sphereNum = CInt(Rnd()*2) + 1

            If sphereNum = 1 And sbx19(0) <> " " Then
               If gameGrid(4) = 2 And gameGrid(7) = 0 Then
                  Call PawnOnGrid("REMOVE", "   ", 4)
                  Call PawnOnGrid("PLACE ", "CPU", 7)
                  sbx19(4) = sbx19(0)
               Else
                  sphereNum = sphereNum + 1
               End If
            End If

            If sphereNum = 2 And sbx19(1) <> " " Then
               If gameGrid(5) = 2 And gameGrid(8) = 0 Then
                  Call PawnOnGrid("REMOVE", "   ", 5)
                  Call PawnOnGrid("PLACE ", "CPU", 8)
                  sbx19(4) = sbx19(1)
               End If
            End If

       Case 20
            sphereNum = CInt(Rnd()*3) + 1

            If sphereNum = 1 And sbx20(0) <> " " Then
               If gameGrid(3) = 2 And gameGrid(6) = 0 Then
                  Call PawnOnGrid("REMOVE", "   ", 3)
                  Call PawnOnGrid("PLACE ", "CPU", 6)
                  sbx20(4) = sbx20(0)
               Else
                  sphereNum = sphereNum + 1
               End If
            End If

            If sphereNum = 2 And sbx20(1) <> " " Then
               If gameGrid(2) = 2 And gameGrid(4) = 1 Then
                  Call PawnOnGrid("REMOVE", "   ", 2)
                  Call PawnOnGrid("PLACE ", "CPU", 4)
                  sbx20(4) = sbx20(1)
               Else
                  sphereNum = sphereNum + 1
               End If
            End If

            If sphereNum = 3 And sbx20(2) <> " " Then
               If gameGrid(2) = 2 And gameGrid(5) = 0 Then
                  Call PawnOnGrid("REMOVE", "   ", 2)
                  Call PawnOnGrid("PLACE ", "CPU", 5)
                  sbx20(4) = sbx20(2)
               End If
            End If

       Case 21
            sphereNum = CInt(Rnd()*2) + 1

            If sphereNum = 1 And sbx21(0) <> " " Then
               If gameGrid(1) = 2 And gameGrid(3) = 1 Then
                  Call PawnOnGrid("REMOVE", "   ", 1)
                  Call PawnOnGrid("PLACE ", "CPU", 3)
                  sbx21(4) = sbx21(0)
               Else
                  sphereNum = sphereNum + 1
               End If
            End If

            If sphereNum = 2 And sbx21(1) <> " " Then
               If gameGrid(4) = 2 And gameGrid(7) = 0 Then
                  Call PawnOnGrid("REMOVE", "   ", 4)
                  Call PawnOnGrid("PLACE ", "CPU", 7)
                  sbx21(4) = sbx21(1)
               End If
            End If

       Case 22
            sphereNum = CInt(Rnd()*2) + 1

            If sphereNum = 1 And sbx22(0) <> " " Then
               If gameGrid(1) = 2 And gameGrid(5) = 1 Then
                  Call PawnOnGrid("REMOVE", "   ", 2)
                  Call PawnOnGrid("PLACE ", "CPU", 5)
                  sbx22(4) = sbx22(0)
               Else
                  sphereNum = sphereNum + 1
               End If
            End If

            If sphereNum = 2 And sbx22(1) <> " " Then
               If gameGrid(4) = 2 And gameGrid(7) = 0 Then
                  Call PawnOnGrid("REMOVE", "   ", 4)
                  Call PawnOnGrid("PLACE ", "CPU", 7)
                  sbx22(4) = sbx22(1)
               End If
            End If

       Case 23
            sphereNum = CInt(Rnd()*2) + 1

            If sphereNum = 1 And sbx23(0) <> " " Then
               If gameGrid(6) = 0 And gameGrid(3) = 2 Then
                  Call PawnOnGrid("REMOVE", "   ", 3)
                  Call PawnOnGrid("PLACE ", "CPU", 6)
                  sbx23(4) = sbx23(0)
               Else
                  sphereNum = sphereNum + 1
               End If
            End If

            If sphereNum = 2 And sbx23(1) <> " " Then
               If gameGrid(4) = 1 And gameGrid(0) = 2 Then
                  Call PawnOnGrid("REMOVE", "   ", 0)
                  Call PawnOnGrid("PLACE ", "CPU", 4)
                  sbx23(4) = sbx23(1)
               End if
            End If

       Case 24
            sphereNum = CInt(Rnd()*2) + 1

            If sphereNum = 1 And sbx24(0) <> " " Then
               If gameGrid(2) = 2 And gameGrid(4) = 1 Then
                  Call PawnOnGrid("REMOVE", "   ", 2)
                  Call PawnOnGrid("PLACE ", "CPU", 4)
                  sbx24(4) = sbx24(0)
               Else
                  sphereNum = sphereNum + 1
               End If
            End If

            If sphereNum = 2 And sbx24(1) <> " " Then
               If gameGrid(5) = 2 And gameGrid(8) = 0 Then
                  Call PawnOnGrid("REMOVE", "   ", 5)
                  Call PawnOnGrid("PLACE ", "CPU", 8)
                  sbx24(4) = sbx24(1)
               End if
            End If
    End Select

    boxExtracted = pBox
End Sub
' -------------------------------------------------------------

' *** CPU MOVE: MATCHS THE CURRENT GRID SCHEMA TO CHOSE THE CORRECT BOX
Sub CpuMove() Static
    Dim totEquals As Byte

    totEquals = 0
    For idx As byte = 0 To maxCells -1
       If gameGrid(idx) = box1(idx) Then totEquals = totEquals + 1
    Next
    If totEquals = 9 Then
        Call ExtractSphere(1)
        Exit Sub
    End If

    totEquals = 0
    For idx As byte = 0 To maxCells -1
       If gameGrid(idx) = box2(idx) Then totEquals = totEquals + 1
    Next
    If totEquals = 9 Then
        Call ExtractSphere(2)
        Exit Sub
    End If

    totEquals = 0
    For idx As byte = 0 To maxCells -1
       If gameGrid(idx) = box3(idx) Then totEquals = totEquals + 1
    Next
    If totEquals = 9 Then
        Call ExtractSphere(3)
        Exit Sub
    End If

    totEquals = 0
    For idx As byte = 0 To maxCells -1
       If gameGrid(idx) = box4(idx) Then totEquals = totEquals + 1
    Next
    If totEquals = 9 Then
        Call ExtractSphere(4)
        Exit Sub
    End If

    totEquals = 0
    For idx As byte = 0 To maxCells -1
       If gameGrid(idx) = box5(idx) Then totEquals = totEquals + 1
    Next
    If totEquals = 9 Then
        Call ExtractSphere(5)
        Exit Sub
    End If

    totEquals = 0
    For idx As byte = 0 To maxCells -1
       If gameGrid(idx) = box6(idx) Then totEquals = totEquals + 1
    Next
    If totEquals = 9 Then
        Call ExtractSphere(6)
        Exit Sub
    End If

    totEquals = 0
    For idx As byte = 0 To maxCells -1
       If gameGrid(idx) = box7(idx) Then totEquals = totEquals + 1
    Next
    If totEquals = 9 Then
        Call ExtractSphere(7)
        Exit Sub
    End If

    totEquals = 0
    For idx As byte = 0 To maxCells -1
       If gameGrid(idx) = box8(idx) Then totEquals = totEquals + 1
    Next
    If totEquals = 9 Then
        Call ExtractSphere(8)
        Exit Sub
    End If

    totEquals = 0
    For idx As byte = 0 To maxCells -1
       If gameGrid(idx) = box9(idx) Then totEquals = totEquals + 1
    Next
    If totEquals = 9 Then
        Call ExtractSphere(9)
        Exit Sub
    End If

    totEquals = 0
    For idx As byte = 0 To maxCells -1
       If gameGrid(idx) = box10(idx) Then totEquals = totEquals + 1
    Next
    If totEquals = 9 Then
        Call ExtractSphere(10)
        Exit Sub
    End If

    totEquals = 0
    For idx As byte = 0 To maxCells -1
       If gameGrid(idx) = box11(idx) Then totEquals = totEquals + 1
    Next
    If totEquals = 9 Then
        Call ExtractSphere(11)
        Exit Sub
    End If

    totEquals = 0
    For idx As byte = 0 To maxCells -1
       If gameGrid(idx) = box12(idx) Then totEquals = totEquals + 1
    Next
    If totEquals = 9 Then
        Call ExtractSphere(12)
        Exit Sub
    End If

    totEquals = 0
    For idx As byte = 0 To maxCells -1
       If gameGrid(idx) = box13(idx) Then totEquals = totEquals + 1
    Next
    If totEquals = 9 Then
        Call ExtractSphere(13)
        Exit Sub
    End If

    totEquals = 0
    For idx As byte = 0 To maxCells -1
       If gameGrid(idx) = box14(idx) Then totEquals = totEquals + 1
    Next
    If totEquals = 9 Then
        Call ExtractSphere(14)
        Exit Sub
    End If

    totEquals = 0
    For idx As byte = 0 To maxCells -1
       If gameGrid(idx) = box15(idx) Then totEquals = totEquals + 1
    Next
    If totEquals = 9 Then
        Call ExtractSphere(15)
        Exit Sub
    End If

    totEquals = 0
    For idx As byte = 0 To maxCells -1
       If gameGrid(idx) = box16(idx) Then totEquals = totEquals + 1
    Next
    If totEquals = 9 Then
        Call ExtractSphere(16)
        Exit Sub
    End If

    totEquals = 0
    For idx As byte = 0 To maxCells -1
       If gameGrid(idx) = box17(idx) Then totEquals = totEquals + 1
    Next
    If totEquals = 9 Then
        Call ExtractSphere(17)
        Exit Sub
    End If

    totEquals = 0
    For idx As byte = 0 To maxCells -1
       If gameGrid(idx) = box18(idx) Then totEquals = totEquals + 1
    Next
    If totEquals = 9 Then
        Call ExtractSphere(18)
        Exit Sub
    End If

    totEquals = 0
    For idx As byte = 0 To maxCells -1
       If gameGrid(idx) = box19(idx) Then totEquals = totEquals + 1
    Next
    If totEquals = 9 Then
        Call ExtractSphere(19)
        Exit Sub
    End If

    totEquals = 0
    For idx As byte = 0 To maxCells -1
       If gameGrid(idx) = box20(idx) Then totEquals = totEquals + 1
    Next
    If totEquals = 9 Then
        Call ExtractSphere(20)
        Exit Sub
    End If

    totEquals = 0
    For idx As byte = 0 To maxCells -1
       If gameGrid(idx) = box21(idx) Then totEquals = totEquals + 1
    Next
    If totEquals = 9 Then
        Call ExtractSphere(21)
        Exit Sub
    End If

    totEquals = 0
    For idx As byte = 0 To maxCells -1
       If gameGrid(idx) = box22(idx) Then totEquals = totEquals + 1
    Next
    If totEquals = 9 Then
        Call ExtractSphere(22)
        Exit Sub
    End If

    totEquals = 0
    For idx As byte = 0 To maxCells -1
       If gameGrid(idx) = box23(idx) Then totEquals = totEquals + 1
    Next
    If totEquals = 9 Then
        Call ExtractSphere(23)
        Exit Sub
    End If

    totEquals = 0
    For idx As byte = 0 To maxCells -1
       If gameGrid(idx) = box24(idx) Then totEquals = totEquals + 1
    Next
    If totEquals = 9 Then
        Call ExtractSphere(24)
        Exit Sub
    End If
End Sub
' -------------------------------------------------------------

' *** CHECK PLAYER MOVE: EVALUATION
Sub EvaluatePlayerMove(pMove As String*4) Static
    If pMove = "c1b1" Then
        If gameGrid(3) = 0 And gameGrid(6) = 1 Then
            Call PawnOnGrid("PLACE ", "PLY", 3)
            Call PawnOnGrid("REMOVE", "   ", 6)
            numMove = numMove + 1
        End If
    End If

    If pMove = "c2b2" Then
         If gameGrid(4) = 0 And gameGrid(7) = 1 Then
            Call PawnOnGrid("PLACE ", "PLY", 4)
            Call PawnOnGrid("REMOVE", "   ", 7)
            numMove = numMove + 1
        End If
    End If

    If pMove = "c1b2" Then
         If gameGrid(4) = 2 And gameGrid(6) = 1 Then
            Call PawnOnGrid("PLACE ", "PLY", 4)
            Call PawnOnGrid("REMOVE", "   ", 6)
            numMove = numMove + 1
        End If
    End If

    If pMove = "c2b1" Then
         If gameGrid(3) = 2 And gameGrid(7) = 1 Then
            Call PawnOnGrid("PLACE ", "PLY", 3)
            Call PawnOnGrid("REMOVE", "   ", 7)
            numMove = numMove + 1
        End If
    End If

    If pMove = "c2b3" Then
         If gameGrid(5) = 2 And gameGrid(7) = 1 Then
            Call PawnOnGrid("PLACE ", "PLY", 5)
            Call PawnOnGrid("REMOVE", "   ", 7)
            numMove = numMove + 1
        End If
    End If

    If pMove = "c3b2" Then
         If gameGrid(4) = 2 And gameGrid(8) = 1 Then
            Call PawnOnGrid("PLACE ", "PLY", 4)
            Call PawnOnGrid("REMOVE", "   ", 8)
            numMove = numMove + 1
        End If
    End If

    If pMove = "b2a3" Then
         If gameGrid(2) = 2 And gameGrid(4) = 1 Then
            Call PawnOnGrid("PLACE ", "PLY", 2)
            Call PawnOnGrid("REMOVE", "   ", 4)
            numMove = numMove + 1
        End If
    End If

    If pMove = "b2a1" Then
         If gameGrid(0) = 2 And gameGrid(4) = 1 Then
            Call PawnOnGrid("PLACE ", "PLY", 0)
            Call PawnOnGrid("REMOVE", "   ", 4)
            numMove = numMove + 1
        End If
    End If

    If pMove = "b1a1"  Then
         If gameGrid(0) = 0  And gameGrid(3) = 1 Then
            Call PawnOnGrid("PLACE ", "PLY", 0)
            Call PawnOnGrid("REMOVE", "   ", 3)
            numMove = numMove + 1
        End If
    End If

    If pMove = "b1a2" Then
         If gameGrid(3) = 1 And gameGrid(1) = 2 Then
            Call PawnOnGrid("PLACE ", "PLY", 1)
            Call PawnOnGrid("REMOVE", "   ", 3)
            numMove = numMove + 1
        End If
    End If

    If pMove = "b2a2" Then
         If gameGrid(1) = 0 And gameGrid(4) = 1 Then
            Call PawnOnGrid("PLACE ", "PLY", 1)
            Call PawnOnGrid("REMOVE", "   ", 4)
            numMove = numMove + 1
        End If
    End If

    If pMove = "b3a3" Then
         If gameGrid(2) = 0 And gameGrid(5) = 1 Then
            Call PawnOnGrid("PLACE ", "PLY", 2)
            Call PawnOnGrid("REMOVE", "   ", 5)
            numMove = numMove + 1
        End If
    End If

    If pMove = "c3b3" And numMove > 0 Then
         If gameGrid(5) = 0 And gameGrid(8) = 1  Then
            Call PawnOnGrid("PLACE ", "PLY", 5)
            Call PawnOnGrid("REMOVE", "   ", 8)
            numMove = numMove + 1
        End If
    End If

    If pMove = "rest" Then
       Call Init()
       Call ClearScreen(0, 0)
       Call ShowTitle(10)
       Call ShowGrid(3)
       Call PlaceInitialPawns()
    End If

    If pMove = "next" And winner <> 0 Then
       numMove = 0

       Call ClearScreen(0, 0)
       Call ShowTitle(10)
       Call ShowGrid(3)
       Call PlaceInitialPawns()

       If winner = 1 Then
          playerWins = playerWins + 1
          Call RemoveSphere(boxExtracted)
       Else
          If winner = 2 Then
             cpuWins = cpuWins + 1
          End If
       End If

       Call ClearSelectedSpheres()
    End If

    Call BoardNumbers(3)

    If pMove <> "rest" And pMove <> "next" Then Call CpuMove()
End Sub

' *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
' -------------------------------------------------------------
' -------------------------------------------------------------
' - M A I N    L O G I C                                     --
' -------------------------------------------------------------
' -------------------------------------------------------------
' *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

Call ClearScreen(0, 0)
Call ShowTitle(10)
Call Init()
Call ShowGrid(3)
Call PlaceInitialPawns()
Call BoardNumbers(15)

Dim keyPressed As String*1, wMove As String*4

Do While 1
   keyPressed = "" : wMove = ""
   locate 33,23: Print "    "

   Poke 646,13: locate 5,1:  Print "cpu wins:";cpuWins
   Poke 646,13: locate 23,1: Print "p1  wins:";playerWins

   Poke 646,14
   Locate 6,22: Print "scrivi <rest> per ricominciare"
   Locate 6,23: Print "scrivi la mossa (es: c1b1): "

   'Call DebugBoxInfo() : REM DEBUG

   Do While Len(wMove) < 4
      'Poke 646,5: Locate 21, 0: Print "winner  :";winner 'debug

      Call CheckWinner()

      If winner <> 0 Then
          Locate 0,23:             Print "                           "
          poke 646,7: Locate 8,23: Print "<next> = prossimo round    "

         If winner = 1 Then
             border 5
             poke 646,7: Locate 6,22: Print "  gioco finito: hai vinto tu!       "
         Else
             If winner = 2 Then
                 border 10
                 poke 646,7: Locate 6,22: Print "  gioco finito: hai perso!      "
             End If
         End If
      End If

      Get keyPressed
      If keyPressed = chr$(13) Then keyPressed = ""
      If keyPressed = "{down}" Or keyPressed = "{up}" Or keyPressed = "{left}" Or keyPressed = "{right}" Then keyPressed = ""
      wMove = wMove + keyPressed
      locate 33,23: Print wMove

      Call EvaluatePlayerMove(wMove)

   Loop

Loop