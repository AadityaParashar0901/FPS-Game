$Console
_Console On
'$Dynamic
'$Include:'player.bi'
'$Include:'packets.bi'

Option _Explicit

Screen _NewImage(960, 544, 32)
_ScreenMove _DesktopWidth - _Width - 64, (_DesktopHeight - _Height) / 2
_Title "Server"

'$Include:'.\server_constants.txt'

'Generate Map
Dim As _Byte X, Y
Dim P$
Dim Shared HOST&
Dim KEYPRESS, CONNECTION&, I As Integer
Randomize Timer: Dim Shared As _Unsigned _Byte Map(-32 To 31, -32 To 31)
For X = -32 To 31: For Y = -32 To 31
        Map(X, Y) = IIF(X = -32 Or Y = -32 Or X = 31 Or Y = 31, 1, IIF(Rnd > 0.95, Rnd * 15 + 1, 0))
Next Y, X
'------------

Dim Shared Players(1 To 64) As Player, EmptyPlayer As Player

Dim Shared CONNECTIONS(1 To 256) As Long, TOTALCONNECTIONS As _Unsigned Integer: TOTALCONNECTIONS = 0
Print "Starting Server"
If 0 Then Line Input "Enter Port: "; P$ Else P$ = "16000"
HOST& = _OpenHost("TCP/IP:" + P$)
If HOST& Then
    _Echo "Server Started: " + _ConnectionAddress(HOST&)
    Cls , 0
End If
Do
    _Limit 60
    On Error GoTo ErrHandler
    KEYPRESS = _KeyHit
    Select Case KEYPRESS
        Case 27: Exit Do
        Case 83, 115: _Echo "Starting Match"
            Start_Match
        Case 69, 101: _Echo "Ending Match"
            End_Match
        Case 67, 99: _Dest _Console: Cls: _Dest 0
    End Select
    CONNECTION& = _OpenConnection(HOST&)
    TOTALCONNECTIONS = 0: For I = LBound(CONNECTIONS) To UBound(CONNECTIONS)
        TOTALCONNECTIONS = TOTALCONNECTIONS - (CONNECTIONS(I) <> 0)
    Next I
    If CONNECTION& Then
        _Echo "Client Connected: " + _ConnectionAddress(CONNECTION&)
        TOTALCONNECTIONS = TOTALCONNECTIONS + 1
        For I = LBound(CONNECTIONS) To UBound(CONNECTIONS)
            If CONNECTIONS(I) = 0 Then Exit For
        Next I
        If I <= UBound(CONNECTIONS) Then CONNECTIONS(I) = CONNECTION& Else _Echo "No more players can be added"
    End If
    For I = LBound(CONNECTIONS) To UBound(CONNECTIONS)
        If CONNECTIONS(I) = 0 Then _Continue
        If _Connected(CONNECTIONS(I)) Then
            ParseClient I
        Else
            _Echo "Client Disconnected: " + _ConnectionAddress$(CONNECTIONS(I))
            Players(I) = EmptyPlayer
            CONNECTIONS(I) = 0
            UpdatePlayers I, I
        End If
    Next I
    Cls , 0
    Print "Total Connections: "; TOTALCONNECTIONS
    For I = 1 To 64
        _PrintString (IIF(I > 32, _Width / 2, 0), IIF(I > 32, I - 32, I) * _FontHeight), Players(I).Name + Str$(Players(I).Health) + ":" + Str$(Players(I).Position.X) + "," + Str$(Players(I).Position.Y) + "," + Str$(Players(I).Position.Z) + ";" + Str$(Players(I).Angle.X) + "," + Str$(Players(I).Angle.Y)
    Next I
    _Display
Loop
_Echo "Stopping Server"
Close HOST&
System

ErrHandler:
_Echo "[Error" + Str$(Err) + ":" + _ErrorMessage$ + " on line" + Str$(_ErrorLine) + "]"
Resume Next

Sub ParseClient (__I As _Unsigned _Byte)
    Dim CLIENT_USERNAME$16
    Dim As Packet Packet
    Get #CONNECTIONS(__I), , Packet
    Select Case Packet.CODE
        Case SERVER_CONST_USERNAME
            Print " New User: "; Packet.Player.Name; __I
            Players(__I).Name = Packet.Player.Name
            Players(__I).Mode = PLAYER_MODE_SPECTATOR
            Players(__I).Health = 10
            UpdatePlayers __I, __I

        Case SERVER_CONST_GET_PLAYERS
            Put #CONNECTIONS(__I), , Players()

        Case SERVER_CONST_GET_CURRENT_PLAYER_ID
            Packet.CODE = SERVER_CONST_REPLY
            Packet.ID = __I
            Packet.Player = Players(__I)
            Put #CONNECTIONS(__I), , Packet

        Case SERVER_CONST_MAP_DATA
            Put #CONNECTIONS(__I), , Map()

        Case SERVER_CONST_PLAYER_UPDATE
            Players(Packet.ID) = Packet.Player
            UpdatePlayers __I, Packet.ID

    End Select
End Sub
Sub UpdatePlayers (__J As _Unsigned _Byte, __K As _Unsigned _Byte) 'notify other players
    Dim As Packet Packet
    Dim __I
    Packet.CODE = SERVER_CONST_PLAYER_UPDATE
    Packet.ID = __K
    Packet.Player = Players(__K)
    For __I = 1 To TOTALCONNECTIONS
        If __I = __J Or CONNECTIONS(__I) = 0 Or Players(__I).ReadyToReceive = 0 Then _Continue
        _PrintString (320, 0), " Updating Connection" + Str$(__I) + " about" + Str$(__J)
        Put #CONNECTIONS(__I), , Packet
    Next __I
End Sub
Sub Start_Match
    Dim Packet As Packet, __I
    Packet.CODE = SERVER_CONST_START_MATCH
    For __I = 1 To TOTALCONNECTIONS
        If CONNECTIONS(__I) = 0 Then _Continue
        If _Connected(CONNECTIONS(__I)) = 0 Then _Continue
        Put #CONNECTIONS(__I), , Packet
    Next __I
End Sub
Sub End_Match
    Dim Packet As Packet, __I
    Packet.CODE = SERVER_CONST_END_MATCH
    For __I = 1 To TOTALCONNECTIONS
        If CONNECTIONS(__I) = 0 Or _Connected(CONNECTIONS(__I)) = 0 Then _Continue
        Put #CONNECTIONS(__I), , Packet
    Next __I
End Sub
'$Include:'include\vector\vector.bm'
'$Include:'include\iif.bm'
