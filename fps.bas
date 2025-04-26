$Console
_Console Off
$Resize:On
'$Dynamic

'$Include:'player.bi'
'$Include:'packets.bi'

$Let GL = 1
Const DEBUG = 0
Dim Shared LOG_FILE_NAME$: LOG_FILE_NAME$ = "log_" + Left$(Date$, 2) + Mid$(Date$, 4, 2) + Right$(Date$, 4) + Left$(Time$, 2) + Mid$(Time$, 4, 2) + Right$(Time$, 2) + ".txt"

Dim Shared Chat$: Chat$ = ListStringNew$

Const Player_Obesity = 1.41
Dim Shared MoveSpeed As Single

Randomize Timer

Screen _NewImage(640, 480, 32): _Title "PUBG": Color , _RGB32(0, 127)

CenterPrint "PUBG", 0: CenterPrint "Generating Textures", 8

Dim Shared As Long Texture, GroundTexture, FaceTexture
Dim Shared As _Bit allowGL, isPaused
Dim Shared __GL_GENERATE_TEXTURE As _Byte

Dim Shared As _Unsigned Integer LFPS, GFPS, LFPSCount, GFPSCount, FOV, NEWFOV: LFPS = 60: FOV = 90: NEWFOV = 90

Dim Shared Vertices(1 To 16384) As Vec3, TexCoords(1 To 16384) As Vec2, TotalPointsToDraw As _Unsigned Long
Dim Shared PlayerVertices(1 To 1536) As Vec3, PlayerTexCoords(1 To 1536) As Vec2

Dim Shared Players(1 To 64) As Player, CurrentPlayer As _Unsigned _Byte
Dim Shared As Vec3 BulletPosition, BulletVelocity

Dim Shared CubeData(1 To 24) As Vec3, CubeDataTexCoords(1 To 24) As Vec2

Dim Shared CLIENT&, Map(-32 To 31, -32 To 31) As _Unsigned _Byte, SendingPlayerData As Single, BACKGROUND&, USERNAME$

Dim ADDRESS$
Dim As Integer I, F, X, Y

'----- Load Textures -----
FaceTexture = load_face&
GroundTexture = _NewImage(1024, 1024, 32)
For X = 0 To 1023 Step 16: For Y = 0 To 1023 Step 16: _PutImage (X, Y), load_stone_bricks&, GroundTexture: Next Y, X
Texture = load_texture&
$If GL Then
    __GL_GENERATE_TEXTURE = -1: While __GL_GENERATE_TEXTURE: Wend
$End If
'-------------------------

Restore CubeData
For I = 1 To 24
    Read CubeData(I).X, CubeData(I).Y, CubeData(I).Z, CubeDataTexCoords(I).X, CubeDataTexCoords(I).Y
    CubeData(I).X = CubeData(I).X - 0.5
    CubeData(I).Z = CubeData(I).Z - 0.5
Next I

F = _FreeTimer: On Timer(F, 1) GoSub FPSCounter

'$Include:'.\server_constants.txt'

CurrentPlayer = 1

UserNameDetails: ' Get Username
BACKGROUND& = load_background&
InputDialog "Enter Your UserName", USERNAME$, BACKGROUND&
USERNAME$ = USERNAME$ + String$(16 - Len(USERNAME$), 0)

'----- Connect to Server -----
Cls , 0: _AutoDisplay: _PutImage (0, 0)-(_Width - 1, _Height - 1), BACKGROUND&
CenterPrint "Attempting to Connect to Localhost", 0
CLIENT& = _OpenClient("TCP/IP:16000:localhost")
If CLIENT& = 0 Then
    ConnectToServer:
    InputDialog "Enter Server Address", ADDRESS$, BACKGROUND&
    If InStr(ADDRESS$, ":") Then CLIENT& = _OpenClient("TCP/IP:" + ADDRESS$) Else CLIENT& = _OpenClient("TCP/IP:16000:" + ADDRESS$)
End If
If CLIENT& Then _Echo "Connected to " + _ConnectionAddress(CLIENT&)
If CLIENT& = 0 Then
    CenterPrint "Connection Failed", 1
    _Delay 1
    GoTo ConnectToServer
End If
'-----------------------------

'----- Get Data from Server -----
Cls , 0: _PutImage (0, 0)-(_Width - 1, _Height - 1), BACKGROUND&
CenterPrint "Registering New User", 0
InitializeClient
Cls , 0: _PutImage (0, 0)-(_Width - 1, _Height - 1), BACKGROUND&
CenterPrint "Generating Map", 0
GenerateMesh
'--------------------------------

Timer(F) On ' FPS Counter

Cls , 0: _GLRender _Behind: allowGL = -1: _MouseHide

Do: _Limit 60: On Error GoTo ErrHandler

    LFPSCount = LFPSCount + 1

    If _Resize Then
        W = _ResizeWidth: H = _ResizeHeight
        If W > 0 And H > 0 Then Screen _NewImage(W, H, 32)
        _PrintMode _KeepBackground
    End If

    isPaused = IIF(_WindowHasFocus, isPaused, 1)
    If _KeyDown(27) Or _KeyDown(80) Or _KeyDown(112) Then
        isPaused = Not isPaused
        While _KeyDown(27) Or _KeyDown(80) Or _KeyDown(112): Wend
    End If
    If isPaused Then _MouseShow Else _MouseHide

    ParseServer
    UpdatePlayers CurrentPlayer
    '----- Add Player Vertices to Array for GL -----
    For I = LBound(Players) To UBound(Players)
        If Players(I).ReadyToReceive = 0 Or I = CurrentPlayer Then
            For J = 1 To 24
                PlayerVertices(I * 24 + J - 24) = EmptyVec3
                PlayerTexCoords(I * 24 + J - 24) = EmptyVec2
            Next J
        Else
            For J = 1 To 24
                PlayerVertices(I * 24 + J - 24) = Players(I).Position
                PlayerVertices(I * 24 + J - 24).X = -PlayerVertices(I * 24 + J - 24).X
                PlayerVertices(I * 24 + J - 24).Y = PlayerVertices(I * 24 + J - 24).Y - 0.5
                PlayerVertices(I * 24 + J - 24).Z = -PlayerVertices(I * 24 + J - 24).Z
                Vec3Add PlayerVertices(I * 24 + J - 24), CubeData(J)
                PlayerTexCoords(I * 24 + J - 24) = CubeDataTexCoords(J)
            Next J
        End If
    Next I
    '-----------------------------------------------

    If Players(CurrentPlayer).Health <= 0 Then PlayerRandomPosition

    If isPaused Then _Continue

    While _MouseInput
        Players(CurrentPlayer).Angle.X = ClampCycle(0, Players(CurrentPlayer).Angle.X + _MouseMovementX / (3 + 90 / FOV), 360)
        Players(CurrentPlayer).Angle.Y = IIF(Players(CurrentPlayer).Mode = PLAYER_MODE_SPECTATOR, Clamp(-90, Players(CurrentPlayer).Angle.Y + _MouseMovementY / (3 + 90 / FOV), 90), 0)
        $If GL Then
            _MouseMove _Width / 2, _Height / 2
        $End If
    Wend
    NEWFOV = 90 + (_KeyDown(67) Or _KeyDown(99)) * 30 + (_KeyDown(86) Or _KeyDown(118)) * 30 + (_KeyDown(88) Or _KeyDown(120)) * 20
    'NEWFOV = IIF(_MouseButton(2) Or _KeyDown(67) Or _KeyDown(99), 30, 90)
    FOV = FOV + DistanceConstraint(NEWFOV - FOV, 8)
    If (_MouseButton(1) Or _KeyDown(32)) And Players(CurrentPlayer).Mode = PLAYER_MODE_PLAYER Then FireBullet

    MoveSpeed = IIF(Players(CurrentPlayer).Mode = PLAYER_MODE_PLAYER, 5, 10)
    Select Case Players(CurrentPlayer).Mode
        Case PLAYER_MODE_PLAYER: If _KeyDown(87) Or _KeyDown(119) Then PlayerMove Players(CurrentPlayer).Angle.X - 90, 0.08 'W
            If _KeyDown(83) Or _KeyDown(115) Then PlayerMove Players(CurrentPlayer).Angle.X + 90, 0.08 'S
            If _KeyDown(65) Or _KeyDown(97) Then PlayerMove Players(CurrentPlayer).Angle.X + 180, 0.08 'A
            If _KeyDown(68) Or _KeyDown(100) Then PlayerMove Players(CurrentPlayer).Angle.X, 0.08 'D
            Players(CurrentPlayer).Position.Y = 0.5
        Case PLAYER_MODE_SPECTATOR: If _KeyDown(87) Or _KeyDown(119) Then PlayerMove Players(CurrentPlayer).Angle.X - 90, 0.16 'W
            If _KeyDown(83) Or _KeyDown(115) Then PlayerMove Players(CurrentPlayer).Angle.X + 90, 0.16 'S
            If _KeyDown(65) Or _KeyDown(97) Then PlayerMove Players(CurrentPlayer).Angle.X + 180, 0.16 'A
            If _KeyDown(68) Or _KeyDown(100) Then PlayerMove Players(CurrentPlayer).Angle.X, 0.16 'D
            Players(CurrentPlayer).Position.Y = Players(CurrentPlayer).Position.Y + IIF(_KeyDown(32), 0.16, 0)
            Players(CurrentPlayer).Position.Y = Players(CurrentPlayer).Position.Y - IIF(_KeyDown(100304), 0.16, 0)
    End Select
    If _KeyDown(84) Or _KeyDown(116) Then StartMatch
    If _KeyDown(89) Or _KeyDown(121) Then EndMatch
Loop
System

FPSCounter:
If LFPSCount Then LFPS = LFPSCount
LFPSCount = 0
GFPS = GFPSCount
GFPSCount = 0
Return

ErrHandler:
_Echo "[Error" + Str$(Err) + ":" + _ErrorMessage$ + " on line" + Str$(_ErrorLine) + "]"
WriteLog "[Error" + Str$(Err) + ":" + _ErrorMessage$ + " on line" + Str$(_ErrorLine) + "]"
Resume Next

GLErrHandler:
_Echo "[GL Error" + Str$(Err) + ":" + _ErrorMessage$ + " on line" + Str$(_ErrorLine) + "]"
WriteLog "[GL Error" + Str$(Err) + ":" + _ErrorMessage$ + " on line" + Str$(_ErrorLine) + "]"
Resume Next

CubeData:
'$Include:'res\cube'

Sub GenerateMesh
    Dim As Integer X, Y, I
    For X = -32 To 31: For Y = -32 To 31
            If Map(X, Y) Then
                For I = 1 To 24
                    If InRange(13, I, 16) Then _Continue
                    TotalPointsToDraw = TotalPointsToDraw + 1
                    NewVec3 Vertices(TotalPointsToDraw), CubeData(I).X + X, CubeData(I).Y, CubeData(I).Z + Y
                    NewVec2 TexCoords(TotalPointsToDraw), (CubeDataTexCoords(I).X + Map(X, Y) - 1) / 15, CubeDataTexCoords(I).Y
                Next I
            End If
    Next Y, X
End Sub

$If GL Then
    Sub _GL
        Static As Long TextureHandle, GroundTextureHandle, FaceTextureHandle
        If __GL_GENERATE_TEXTURE Then
            If Texture < -1 Then GL_Generate_Texture TextureHandle, Texture
            If GroundTexture < -1 Then GL_Generate_Texture GroundTextureHandle, GroundTexture
            If FaceTexture < -1 Then GL_Generate_Texture FaceTextureHandle, FaceTexture
            __GL_GENERATE_TEXTURE = 0
        End If
        If allowGL = 0 Then Exit Sub

        On Error GoTo GLErrHandler
        _glViewport 0, 0, _Width - 1, _Height - 1
        _glEnable _GL_DEPTH_TEST
        _glClearColor 0, 0.5, 1, 0
        _glClear _GL_DEPTH_BUFFER_BIT Or _GL_COLOR_BUFFER_BIT
        _glRotatef Players(CurrentPlayer).Angle.Y, 1, 0, 0
        _glRotatef Players(CurrentPlayer).Angle.X, 0, 1, 0
        _glTranslatef Players(CurrentPlayer).Position.X, -Players(CurrentPlayer).Position.Y, Players(CurrentPlayer).Position.Z
        _glMatrixMode _GL_PROJECTION
        _glLoadIdentity
        _gluPerspective FOV, _Width / _Height, 0.1, 256
        _glMatrixMode _GL_MODELVIEW
        _glEnable _GL_TEXTURE_2D

        _glBindTexture _GL_TEXTURE_2D, GroundTextureHandle
        _glBegin _GL_QUADS
        For I = 13 To 16
            _glVertex3f CubeData(I).X * 64 - 0.5, CubeData(I).Y, CubeData(I).Z * 64 - 0.5
            _glTexCoord2f CubeDataTexCoords(I).X, CubeDataTexCoords(I).Y
        Next I
        _glEnd

        _glBindTexture _GL_TEXTURE_2D, TextureHandle
        _glEnableClientState _GL_VERTEX_ARRAY
        _glEnableClientState _GL_TEXTURE_COORD_ARRAY
        _glVertexPointer 3, _GL_FLOAT, 0, _Offset(Vertices(1))
        _glTexCoordPointer 2, _GL_FLOAT, 0, _Offset(TexCoords(1))
        _glDrawArrays _GL_QUADS, 0, TotalPointsToDraw
        _glDisableClientState _GL_TEXTURE_COORD_ARRAY
        _glDisableClientState _GL_VERTEX_ARRAY

        '_glTranslatef -Players(CurrentPlayer).Position.X, Players(CurrentPlayer).Position.Y, -Players(CurrentPlayer).Position.Z
        _glBindTexture _GL_TEXTURE_2D, FaceTextureHandle
        _glEnableClientState _GL_VERTEX_ARRAY
        _glEnableClientState _GL_TEXTURE_COORD_ARRAY
        For I = LBound(Players) To UBound(Players)
            If Players(I).ReadyToReceive = 0 Then _Continue
            _glPushMatrix
            '_glTranslatef -Players(I).Position.X, -0.5 - Players(I).Position.Y, -Players(I).Position.Z
            _glVertexPointer 3, _GL_FLOAT, 0, _Offset(PlayerVertices(I * 24 - 23))
            _glTexCoordPointer 2, _GL_FLOAT, 0, _Offset(PlayerTexCoords(I * 24 - 23))
            _glDrawArrays _GL_QUADS, 0, 24
            _glPopMatrix
        Next I
        _glDisableClientState _GL_TEXTURE_COORD_ARRAY
        _glDisableClientState _GL_VERTEX_ARRAY

        _glDisable _GL_TEXTURE_2D
        _glDisable _GL_DEPTH_TEST
        _glFlush

        Cls 2, 0
        Print "FPS (G/L):"; GFPS; "/"; LFPS
        Print Players(CurrentPlayer).Position.X; Players(CurrentPlayer).Position.Y; Players(CurrentPlayer).Position.Z
        Print Players(CurrentPlayer).Angle.X; Players(CurrentPlayer).Angle.Y

        If Players(CurrentPlayer).Mode = PLAYER_MODE_PLAYER Then _PutImage (_Width / 2 - 8, _Height / 2 - 8), load_cross&
        Line (_Width * 0.2, _Height - 8)-(_Width * 0.2 + Players(CurrentPlayer).Health / 10 * _Width * 0.6, _Height), _RGB32(191, 191, 0), BF

        CenterPrint Str$(Players(CurrentPlayer).Health), Int(_Height / 2 / _FontHeight) - 1

        If isPaused Then
            Line (0, 0)-(_Width - 1, _Height - 1), _RGB32(0, 127), BF
            CenterPrint "Paused", 0
        End If
        If DEBUG Then _PrintString (_Width - 80, 0), Str$(SendingPlayerData)

        If ListStringLength(Chat$) > 5 Then
            ListStringDelete Chat$, 1
        End If
        L~& = ListStringLength(Chat$)
        Color , _RGB32(0, 127)
        For I = 1 To L~&
            _PrintString (0, _Height + (I - L~& - 2) * _FontHeight), ListStringGet$(Chat$, I)
        Next I
        Color , 0
        _Display
        GFPSCount = GFPSCount + 1
    End Sub

    '$Include:'include\gl_generate_texture.bm'
$End If
Sub FireBullet
    Static As _Unsigned Integer L, J
    Static As Single LastFireTime
    If Timer - LastFireTime < 1 Then Exit Sub
    LastFireTime = Timer
    BulletPosition = Players(CurrentPlayer).Position
    NewVec3 BulletVelocity, Cos(_D2R(Players(CurrentPlayer).Angle.X + 90)), 0, Sin(_D2R(Players(CurrentPlayer).Angle.X + 90))
    Vec3Normalize BulletVelocity
    For L = 1 To 64
        Vec3MultiplyAdd BulletPosition, BulletVelocity, 0.5
        If InRange(-32, BulletPosition.X, 31) And InRange(-32, BulletPosition.Z, 31) Then
            If Map(BulletPosition.X, BulletPosition.Z) Then Exit For
            For J = LBound(Players) To UBound(Players)
                If Vec3Dis(Players(J).Position, BulletPosition) <= 2 * Player_Obesity And J <> CurrentPlayer Then
                    Players(J).Health = Players(J).Health - 1
                    Players(J).TargetedBy = CurrentPlayer
                    UpdatePlayers J
                    Exit For
                End If
            Next J
        End If
    Next L
End Sub

Sub InitializeClient (): Dim As Packet Packet
    Packet.CODE = SERVER_CONST_USERNAME: Packet.Player.Name = USERNAME$: Put #CLIENT&, , Packet
    Packet.CODE = SERVER_CONST_GET_CURRENT_PLAYER_ID: Put #CLIENT&, , Packet: _Delay 0.3: Get #CLIENT&, , Packet: CurrentPlayer = Packet.ID: Players(CurrentPlayer) = Packet.Player: _Echo Str$(CurrentPlayer)
    Packet.CODE = SERVER_CONST_MAP_DATA: Put #CLIENT&, , Packet: _Delay 0.3: Get #CLIENT&, , Map()
    NewVec3 Players(CurrentPlayer).Position, 64 * Rnd - 32, 5, 64 * Rnd - 32
    Players(CurrentPlayer).ReadyToReceive = 1: UpdatePlayers CurrentPlayer
    _Echo Players(CurrentPlayer).Name: _Echo "CurrentPlayerID:" + Str$(CurrentPlayer)
End Sub
Sub ParseServer (): Dim As Packet Packet
    Get #CLIENT&, , Packet
    Select Case Packet.CODE
        Case SERVER_CONST_PLAYER_UPDATE
            If Players(Packet.ID).ReadyToReceive <> Packet.Player.ReadyToReceive Then
                If Players(Packet.ID).ReadyToReceive = 0 Then ChatNewMessage Packet.Player.Name + " joined" Else ChatNewMessage Players(Packet.ID).Name + " left"
            End If
            Players(Packet.ID) = Packet.Player

        Case SERVER_CONST_OPPONENT_HEALTH
            If Players(Packet.ID).TargetedBy <> 0 Then
                ChatNewMessage Players(Packet.ID).Name + " was shot by " + Players(Players(Packet.ID).TargetedBy).Name
                If Packet.ID = CurrentPlayer Then PlayerRandomPosition
            End If

        Case SERVER_CONST_START_MATCH
            Players(CurrentPlayer).Mode = PLAYER_MODE_PLAYER
            PlayerRandomPosition

        Case SERVER_CONST_END_MATCH
            Players(CurrentPlayer).Mode = PLAYER_MODE_SPECTATOR
            Players(CurrentPlayer).Position.Y = 5
            UpdatePlayers CurrentPlayer

    End Select
End Sub

Sub StartMatch
    If CurrentPlayer - 1 Then Exit Sub
    Dim As Packet Packet
    Packet.CODE = SERVER_CONST_START_MATCH
    Put #CLIENT&, , Packet
End Sub
Sub EndMatch
    If CurrentPlayer - 1 Then Exit Sub
    Dim As Packet Packet
    Packet.CODE = SERVER_CONST_END_MATCH
    Put #CLIENT&, , Packet
End Sub

Sub PlayerWasShot
    Dim As Packet Packet
    Packet.CODE = SERVER_CONST_OPPONENT_HEALTH
    Packet.ID = CurrentPlayer
    Put #CLIENT&, , Packet
    PlayerRandomPosition
End Sub

Sub PlayerRandomPosition
    Players(CurrentPlayer).Health = 10
    NewVec3 Players(CurrentPlayer).Position, Int(62 * Rnd - 31) + 0.5, 0.5, Int(62 * Rnd - 31) + 0.5
    UpdatePlayers CurrentPlayer
End Sub

Sub UpdatePlayers (__I As _Unsigned Integer)
    Dim As Packet Packet
    Packet.CODE = SERVER_CONST_PLAYER_UPDATE
    Packet.ID = __I
    Packet.Player = Players(__I)
    Put #CLIENT&, , Packet
    SendingPlayerData = Timer
End Sub
Sub PlayerMove (Angle As Single, Speed As Single)
    Static As Single dX, dY
    dX = Cos(_D2R(Angle)) * Speed
    dY = Sin(_D2R(Angle)) * Speed
    Players(CurrentPlayer).Position.X = Clamp(-30, Players(CurrentPlayer).Position.X - dX, 31)
    Players(CurrentPlayer).Position.Z = Clamp(-30, Players(CurrentPlayer).Position.Z - dY, 31)
End Sub
Sub CenterPrint (T$, N): _PrintString (_Width / 2 - Len(T$) * _FontWidth / 2, _Height / 2 + _FontHeight * (N - 0.5)), T$: End Sub
Sub InputDialog (T$, S$, B&)
    Do
        _Limit 60
        K& = _KeyHit
        Select Case K&
            Case 8: If Len(S$) Then S$ = Left$(S$, Len(S$) - 1)
            Case 13: Exit Do
            Case 27: System
            Case 32 To 126: S$ = S$ + Chr$(K&)
        End Select
        Cls , 0
        If B& Then _PutImage (0, 0)-(_Width - 1, _Height - 1), B&
        Line (0, _Height / 2 - 2 * _FontHeight)-(_Width - 1, _Height / 2 + 2 * _FontHeight), _RGB32(0, 127), BF
        CenterPrint T$, -1
        CenterPrint S$, 0
        _Display
    Loop
End Sub
Sub WriteLog (L$)
    If DEBUG = 0 Then Exit Sub
    LOG_FILE = FreeFile
    Open LOG_FILE_NAME$ For Append As #LOG_FILE
    Print #LOG_FILE, L$
    Close #LOG_FILE
End Sub

Function DistanceConstraint (A, B)
    DistanceConstraint = IIF(Abs(A) > B, B * Sgn(A), A)
End Function

Sub ChatNewMessage (MSG$)
    ListStringAdd Chat$, MSG$
End Sub

'$Include:'lib\clamp.bm'
'$Include:'lib\vector\vector.bm'
'$Include:'lib\inrange.bm'
'$Include:'lib\iif.bm'
'$Include:'res\background.png.bi'
'$Include:'res\stone_bricks.png.bi'
'$Include:'res\cross.png.bi'
'$Include:'res\texture.png.bi'
'$Include:'res\face.png.bi'
'$Include:'lib\liststring.bas'
