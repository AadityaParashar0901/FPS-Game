Function load_background&
    Static O&: If O& = 0 Then
        O& = _NewImage(160, 90, 32)
        __Dest& = _Dest: _Dest O&
        Paint (0, 0), &HFF22B14C
        _Dest __Dest&
    End If
    load_background = O&
End Function
