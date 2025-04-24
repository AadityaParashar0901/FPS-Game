'$Include:'include\vector\vector.bi'
Type Player
    As String * 16 Name
    As _Unsigned Integer TargetedBy
    As Vec3 Position
    As Vec2 Angle
    As _Unsigned _Byte Mode, ReadyToReceive
    As _Byte Health
End Type
