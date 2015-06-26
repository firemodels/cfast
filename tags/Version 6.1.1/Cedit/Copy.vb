Module Copy
    Public Sub FireCopy(ByVal FromFire As Fire, ByVal ToFire As Fire)
        ' Copies an entire fire record from one structure to another
        Dim NumDataPoints As Integer
        ToFire.FireType = FromFire.FireType
        ToFire.Compartment = FromFire.Compartment
        ToFire.XPosition = FromFire.XPosition
        ToFire.YPosition = FromFire.YPosition
        ToFire.ZPosition = FromFire.ZPosition
        ToFire.XNormal = FromFire.XNormal
        ToFire.YNormal = FromFire.YNormal
        ToFire.ZNormal = FromFire.ZNormal
        ToFire.IgnitionType = FromFire.IgnitionType
        ToFire.IgnitionValue = FromFire.IgnitionValue
        ToFire.PlumeType = FromFire.PlumeType
        ToFire.FireObject = FromFire.FireObject
        ToFire.Name = FromFire.Name
        ToFire.Length = FromFire.Length
        ToFire.Width = FromFire.Width
        ToFire.Thickness = FromFire.Thickness
        ToFire.MolarMass = FromFire.MolarMass
        ToFire.TotalMass = FromFire.TotalMass
        ToFire.Material = FromFire.Material
        ToFire.HeatofCombustion = FromFire.HeatofCombustion
        ToFire.HeatofGasification = FromFire.HeatofGasification
        ToFire.VolitilTemp = FromFire.VolitilTemp
        ToFire.RadiativeFraction = FromFire.RadiativeFraction
        Dim aFireData(12, 0) As Single
        FromFire.GetFireData(aFireData, NumDataPoints)
        ToFire.SetFireData(aFireData)
        ToFire.Changed = FromFire.Changed
    End Sub
    Public Sub PropertyCopy(ByVal FromMaterial As ThermalProperty, ByVal ToMaterial As ThermalProperty)
        ' Copies and entire thermal property record from on structure to another
        ToMaterial.ShortName = FromMaterial.ShortName
        ToMaterial.Name = FromMaterial.Name
        ToMaterial.Conductivity = FromMaterial.Conductivity
        ToMaterial.SpecificHeat = FromMaterial.SpecificHeat
        ToMaterial.Density = FromMaterial.Density
        ToMaterial.Thickness = FromMaterial.Thickness
        ToMaterial.Emissivity = FromMaterial.Emissivity
        Dim Vector1() As Single
        FromMaterial.GetHCl(Vector1)
        ToMaterial.SetHCl(Vector1)
    End Sub
End Module