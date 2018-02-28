Module Copy
    Public Sub FireCopy(ByVal FromFire As Fire, ByVal ToFire As Fire)
        ' Copies an entire fire record from one structure to another
        Dim NumDataPoints As Integer
        ToFire.ObjectType = FromFire.ObjectType
        ToFire.Compartment = FromFire.Compartment
        ToFire.XPosition = FromFire.XPosition
        ToFire.YPosition = FromFire.YPosition
        ToFire.IgnitionType = FromFire.IgnitionType
        ToFire.IgnitionValue = FromFire.IgnitionValue
        ToFire.Target = FromFire.Target
        ToFire.PlumeType = FromFire.PlumeType
        ToFire.ReferencedFireDefinition = FromFire.ReferencedFireDefinition
        ToFire.Name = FromFire.Name
        ToFire.ChemicalFormula(formula.C) = FromFire.ChemicalFormula(formula.C)
        ToFire.ChemicalFormula(formula.H) = FromFire.ChemicalFormula(formula.H)
        ToFire.ChemicalFormula(formula.O) = FromFire.ChemicalFormula(formula.O)
        ToFire.ChemicalFormula(formula.N) = FromFire.ChemicalFormula(formula.N)
        ToFire.ChemicalFormula(formula.Cl) = FromFire.ChemicalFormula(formula.Cl)
        ToFire.HeatofCombustion = FromFire.HeatofCombustion
        ToFire.RadiativeFraction = FromFire.RadiativeFraction
        ToFire.FlamingTransitionTime = FromFire.FlamingTransitionTime
        ToFire.Height = FromFire.Height
        ToFire.COYield = FromFire.COYield
        ToFire.HClYield = FromFire.HClYield
        ToFire.HCNYield = FromFire.HCNYield
        ToFire.SootYield = FromFire.SootYield
        ToFire.TSYield = FromFire.TSYield
        ToFire.AreaRampID = FromFire.AreaRampID
        ToFire.HeightRampID = FromFire.HeightRampID
        ToFire.CORampID = FromFire.CORampID
        ToFire.HClRampID = FromFire.HClRampID
        ToFire.HCNRampID = FromFire.HCNRampID
        ToFire.SootRampID = FromFire.SootRampID
        ToFire.TraceRampID = FromFire.TraceRampID
        Dim aFireData(12, 0) As Single
        FromFire.GetFireData(aFireData, NumDataPoints)
        ToFire.SetFireData(aFireData)
        ToFire.Changed = FromFire.Changed
    End Sub
    Public Sub PropertyCopy(ByVal FromMaterial As ThermalProperty, ByVal ToMaterial As ThermalProperty)
        ' Copies an entire thermal property record from one structure to another
        ToMaterial.ShortName = FromMaterial.ShortName
        ToMaterial.Name = FromMaterial.Name
        ToMaterial.Conductivity = FromMaterial.Conductivity
        ToMaterial.SpecificHeat = FromMaterial.SpecificHeat
        ToMaterial.Density = FromMaterial.Density
        ToMaterial.Thickness = FromMaterial.Thickness
        ToMaterial.Emissivity = FromMaterial.Emissivity
        Dim Vector1() As Single = {0}
        FromMaterial.GetHCl(Vector1)
        ToMaterial.SetHCl(Vector1)
    End Sub
    Public Sub RampCopy(ByVal FromRamp As Ramp, ByVal ToRamp As Ramp)
        'Copies an entire Ramp from one structure to another
        ToRamp.Name = FromRamp.Name
        ToRamp.IsT = FromRamp.IsT
        ToRamp.Type = FromRamp.Type
        Dim aRampData(2, 0) As Single
        FromRamp.GetRampData(aRampData)
        ToRamp.SetRampData(aRampData)
    End Sub
End Module