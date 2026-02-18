//-----------------------------------------------------------------------
// <copyright file="ForecastFunctionsTests.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open Formula.Parser.Ast
open Formula.Parser.Integration

[<TestClass>]
type ForecastFunctionsTests () =

    [<TestMethod>]
    member this.TestForecastFunctionList () =
        Assert.IsTrue(ForecastFunctionProvider.Instance.IsDefined "SES")
        Assert.IsTrue(ForecastFunctionProvider.Instance.IsDefined "FORECAST")
        Assert.IsTrue(ForecastFunctionProvider.Instance.IsDefined "HOLT")
        Assert.IsTrue(ForecastFunctionProvider.Instance.IsDefined "DES")
        Assert.IsTrue(ForecastFunctionProvider.Instance.IsDefined "HWA")
        Assert.IsTrue(ForecastFunctionProvider.Instance.IsDefined "HOLTWINTERS")
        Assert.IsTrue(ForecastFunctionProvider.Instance.IsDefined "HWM")
        Assert.IsTrue(ForecastFunctionProvider.Instance.IsDefined "HOLTWINTERSM")

    [<TestMethod>]
    member this.TestForecastFunctionListCount () =
        let count = ForecastFunctionProvider.Instance.KnownFunctions |> Seq.length
        Assert.AreEqual(8, count)

    [<TestMethod>]
    member this.TestForecastFunctionSes () =
        let functionImplementation = ForecastFunctionProvider.Instance.Lookup "SES"
        Assert.AreEqual("SES", functionImplementation.Name)

        // Validate
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [Number(0.5); Number(10.0); Number(12.0)]))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [Number(0.5); Number(10.0); Number(12.0); Number(14.0)]))
        Assert.AreEqual((false, "SES expects at least three arguments."), functionImplementation.Validate (List.toArray []))
        Assert.AreEqual((false, "SES expects at least three arguments."), functionImplementation.Validate (List.toArray [Number(0.5)]))
        Assert.AreEqual((false, "SES expects at least three arguments."), functionImplementation.Validate (List.toArray [Number(0.5); Number(10.0)]))
        Assert.AreEqual((false, "SES expects at least three arguments."), functionImplementation.Validate (null))

        // SES(0.5, 10, 12, 14): s0=10, s1=0.5*12+0.5*10=11, s2=0.5*14+0.5*11=12.5
        Assert.AreEqual(12.5, (functionImplementation.Execute (List.toArray [Number(0.5); Number(10.0); Number(12.0); Number(14.0)])).NumberValue, 0.000001)

        // SES(0.3, 100, 110, 120): s0=100, s1=0.3*110+0.7*100=103, s2=0.3*120+0.7*103=108.1
        Assert.AreEqual(108.1, (functionImplementation.Execute (List.toArray [Number(0.3); Number(100.0); Number(110.0); Number(120.0)])).NumberValue, 0.000001)

        // Nothing values in data are skipped: SES(0.5, 10, Nothing, 12, 14) same as SES(0.5, 10, 12, 14) = 12.5
        Assert.AreEqual(12.5, (functionImplementation.Execute (List.toArray [Number(0.5); Number(10.0); Nothing; Number(12.0); Number(14.0)])).NumberValue, 0.000001)

        // All data Nothing returns Nothing
        Assert.AreEqual(Nothing, functionImplementation.Execute (List.toArray [Number(0.5); Nothing; Nothing]))

    [<TestMethod>]
    member this.TestForecastFunctionSesAlias () =
        let functionImplementation = ForecastFunctionProvider.Instance.Lookup "FORECAST"
        Assert.AreEqual("SES", functionImplementation.Name)
        Assert.AreEqual(12.5, (functionImplementation.Execute (List.toArray [Number(0.5); Number(10.0); Number(12.0); Number(14.0)])).NumberValue, 0.000001)

    [<TestMethod>]
    member this.TestForecastFunctionHolt () =
        let functionImplementation = ForecastFunctionProvider.Instance.Lookup "HOLT"
        Assert.AreEqual("HOLT", functionImplementation.Name)

        // Validate
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [Number(0.5); Number(0.3); Number(10.0); Number(12.0)]))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [Number(0.5); Number(0.3); Number(10.0); Number(12.0); Number(14.0)]))
        Assert.AreEqual((false, "HOLT expects at least four arguments."), functionImplementation.Validate (List.toArray []))
        Assert.AreEqual((false, "HOLT expects at least four arguments."), functionImplementation.Validate (List.toArray [Number(0.5); Number(0.3)]))
        Assert.AreEqual((false, "HOLT expects at least four arguments."), functionImplementation.Validate (List.toArray [Number(0.5); Number(0.3); Number(10.0)]))
        Assert.AreEqual((false, "HOLT expects at least four arguments."), functionImplementation.Validate (null))

        // HOLT(0.5, 0.3, 10, 12, 14):
        //   level0=10, trend0=2
        //   i=1: level1=0.5*12+0.5*(10+2)=12, trend1=0.3*(12-10)+0.7*2=2
        //   i=2: level2=0.5*14+0.5*(12+2)=14, trend2=0.3*(14-12)+0.7*2=2
        //   forecast=14+2=16
        Assert.AreEqual(16.0, (functionImplementation.Execute (List.toArray [Number(0.5); Number(0.3); Number(10.0); Number(12.0); Number(14.0)])).NumberValue, 0.000001)

        // HOLT(0.8, 0.2, 100, 110, 115):
        //   level0=100, trend0=10
        //   i=1: level1=0.8*110+0.2*(100+10)=110, trend1=0.2*(110-100)+0.8*10=10
        //   i=2: level2=0.8*115+0.2*(110+10)=116, trend2=0.2*(116-110)+0.8*10=9.2
        //   forecast=116+9.2=125.2
        Assert.AreEqual(125.2, (functionImplementation.Execute (List.toArray [Number(0.8); Number(0.2); Number(100.0); Number(110.0); Number(115.0)])).NumberValue, 0.000001)

        // Nothing values in data are skipped: HOLT(0.5, 0.3, 10, Nothing, 12, 14) same as HOLT(0.5, 0.3, 10, 12, 14) = 16
        Assert.AreEqual(16.0, (functionImplementation.Execute (List.toArray [Number(0.5); Number(0.3); Number(10.0); Nothing; Number(12.0); Number(14.0)])).NumberValue, 0.000001)

        // Insufficient data after filtering returns Nothing
        Assert.AreEqual(Nothing, functionImplementation.Execute (List.toArray [Number(0.5); Number(0.3); Number(10.0); Nothing]))

    [<TestMethod>]
    member this.TestForecastFunctionHoltAlias () =
        let functionImplementation = ForecastFunctionProvider.Instance.Lookup "DES"
        Assert.AreEqual("HOLT", functionImplementation.Name)
        Assert.AreEqual(16.0, (functionImplementation.Execute (List.toArray [Number(0.5); Number(0.3); Number(10.0); Number(12.0); Number(14.0)])).NumberValue, 0.000001)

    [<TestMethod>]
    member this.TestForecastFunctionHwa () =
        let functionImplementation = ForecastFunctionProvider.Instance.Lookup "HWA"
        Assert.AreEqual("HWA", functionImplementation.Name)

        // Validate
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [Number(0.5); Number(0.3); Number(0.4); Number(3.0); Number(10.0); Number(20.0); Number(30.0)]))
        Assert.AreEqual((false, "HWA expects at least seven arguments."), functionImplementation.Validate (List.toArray []))
        Assert.AreEqual((false, "HWA expects at least seven arguments."), functionImplementation.Validate (List.toArray [Number(0.5); Number(0.3); Number(0.4); Number(3.0); Number(10.0); Number(20.0)]))
        Assert.AreEqual((false, "HWA expects at least seven arguments."), functionImplementation.Validate (null))

        // HWA(0.5, 0.3, 0.4, 3, 10, 20, 30, 12, 22, 32):
        //   period=3, avg(first season)=(10+20+30)/3=20
        //   seasonal[0]=10-20=-10, seasonal[1]=20-20=0, seasonal[2]=30-20=10
        //   level0=20, trend0=(12-10)/3=2/3
        //   i=3: level3=0.5*(12-(-10))+0.5*(20+2/3)=0.5*22+0.5*20.6667=11+10.3333=21.3333
        //         trend3=0.3*(21.3333-20)+0.7*0.6667=0.4+0.4667=0.8667
        //         seasonal[3]=0.4*(12-21.3333)+0.6*(-10)=0.4*(-9.3333)+(-6)=-3.7333-6=-9.7333
        //   i=4: level4=0.5*(22-0)+0.5*(21.3333+0.8667)=11+11.1=22.1
        //         trend4=0.3*(22.1-21.3333)+0.7*0.8667=0.23+0.6067=0.8367
        //         seasonal[4]=0.4*(22-22.1)+0.6*0=-0.04
        //   i=5: level5=0.5*(32-10)+0.5*(22.1+0.8367)=11+11.4683=22.4683
        //         trend5=0.3*(22.4683-22.1)+0.7*0.8367=0.1105+0.5857=0.6962
        //         seasonal[5]=0.4*(32-22.4683)+0.6*10=3.8127+6=9.8127
        //   forecast=22.4683+0.6962+seasonal[6-3]=22.4683+0.6962+seasonal[3]
        //           =22.4683+0.6962+(-9.7333)=13.4312
        let result = (functionImplementation.Execute (List.toArray [Number(0.5); Number(0.3); Number(0.4); Number(3.0); Number(10.0); Number(20.0); Number(30.0); Number(12.0); Number(22.0); Number(32.0)])).NumberValue
        Assert.AreEqual(13.4312, result, 0.001)

        // Nothing values in data are skipped
        let resultWithNulls = (functionImplementation.Execute (List.toArray [Number(0.5); Number(0.3); Number(0.4); Number(3.0); Number(10.0); Number(20.0); Nothing; Number(30.0); Number(12.0); Number(22.0); Number(32.0)])).NumberValue
        Assert.AreEqual(result, resultWithNulls, 0.000001)

        // Insufficient data after filtering returns Nothing
        Assert.AreEqual(Nothing, functionImplementation.Execute (List.toArray [Number(0.5); Number(0.3); Number(0.4); Number(3.0); Number(10.0); Number(20.0); Number(30.0)]))

    [<TestMethod>]
    member this.TestForecastFunctionHwaAlias () =
        let functionImplementation = ForecastFunctionProvider.Instance.Lookup "HOLTWINTERS"
        Assert.AreEqual("HWA", functionImplementation.Name)

    [<TestMethod>]
    member this.TestForecastFunctionHwm () =
        let functionImplementation = ForecastFunctionProvider.Instance.Lookup "HWM"
        Assert.AreEqual("HWM", functionImplementation.Name)

        // Validate
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [Number(0.5); Number(0.3); Number(0.4); Number(3.0); Number(10.0); Number(20.0); Number(30.0)]))
        Assert.AreEqual((false, "HWM expects at least seven arguments."), functionImplementation.Validate (List.toArray []))
        Assert.AreEqual((false, "HWM expects at least seven arguments."), functionImplementation.Validate (List.toArray [Number(0.5); Number(0.3); Number(0.4); Number(3.0); Number(10.0); Number(20.0)]))
        Assert.AreEqual((false, "HWM expects at least seven arguments."), functionImplementation.Validate (null))

        // HWM(0.5, 0.3, 0.4, 3, 10, 20, 30, 12, 24, 36):
        //   period=3, avg(first season)=(10+20+30)/3=20
        //   seasonal[0]=10/20=0.5, seasonal[1]=20/20=1.0, seasonal[2]=30/20=1.5
        //   level0=20, trend0=(12-10)/3=2/3
        //   i=3: level3=0.5*(12/0.5)+0.5*(20+2/3)=0.5*24+0.5*20.6667=12+10.3333=22.3333
        //         trend3=0.3*(22.3333-20)+0.7*0.6667=0.7+0.4667=1.1667
        //         seasonal[3]=0.4*(12/22.3333)+0.6*0.5=0.4*0.5373+0.3=0.2149+0.3=0.5149
        //   i=4: level4=0.5*(24/1.0)+0.5*(22.3333+1.1667)=12+11.75=23.75
        //         trend4=0.3*(23.75-22.3333)+0.7*1.1667=0.425+0.8167=1.2417
        //         seasonal[4]=0.4*(24/23.75)+0.6*1.0=0.4*1.0105+0.6=0.4042+0.6=1.0042
        //   i=5: level5=0.5*(36/1.5)+0.5*(23.75+1.2417)=12+12.4958=24.4958
        //         trend5=0.3*(24.4958-23.75)+0.7*1.2417=0.2238+0.8692=1.0930
        //         seasonal[5]=0.4*(36/24.4958)+0.6*1.5=0.4*1.4696+0.9=0.5878+0.9=1.4878
        //   forecast=(24.4958+1.0930)*seasonal[6-3]=(25.5889)*seasonal[3]
        //           =25.5889*0.5149=13.1758
        let result = (functionImplementation.Execute (List.toArray [Number(0.5); Number(0.3); Number(0.4); Number(3.0); Number(10.0); Number(20.0); Number(30.0); Number(12.0); Number(24.0); Number(36.0)])).NumberValue
        Assert.AreEqual(13.1758, result, 0.01)

        // Nothing values in data are skipped
        let resultWithNulls = (functionImplementation.Execute (List.toArray [Number(0.5); Number(0.3); Number(0.4); Number(3.0); Nothing; Number(10.0); Number(20.0); Number(30.0); Number(12.0); Number(24.0); Number(36.0)])).NumberValue
        Assert.AreEqual(result, resultWithNulls, 0.000001)

        // Insufficient data after filtering returns Nothing
        Assert.AreEqual(Nothing, functionImplementation.Execute (List.toArray [Number(0.5); Number(0.3); Number(0.4); Number(3.0); Number(10.0); Number(20.0); Number(30.0)]))

    [<TestMethod>]
    member this.TestForecastFunctionHwmAlias () =
        let functionImplementation = ForecastFunctionProvider.Instance.Lookup "HOLTWINTERSM"
        Assert.AreEqual("HWM", functionImplementation.Name)
