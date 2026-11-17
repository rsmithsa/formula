//-----------------------------------------------------------------------
// <copyright file="AssemblyInfo.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

module AssemblyInfo

open System.Reflection
open System.Runtime.InteropServices

[<assembly: AssemblyFileVersionAttribute("2.0.0.0")>]
[<assembly: AssemblyInformationalVersionAttribute("2.0.0-beta001")>]
[<assembly: AssemblyProductAttribute("Formula.Parser")>]
[<assembly: AssemblyTitleAttribute("Formula.Parser")>]
[<assembly: AssemblyVersionAttribute("2.0.0.0")>]
[<assembly: AssemblyDescriptionAttribute("A simple extensible formula language for .NET")>]
[<assembly: AssemblyCopyrightAttribute("Richard Smith")>]
[<assembly: System.Runtime.CompilerServices.InternalsVisibleTo("Formula.Parser.Tests")>]
do ()

