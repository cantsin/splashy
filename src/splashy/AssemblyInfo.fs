namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("splashy")>]
[<assembly: AssemblyProductAttribute("splashy")>]
[<assembly: AssemblyDescriptionAttribute("A fluid simulator.")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
