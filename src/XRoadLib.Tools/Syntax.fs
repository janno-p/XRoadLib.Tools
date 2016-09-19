module XRoadLib.Tools.Syntax

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.Formatting
open System.IO

type Sf = Microsoft.CodeAnalysis.CSharp.SyntaxFactory

type SourceFile =
    { Path: FileInfo
      CompilationUnit: CompilationUnitSyntax }
    static member New(path) =
        { Path = path
          CompilationUnit = Sf.CompilationUnit() }

let attributeTarget = Sf.AttributeTargetSpecifier(Sf.Token(SyntaxKind.AssemblyKeyword))

let addUsing name sourceFile =
    { sourceFile with CompilationUnit = sourceFile.CompilationUnit.AddUsings(Sf.UsingDirective(Sf.IdentifierName(name: string))) }

let addAssemblyDescription description sourceFile =
    let args = Sf.SeparatedList([Sf.AttributeArgument(Sf.LiteralExpression(SyntaxKind.StringLiteralExpression, Sf.Literal(description: string)))])
    let attr = Sf.Attribute(Sf.IdentifierName(Sf.Identifier("AssemblyDescription")), Sf.AttributeArgumentList(args))
    { sourceFile with CompilationUnit = sourceFile.CompilationUnit.AddAttributeLists(Sf.AttributeList(attributeTarget, Sf.SeparatedList([attr]))) }

let saveFile (sourceFile: SourceFile) =
    if not sourceFile.Path.Directory.Exists then
        sourceFile.Path.Directory.Create()
    let node = Formatter.Format(sourceFile.CompilationUnit, new AdhocWorkspace())
    use stream = sourceFile.Path.OpenWrite()
    stream.SetLength(0L)
    use writer = new StreamWriter(stream)
    node.WriteTo(writer)
