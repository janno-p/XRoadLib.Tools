module XRoadLib.Tools.Syntax

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.Formatting
open System
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

let addNamespace name content sourceFile =
    let ns = Sf.NamespaceDeclaration(Sf.IdentifierName(name: string)).AddMembers(content |> Seq.toArray)
    { sourceFile with CompilationUnit = sourceFile.CompilationUnit.AddMembers(ns) }

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

let token arg = Sf.Token(arg: SyntaxKind)
let semicolonToken = token SyntaxKind.SemicolonToken
let parseTypeName arg = Sf.ParseTypeName(arg)

let addClassMembers (xs: seq<MemberDeclarationSyntax>) (cls: ClassDeclarationSyntax) =
    cls.AddMembers(xs |> Array.ofSeq)

let declareClass name =
    let cls = Sf.ClassDeclaration(name: string)
    cls.AddModifiers(token SyntaxKind.PublicKeyword)

let declareInterface name =
    let iface = Sf.InterfaceDeclaration(name: string)
    iface.AddModifiers(token SyntaxKind.PublicKeyword)

let declareProperty typeName (name: string) initializer =
    let prp = Sf.PropertyDeclaration(parseTypeName typeName, name)
    prp.AddModifiers(token SyntaxKind.PublicKeyword)
       .AddAccessorListAccessors(Sf.AccessorDeclaration(SyntaxKind.GetAccessorDeclaration).WithSemicolonToken(semicolonToken))
       .WithInitializer(Sf.EqualsValueClause(initializer))
       .WithSemicolonToken(semicolonToken)

let stringLiteral v = Sf.LiteralExpression(SyntaxKind.StringLiteralExpression, Sf.Literal(v: string)) :> ExpressionSyntax

let createObjExpr typeName =
    Sf.ObjectCreationExpression(parseTypeName typeName)

let addArguments (args: ExpressionSyntax list) (o: ObjectCreationExpressionSyntax) =
    o.AddArgumentListArguments(args |> List.map Sf.Argument |> List.toArray)

let addBaseType name (cls: ClassDeclarationSyntax) =
    cls.AddBaseListTypes(Sf.SimpleBaseType(parseTypeName name))

let createCtor name =
    let ctor = Sf.ConstructorDeclaration(name: string)
    ctor.AddModifiers(token SyntaxKind.PublicKeyword)

let addParameters (param: ParameterSyntax list) (ctor: ConstructorDeclarationSyntax) =
    ctor.AddParameterListParameters(param |> List.toArray)

let addBody body (ctor: ConstructorDeclarationSyntax) =
    ctor.WithBody(body)

let emptyBlock = Sf.Block()

let toMember (x: #MemberDeclarationSyntax) = x :> MemberDeclarationSyntax
