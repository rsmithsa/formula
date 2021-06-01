# formula
A simple extensible formula language for .NET

## Syntax
- Arithmetic: + - * % / ^ ( )
- Functions: FunctionName(Parameter, Parameter, ...)
- Variables: VariableName OR [Variable Name With Spaces]
  - Ranges: VariableName|LowerExpr:UpperExpr| OR [Variable Name With Spaces]|LowerExpr:UpperExpr|
- Constants:
  - Number: 123.0
  - Boolean: true/false
  - Text: "A string"
- Branching: IF Expr THEN Expr ELSE Expr
- Logical: = <> > < >= <= ! && ||

### Examples
- (1 + 2) * 3
- SQRT(16)
- MyVar * 2

## Usage
- From C#
  - CsWrapper.InterpretFormula("Test * 2", new Dictionary<string, double>() { { "Test", 1 } }, DefaultFunctionProvider.Instance)
  - CsWrapper.ParseFormula("Test * 2")
- From F#
  - Formula.Parser.Parser.parseFormulaString
  - Formula.Parser.Interpreter.interpretFormula

## Version History

### 0.9.0
- Support escaped characters in string literals
- Missing dependencies reported from dependency extactor with function defined ranges

### 0.8.0
- Better parser error reporting - new ParserException
- Postion information reported from parser
- Postition information reported by dependency extactor

### 0.7.0
- Syntax support for ranges on variables e.g. MyVar|1:5|
- Range support for variable providers
- Range dependency information
- SUM, AVG functions

### 0.6.0
- Typing changes to allow for typed function parameters - number, boolean, text

### 0.5.1
- Target .NETStandard 2.0

### 0.5.0
- Syntax changes
  - Support for escaping identifiers between []
  - Change function parameters to use ()

### 0.4.0
- Financial functions
- Dependency extractor
- Composite IVariableProvider
- Mutable IVariableProvider
- Testing enhancements

### 0.3.0
- Expression compiler
- Branching & logical operators
- Expression based IVariableProvider
- Composite IFunctionProvider

### 0.2.0
- Extension points for functions and variables
  - IFunctionProvider
  - IVariableProvider
- Modulus function & operator
  - MOD
  - %

### 0.1.0
- Support basic arithmetic
  - Addition/Subtraction
  - Multiplication/Division
  - Exponentiation
  - Negation
- Handful of functions
  - SQRT
  - PI
  - POW
  - COUNT
- Constant valued variables
