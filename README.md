# formula
A simple extensible formula language for .NET

## Syntax
- Arithmetic: + - * % / ^ ( )
- Functions: FunctionName(Parameter, Parameter, ...)
- Variables: VariableName OR [Variable Name With Spaces]
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
