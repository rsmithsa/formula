# formula
A simple extensible formula language for .NET

## Syntax
- Arithmetic: + - * % / ^ ( )
- Functions: FunctionName[Parameter, Parameter, ...]
- Variables: VariableName

### Examples
- (1 + 2) * 3
- SQRT[16]
- MyVar * 2

## Usage
- From C#
  - CsWrapper.InterpretFormula("Test * 2", new Dictionary<string, double>() { { "Test", 1 } }, DefaultFunctionProvider.Instance)
  - CsWrapper.ParseFormula("Test * 2")
- From F#
  - Formula.Parser.Parser.parseFormulaString
  - Formula.Parser.Interpreter.interpretFormula

## Version History

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
