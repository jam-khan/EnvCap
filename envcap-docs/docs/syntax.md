Here's the complete syntax guide in a clean markdown format that you can save as a `.md` file:

```markdown
# ENVCAP Syntax Guide

## Overview
ENVCAP is a statically-typed functional language that introduces three key innovations:
1. First-class environments
2. Capability-based first-class modules  
3. Separate compilation with unified types and interfaces

## Program Structure

### Module Declarations
```envcap
@pure module ModuleName { ... }
@resource module ModuleName { ... }
```

### Imports and Requirements
```envcap
import Module1, Module2;
require Module3, Module4;
```

## Basic Syntax

### Literals
```envcap
42              // Integer
"hello"         // String
'world'         // Alternative string
True            // Boolean
False           // Boolean
unit            // Unit value
```

### Variables and Bindings
```envcap
val x = 42
let y = "hello"
letrec { fact = λ(n:Int).if n == 0 then 1 else n * fact(n-1) }
```

### Functions
```envcap
function add(x:Int, y:Int):Int { x + y }
λ(x:Int, y:Int).x + y         // Anonymous function
```

## Types

### Basic Types
```envcap
Int
String
Bool
Unit
```

### Composite Types
```envcap
A -> B         // Function type
[A]            // List type
{l:A}          // Record type
A & B          // Intersection type
A | B          // Union type
Sig[A,B]       // Module signature
```

## Expressions

### Arithmetic Operations
```envcap
x + y
x - y  
x * y
x / y
x % y
```

### Comparisons
```envcap
x == y
x != y
x < y
x <= y
x > y
x >= y
```

### Boolean Operations
```envcap
x && y
x || y
!x
```

### Control Flow
```envcap
if (condition) then {e1} else {e2}

match expr of
  case pattern1 => e1
  case pattern2 => e2
```

### Environment Operations
```envcap
env           // Current environment
with e1 in e2 // Evaluate e2 in environment e1
e1 ,, e2      // Environment merge
e.n           // Projection by index
e.field       // Record field access
```

### Modules
```envcap
module M { ... }                     // Simple module
functor F(x:A):B { ... }             // Functor (parameterized module)
module struct(x:A) { ... }           // Anonymous module
open M(N)                            // Module instantiation
```

### ADTs and Pattern Matching
```envcap
type Shape = Circle Int | Rectangle Int Int

let circ = {Circle 10} as Shape

match shape of
  case Circle r => ...
  case Rectangle w h => ...
```

### Lists
```envcap
[1, 2, 3]<Int>          // Typed list
match list of
  case [] => ...
  case (x:xs) => ...
```

## Separate Compilation

### Interface Files
```envcap
// File.epi
@pure interface ModuleName
require Dependency
val x : Int
function f(y:Int):Int
```

## Capability System

### Pure vs Resource Modules
```envcap
@pure module PureModule {
  // Can only import other pure modules
  // Must receive resources as parameters
}

@resource module ResourceModule {
  // Can import both pure and resource modules
  // Can be passed as capabilities
}
```

## Advanced Features

### Let Expressions
```envcap
let {
  x:Int = 1;
  y:String = "test"
} in {
  x + length(y)
}
```

### Records
```envcap
{"field1" = 42, "field2" = "value"}
record.field1
```

### Tuples
```envcap
(1, "two", True)
```

### Tagged Values
```envcap
{Tag value1 value2} as Type
```

### Dependent Merges
```envcap
e1 ,, e2   // e2 can depend on e1
```

### First-class Environments
```envcap
let env1 = {"x"=1} ,, {"y"=2}
with env1 in env.x + env.y
```
```

You can save this directly as `envcap_syntax.md`. The content is properly formatted with clear section headers and code blocks for all syntax examples.
