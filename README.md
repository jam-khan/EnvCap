# EnvCap: Capabilities as First-Class Modules

## Overview

EnvCap is an environment-based programming language with first-class capabilities and modules. The language builds upon λ_E, a core calculus that uses small-step environment-based semantics instead of traditional substitution-based approaches.

Key features include:
- **Environment-Based Semantics**: Closer to practical implementations than substitution-based approaches
- **First-Class Environments**: Treat environments as first-class values
- **Capability-Based Module System**: Control authority through capabilities
- **Basic Operations**: Application, merging, and projection operations
- **Advanced Features**: Lists, Algebraic Data Types, Pattern Matching on ADTs and Lists, Recursion and _first-class_ environments.
- **Bidirectional Type Checker**: For static type safety

## Getting Started

### Prerequisites

- [Haskell Tool Stack](https://haskellstack.org/) or Cabal
- Happy parser generator (`cabal install happy` or `stack install happy`)

### Installation

1. Clone this repository
2. Generate parsers:
   ```bash
   happy src/ENVCAP/Parser/Implementation/ParseImp.y --ghc --info
   happy src/ENVCAP/Parser/Interface/ParseIntf.y --ghc --info
   ```
3. Build the project:
   ```bash
   cabal build
   ```
   or
   ```bash
   stack build
   ```

### Running Programs

To run a file:
```bash
cabal repl
ghci> :l ENVCAP.Interpreter
ghci> runFile "examples/Modules/Modules3.ep"
```

For the REPL:
```bash
cabal repl
ghci> :l ENVCAP.Repl
ghci> repl
```
Then enter expressions like:
```
module Test 1 + 2
```

## Language Overview

EnvCap extends the λ_E calculus with capabilities and modules. The environment-based semantics provide several benefits:

1. **Simpler formal reasoning**: Avoids complications of substitution
2. **Closer to implementations**: Uses environments and closures like real implementations
3. **Expressive power**: First-class environments enable new programming patterns

The language can model:
- First-class modules
- Capability-based security patterns
- Traditional functional programming constructs

Work-in progress (beyond scope of HKU FYP):
1. Dependency Management
2. Separate Compilation

## References

1. Tan, J., & Oliveira, B. C. d. S. (2024). A Case for First-Class Environments. *Proceedings of the ACM on Programming Languages*, 8(OOPSLA2), 360. https://doi.org/10.1145/3689800

2. Tan, J., & Oliveira, B. C. d. S. (2023). Dependent Merges and First-Class Environments. *ECOOP 2023*. https://doi.org/10.4230/LIPIcs.ECOOP.2023.34

3. Melicher, D., Shi, Y., Potanin, A., & Aldrich, J. (2017). A Capability-Based Module System for Authority Control. *ECOOP 2017*. https://doi.org/10.4230/LIPIcs.ECOOP.2017.20

4. Steed, G. (2016). *A Principled Design of Capabilities in Pony* [Master's thesis]. Imperial College London.

5. Cardelli, L. (1997). Program fragments, linking, and modularization. *POPL '97*. https://doi.org/10.1145/263699.263735

6. Pierce, B. C. (2004). *Advanced Topics in Types and Programming Languages*. MIT Press.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.
