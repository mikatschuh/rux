# Formal Specification of the Rux Syntax
This is a formal description of the syntax for the Rux programming language. This is also the basis for `src/parser/` which does follow the here described principles. (This got so ugly its probably easier to just read `src/parser`   :(   )
## Tokens
First of all, the semantic units of the Rux language are tokens. Tokens can have arbitrary amounts of whitespace between them. What counts as whitespace is defined by the Unicode Property: `White_Space=yes`.
Tokens are:
- `OPERATOR`: Those consist of the following characters: `!.=+-*/%|&<>:;,()[]{}` and can be compounded freely into many different operators.
- `NAME`: Everything sequence of chars not disrupted by `OPERATOR`s or whitespaces.  
- `KEYWORD` are promoted `NAME`s. Possible ones are `let`, `var`, `continue`, `break`, `return`, `if`, `else`, `match`, `loop`, `for`, `unreachable`, `fn`, `struct`, `enum`, `void`, `never`, `type`, `structtype`, `enumtype`, `bool`, `f16`, `f32`, `f64` and `f128` 
- `LITERAL` are __all__ `NAME`s which starts with a digit. First comes an optional base prefix. The possible bases (case sensitive) are: Binary: `0b`, Seximal: `0s`, Octal: `0o`, Dozenal: `0d`, Hexadecimal: `0x`. If no base prefix is given directly at the start the base will assumed to be Decimal. After that there has to come a body of digits. A body of digits, those are (not case sensitive) normal digits `0`-`9` and letters `a`-`z` or `A`-`Z`. Digits that are too large for a certain base are getting rejected. Then there can come a dot. After the dot another body of digits __can__ follow. After that exponent notation finds its place. If directly after the body either an `e` or in the case of a Hexadecimal base a `p` is placed, then the following chars will be consumed as an exponent. The first char can be either `+` or a `-` or neither in which case a `+` is assumed. When given a `-` the sign of the exponent will be assumed to be negative. When given a `+` the assumed sign will be positive. The exponent itself takes a base prefix and then a body of digits, but not another dot or a nested exponent. 
> __Note:__ the value of a literal is its primary body of digits (both before and after dot) $D$ times its base $B$ to the power of the negative number of digits after the dot $n$: $DB^{-n}$. If an exponent notation exists on the literal then the formula gets more complicated: E being the exponent as an integer, the exponent multiplier is added onto the number which is for every $B≠16$: $B^E$ and for $B=16$ its strictly $2^E$. Making the hole formula: $B=16$ => $DB^{E-n}$; $B≠16$ => $DB^{-n}2^E$.
- `IDENTIFIER`: `IDENTIFIER`s are `NAME`s that didn't get promoted. So everything that gets through essentially. That means that in theory `IDENTIFIER`s can be Emojis and weird unicode characters.
## Syntax Rules
Rules are combination of tokens following certain rules. Rules will have no implicit priority. 
- `EXPR` is just a combination of operators and values, typical a mathematical expression. Certain expressions have a certain binding power and forbid other expressions with a lower binding power to appear as a direct argument of them. When a binding power sensitive expression has two arguments the first one can have the same binding power while the second one must have a strictly higher binding power. This is called left-associativity. The expressions with the highest priority are (priority decreasing): 
  - `UNARY`: 
    - `+ EXPR`
    - `- EXPR`
    - `! EXPR` 
  - `MULTIPLICATIVE`:
    - `EXPR * EXPR`
    - `EXPR / EXPR`
    - `EXPR % EXPR`
  - `ADDITIVE`: 
    - `EXPR + EXPR` 
    - `EXPR - EXPR` 
  - `BITWISE`: 
    - `EXPR | EXPR`
    - `EXPR !| EXPR`
    - `EXPR >| EXPR`      
    - `EXPR !>| EXPR`     
    - `EXPR & EXPR`
    - `EXPR !& EXPR`      
    - `EXPR >> EXPR`      
    - `EXPR >>> EXPR`        
    - `EXPR << EXPR`
  - `COMPARISON`: 
    - `EXPR == EXPR`      
    - `EXPR != EXPR`
    - `EXPR < EXPR`  _alias: `!>=`_ 
    - `EXPR <= EXPR` _alias: `!>`_
    - `EXPR > EXPR`  _alias: `!<=`_
    - `EXPR >= EXPR` _alias: `!<`_
  - `BOOLEANS`: 
    - `EXPR || EXPR`
    - `EXPR !|| EXPR`
    - `EXPR >|| EXPR`
    - `EXPR !>|| EXPR`
    - `EXPR && EXPR`
    - `EXPR !&& EXPR`

  Some expressions also operate outside of the binding power system. They can occur in every place where `EXPR` is required. Those are:
  - `IDENTIFIER: EXPRSTMT`            _Rust: `'label: {}`_
  - `if EXPR EXPRSTMT`                _implicit `else {}`_
  - `if EXPR EXPRSTMT else EXPRSTMT`
  - `match EXPR { BODY }`: the `BODY` is made of an freely decidable number of `PATTERN EXPRSTMT` clauses with no commas in between.  
  - `loop EXPRSTMT`                   _C: `while (true)`_
  - `for EXPR => PATTERN EXPRSTMT`    _for in_
- `EXPRSTMT` can be a `EXPR` but also something that can't be part of a larger expression. Those other things are the following:
  - `IDENTIFIER = EXPR` _assignment_
  - `IDENTIFIER OPERATOR= EXPR`: The assignment operators consist of __any__ infix operator and an equal behind it without whitespace in between. This excludes comparison operators as including them would introduce ambiguity.
  - `IDENTIFIER ++`            _Rust: `+= 1`_
  - `IDENTIFIER --`            _Rust: `-= 1`_
  - `unreachable`              _Rust: `unreachable!()`_
  - `continue EXPR`
  - `continue:IDENTIFIER EXPR` _Rust: `continue 'ident expr`_
  - `break EXPR`
  - `break:IDENTIFIER EXPR`    _Rust: `break 'ident expr`_
  - `return EXPR`
- `DECLSTMT` can be a `EXPRSTMT` but also something that doesnt even make sense as the body of a control flow branch. The things it does include which `EXPRSTMT` doesn't are:
  - `;`                          _optional semicolon_
  - `let IDENTIFIER EXPR = EXPR` 
  - `let IDENTIFIER EXPR`        _checked uninitialization_ 
  - `let IDENTIFIER = EXPR`      _type inference_
  - `var IDENTIFIER EXPR = EXPR`
  - `var IDENTIFIER EXPR`        _checked uninitialization_
  - `var IDENTIFIER = EXPR`      _type inference_
