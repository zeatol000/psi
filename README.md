# Psi - Scala Based Language

Psi is a language that is based off of Scala and (sorta) Rust.  
It compiles to Java and runs on the JVM, however it also supports some fun stuff that wouldn't be possible in almost any other language.

<!-- 
!NOTE:
- Use the Scala 1.0.0-b5 (earliest) compiler as examples
- Use Scala 3 compiler for better examples
- Get it working before making it good

!TODO:
- [ ] Docs:
   -  Options.md        - Language options like -source, -lethal-warns, etc
   -  Syntax.md         - Syntax highlighting
   -  Annotations.md    - Annotations
   -  Children.md       - Children of classes and modules
   -  Quantum.md        - Quantum stuff
   -  Functions.md      - Programs, Subroutines, Coroutines, and functions
   -  Data.md           - Data types
   -  Modules.md        - Module bullshit.
   -  References.md     - 
   -  
- [ ] Actual Compiler:
   -  Divide into phases:
      Stuff like parsing, typer, super calls, etc.
      Use each phase as an instantialised class
   -  Initial - parses in-file options and throws errors with missing files
   -  Parser - First change each file into a token stream. Then turn it into
        An untyped AST for the typer to then turn into a Typed AST (TAST)
   -  Analyzer and Typer - Check member names and other things.
        Typer will also create something called a Member Tree, which
        represents abstract members to allow for multi-threaded compilation.
   - ...
- [ ] Builtin Packages:
   -  Stuff like Lists and stuff
   -
-->


## Philosophy

You should have control over your code. Too many abstraction layers destroys control.         <!-- cough cough, python.. -->
Compilation time is irrelevant as long as it means efficient or fast code.

## Usage

For the command line, these are the large majority of commands that you'll use:  
```bash
$ psi                       # point to a help menu
$ psi main.psi              # run file main.psi
$ psi main.psi -- hi        # run file main.psi and push arg "hi"
$ psi repl                  # open the REPL
$ psi repl main.psi         # open the REPL, but include main.psi's values
$ psi package main.psi      # compile main.psi into an executable (if it has an entrypoint)
                            # or compile main.psi into a jar file for other projects.
```
<!-- other examples -->
For other options, see [language options](./docs/Options.md)

Language options can be included in the command line by just using the `--<whatever>` flag.
Ex:
```bash
$ psi --lethal-warns main.psi
```

This can get bloated if you choose to run a bunch of options, so luckily psi has in-file configuration:  
In either your main file, or a file named something useful like 'build.psi',
At the top of the file, you will add some lines that begin with >  
\> declares a single line compiler option
Example:
```psi
>--lethal-warns
>main.psi
>test.psi
>util.psi
```

You can also change arguments depending on the mode of the compiler..
```psi
>--verbose

>in run
>--no-warns

>in compile
>--lethal-warns

>in package
>--lethal-warns
```

## Syntax

Psi itself looks similar to Scala and Rust.
But it also uses a few more syntaxes, like:
- // comments       - Single line comments.
- /* comments */    - Multi line comments.
- @annotations      - Annotations are ways to tell the compiler what to do.     ( [Annotations](./docs/Annotations.md) )
- ( parameters )    - Parameters for functions, classes, lists, hash maps, etc.
- { code }          - Code blocks.
- \[ type \]          - Type parameters and bounds.
                      For (), {}, [], and <>, see [Syntax](./docs/Syntax.md).
- :                 - Declare a type for a value.
- =                 - Assign a value to a variable.
- .                 - Access a value in a class or object.
- ::                - Access a value in a module.
                      See [Children](./docs/Children.md).
Other syntaxes can be found in [Syntax](./docs/Syntax.md).
But for the most part, most syntaxes are self-explanatory or arent built into Psi.
Such as Int + Int. + isn't a syntax, its a method,
meaning that 1 + 2 can be rewritten as 1.+(2). Hideous, I know.

## Value types

This section does not talk about data types like Int, String, etc.
It talks about what the values can be.
Psi supports most types supported by Java, as well as some more:
- val               - Immutable value.
- var               - Mutable value.
- pro               - Program: the entrypoint for a Psi program.                ( [Functions](./docs/Functions.md) )
- fn                - Function: where most of your code goes.                   ( [Functions](./docs/Functions.md) )
- sub               - Subroutine: used for concurrent syncronised code.         ( [Functions](./docs/Functions.md) )
- co                - Coroutine: used for concurrent unsyncronised code
- op                - Operation: experimental features for quantum computiung   ( [Quantum](./docs/Quantum.md) )
- pack              - Package: used for multi-file code.
- obj               - Object. Effectively a package but inside another value like a class or package. Mostly usef for organizing code.
- class             - Intsantialisable class. Can take parameters, unlike obj or pack.
- trait             - Traits are like classes, but cannot be instantiated. Mostly used for interfaces or organizing values.
- type              - Aliases. Effectively just ways of naming values. Eg. `type foo = bar[Int]`
                      Can also be used for naming functions in different ways. `type foo_bar(x) = fooBar(x)`

## Primitive types

Name            Type aliases        Description
- Byte              - I8            Default for small iterables
- Short             - I16
- Int               - I32           Default for most values
- Long              - I64
- Float             - F32
- Double            - F64           Default for decimals
- Char              - Character
- String            - String
- Bit               - Boolean
- Unsigned          - U16           Not really 'primitive' but its basically just a char being used as a number
- Null              - Unit, Void    Represents nothing. Should not be assigned to variables, only functions.

[Types](./docs/Types.md)

<!--
## Classes vs Modules

Classes (including objects and packages) can have values inside them.
Lets take this example:
```psi
pack example

class main {
  val foo = 1
  val bar = 2
  val baz = 3
}
```
This results in a package that contains a class with 3 values.
If you wanted to access the values, you would do:
```psi
import example.*

val a = new main
a.foo               // 1
```
Makes sense, right?

Modules are similar to classes/objects, but they cannot be declared in the normal sense.
Lets take the example from above.
The package is of module package. So it can be represented as package::example.
The class main is of module class. But since it has not been instantiated, it is not a member of a module.
However, the value a has been instantiated, and so it is a member of a module.
That means that it can be represented as main::a.
Also meaning that a.foo can be represented as a.Int::foo or Int::a.foo.

So this is just type checking? No.
Because these values are members of different modules, you can do stuff like:
```psi
val x: Int = 8          // Int::x, valid
val x: String = "hi"    // String::x, also valid
val x: Int = 2          // Int::x has already been declared, so invalid
```

This gets a lot more complicated especially if you try to print one of the values.

[Modules](./docs/Modules.md)
-->

