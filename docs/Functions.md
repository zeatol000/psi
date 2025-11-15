# Functions

Psi has a lot of different function types; pro, sub, co, fn, and op.
Op, or operation, does stuff only for quantum computing and is more advanced  
than average functions. Because of this, it has it's own section, [Quantum](./Quantum.md)

## Programs

Programs are declared with the keyword `pro`.
As you may expect, they signify the entrypoint of a program.

### Arguments

By default, all programs are pushed these parameters:
```psi
pro main(args: String*): Null = ...
```
All command line args (split by spaces) are pushed into a Seq[String], excluding the name of the program.

However, explicit argument syntax can be applied
```psi
pro main(a: String*, b: Int): Null = ...
```
This example would throw all arguments into a, then when it finds an argument  
that is only an integer, it will be thrown into b and the rest of the args will  
be ignored. Failure to fullfill all of the args will throw an error.            <!-- TODO: spell check :< -->  
This can be prevented by setting the type to T|Null, as null type arguments are  
ignored

Most programs have the ability to push flags, Psi included. This can be easily  
done by having a second set of arguments.
```psi
pro ls(dir: String|Null)(a: Bit = false, ...): U8 = ...
```
The name of the parameter determines how to call the flag. The above example  
recreates the `ls` command, with the possible flag -a. Other data types can be  
pushed as flags. When this happens, the next argument will pe set as that flag.  
All flags must either have a default value or be type T|Null.

### Concurrency

Projects can have multiple programs in each. The only requirement is that the  
programs have either or both: differing names or differing packages.

All programs will have access to the same command line args so if one program  
doesn't have an arguement correctly passed, it may cause problems for the  
entire project.

### Misc

* Programs cannot be called by other functions.
* If one program exits, all do aswell.
* Programs can have exit values of Null, integers, and unsigned integers


## Functions

Functions are the functions that almost every language has.
They are declared with the keyword `fn`.

### Arguments

Functions have 4 types of arguments:
* () - Normal data parameters
* [] - Type parameters
* {} - Code block parameters
* <> - Reference 'parameters'

#### ( Data )

Data parameters are extremely simple and can be understood by just looking at  
examples. However function parameters differ from program parameters in 1 key  
way: null values can be passed, even if the type of the parameter does not  
inherit Null. Example:
```psi
fn f(x: Int): Int = {
    ret x + 2
}

pro main(): Null = {

    println(3)      // prints 5
    println(0.2)    // type error: cannot pass Float|Double into Int
    println(null)   // type error: value + is not a value of module Null

}
```

#### [ Type ]

Type parameters can be used just as parameters, as adjustments for data  
parameter types, or even changing the return type of the function. Example:
```psi
fn f[T](x: T): T = ...

pro main(): Null = {

    println(f[Int](4))  // explicit type push
    println(f("hi"))    // implicit type push
    println(f(null))    // type error: type T cannot be inferred from null as  
null: Null extends all data types
}
```

#### { Block }

Block parameters and data parameters are basically the same thing. The only  
difference is that block parameters force the user to push code blocks instead  
of just letting the user do what they want. Ex:
```psi
fn whileV2(x: Bit){y: => Null}: Null = {
    if (x) {
        y
        whileV2(x){y}
    }
}

fn lambda{x: Int => Bit}: Bit = {
    for (i <- 1 to 14) {
        val temp = x(i)
        if (!temp) ret false
    }
}



```
