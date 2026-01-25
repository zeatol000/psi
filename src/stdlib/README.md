# Standard Library

<!--
─
│
├
└
-->

## Structure
```
src/stdlib/
├─ psi_std.psi      - psi
├─ _/               - psi._
├─ jvm/             - psi.jvm
├─ nat/             - psi.nat
└─ qu/              - psi.qu
```

---
## Overview

#### psi
Built-in definitions required at compiletime.  
Includes basic stuff such as `obj System` and primitive data types.

#### psi.\_
Libraries available for all compilation targets.

#### psi.jvm
Values isolated to only the JVM runtime such as System.arraycopy.

#### psi.nat
Values isolated to only native runtime such as unsigned integers.

#### psi.qu
Values isolated for only quantum computers.  
Primarily contains values such as `class Qubit`.
