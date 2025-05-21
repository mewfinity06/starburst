# Starburst - A compiler for Nova

![image](./images/icon.svg)

## Goals

## Syntax

### Variable decl

```
-- Mutable variable decl #
mut foo |: 4

-- Immutable variable decl (can be made into mutable) #
val bar |: 8

-- Const variable decl #
const BAZ |: 12

-- With type annotations #
var foo_bar | Int : foo + bar
```

### Functions

```
-- Named function, must be const'ed
const add | func [a, b: Int] -> Int :  a + b
const sub | func [a: Int, b: Int] -> Int : {
    a - b
}

-- Anon function
func [arg1: Int, arg2: Int] -> Int : { 2 * arg1 / arg2 }

-- Calling function
val five_plus_four |: add[a: 5, b: 4]
val seven_sub_two  |: sub[7, 2]
```

