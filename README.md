# The Phage Programming Language

## Usage

```bash
$ stack build        # to build
$ stack exec phage   # gives a repl
```

## Implemented

* Arithmetic
* Function Application
* REPL

## TODO

* Function definition
* Do blocks

## Using the REPL

```
$ stack exec phage
The Phage Programming Language REPL
0.1 pre-alpha
λ: 42
>> 42
λ: (+ 3 5)
>> 8
λ: (min 5 6)
>> 5
λ: (/ 8 3)
>> 2
λ: (% 8 3)
>> 2
λ: (+ 3)
>> <func | arity: 1, bound params: [3]>
λ: ((+ 3) 5)
>> 8
```
