# axolotl-lang
🌊 a statically typed lisp-like programming language

```clojure
(def name (str "hi, " "i am" " udit!"))
(print (str name " nice to meet you"))

(print "this is my programming language, axolotl")

(defun get-count [(argument-for-no-reason: int)] {
  (+i argument-for-no-reason 1)
})

(def text1 "If I have ")

(defun arbitrary-function [(initial: int) (text2: string)] {
  (print "expressions are written in braces")
  (print "and the last value is returned!")
  (print (str text1 (+i initial 2) " apples and I give " (get-count 2) text2 (/f (+i initial 2) (get-count 4)) " apples!"))
  420
})

(def initial 37)
(def t2 " apples to everyone, i'll be left with ")

(print (arbitrary-function initial t2))
```
_Outputs:_
```
hi, i am udit! nice to meet you
this is my programming language, axolotl
expressions are written in braces
and the last value is returned!
If I have 39 apples and I give 3 apples to everyone, i'll be left with 7.8 apples!
420
```

# Syntax Guide
## Files
Axolotl uses the `.axl` file extension. Note that this is just for convenience, and the language tooling does not enforce it in any way.
Files can be run using `axl` -- `axl run program.axl`
  
## Comments
Single line comments start with `;` and multi-line comments start and end with `#|` and `|#` respectively.
Note that as of 0.1.0.0 alpha, there's a parsing bug when a comment is the first thing in an axl file.
  
## Calling Functions
Since axolotl is lisp-like, functions are called using `(function-name arg1 arg2 ...)` instead of `functionName(arg1, arg2, ...)` that many other languages use.
  
## Variables
Variables can be declared using the `def` function, where the first argument is the name of the variable (optionally typed) and the second argument is the value to assign to it:
```clojure
;; type of name will be inferred
(def name "cody") ;; creates a variable 'name' with the value "cody"
;; you can also manually define the type
(def (name2: string) "paul") ;; creates a variable 'name2' with the value "paul"
(print name " and " name2 " are friends") ;; cody and paul are friends
```
Variables are accessible in their immediate scope **after** they are defined, and are immutable; as of 0.1.0.0 alpha there's no way to mutate them, though something is planned.

## Functions
Functions can be defined using the `defun` function, where the first argument is the name of the function (optionally typed), the second argument is an array of arguments to it (which must be manually typed), and the third argument is the set of expressions to evaluate inside braces (this creates a new scope that includes all the defined variables above the function definition, and the arguments to it:
```clojure
;; argument types can't be inferred and must be defined manually
;; return type of the function will be inferred automatically
(defun get-wish [(name: string) (time: string)] { (str "good " time ", " name) })
(print (get-wish "Tom" "morning")) ;; good morning, Tom
;; you can also define the return type manually
(defun (get-goodbye: string) [(name: string) (section-of-day: string)] { (str "have a good " section-of-day ", " name) })
(print (get-goodbye "Matt" "evening")) ;; have a good evening, Matt
```
  
## Mathematical Functions
Only the operators +, -, * and / are available for now, each is a function that can take any number of arguments.
As of 0.1.0.0 alpha, there are two versions of these operators - one that returns an integer, and one that returns a float.
The integer version rounds the result if it gets a float, and the float version converts to float if it gets an int.
If you want the integer version, append `i` to the function name, and if you want the float version, append `f` to the function name.
For example,
```clojure
(print (+i 2 4.95)) ;; 7
(print (+f 2 4.95)) ;; 6.95

(print (-i 2 4.95)) ;; -3
(print (-f 2 4.95)) ;; -2.95

(print (*i 2 4.95)) ;; 10
(print (*f 2 4.95)) ;; 9.9

(print (/i 2 4.95)) ;; 0
(print (/f 2 4.95)) ;; 0.40404040404040403
```
  
# Utility Functions
As of 0.1.0.0 alpha, there's only two utility functions - `str` and `print`
```clojure
;; str can be used to concatenate any number of arguments of any data type to form a string
;; print prints the arguments it gets to stdout with a newline
(print (str 1 " " 2.5629221 " " "hello")) ;; 1 2.5629221 hello
;; print also accepts any number of arguments and prints them on the same line with a newline at the end
(print 1 2 3 " hello there " 4.20) ;; 123 hello there 4.2
```
