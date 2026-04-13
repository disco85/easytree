# Easytree tool (with ACL2 theorem proving) #

Easytree is a tool parsing output of [tree](https://linux.die.net/man/1/tree "tree(1)")
and recreating it in the current folder or somewhere else (it can prepend every result
path with a prefix-directory). Also it can generate a POSIX shell script doing the same.
Dry mode is supported as well.

## What is so special about it? ##

It's written with [theorem proving](https://en.wikipedia.org/wiki/Automated_theorem_proving)
in [ACL2](https://en.wikipedia.org/wiki/ACL2) friendly [Common Lisp](https://en.wikipedia.org/wiki/Common_Lisp),
see [here](https://github.com/disco85/easytree/blob/main/acl2-utils.lisp).

Also the code includes [unit testing](https://github.com/disco85/easytree/blob/main/easytree-test.lisp).

## How do I get set up? ##

Install [SBCL](https://www.sbcl.org/) first, then install [QuickLisp](https://www.quicklisp.org). After it:

```lisp
(ql:quickload :qlot)
;; initialize project
(qlot:init #P"/path/to/project/")
;; install deps
(qlot:install)
```

Or to install qlot (**preferred**) as binary (not library only):

``` shell
curl -L https://qlot.tech/installer | sh
qlot install
```

Build of project:

``` shell
qlot exec ./build.sh
```

or (if you wanna get small executable):

``` shell
RELEASE= qlot exec ./build.sh
```

To load in SBCL/SLY/SLIME and run tests:

``` lisp
(asdf:load-system :easytree)
(asdf:test-system :easytree)
```

## Verification ##

First, install ACL2.

Then, to verify:

``` lisp
# run ACL2 first: /some/path/saved_acl2  and in its REPL:

(ld "acl2-utils.lisp")
```

Or better use the script [verify.sh](https://github.com/disco85/easytree/blob/main/verify.sh):

``` shell
./verify.sh
```

Theorem proving (TP) does not use all power of books, only simple and immediately available books:

``` shell
(in-package "ACL2")  ;; all TP code should be in ACL2 package, it's simplest approach

(include-book "std/lists/suffixp" :dir :system)
(include-book "std/lists/prefixp" :dir :system)
(include-book "utils")
```

ACL2 is the most automatic TP system, you can prove some statements even without hints to
the prover! Let's look at some of them.

``` lisp
(defthm extract-fname-and-indent--ends-in-unexpected-when-was-not-extracted
    (implies (and (stringp str)
                  (not (extract-fname-and-indent str)))
             (eql (car (extract-fname-and-indent-1 str 0 :decorations))
                  :unexpected)))
```

This defines a theorem `EXTRACT-FNAME-AND-INDENT--ENDS-IN-UNEXPECTED-WHEN-WAS-NOT-EXTRACTED` which
will be automatically proven by ACL2. It states: if `(EXTRACT-FNAME-AND-INDENT STR)` failed then
the FSM ends in `:UNEXPECTED` state. It can be read as:

```
       STR is string && EXTRACT-FNAME-AND-INDENT(str) is NIL
-----------------------------------------------------------------------
head of EXTRACT-FNAME-AND-INDENT-1(STR, 0, :DECORATIONS) is :UNEXPECTED
```

