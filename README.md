# README #

This README would normally document whatever steps are necessary to get your application up and running.

### What is this repository for? ###

* Quick summary
* Version
* [Learn Markdown](https://bitbucket.org/tutorials/markdowndemo)

### How do I get set up? ###

* Summary of set up

```
(ql:quickload :qlot)
;; initialize project
(qlot:init #P"/path/to/project/")
;; install deps
(qlot:install)
```
Or to install qlot (preferred) as binary (not library only):

``` shell
curl -L https://qlot.tech/installer | sh
qlot install
```

Build of project:

``` shell
qlot exec ./build.sh
# RELEASE= qlot exec ./build.sh  # for release
```

To load in SBCL/SLY/SLIME and run tests:

``` lisp
(asdf:load-system :easytree)
(asdf:test-system :easytree)
```

To verify:

``` lisp
# run ACL2 first: /some/path/saved_acl2  and in its REPL:

(ld "acl2-utils.lisp")
```

* Configuration
* Dependencies
* Database configuration
* How to run tests
* Deployment instructions

### Contribution guidelines ###

* Writing tests
* Code review
* Other guidelines

### Who do I talk to? ###

* Repo owner or admin
* Other community or team contact
