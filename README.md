# Principles and Practice in Programming Languages

# Lab 5

# Imperative Computation

This repository contains the student project files. If you are an instructor looking to re-use these materials, please contact [Bor-Yuh Evan Chang](https://plv.colorado.edu/bec).

Refer to the [lab handout](imperative-computation-lab.ipynb) in this repository for details about the assignment. The current version of the lab handout is available on the web [here](https://csci3155.cs.colorado.edu/pppl-course/book/imperative-computation-lab.html).

## Integrity of the Course Materials

The development effort in the course materials, including these lab assignments, the exercises, and the exams, is significant. You agree that you will not share any course materials publicly. The course materials, include your or anyone else's solutions to the lab assignments, exercises, and exams. In particular, you agree not to post your solutions to the lab assignments in a public source code repository, such as public Github repositories. Please use private source code repositories for your work.

## Project Files Organization

For Lab 5, the most important project files are shown below.

```
.
├── README.md (this file)
├── imperative-computation-lab.ipynb                (the lab handout as a Jupyter Notebook)
└── src
    ├── main
    │   └── scala
    │       └── jsy
    │           ├── lab5
    │           │   ├── Lab5.scala                  (implementation template for you to **edit and submit**)
    │           │   ├── Lab5.worksheet.ts           (a scratch worksheet you can use to experiment with TypeScript)
    │           │   ├── Lab5.worksheet.sc           (a scratch worksheet you can use to experiment with your code)
    │           │   ├── Parser.scala                (a parser provided for you)
    │           │   └── ast.scala                   (the AST definition)
    │           └── util
    │               ├── DoWith.scala                (the DoWith library)
    │               └── JsyApplication.scala        (an interface to load and run Javascripty applications)
    └── test
        ├── scala
        │   └── jsy
        │       └── lab5
        │           └── Lab5Spec.scala              (test cases you can use to test your implementation)
        └── resources
            ├── lab5
            │   ├── test[...].ans                   (the expected output for a test .jsy file with the same name)
            │   └── test[...].jsy                   (JavaScripty program that your interpreter will be tested with)
            ├── lab5xc
            │   ├── test[...].ans                   (the expected output for a test .jsy file with the same name)
            │   └── test[...].jsy                   (JavaScripty program that your interpreter will be tested with for the extra credit section)
            └── tester
                └── JavascriptyTester.scala         (an interface to load and test a JsyApplication)
```

## Building and Testing

See [pppl-lab1](https://github.com/csci3155/pppl-lab1) for instructions on building and testing.