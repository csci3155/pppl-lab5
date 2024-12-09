/*
 * CSCI 3155: Lab 5 Worksheet
 *
 * This worksheet demonstrates how you could experiment
 * interactively with your implementations in Lab5.scala.
 */
import jsy.lab5._

// Imports the parse function from Parser
import Parser.{parse,parseFile}

// Imports the ast nodes
import ast._

// Imports all of the functions from Lab5 (your implementations in Lab5.scala)
import Lab5._

// Parse code with assignments
parse("var x = 1; x = 2; console.log(x)")
parse("const x = {f: 1}; x.f = 2; console.log(x.f)")

// Parse code with null
// parse("null")
// parse("<Null>null")
// parse("<{f: number}>null")

// Parse functions
parse("(x: number) => x")
parse("function (x: var number) { x = 0; return x }")
parse("var y = 1; (function (x: ref number) { x = 0; return x })(y)")
// parse("((x: name number) => 0)(console.log(100))")

// Aliasing example
val aliasingex = parse("""
  const x = { f: 1 }
  const y = x
  x.f = 2
  console.log(y.f)
""")
//iterateStep(aliasingex) // uncomment when you are ready to test your step function.

// Parse the JavaScripty expression in your worksheet
val worksheetJsy = parseFile("src/main/scala/jsy/lab5/Lab5.worksheet.ts")

// Interpret the JavaScripty expression in your worksheet
//inferType(worksheetJsy)
//iterateStep(worksheetJsy)