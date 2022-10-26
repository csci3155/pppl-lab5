/*
 * CSCI 3155: Lab 5 TypeScript Worksheet
 *
 * This worksheet is a place to experiment with TypeScript expressions.
 */

// New language features in lab 5 are mutable parameters, and call-by-reference, along with objects
// Functional programming-wise, we work with higher-order functions

const two_int = (a:number, b:number, f:(a:number,b:number)=>number) => f(a,b);
two_int(1,2, (a:number,b:number)=>a*b)

