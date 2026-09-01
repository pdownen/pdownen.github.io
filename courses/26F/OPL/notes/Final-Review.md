Final Exam
==========

The format of the exam is similar to the Midterm. It will be:

* 8 questions long

  - 6 are required
  
  - 2 are extra credit

* Questions can include multiple parts, such as

  - Multiple choice

  - "Show your work" to solve/explain the multiple choice answer, in some
    appropriate way for that question

* Questions are similar to homework assignments 5, 7, 9 (the non-programming
  assignments after the midterm)

* Don't have to write real code (like Ruby, etc), but WILL have to read it (like
  C or Standard ML)

Topics follow the structure of the lectures from the semester after the midterm.
Questions follow homework exercises, including:

1. (5) Functional Programming

  - Read and understand a higher-order Standard ML function
  
  - Interpret it as a lambda calculus term
  
  - Use beta reduction in the lambda calculus to predict what it will return
    when called with some arguments
  
  - Like assignment 5, exercises 1, and 2

2. (7) Types in Theory and Practice

  - Draw a derivation tree for checking the type of
  
    * a simple arithmetic and conditional operations without variables
	
	* a lambda calculus term with functions and variables
	
  - Infer the type of a Standard ML function using Hindley-Milner type inference
    by annotating the types on each node of the syntax tree and gather
    constraints
  
  - like assignment 7, exercises 1, 2, and 6

3. (9) Machines

  - Fill in the activation records on a call stack for a
  
    * first-order C / Standard ML program (where functions are just top-level
      definitions as pointers to code)
	  
	* higher-order C / Standard ML program (where functions point to closure
      values which can be passed and returned to other functions)
	  
  - Determine the difference between static scope (following the access links)
    and dynamic scope (ignoring the access links) on a run-time call stack
	
  - Write down the exact steps of an abstract machine for simple arithmetic and
    conditional operations to determine properties of how the machine runs (for
    example, how many steps does it take)
	
  - Use the Krivine machine to calculate the answer to a lambda calculus term
    using environments, call stacks, and closures, and compare that answer to
    beta reduction in the lambda calculus
	
  - like assignment 9, exercises 2, 3, 4, 5, and extra credit 1
