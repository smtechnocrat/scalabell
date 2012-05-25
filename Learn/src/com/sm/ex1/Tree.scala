/**
 *
 */
package com.sm.ex1

/**
 * In this exercise, trees are represented and manipulated in Scala
 * through a small calculator program. The aim of this program is to manipulate very
 * simple arithmetic expressions composed of sums, integer constants and variables.
 * Two examples of such expressions are 1Å2 and (x Å x)Å(7Å y).
 * We ﬁrst have to decide on a representation for such expressions. The most natural
 * one is the tree, where nodes are operations (here, the addition) and leaves are values
 * (here constants or variables).
 * @author smazumder
 *
 */

/**
 * Case classes means that they differ from standard classes in several respects:6 Case classes and pattern matching 9
 * 1) the new keyword is not mandatory to create instances of these classes (i.e. one
 *    can write Const(5) instead of new Const(5)),
 *
 * 2) getter functions are automatically deﬁned for the constructor parameters (i.e.
 *    it is possible to get the value of the v constructor parameter of some instance
 *    c of class Const just by writing c.v),
 *
 * 3) default deﬁnitions for methods equals and hashCode are provided, which work
 *    on the structure of the instances and not on their identity,
 *
 * 4) a default deﬁnition for method toString is provided, and prints the value in a
 *    “source form” (e.g. the tree for expression xÅ1 prints as Sum(Var(x),Const(1))),
 *    instances of these classes can be decomposed through pattern matching as
 *    we will see below.
 */
abstract class Tree
case class Sum(l: Tree, r: Tree) extends Tree
case class Var(s: String) extends Tree
case class Const(v: Int) extends Tree

object Tree {

  type Environment = String => Int

  /**
   * This evaluation function works by performing pattern matching on the tree t. Intuitively, the meaning of the above deﬁnition should be clear:
   * 1. it ﬁrst checks if the tree t is a Sum, and if it is, it binds the left sub-tree to a new
   *    variable called l and the right sub-tree to a variable called r, and then proceeds with the evaluation of the expression following the arrow; this expression can (and does) make use of the variables bound by the pattern appearing
   *    on the left of the arrow, i.e. l and r,
   *
   * 2. if the ﬁrst check does not succeed, that is if the tree is not a Sum, it goes on and
   *    checks if t is a Var; if it is, it binds the name contained in the Var node to a
   *    variable n and proceeds with the right-hand expression,
   *
   * 3. if the second check also fails, that is if t is neither a Sum nor a Var, it checks
   *    if it is a Const, and if it is, it binds the value contained in the Const node to a
   *    variable v and proceeds with the right-hand side,
   *
   * 4. ﬁnally, if all checks fail, an exception is raised to signal the failure of the pattern matching expression; this could happen here only if more sub-classes of
   *    Tree were declared.
   */
  def eval(t: Tree, env: Environment): Int = t match {
    case Sum(l, r) => eval(l, env) + eval(r, env)
    case Var(n) => env(n)
    case Const(v) => v
  }

  def main(args: Array[String]) = {

    val exp: Tree = Sum(Sum(Var("x"), Var("x")), Sum(Const(7), Var("y")))
    val env: Environment = { case "x" => 5 case "y" => 7 }
    println("Expression: " + exp)
    println("Evaluation with x=5, y=7: " + eval(exp, env))
  }
}

