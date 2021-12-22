import scala.collection.mutable.ArrayBuffer
import scala.{+:, ::}

/*
 * CS3210 - Principles of Programming Languages - Fall 2021
 * Instructor: Thyago Mota
 * Description: Prg 02 - Functional Programming BST
 * Student(s) Name(s): Eesha Patel & Alejandro Rojas
 */

class BinTree[T <: Comparable[T]]() {

  private var value: T = _
  private var left: BinTree[T] = _
  private var right: BinTree[T] = _

  def this(value: T) {
    this();
    this.value = value
  }

  def add(value: T): Unit = {

    def add(current: BinTree[T]): BinTree[T] = {
      if (current == null)
        new BinTree[T](value)
      else {
        if (value.compareTo(current.value) < 0)
          current.left = add(current.left)
        else if (value.compareTo(current.value) > 0)
          current.right = add(current.right)
        current
      }
    }

    if (this.value == null)
      this.value = value
    else
      add(this)
  }

  def sep: String = ???

  // TODOd #1: returns a string representation of the tree using tabs to show its structure
  def mkString(): String = {
    def mkString(current: BinTree[T]): String = {
      var out = ""
      if (current != null) {
        out = current.value + sep
        out += mkString(current.left)
        out += mkString(current.right)
      }
      out
    }

    var out = mkString(this)
    out = out.substring(0, out.length - sep.size)
    out
  }

  // TODOd #2: returns a new tree (from the callee) with only the values that passed the test denoted by function f
  def filter(f: T => Boolean): BinTree[T] = {
    val binTree = new BinTree[T]()

    def filter(current: BinTree[T]): Unit = {
      if (current != null) {
        if (f(current.value)) {
          binTree.add(current.value)
        }
        filter(current.left)
        filter(current.right)
      }
    }

    filter(this)
    binTree
  }

  // TODOd #3: returns a new tree (from the callee) by applying function f to each of the callee's elements
  def map(f: T => T): BinTree[T] = {
    val binTree = new BinTree[T]()

    def map(current: BinTree[T]): Unit = {
      if (current != null) {
        binTree.add(f(current.value))
        map(current.left)
        map(current.right)
      }
    }

    map(this)
    binTree
  }

  // TODOd #4: applies function f to each of the tree's elements
  def foreach(f: T => T): Unit = {
    def map(current: BinTree[T]): Unit = {
      if (current != null) {
        current.value = f(current.value)
        map(current.left)
        map(current.right)
      }
    }

    map(this)
  }

  // TODOd #5: similar to foldLeft for collections
  def foldLeft(value: T)(f: (T, T) => T): T = {
    var accumulatedValue = value

    def foldleft(current: BinTree[T]): Unit = {
      if (current != null) {
        accumulatedValue = f(accumulatedValue, current.value)
        foldleft(current.left)
        foldleft(current.right)
      }
    }

    foldleft(this)
    accumulatedValue
  }

  // TODOd #6: similar to foldRight for collections
  def foldRight(value: T)(f: (T, T) => T): T = {
    var accumulatedValue = value

    def foldRight(current: BinTree[T]): Unit = {
      if (current != null) {
        accumulatedValue = f(current.value, accumulatedValue)
        foldRight(current.left)
        foldRight(current.right)
      }
    }

    foldRight(this)
    accumulatedValue
  }

  // TODOd #7: similar to foldLeft
  def fold(value: T)(f: (T, T) => T): T = {
    foldLeft(value)(f)
  }

  // TODOd #8: returns the height of the tree
  def height(): Int = {
    if (this == null) {
    }

    def height(current: BinTree[T]): Int = {
      if (current != null) {
        var currentHeight = 0
        currentHeight = (Math.max(height(current.left), height(current.right))) + 1
        currentHeight
      } else {
        0
      }
    }

    height(this)
  }

  // TODOd #9: returns the number of elements of the tree
  def size(): Int = {
    var countOfElements = 0

    def size(current: BinTree[T]): Unit = {
      if (current != null) {
        countOfElements = countOfElements + 1
        size(current.left)
        size(current.right)
      }
    }

    size(this)
    countOfElements
  }

  // TODOd #10: returns true/false depending whether value is/is not found in the tree
  def search(value: T): Boolean = {
    var found = false

    def search(current: BinTree[T]): Unit = {
      if (current != null && !found) {
        if (current.value == value) {
          found = true
        }
        search(current.left)
        search(current.right)
      }
    }

    search(this)
    found
  }

  object BinTree {

    def main(args: Array[String]): Unit = {

      // tree 1 (a BST of integers)
      var tree1 = new BinTree[Integer]()

      // TODO #11a: use array's foreach to add the following elements: 2, 1, 7, 3, 9, 10; display the tree next
      val array = Array(2, 1, 7, 3, 9, 10)
      array.foreach(println _)

      // TODO #11b: use foreach to multiply all of its elements by 2; display the tree next
      array.foreach(Math.multiplyExact(_,2))
      println(array)
      // TODO #11c: use filter to obtain another tree whose elements are > 5; display the resulting tree next
      val byTwo = array.filter(_ > 5 == 0)
      println(byTwo.mkString(","))
      // TODO #11d: use map to subtract 2 from each element of the original tree (modified by TODO #11b); display the resulting tree next
      if (byTwo.isEmpty) List.empty
      else byTwo.lazyZip(byTwo.tail).map {
        case (x, y) => y - x
      }
      // TODO #11e: use fold to compute the sum of all of the elements of the (original) tree (modified by TODO #11d)
      array.foldLeft(0)(_ + _)
      // TODO #11f: use fold and size to compute the average of the elements of the (original) tree (modified by TODO #11d)
      var total = 0.0;
      for ( i <- 0 to (array.length - 1)) {
        total += array(i);
      }
      // TODO #11g: show the height of the tree

      // TODO #11h: search for elements 4 and 17 in the (original) tree (modified by TODO #11d)

      // tree 2 (a BST of strings)
      var tree2 = new BinTree[String]()

      // TODO #12a: use array's foreach to add the following elements: "perry rhodan", "icho tolot", "denetree", "deshan apian", "levian paronn", "roder roderich", "gressko gurrad"; display the tree next
      val array2= Array("perry rhodan", "icho tolot", "denetree", "deshan apian", "levian paronn", "roder roderich", "gressko gurrad")

      // TODO #12b: use foreach to capitalize the first letter of each word of each tree node; display the tree next
      "perry rhodan icho tolot denetree deshan apian levian paronnroder roderich gressko gurrad".split(" ").map(_.capitalize).mkString(" ")
      // TODO #12c: use filter to obtain another tree whose names are made of at least 2 words; display the resulting tree next

      // TODO #12d: use map to capitalize all of the letters from each of the names in the (original) tree; display the resulting tree next

      // TODO #12e: use fold to compute the sum of all of the letters from each of the names in the tree

      // TODO #12f: use fold and size to compute the average number of characters of the names in the tree

      // TODO #12g: show the height of the tree

      // TODO #12h: search for elements "PERRY RHODAN" and "ATLAN"

    }
  }
}