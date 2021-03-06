/* week4 */
/* ksnt  */

//* Data Structures for Parallel Computing *//
// NOTE: Psuedo codes are inculded

import scala.reflect.ClassTag
import scala.collection._
import scala.collection.parallel.Combiner
import org.scalameter._
import scala.collection.mutable.ArrayBuffer
import common._

//　1.Implementing Combiners
// Builders
// Builders are used in sequential collection methods
trait Builder[T, Repr]{
	def +=(elem: T): this.type
	def result: Repr
}

// Combiners
trait Combiner[T,Repr] extends Builder[T,Repr]{def combine(that: Combiner[T,Repr]): Combiner[T,Repr]}

// unefficient way to implement combine
def combine(xs: Array[Int], ys: Array[Int]): Array[Int] = {
	val r = new Array[Int](xs.length + ys.length) // n+m steps
	Array.copy(xs,0,r,0,xs.length) // n steps
	Array.copy(ys,0,r,xs.length,ys.length) // m steps
	r // 2(n + m) steps, ie O(m+n)
}

//Sets
// hash table - expecxted O(1)
// balanced trees - O(log n)
// linked lists - O(n)

//Sequences
// mutabble liked lists - O(1) prepend and append, O(n) insertion
// functional (cons) lists - O(1) prepend operations, everything else O(n)
// array lists - amortized O(a) append, O(1) random access, otherwise O(n)


// 2. Parallel Two-phase Construction
// Most data structures can be constructed in prallel using two-phase construction
// THe intermediate data structure is a data structure that:
// - thas an efficient combine method - O(log n + log m) or better
// - has an efficient += method
// - can be converted to the resulting data structure in O(n/P) time // P is the number of processors

// Array Combiner
class ArrayCombiner[T <: AnyRef: ClassTag](val parallelism: Int){
	private var numElems = 0
	private val buffers = new ArrayBuffer[ArrayBuffer[T]]
	buffers += new ArrayBuffer[T]
}

// 3.Conc-tree Data Structure

sealed trait Tree[+T]

case class Node[T](left: Tree[T], right: Tree[T]) extends Tree[T]
case class Leaf[T](elem: T) extends Tree[T]
case object Empty extends Tree[Nothing]

//Filter on Trees
def fliter[T](t: Tree[T])(p: T => Boolean): Tree[T] = t match {
	case Node(left,right) => Node(parallel(filter(left)(p), filter(right)(p)))
	case Leaf(elem) => if (p(elem)) t else Empty
	case Empty => Empty
}

//Remark: Treesa are not good for parallelism unless they are balanced

// Conc: a data type for balanced trees
import scala.annotation.tailrec

sealed trait Conc[+T]{
	def level: Int
	def size: Int
	def left: Conc[T]
	def right: Conc[T]
}

// 4.Amortized, Constant-time Append Operation

var xs: Conc[T] = Empty
def +=(elem: T){
	xs = xs <> Single(elem)
}


// 5.Conc-Tree Combiners

