/* week3 */
/* ksnt  */

//* Data-Parallelism *//
// NOTE: Psuedo codes might be inculded

import scala.collection._
import scala.collection.parallel._
import org.scalameter._
import common._

//　1.Data-Parallel Programming

// task-parallel programming: A form of parallelization that distributes execution processes across computing nodes
// data-parallel programming: A form of parallelization that distributes data across computing nodes

// Example: Mandelbrot Set

private def computePixel(xc: Double, yc: Double, maxIterations: Int): Int = {
	var i = 0
	var x,y = 0.0
	while(x * x + y * y < 4 && i < maxIterations){
		val xt = x * x - y * y + xc
		val yt = 2 * x * y + yc
		x = xt; y = yt
		i += 1
	}
	color(i) // this method is not defined
}


// 2. Data-Parallel Operations-1

(1 until 1000).par.filter(n => n % 3 == 0).count(n => n.toString == n.toString.reverse)

// this method can not be run in parallel
def sum(xs: Array[Int]): Int = {xs.par.foldLeft(0)(_ + _)}

//def fold(z: A)(f: (A,A) => A):A

def max(xs: Array[Int]): Int = {xs.par.fold(Int.MinValue)(math.max)}

// 3.Data-Parallel Operations-2

//Count the number of vowels in a character array
//Array('E','P','F','L').par.aggregate(0)( (count, c) => if (isVowel(c)) count + 1 else count, _ + _ )  // is Vowel is not defined

// 4.Scala Parallel Collections

//Writing Parallelism-Agnostic Code
// Example - find the largest palindrome in the sequencescala.collection.

def largestPalindrome(xs: GenSeq[Int]): Int = {
	xs.aggregate(Int.MinValue)(
		(largest,n) =>
		if (n > largest && n.toString == n.toString.reverse) n else largest,
		math.max
		)
}
val array = (0 until 1000000).toArray
//scala> largestPalindrome(array)
//res7: Int = 999999

import java.util.concurrent._
def intersection(a: GenSet[Int], b: GenSet[Int]) = {
	val result = new ConcurrentSkipListSet[Int]()
	for (x <- a) if (b contains x) result += x
	result
} 
/*now error is occuring "24: error: value += is not a member of java.util.concurrent.ConcurrentSkipListSet[Int] for (x <- a) if (b contains x) result += x */
// this should be solved
intersection((0 until 1000).toSet, (0 until 1000 by 4).toSet)
intersection((0 until 1000).par.toSet, (0 until 1000 by 4).par.toSet)

// Side-eefect can be avoided by using the correct combinators. As in we can use fileter to compute intersection

def intersection(a:GenSet[Int], b: GenSet[Int]): GenSet[Int] = {
	if (a.size < b.size) a.filter(b(_))
	else b.filter(a(_))
}
intersection((0 until 1000).toSet, (0 until 1000 by 4).toSet)
intersection((0 until 1000).par.toSet, (0 until 1000 by 4).par.toSet)

// 5.Splitters and Combiners

// Data-Parallel Abstractions
// iterators
// splitters
// builders
// combiners
/*
trait Iterator[A]{
	def next()
	def hasNext: Boolean
}
*/

//def iterator: Iterator[A] // on every collection

// foldLeft implementation using Iterators
def foldLeft[B](z:B)(f: (B,A) => B): B = {
	var s = z
	while (hasNext) s = f(s,next())
	s
}


//implementation fold on a splitter
def fold(z: A)(f: (A,A) => A): A = {
	if (remaining < threshold) foldLeft(z)(f)
	else{
		val children = for (child <- split) yield task {child.fold(z)(f)}
		children.map(_.join()).foldLeft(z)(f)
	}
}
// Builder

trait Builder[A, Repr]{
	def +=(elem: A): Builder[A,Repr]
	def result: Repr
}
def newBuiler: Builder[A,Repr] // on every collection

//filter method using Builder
def filter(p: T => Boolean): Traversable[T] = {
	val n = newBuiler
	for (x <- this) if (p(x)) b += x
	b.result
}

// Combiner: parallel version of filter
trait Combiner[A,Repr] extends Builder[A,Repr]{def combine(that: Combiner[A,Repr]): Combiner[A,Repr]}
def newCombiner: Combiner[T,Repr]// on every parallel collection

//The combiner contract
//calling combine returns a new combiner that contains elements of input combiners
//calling combine leaves both original Combiner in an undefined state
//combine is an efficient method - O(log n) or better
