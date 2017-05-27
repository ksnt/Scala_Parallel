/* week2 */
/* ksnt  */

//* Basic Task Parallel Algorithms *//
// NOTE: Psuedo codes are inculded

/*
val scheduler =
  new DynamicVariable[TaskScheduler](new DefaultTaskScheduler)

def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
  scheduler.value.parallel(taskA, taskB)
}
*/

//　1.Parallel Sorting

import common._

val threshold = 10000 // they say this should be large enough

// Sorting the Array
def sort(from: Int, until: Int, depth: Int): Unit = {
  if(depth == maxDepth){
    quicksort(xs,from,until - from)
    } else {
    val mid = (from + until) / 2
    parallel(sort(mid,until,depth+1), sort(from,mid,depth+1))
    val flip = (maxDepth - depth) %2 == 0
    val src = if (flip) ys else xs
    val dst = if (flip) xs else ys
    merge(src, dst, from, mid, until)
    }
    }
    sort(0,xs.length,0)


// 2. Data Operations and Parallel Mapping

def mapSeq[A,B](lst: List[A], f : A => B): List[B] = lst match{
    case Nil => Nil
    case h :: t => f(h) :: mapSeq(t,f)
}


//sequential version
def mapASegSeq[A,B](inp: Array[A], left: Int, right: Int, f: A => B, out: Array[B]) = {
  // Writes to out(1) for left <= i <= right-1
  var i = left
  while(i<right){
    out(i) = f(inp(i))
    i = i + 1
    }
}

val in = Array(2,3,4,5,6)
val out = Array(0,0,0,0,0)
val f = (x:Int) => x*x

mapASegSeq(in,1,3,f,out)
//> out
// res22: Array[Int] = Array(0, 9, 16, 0, 0)


//Parallel version of the program above
def mapASegPar[A,B](inp: Array[A], left: Int, right: Int, f: A => B, out: Array[B]):Unit = {
    // Writes to out(1) for left <= i <= right-1
    if(right - left < threshold)
      mapASegSeq(inp,left,right,f,out)
    else{
      val mid = left + (right - left)/2
      parallel(mapASegPar(inp,left,mid,f,out),
               mapASegPar(inp,mid,right,f,out))
      }
}

//mapASegPar(in,0,in.length,f,out)
//out
//res4: Array[Int] = Array(4, 9, 16, 25, 36)

var r = new scala.util.Random
val input:Array[Int] = 1 to 2000000 map (_ => r.nextInt(100))
val output = Array.fill(2000000)(0)
mapASegPar(input,0,input.length,f,output)


// parallel map on immutable trees
def mapTreePar[A:Manifest, B:Manifest](t: Tree[A], f: A => B) : Tree[B] = t match {
  case Leaf(a) => {
    val len = a.length; val b = new Array[B](len)
    var i = 0
    while(i < len) {b(i) = f(a(i)); i = i + 1}
    Leaf(b)}
  case Node(l,r) => {
    val (lb,rb) = parallel(mapTreePar(l,r), mapTreePar(r,f))
    Node(lb,rb) }
 }
 
//Comparison of array and immutable trees
/*
Arrauys:
(+) random access to elements, on shared memory can share array
(+) good memory locality
(-) imperative: must ensure parallel tasks write to disjoint parts
(-) expensive to concatenate

Immutable trees:
(+) purely functional, produce new trees, keep old ones
(+) no need to worry wbout dijointness of writes by parllel tasks
(+) efficient to combine two trees
(-) high memory allocation overhead
(-) bad locality
*/


// 3.Parallel Fold (Reduce) Operation

// List(1,3,8).map(x => x*x) == List(1,9,64)
// List(1,3,8).fold(100)((s,x) => s+x) == 112 (== 100 + 1 + 3 + 8)

// List(1,3,8).foldLeft(100)((s,x) => s - x) == ((100-1)-3)-8 == 88
// List(1,3,8).foldRight(100)((s,x) => s - x) == 1- (3 - (8-100)) == -94
// List(1,3,8).reduceLeft((s,x) => s - x) == (1-3) - 8 == -10
// List(1,3,8).reduceRight((s,x) => s - x) ==  1 - (3 - 8) == 6

// a expression tree
sealed abstract class Tree[A]
case class Leaf[A](value: A) extends Tree[A]
case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// reduce of this tree (sequential version)
def reduce[A](t: Tree[A], f: (A,A) => A): A = t match {
  case Leaf(v) => v
  case Node(l,r) => f(reduce[A](l,f), reduce[A](r,f)) // Node -> f
}

def tree = Node(Leaf(1),Node(Leaf(3),Leaf(8)))
def fMinus = (x:Int,y:Int) => x - y
def res = reduce[Int](tree,fMinus)


// parallel version
def reduce_par[A](t:Tree[A], f: (A,A) => A): A = t match {
  case Leaf(v) => v
  case Node(l,r) => {
    var (lV,rV) = parallel(reduce_par[A](l,f), reduce_par[A](r,f))
    f(lV,rV)
  }
}


def toList[A](t:Tree[A]): List[A] = t match{
  case Leaf(v) => List(v)
  case Node(l,r) => toList[A](l) ++ toList[A](r)
}

// map of tree
def map[A,B](t: Tree[A], f: A => B): Tree[B] = t match {
  case Leaf(v) => Leaf(f(v))
  case Node(l,r) => Node(map[A,B](l,f),map[A,B](r,f))
}

//toList(t) == reduce(map(t,List(_)), _ ++ _)

/*
We have seen reduction on trees.
Often we work with collections that we only know the ordering and not the tree structure => How can we do reduction in case of e.g. array?

- convert it into balanced tree
- do tree reduction

Because of associativity we can choose any tree that preserves the order of elements of the original collection

*?


// 4.Associativity-1

/* Def (Associative)

Operation f: (A,A) => A is associative iff for every x,y,x:

  f(x,f(y,z)) = f(f(x,y),z)
  
*/

/* Def (Commutative)

Operation f: (A,A) => A is commutative iff for every x,y:

  f(x,y) = f(y,x)

*/



// 5.Associativity-2

// 6.Parallel Scan (Prefix Sum) Operation
