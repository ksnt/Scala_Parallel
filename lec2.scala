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

// 3.Parallel Fold (Reduce) Operation


// 4.Associativity-1

// 5.Associativity-2

// 6.Parallel Scan (Prefix Sum) Operation
