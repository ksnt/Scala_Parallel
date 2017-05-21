/* week1 */
/* ksnt  */

//* Running Computations in Parallel *//
// NOTE: Psuedo codes are inculded

//　1.Recursive calculation for p-norm
def pNormRec(a:Array[Int], p: Double): Int =
  power(segmentRec(a,p,0,a.length),1/p)
  
def segmentRec(a:Array[Int], p: Double, s: Int, t: Int) = {
  if(t-s < threshold)
    sumSegment(a,p,s,t) // small segment: do it sequentially
  else{
    val m = s + (t-s)/2
    val (sum1,sum2) = parallel(segmentRec(a,p,s,m),
                               segmentRec(a,p,m,t))
    sum1 + sum2
    }
}

// def parallel[A,B](taskA: => A, taskB: => B): (A,B) = {.....} // call by name
// def parallel[A,B](taskA: A,    taskB: B):(A,B) = {.....}     // call by value


// Sum up array elements
// add s-th element to t-th element in the array 
def sum1(a: Array[Int], p: Double, s: Int, t: Int) = {
  var i=s; var sum: Int = 0
  while(i < t){
    sum = sum + a(i)
    i = i + 1
  }
  sum
}

val ((sum1,sum2),(sum3,sum4)) = parallel(
  parallel(sum1(a,p,0,m1), sum1(a,p,m1,m2)),
  parallel(sum1(a,p,m2,m3), sum1(a,p,m3,a.length))

)

// 2. Monte Carlo Method to Estimate Pi

// (i) direct version
import scala.util.Random
def mcCount(iter: Int): Int = {
  val randomX = new Random
  val randomY = new Random
  var hits = 0
  for(i <- 0 until iter){
    val x = randomX.nextDouble // in [0,1]
    val y = randomY.nextDouble // in [0,1]
    if (x*x + y*y < 1) hits = hits + 1
  }
  hits
}

def monteCarloPiSeq(iter: Int): Double = 4.0 * mcCount(iter) / iter

/*
scala> monteCarloPiSeq(100)
res4: Double = 2.96

scala> monteCarloPiSeq(1000)
res5: Double = 3.128

scala> monteCarloPiSeq(10000)
res6: Double = 3.1544

scala> monteCarloPiSeq(100000)
res7: Double = 3.14156
*/

// (ii) Parallel version
// psuedo code
def monteCarloPiPar(iter: Int): Double = {
  val ((pi1,pi2), (pi3,pi4)) = parallel(
   parallel(mcCount(iter/4), mcCount(iter/4)),
   parallel(mcCount(iter/4), mcCount(iter - 3*(iter/4))))
   4.0 * (pi1 + pi2 + pi3 + pi4) / iter
}


// 3.First-Class Tasks

def parallel[A,B](cA: => A, cB: => B): (A,B) = {
  val tB: Task[B] = task {cB}
  val tA: A = cA
  (tA, tB.join)
}


// 4.How fast are Parallel Programs

// Asymptoti analysis

//segmentRec
// work  W: O(t-s)
// depth D: O(log(t-s))

// Parallelism and Amdahl's Law
/*
Suppose that we havetwo parts of a sequential computation:
- part1 takes fraction f of the computation time(eg 40%)
- part2 take the remaining 1 - f fraction of time (eg 60%) and we can speed it up

if we make part2 P times faster the speedup is 

1/(f + (1-f)/P)

for P = 100 and f = 0.4 we obtain 2.46

even if we speed the second part infinitely, we can obtain at most 1/0.4=2.5 speed up.
*/

// 5.Benchmarking Parallel Programs
/*
testing - a binary outputa - rogram or its part is either correct or it is not
benchmarking a continuous value - denotes the extent to which the program is correct
*/

//using scalameter
import org.scalameter._
measure {
       (0 until 1000000).toArray
     }


withWarmer(new Warmer.Default) measure { (0 until 1000000).toArray}

withMeasurer(new Measurer.MemoryFootprint) measure { (0 until 1000000).toArray}
//res10: Double = 3974.048

