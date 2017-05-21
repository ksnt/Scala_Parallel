/* week1 */
/* ksnt  */

//* Running Computations in Parallel *//

// Recursive calculation for p-norm
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


// def parallel[A,B](taskA: => A, taskB: => B): (A,B) = {.....}
