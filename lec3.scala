/* week3 */
/* ksnt  */

//* Data-Parallelism *//
// NOTE: Psuedo codes might be inculded

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




// 3.Data-Parallel Operations-2


// 4.Scala Parallel Collections

// 5.Splitters and Combiners


