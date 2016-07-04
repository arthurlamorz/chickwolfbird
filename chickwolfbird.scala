package cwb





object chickwolfbird {

  type Row = List[Int]

type Matrix = List[Row]

  def gcd(a: Int, b: Int): Int = if (b == 0) a.abs else gcd(b, a % b)
  
  def lcm(a: Int, b: Int): Int = (a * b).abs / gcd(a, b)
  
  def lcmm(l: List[Int]): Int = if (l.length == 1) l.head else lcm(l.head, lcmm(l.tail))
  
  def gcdm(l: List[Int]): Int = if (l.length == 1) l.head else gcd(l.head, gcdm(l.tail))
  

  def solve(M: Matrix): Matrix =
    {

      def rowCount = M.length
      def colCount = M(0).length

      def getColumn(M: Matrix, col: Int, index: Int): List[Int] = //get the column of matrix as a list
        if (index == M.length - 1) List(M(index)(col)) else
          List(M(index)(col)) ++ getColumn(M, col, index + 1)

      def multiplyCol(m: Matrix, rowNo: Int, colNo: Int): Matrix = //multiply each row by the LCM/self of column specified
        if (rowNo >= rowCount) List() else
          List(m(rowNo).map(x => x * (lcmm(getColumn(m, colNo, 0)) / (m(rowNo)(colNo))))) ++ multiplyCol(m, rowNo + 1, colNo)

      def divideByGCD(m: Matrix, index: Int): Matrix =
        if (index >= rowCount) List() else
          List(m(index).map(x => x / gcdm(m(index)))) ++ divideByGCD(m, index + 1)

      def subtractRow(m: Matrix, rowNo: Int, index: Int): Matrix = //subtract all other row by the row specified
        if (index >= rowCount) List() else if (index == rowNo)
          List(m(rowNo)) ++ subtractRow(m, rowNo, index + 1)
        else
          List((m(index), m(rowNo)).zipped.map(_ - _)) ++ subtractRow(m, rowNo, index + 1)

      def eliminateRows(m: Matrix, colNo: Int): Matrix = //method of elimination, first multiply, then subtract
        subtractRow(multiplyCol(m, 0, colNo), colNo, 0)

      def eliminate(m: Matrix, colNumber: Int): Matrix =
        if (colNumber >= rowCount) m else
          eliminate(eliminateRows(m, colNumber), colNumber + 1)

      def condition(row: Row): Int => Boolean = {
        val newrow = row.filter(x => x != 0.0 && x != 1.0)
        if (newrow.head > 0) x => x < -1.0 * newrow.last / newrow.head else
          x => x > -1.0 * newrow.last / newrow.head
      }

      def getResultSet(m: Matrix, rowToFilter: Row, index: Int): Row =
        if (index >= m.length) rowToFilter else
          getResultSet(m, rowToFilter.filter(condition(m(index))), index + 1)

      def getOtherResults(resultMatrix: Matrix, oneAnswer: Row, columnToFind: Int): Row =
        oneAnswer.map(x => {
          val param = resultMatrix.filter(row => row(columnToFind) == 1)(0).filter(x => x != 0.0 && x != 1.0)
          -(x * param.head + param.last)
        })

      val resultM = divideByGCD(eliminate(M, 0), 0)

      
      val zList = getResultSet(resultM, 0 to 1000 toList, 0)
      val xList = getOtherResults(resultM, zList, 0)
      val yList = getOtherResults(resultM, zList, 1)

     
      List(xList, yList, zList).transpose
    } 

 

}

object Main extends App {

  def printMatrix(m: chickwolfbird.Matrix) =
  m foreach{case a => print("| "); a foreach {b => print(b.toString + " ")}; print("|\n")}


  val M1 = (List(List(1, 1, 9, -100), List(2, 4, 2, -100)))
  println("First system of equation to be solved:")
  printMatrix(M1)

  println("Solution of first system (in order of chick, wolf, bird):")
  val s1 = chickwolfbird.solve(M1) 
  printMatrix(s1)

  val M2 = (List(List(2, 2, 10, -100), List(3, 5, 3, -100)))
  println("Second system of equation to be solved:")
  printMatrix(M2)
  
  println("Solution of second system (in order of chick, wolf, bird):")
  val s2 = chickwolfbird.solve(M2) 
  printMatrix(s2)
}
