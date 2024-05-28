import scala.collection.mutable
object main {
  var Operations = 0
  def floydWarshall(matrix: Array[Array[Int]]) = {
    // Размерность графа
    val n = matrix.length

    // Копируем исходную матрицу смежности в матрицу расстояний
    val dist = Array.ofDim[Int](n, n)
    for (i <- 0 until n; j <- 0 until n) {
      dist(i)(j) = matrix(i)(j)
    }

    // Алгоритм Флойда-Уоршелла
    for (k <- 0 until n) {
      for (i <- 0 until n) {
        for (j <- 0 until n) {
          if (dist(i)(k) + dist(k)(j) < dist(i)(j)) {
            dist(i)(j) = dist(i)(k) + dist(k)(j)
            Operations += 1
          }
        }
      }
    }
    dist
  }
  // Пример использования
  val graph = Array(
    Array(),
    Array(),
    Array(),
  )

  def main(args: Array[String]): Unit = {
    val shortestPaths = floydWarshall(graph)
    val n = shortestPaths.length
    for (k <- 0 until n) {
      for (i <- 0 until n) {
        print(shortestPaths(k)(i))
        print(" ")
      }
      println("")
    }
    println("Операций:" + Operations)
  }

}
