import scala.collection.mutable
object main {
  var Operations = 0

  def floydWarshall(matrix: Seq[Seq[Int]]) = {
    // Размерность графа
    val n = matrix.length
    // Копируем исходную матрицу смежности в матрицу расстояний
    val dist = Array.tabulate(n, n) { (i, j) =>
      val value = matrix(i)(j)
      if (value == Int.MaxValue) Int.MaxValue/2 else value
    }
    // Алгоритм Флойда-Уоршелла
    for {
      k <- matrix.indices
      i <- matrix.indices
      j <- matrix.indices
      if dist(i)(k) + dist(k)(j) < dist(i)(j)
    } {
      dist(i)(j) = dist(i)(k) + dist(k)(j)
    }
    dist
  }

  // Пример использования
  val graph = Seq(
    Seq(0, 4, 16, Int.MaxValue, 13, Int.MaxValue, 2),
    Seq(4, 0, Int.MaxValue, 1, 5, Int.MaxValue, 44),
    Seq(16, Int.MaxValue, 0, 9, Int.MaxValue, 2, Int.MaxValue),
    Seq(Int.MaxValue, 1, 9, 0, Int.MaxValue, Int.MaxValue, Int.MaxValue),
    Seq(13, 5, Int.MaxValue, Int.MaxValue, 0, Int.MaxValue, Int.MaxValue),
    Seq(Int.MaxValue, Int.MaxValue, 2, Int.MaxValue, Int.MaxValue, 0, 14),
    Seq(2, 44, Int.MaxValue, 8, Int.MaxValue, 14, 0),
  )

  def main(args: Array[String]): Unit = {
    graph.indices.foreach { i =>
      graph.indices.foreach(j => print(floydWarshall(graph)(i)(j) + " "))
      println("")
    }
  }
}
