object main {
  var Operations = 0
  def bellmanFord(graph: Array[Array[Int]], source: Int): Option[(Array[Int], Array[Int])] = {
    // Размерность графа
    val n = graph.length

    // Инициализация массива расстояний и предшественников
    var distances = Array.fill(n)(20000)
    var predecessors = Array.fill(n)(-1)

    // Расстояние от источника до самого себя равно 0
    distances(source) = 0

    // Релаксация рёбер
    for (_ <- 0 until n - 1) {
      for (u <- 0 until n) {
        for (v <- 0 until n if u != v && graph(u)(v) != 20000) {
          Operations += 1
          val newDist = distances(u) + graph(u)(v)
          if (newDist < distances(v)) {
            distances(v) = newDist
            predecessors(v) = u
          }
        }
      }
    }

    // Поиск отрицательного цикла
    for (u <- 0 until n) {
      for (v <- 0 until n if u != v && graph(u)(v) != 20000) {
        val newDist = distances(u) + graph(u)(v)
        if (newDist < distances(v)) {
          return None
        }
      }
    }

    // Возвращаем кортеж с массивом расстояний и массивом предшественников
    Option((distances, predecessors))
  }

  // Пример использования
  val graph = Array(
    Array(),
    Array(),
    Array(),
  )

  def main(args: Array[String]): Unit = {
    for (i <- 0 until 7) {
      val result = bellmanFord(graph, i)
      if (!result.isEmpty) {
        val (distances, predecessors) = result.get
        println("Вершина № " + i)
        for (k <- 0 until distances.length) {
          print(distances(k))
          print(" ")
        }
        println("")
        println(Operations)
        Operations = 0
      } else {
        println("Отрицательный цикл найден")
        Operations = 0
      }
    }
  }
}

