object main {
  def bellmanFord(graph: Seq[Seq[Int]], source: Int): Option[(Seq[Int], Seq[Int])] = {
    // Размерность графа
    val n = graph.length
    // Инициализация массива расстояний и предшественников
    var distances = Array.fill(n)(Int.MaxValue/2)
    var predecessors = Array.fill(n)(-1)
    // Расстояние от источника до самого себя равно 0
    distances(source) = 0

    // Релаксация рёбер
    for{
      _ <- 0 until graph.length - 1
      u <- graph.indices
      v <- graph.indices
      if u != v && graph(u)(v) != Int.MaxValue
      if distances(u) + graph(u)(v) < distances(v)
    }{
      distances(v) = distances(u) + graph(u)(v)
      predecessors(v) = u
    }

    val result = (0 until n)
      .flatMap(u => (0 until n)
        .filter(v => u != v && graph(u)(v) != Int.MaxValue)
        .map(v => {
          val newDist = distances(u) + graph(u)(v)
          if (newDist < distances(v)) {
            None
          } else {
            true
          }
        }))
    // Возвращаем кортеж с массивом расстояний и массивом предшественников
    if (result.contains(None)) {
      None}
    else {
      Option((distances, predecessors))
    }
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
    (0 until 7).foreach { i =>
      bellmanFord(graph, i).map { result =>
        val (distances, predecessors) = result
        println(s"Вершина № $i")
        distances.foreach(d => print(s"$d "))
        println("")
      }.getOrElse(println("Отрицательный цикл найден"))
    }
  }
}

