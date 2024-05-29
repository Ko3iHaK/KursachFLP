import scala.collection.mutable
object main {
  def dijkstra(graph: Seq[Seq[Int]], start: Int): Array[Int] = {
    // Массив для хранения расстояний от стартовой вершины
    val distances = Array.fill(graph.length)(Int.MaxValue)
    distances(start) = 0

    // Очередь с приоритетами для выбора вершины с минимальным расстоянием
    val priorityQueue = mutable.PriorityQueue[(Int, Int)]((0, start))

    // Перебираем вершины
    while (priorityQueue.nonEmpty) {
      // Извлекаем вершину с минимальным расстоянием
      val (distance, current) = priorityQueue.dequeue()
      // Если текущее расстояние больше уже известного, пропускаем эту вершину
      if (distance <= distances(current)) {
        // Перебираем соседей текущей вершины
        for (neighbor <- graph.indices
             if graph(current)(neighbor) != Int.MaxValue
               && distance + graph(current)(neighbor) < distances(neighbor)) {
          distances(neighbor) = distance + graph(current)(neighbor)
          priorityQueue.enqueue((distance + graph(current)(neighbor), neighbor))
        }
      }
    }
    // Возвращаем массив расстояний
    distances
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
    graph.indices.map(i => dijkstra(graph, i)).foreach { shortestPaths =>
      shortestPaths.foreach(path => print(s"$path "))
      println("")
    }
  }
}