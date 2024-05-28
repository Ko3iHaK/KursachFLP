import scala.collection.mutable
object main {
  var Operations = 0
  def dijkstra(graph: Array[Array[Int]], start: Int) = {
    // Размерность графа
    val n = graph.length
    // Массив для хранения расстояний от стартовой вершины
    val distances = Array.fill(n)(20000)
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
        for (neighbor <- 0 until n if graph(current)(neighbor) != 20000) {
          Operations += 1
          // Рассчитываем новое расстояние
          val newDistance = distance + graph(current)(neighbor)
          // Если новое расстояние меньше известного, обновляем известное расстояние и добавляем в очередь
          if (newDistance < distances(neighbor)) {
            distances(neighbor) = newDistance
            priorityQueue.enqueue((newDistance, neighbor))

          }

        }
      }

    }

    // Возвращаем массив расстояний
    distances
  }

  // Пример использования
  val graph = Array(
    Array(),
    Array(),
    Array(),
  )


  def main(args: Array[String]): Unit = {
    for (i <- 0 until 7) {
      val shortestPaths = dijkstra(graph, i)
      val n = shortestPaths.length
      println("Вершина № " + i)
      for (k <- 0 until n) {
        print(shortestPaths(k))
        print(" ")
      }
      println("")
      println("Операций:" + Operations)
      Operations = 0
    }
  }
}