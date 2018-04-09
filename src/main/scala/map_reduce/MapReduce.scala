package map_reduce
import my_utils.parallel

object mapReduce {
  val nWorkers = 4
  val chunkSize = 3
  def map(input: List[String]): List[(String, Int)] = {
    input.map(word => (word, 1))
  }

  def reduce(input: List[(String, Int)], f: (Int, Int) => Int): (String, Int) = {
    //println(input)
    val key = input.head._1
    //println(input.foldLeft(0)((acc, tup) => f(acc,tup._2)))
    //println((key, input.foldLeft(0)((acc, tup) => f(acc,tup._2))))
    (key, input.foldLeft(0)((acc, tup) => f(acc,tup._2)))
    /*var result = List[(String, Int)]
    for (element <- input) {

      println(element)

      val result = result :: ))
    }*/
    //println(input.foreach(x => x.foldLeft(0)((acc, tup) => acc + tup._2)))
    //input.foreach(x => x.foldLeft(0)((acc, tup) => acc + tup._2))
  }

  /*
    def shuffle() {}

    def worker(list_words: List[String],
               map: String => (String, Int),
               reduce: (String, Int) => (String, Int)): Unit = {

    }*/
  def main(args: Array[String]): Unit = {
    val array = "Little Car Seven Nine Top Seven Car Nine Seven"
    val list_words = array.split(" ").toList
    println(list_words.length)
    println(list_words.length/4)
    //println(list_words mkString "\n")
    val split_list = list_words.grouped(list_words.length/chunkSize).toList
    println(split_list)
    val all_list = split_list.map(x => map(x))
      //.foldRight(List("New"))((x,y) => x ::: y)
    val sort = all_list.foldRight(List[(String, Int)]())((x,y) => x ::: y)
    println(sort)
    println(sort.groupBy(x => x._1).valuesIterator.toList)
    val last_list = sort.groupBy(x => x._1).valuesIterator.toList
      for (element <- last_list){
        println(reduce(element, (_+_)))
      }

    //println(sort.map(x => _+_))
    //val reduce = sort.map(x => )
   /* println(worker(list_words,
                    map : word => (word, 1),
                    reduce: (key, value) => (key, sum(values)) ))*/
  }
}
