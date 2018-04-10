package map_reduce
import my_utils.parallel

object MapReduce {
  var threshold = 4
  def map(input: List[String], f: String => (String, Int)): List[(String, Int)] = {
    input.map(f)
  }
  def reduce(input: List[(String, Int)], f: (Int, Int) => Int): (String, Int) = {
    val key = input.head._1
    (key, input.foldLeft(0)((acc, tup) => f(acc,tup._2)))
  }
  def mapParallel(input: List[List[String]], f: String => (String, Int)): List[(String, Int)] = input match{
    case x if x.length > 1 =>
      val (x,y) = parallel(map(input.head, f: String => (String, Int)),mapParallel(input.tail, f: String => (String, Int)))
      x ::: y

    case _ => map(input.head, f: String => (String, Int))
  }
  def reduceParallel(input: List[List[(String, Int)]], f: (Int, Int) => Int):List[(String, Int)] = input match{
    case x if x.length > 1 =>
      val (x,y) = parallel(reduce(input.head, f),reduceParallel(input.tail, f))
      List(x) ::: y

    case _ => List(reduce(input.head, f))
  }
  def worker(input: List[String], map_func: String => (String, Int), reduce_func:(Int, Int) => Int):List[(String, Int)] = {
    val split_list = input.grouped(input.length/threshold).toList
    val mapped_list = mapParallel(split_list, map_func)
    val sorted_list = mapped_list.groupBy(x => x._1).valuesIterator.toList
    reduceParallel(sorted_list, reduce_func)

  }
  def nonParallelMapReduce(list_words:List[String], map_func: String => (String, Int), reduce_func:(Int, Int) => Int) = {
    val split_list = list_words.grouped(list_words.length/threshold).toList
    val all_list = split_list.map(x => map(x,map_func))
    val sort = all_list.foldRight(List[(String, Int)]())((x,y) => x ::: y)
    val last_list = sort.groupBy(x => x._1).valuesIterator.toList
    last_list.map(el => reduce(el, reduce_func))
  }
  def main(args: Array[String]): Unit = {
    /*
    *
    * I was curious to start with sequence of words
    */
    val array = "Little Car Seven Nine Top Seven Car Nine Seven Nine Car Car Car Seven Nine Top"
    val list_words = array.split(" ").toList
    println("List for work: "+list_words)

    val t0 = System.nanoTime()
    val result1 = nonParallelMapReduce(list_words, el => (el,1), _+_)
    val t1 = System.nanoTime()
    println(result1)
    println("Elapsed time (non parallel): "+(t1-t0) + " ns")
    val t3 = System.nanoTime()
    val result2 = worker(list_words, el => (el,1), _+_)
    val t4 = System.nanoTime()
    println(result2)
    println("Elapsed time (parallel): "+(t4-t3) + " ns")

    /*
    *
    * AND now numbers
    */
    threshold = 16
    val r = scala.util.Random
    val num_list = List.fill(1024)(r.nextInt(10).toString)
    print("List for work: "+num_list)
    val t20 = System.nanoTime()
    val result21 = nonParallelMapReduce(num_list, el => (el,1), _+_)
    val t21 = System.nanoTime()
    println(result21)
    println("Elapsed time (non parallel): "+(t21-t20) + " ns")
    val t23 = System.nanoTime()
    val result22 = worker(num_list, el => (el,1), _+_)
    val t24 = System.nanoTime()
    println(result22)
    println("Elapsed time (parallel): "+(t24-t23) + " ns")
  }
}
