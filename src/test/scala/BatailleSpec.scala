import org.scalatest.{FlatSpec, Matchers}

class BatailleSpec extends FlatSpec with Matchers {


  def bataille(j1: Stream[Int], j2: Stream[Int]): Tuple2[Stream[Int],Stream[Int]] = {
    var win = Tuple2(Stream.empty[Int], Stream.empty[Int])
    j1.zip(j2).foldLeft(win)(
      (w, pli) => {
        if(pli._1 > pli._2)  Tuple2(w._1 ++ Stream(pli._1 , pli._2), w._2)
        else Tuple2(w._1 , w._2++ Stream(pli._1 , pli._2))
      }
    )
  }

  it should "joueur 1 gagne un pli" in {

    var j1: Stream[Int] = Stream(7)
    var j2: Stream[Int] = Stream(1)
    assert(bataille(j1, j2) == Tuple2(Stream(7, 1), Stream.empty))
  }

  it should "joueur 1 ne gagne pas" in {

    var j1: Stream[Int] = Stream(7, 5)
    var j2: Stream[Int] = Stream(3, 2)
    assert(bataille(j1, j2) == Tuple2(Stream(7, 3, 5, 2), Stream.empty))
    assert(bataille(j2, j1) == Tuple2(Stream.empty, Stream(3, 7, 2, 5)))
  }

}
