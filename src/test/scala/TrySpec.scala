

import org.scalatest._

import scala.util.Try

object TrySpec {


  case class Product(property: String) {
    def eat: String = {
      property match {
        case "edible" => property
        case "inedible" => throw new Exception(s"Product is inedible")
        case _ => throw new Exception(s"Product is undefined with property = $property")
      }
    }
  }

  object Product {
    val edibleProduct: Product = Product("edible")
    val inedibleProduct: Product = Product("inedible")
    val undefinedProduct: Product = Product("abracadabra")
  }

}

class TrySpec extends FlatSpec with Matchers {


  it should "Try create" in {
    // Try is Box  => Red Green or Non Defined

    //    val list0Int: List[Int] = Nil
    //    list0Int shouldBe List()
    //
    //    // add new matrioshka
    //    val list1Int: List[Int] = 30 :: Nil
    //    list1Int shouldBe List(30)
    //
    //    val list2Int: List[Int] = 20 :: 30 :: Nil
    //    list2Int shouldBe List(20, 30)
    //
    //    val list3Int: List[Int] = 10 :: list2Int
    //    list3Int shouldBe List(10, 20, 30)

  }

  it should "Try for map" in {

    //    val list3Int: List[Int] = List(10, 20, 30)
    //    list3Int.map(_ + 2) shouldBe List(12, 22, 32)
    //
    //    val emptyList: List[Int] = List.empty
    //    emptyList.map(_ + 2) shouldBe List()
  }


  it should "Try flatten" in {
    //    val listNested3Int: List[List[Int]] = List(List(10), List(20, 30))
    //    listNested3Int.flatten shouldBe List(10, 20, 30)
    //
    //    val listNested3IntOther: List[List[Int]] = List(List(10), List(20), List(30))
    //    listNested3IntOther.flatten shouldBe List(10, 20, 30)
    //
    //    val listNested2Int: List[List[Int]] = List(List(), List(20, 30), List())
    //    listNested2Int.flatten shouldBe List(20, 30)
    //
    //    val listNested0Int: List[List[Int]] = List(List())
    //    listNested0Int.flatten shouldBe List()

    // flatMap = flatten + map
  }


  it should "Try filter" in {
    //    val list3Int: List[Int] = List(10, 20, 30)
    //    list3Int.filter(_ < 10) shouldBe List()
    //    list3Int.filter(_ > 10) shouldBe List(20, 30)
    //
    //    val emptyList: List[Int] = List.empty
    //    emptyList.filter(_ < 10) shouldBe List()
    //    emptyList.filter(_ > 10) shouldBe List()
  }


  //todo add getOrElse // add example bad style
  //todo  bad style  example if(try.isSuccess) try.get else ??? good stile ....
  //todo add orElse
  //todo add recover
  //todo add recoverWith
  //todo add toOption
  //todo example with validation List[Product] => List[Exceptions]


  it should "Try flatten 3x" in {
    //    val listDeepNested4Int: List[List[List[Int]]] = List(List(List(1, 1), List(2)), Nil, List(List(3), List(4)), Nil)
    //
    //    listDeepNested4Int.flatten shouldBe List(List(1, 1), List(2), List(3), List(4))
    //
    //    val listDeepNested0Int: List[List[List[Int]]] = List(List(List()))
    //    listDeepNested0Int.flatten shouldBe List(List())

  }


}

