

import org.scalatest._

import scala.util.Try
import scala.util.Success
import scala.util.Failure

object TrySpec {


  case class Product(name: String) {
    def eat: Product = {
      name.toLowerCase match {
        case "orange" => this
        case "stone" => throw new Exception(s"Product is inedible")
        case _ => throw new Exception(s"Product with undefined name = $name")
      }
    }
  }

  object Product {
    val orange: Product = Product("orange") // orange
    val stone: Product = Product("stone") // stone
    val productNoName: Product = Product("abracadabra") // (?)
  }

}

class TrySpec extends FlatSpec with Matchers {

  import TrySpec.Product
  import TrySpec.Product._

  it should "Try create" in {
    //     Try is Box  => Try (Yellow), Success - Green (with result), Failure - red (with exception)

    Try(orange.eat) shouldBe Success(orange)
    Try(stone.eat).isFailure shouldBe true
    println(Try(stone.eat))
    Try(productNoName.eat).isFailure shouldBe true
    println(Try(productNoName.eat))
    Try(productNoName.eat) != Try(stone.eat) shouldBe true
  }

  it should "Try for map" in {

    Try(orange.eat).map(_.name + " can eat") shouldBe Success(orange.name + " can eat")
    Try(stone.eat).map(_.name + " can eat").isFailure shouldBe true
  }


  it should "Try flatten" in {
    val successSuccessOrange: Try[Try[Product]] = Try(Try(orange.eat))
    successSuccessOrange.flatten shouldBe Success(orange.eat)

    val failureStone = Try(stone.eat)
    val successFailedStone: Try[Try[Product]] = Try(failureStone)
    successFailedStone.flatten shouldBe failureStone

    val failure = Failure(new Exception("e"))
    val failureExceptions: Try[Try[Product]] = failure
    failureExceptions.flatten shouldBe failure

    // flatMap = flatten + map
  }

  it should "Try filter" in {
    Try(orange.eat).filter(_.name.nonEmpty) shouldBe Success(orange)
    Try(orange.eat).filter(_.name.isEmpty).isFailure shouldBe true

    Try(stone.eat).filter(_.name.nonEmpty).isFailure shouldBe true
    Try(stone.eat).filter(_.name.isEmpty).isFailure shouldBe true

  }

  it should "Try recover" in {
    Try(orange.eat).recover { case _: Exception => productNoName } shouldBe Success(orange.eat)
    Try(stone.eat).recover { case _: Exception => productNoName } shouldBe Success(productNoName)

  }

  it should "Try recoverWith" in {
    Try(orange.eat).recoverWith { case _: Exception => Success(productNoName) } shouldBe Success(orange.eat)
    Try(stone.eat).recoverWith { case _: Exception => Success(productNoName) } shouldBe Success(productNoName)

  }

  it should "Try getOrElse" in {
    Try(orange.eat).getOrElse(productNoName) shouldBe orange.eat
    Try(stone.eat).getOrElse(productNoName) shouldBe productNoName


    // get not using!!!
    Try(Try(stone.eat).get).isFailure shouldBe true
  }

  it should "Try !!! bad style !!! " in {

    val successOrange = Try(orange.eat)

    //bad style
    val myOrange = if (successOrange.isSuccess) successOrange.get else ???
    myOrange shouldBe orange.eat


    val failureStone = Try(stone.eat)

    //bad style
    val tryStone = Try(if (failureStone.isFailure) failureStone.get else ???)
    tryStone.isFailure
  }

  it should "Try orElse" in {
    Try(orange.eat).orElse(Success(productNoName)) shouldBe Success(orange.eat)
    Try(stone.eat).orElse(Success(productNoName)) shouldBe Success(productNoName)
  }

  it should "Try toOption" in {
    Try(orange.eat).toOption shouldBe Some(orange.eat)
    Try(stone.eat).toOption shouldBe None
  }

  it should "Try flatten 3x" in {
    val sssProduct: Try[Try[Try[Product]]] = Try(Try(Try(orange.eat)))
    sssProduct.flatten shouldBe Success(Success(orange.eat))

    val failure = Failure(new Exception("e"))

    val ssfProduct: Try[Try[Try[Product]]] = Success(Success(failure))

    ssfProduct.flatten shouldBe Success(failure)

    val sfProduct: Try[Try[Try[Product]]] = Success(failure)

    sfProduct.flatten shouldBe failure

    val fProduct: Try[Try[Try[Product]]] = failure

    fProduct.flatten shouldBe failure

  }


}

