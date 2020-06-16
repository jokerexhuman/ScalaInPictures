import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Success, Try}

class SugarForSpec extends FlatSpec with Matchers {

  it should "Option for" in {

    def getWater: Option[Int] = Some(200)

    def getCoffee: Option[String] = Some("coffee")

    def getSugar: Option[String] = Some("sugar")

    val cupOfCoffee: Option[String] = getWater.filter(water => water >= 100).flatMap { water =>
      getCoffee.flatMap {
        coffee =>
          getSugar.map {
            sugar => s"Cup of $coffee with $sugar and water $water ml"
          }
      }
    }
    cupOfCoffee shouldBe Some("Cup of coffee with sugar and water 200 ml")

    val cupOfCoffeeFor: Option[String] = for {
      water <- getWater if water >= 100
      coffee <- getCoffee
      sugar <- getSugar
    } yield s"Cup of $coffee with $sugar and water $water ml"

    cupOfCoffeeFor shouldBe Some("Cup of coffee with sugar and water 200 ml")

  }

  it should "List for" in {

    def getWater: List[Int] = List(200, 50, 150)

    def getCoffee: List[String] = List("coffee", "tea")

    def getSugar: List[String] = List("sugar", "jam")

    val cupOfCoffee: List[String] = getWater.filter(water => water >= 100).flatMap { water =>
      getCoffee.flatMap {
        coffee =>
          getSugar.map {
            sugar => s"Cup of $coffee with $sugar and water $water ml"
          }
      }
    }
    cupOfCoffee shouldBe List(
      "Cup of coffee with sugar and water 200 ml",
      "Cup of coffee with jam and water 200 ml",
      "Cup of tea with sugar and water 200 ml",
      "Cup of tea with jam and water 200 ml",
      "Cup of coffee with sugar and water 150 ml",
      "Cup of coffee with jam and water 150 ml",
      "Cup of tea with sugar and water 150 ml",
      "Cup of tea with jam and water 150 ml")

    val cupOfCoffeeFor: List[String] = for {
      water <- getWater if water >= 100
      coffee <- getCoffee
      sugar <- getSugar
    } yield s"Cup of $coffee with $sugar and water $water ml"

    cupOfCoffeeFor shouldBe List(
      "Cup of coffee with sugar and water 200 ml",
      "Cup of coffee with jam and water 200 ml",
      "Cup of tea with sugar and water 200 ml",
      "Cup of tea with jam and water 200 ml",
      "Cup of coffee with sugar and water 150 ml",
      "Cup of coffee with jam and water 150 ml",
      "Cup of tea with sugar and water 150 ml",
      "Cup of tea with jam and water 150 ml")

  }

  it should "Try for" in {

    def getWater: Try[Int] = Success(200)

    def getCoffee: Try[String] = Success("coffee")

    def getSugar: Try[String] = Success("sugar")

    val cupOfCoffee: Try[String] = getWater.filter(water => water >= 100).flatMap { water =>
      getCoffee.flatMap {
        coffee =>
          getSugar.map {
            sugar => s"Cup of $coffee with $sugar and water $water ml"
          }
      }
    }
    cupOfCoffee shouldBe Success("Cup of coffee with sugar and water 200 ml")

    val cupOfCoffeeFor: Try[String] = for {
      water <- getWater if water >= 100
      coffee <- getCoffee
      sugar <- getSugar
    } yield s"Cup of $coffee with $sugar and water $water ml"

    cupOfCoffeeFor shouldBe Success("Cup of coffee with sugar and water 200 ml")

  }

  it should "Try foreach" in {

    // Option.foreach
    // List.foreach
    // Try.foreach

    def getWater: Try[Int] = Success(200)

    def getCoffee: Try[String] = Success("coffee")

    def getSugar: Try[String] = Success("sugar")

    getWater.filter(water => water >= 100).flatMap { water =>
      getCoffee.flatMap {
        coffee =>
          getSugar.map {
            sugar => s"Cup of $coffee with $sugar and water $water ml"
          }
      }
    }.foreach(println)


    for {
      water <- getWater if water >= 100
      coffee <- getCoffee
      sugar <- getSugar
    } println(s"Cup of $coffee with $sugar and water $water ml")
    // without yield  - foreach
  }

  it should "List with Option - magic" in {

    def getWater: List[Int] = List(200)

    def getCoffee: Option[String] = Some("coffee")

    def getSugar: Option[String] = Some("sugar")

    val cupOfCoffee: List[String] = getWater.filter(water => water >= 100).flatMap { water =>
      getCoffee.flatMap {
        coffee =>
          getSugar.map {
            sugar => s"Cup of $coffee with $sugar and water $water ml"
          }
      }
    }
    cupOfCoffee shouldBe List("Cup of coffee with sugar and water 200 ml")

    val cupOfCoffeeFor: List[String] = for {
      water <- getWater if water >= 100
      coffee <- getCoffee
      sugar <- getSugar
    } yield s"Cup of $coffee with $sugar and water $water ml"

    cupOfCoffeeFor shouldBe List("Cup of coffee with sugar and water 200 ml")
  }

  it should "Option with List - no magic" in {

    def getWater: Option[Int] = Some(200)

    def getCoffee: List[String] = List("coffee")

    def getSugar: List[String] = List("sugar")

    // not compiling
    //    val cupOfCoffee = getWater.filter(water => water >= 100).flatMap { water =>
    //      getCoffee.flatMap {
    //        coffee =>
    //          getSugar.map {
    //            sugar => s"Cup of $coffee with $sugar and water $water ml"
    //          }
    //      }
    //    }
    //
    //    val cupOfCoffeeFor = for {
    //      water <- getWater if water >= 100
    //      coffee <- getCoffee
    //      sugar <- getSugar
    //    } yield s"Cup of $coffee with $sugar and water $water ml"
    //

  }

  it should "List with Try - no magic" in {

    def getWater: List[Int] = List(200)

    def getCoffee: Try[String] = Success("coffee")

    def getSugar: Try[String] = Success("sugar")

    // not compiling
    //    val cupOfCoffee = getWater.filter(water => water >= 100).flatMap { water =>
    //      getCoffee.flatMap {
    //        coffee =>
    //          getSugar.map {
    //            sugar => s"Cup of $coffee with $sugar and water $water ml"
    //          }
    //      }
    //    }
    //
    //
    //    val cupOfCoffeeFor = for {
    //      water <- getWater if water >= 100
    //      coffee <- getCoffee
    //      sugar <- getSugar
    //    } yield s"Cup of $coffee with $sugar and water $water ml"


  }

}
