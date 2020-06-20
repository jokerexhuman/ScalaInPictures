
import org.scalatest._

import scala.util.Try

object OptionSpec {

  case class Orange(size: Int, variety: String) {
    def peel: PulpOrange = PulpOrange(size)

    def isCitrus: Boolean = true
  }

  case class PulpOrange(size: Int)

}

class OptionSpec extends FlatSpec with Matchers {

  import OptionSpec._

  it should "Option for null" in {

    //option is box close or open (open empty or open non empty)

    val orange: Orange = Orange(13, "Salustiana")
    Option(orange) shouldBe Some(orange)

    val orangeNull: Orange = null
    Option(orangeNull) shouldBe None

    val string: String = "Hello"
    Option(string) shouldBe Some(string)

    val stringNull: String = null
    Option(stringNull) shouldBe None
  }

  it should "Option for map" in {

    val orange = Orange(13, "Salustiana")
    Some(orange).map(_.peel) shouldBe Some(orange.peel)

    val noneOrange: Option[Orange] = None
    noneOrange.map(_.peel) shouldBe None

    val string: String = "Hello"
    Some(string).map(x => x + ", world!") shouldBe Some("Hello, world!")

    val noneString: Option[String] = None
    noneString.map(x => x + ", world!") shouldBe None
  }


  it should "Option flatten" in {
    val orange: Orange = Orange(13, "Salustiana")

    val someSome: Option[Option[Orange]] = Some(Some(orange))
    someSome.flatten shouldBe Some(orange)

    val someNone: Option[Option[Orange]] = Some(None)
    someNone.flatten shouldBe None

    val none: Option[Option[Orange]] = None
    none.flatten shouldBe None

    // flatMap = flatten + map
  }

  it should "Option filter" in {
    val orange = Orange(13, "Salustiana")
    Some(orange).filter(_.isCitrus) shouldBe Some(orange)
    Some(orange).filter(!_.isCitrus) shouldBe None

    val noneOrange: Option[Orange] = None
    noneOrange.filter(_.isCitrus) shouldBe None
    noneOrange.filter(!_.isCitrus) shouldBe None

    val string: String = "Hello"
    Some(string).filter(_ == string) shouldBe Some("Hello")
    Some(string).filter(_ != string) shouldBe None

    val noneString: Option[String] = None
    noneString.filter(_ == string) shouldBe None
    noneString.filter(_ != string) shouldBe None
  }

  it should "Option getOrElse" in {
    val orange = Orange(13, "Salustiana")
    Some(orange).getOrElse(Orange(14, "Other")) shouldBe orange

    val noneOrange: Option[Orange] = None
    noneOrange.getOrElse(Orange(14, "Other")) shouldBe Orange(14, "Other")

    val string: String = "Hello"
    Some(string).getOrElse("") shouldBe string

    val noneString: Option[String] = None
    noneString.getOrElse("") shouldBe ""

    // get not using!!!
    Try(noneString.get).isFailure shouldBe true
  }

  it should "Option !!! bad style !!! " in {

    val someOrange: Option[Orange] = Some(Orange(13, "Salustiana"))

    //bad style
    val orange = if (someOrange.isDefined) someOrange.get else ???
    orange shouldBe Orange(13, "Salustiana")


    val noneOrange: Option[Orange] = None
    //bad style
    val tryOrange = Try(if (noneOrange.isEmpty) noneOrange.get else ???) // this logic error  !!!  noneOrange.isEmpty !!!
    tryOrange.isFailure shouldBe true
  }

  it should "Option orElse" in {

    val orange = Orange(13, "Salustiana")
    Some(orange).orElse(Some(Orange(14, "Other"))) shouldBe Some(orange)

    val noneOrange: Option[Orange] = None
    noneOrange.orElse(Some(Orange(14, "Other"))) shouldBe Some(Orange(14, "Other"))

  }

  it should "Option toSeq" in {

    val orange = Orange(13, "Salustiana")
    Some(orange).toSeq shouldBe Seq(orange)
    Some(orange).toList shouldBe List(orange)

    val noneOrange: Option[Orange] = None
    noneOrange.toSeq shouldBe Seq.empty[Orange]
    noneOrange.toList shouldBe Nil

  }


  it should "Option flatten 3x" in {
    val orange: Orange = Orange(13, "Salustiana")

    val someSomeSome: Option[Option[Option[Orange]]] = Some(Some(Some(orange)))
    someSomeSome.flatten shouldBe Some(Some(orange))

    val someSomeNone: Option[Option[Option[Orange]]] = Some(Some(None))
    someSomeNone.flatten shouldBe Some(None)

    val someNone: Option[Option[Option[Orange]]] = Some(None)
    someNone.flatten shouldBe None

    val none: Option[Option[Option[Orange]]] = None
    none.flatten shouldBe None

  }

  it should "Option match" in {

    val someHello = Some("hello")
    val someBye = Some("bye")

    def transform(option: Option[String]) = option match {
      case Some("hello") => 1
      case Some("bye") => 2
      case Some(_) => -1
      case None => 0
    }

    def transform2(option: Option[String]) = option.map {
      value =>
        if (value == "hello") {
          1
        } else if (value == "bye") {
          2
        } else {
          -1
        }
    }.getOrElse(0)

    transform(someHello) shouldBe 1
    transform(someBye) shouldBe 2
    transform(Some("abracadabra")) shouldBe -1
    transform(None) shouldBe 0


    transform2(someHello) shouldBe 1
    transform2(someBye) shouldBe 2
    transform2(Some("abracadabra")) shouldBe -1
    transform2(None) shouldBe 0


  }

}
