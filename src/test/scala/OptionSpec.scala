
import org.scalatest._

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


}
