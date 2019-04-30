import org.scalatest.FunSuite
import scala.concurrent.duration._

class MainTest extends FunSuite {

  test("Main.retry") {
    assert(Main.retry[Int](
      block = () => 1 + 2,
      acceptResult = res => res % 2 == 0,
      retries = List()
    ) === 3)

    assert(Main.retry[Int](
      block = () => 2,
      acceptResult = res => res % 2 == 0,
      retries = List()
    ) === 2)

    assert(Main.retry[Int](
      block = () => 1 + 2,
      acceptResult = res => res % 2 == 0,
      retries = List(1.seconds)
    ) === 3)

    assert(Main.retry[Int](
      block = () => 2,
      acceptResult = res => res % 2 == 0,
      retries = List(1.seconds)
    ) === 2)
  }

}