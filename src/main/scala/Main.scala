import scala.annotation.tailrec
import scala.concurrent.duration._

object Main extends App {

  retry[Int](
    block = () => 1 + 2,
    acceptResult = res => res % 2 == 0,
    retries = List(0.seconds, 1.seconds, 2.seconds)
  )

  @tailrec
  def retry[A](block: () => A,
               acceptResult: A => Boolean,
               retries: List[FiniteDuration]): A = {
    val res = block()
    println("Current res:" + res)
    if (acceptResult(res) || retries.isEmpty) {
      res
    } else {
      Thread.sleep(retries.head.toMillis)
      retry(block, acceptResult, retries.tail)
    }
  }

}
