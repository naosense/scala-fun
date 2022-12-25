import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

val failure = Future { 42 / 0 }
failure.value

val success = Future { 42 / 1 }
success.value

val fallback = failure.fallbackTo(success)

fallback.value

val failedFallback = failure.fallbackTo { Future { val res = 42; require(res < 0); res } }

failedFallback.value

val recovered = failedFallback.recover { case ex: ArithmeticException => -1 }

recovered.value

val unrecovered = fallback.recover { case ex: ArithmeticException => -1 }
unrecovered.value

val first = success.transform(
  res => res * 1,
  ex => new Exception("se", ex))

first.value

val second = failure.transform(
  res => res * 1,
  ex => new Exception("se", ex))

second.value
