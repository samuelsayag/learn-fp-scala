package utils

object Helper {

  import scala.reflect.runtime.{universe => ru}

  def getTypeTag[T: ru.TypeTag](o: T) = ru.typeTag[T]

  def getType[T: ru.TypeTag](o: T) = getTypeTag(o).tpe

  def wrapPrint(f: () => Unit, sep: String = "=") = {
    val size = 100
    val line = List.fill(size)(sep).mkString("")
    println(line)
    f()
    println(line)
  }
}
