package utils

object Helper {

  import scala.reflect.runtime.{universe => ru}

  def getTypeTag[T: ru.TypeTag](o: T) = ru.typeTag[T]

  def getType[T: ru.TypeTag](o: T) = getTypeTag(o).tpe

}
