/**
  * Implement validator typeclass that should validate arbitrary value [T].
  * @tparam T the type of the value to be validated.
  */
trait Validator[T] {
  /**
    * Validates the value.
    * @param value value to be validated.
    * @return Right(value) in case the value is valid, Left(message) on invalid value
    */
  def validate(value: T): Either[String, T]

  /**
    * And combinator.
    * @param other validator to be combined with 'and' with this validator.
    * @return the Right(value) only in case this validator and <code>other</code> validator returns valid value,
    *         otherwise Left with error messages from the validator that failed.
    */
  def and(other: Validator[T]): Validator[T] = {
    val thisValidator = this
    new Validator[T] {
      override def validate(t: T): Either[String, T] = {
        val thisValidation = thisValidator validate t
        if (thisValidation isRight) other validate t else thisValidation
      }
    }
  }

  /**
    * Or combinator.
    * @param other validator to be combined with 'or' with this validator.
    * @return the Right(value) only in case either this validator or <code>other</code> validator returns valid value,
    *         otherwise Left with error messages from both validators.
    */
  def or(other: Validator[T]): Validator[T] = {
    val thisValidator = this
    new Validator[T] {
      override def validate(t: T): Either[String, T] = {
        val thisValidation = thisValidator validate t
        val otherValidation = other validate t
        if (thisValidation isRight)
          thisValidation
        else if (otherValidation isRight)
          otherValidation
        else
          Left(thisValidation.left + "; " + otherValidation.left)
      }
    }
  }
}


object Validator {
  val positiveInt : Validator[Int] = new Validator[Int] {
    override def validate(t: Int): Either[String, Int] =
      if (t > 0) Right(t) else Left("Not positive Int")
  }

  def lessThan(n: Int): Validator[Int] = new Validator[Int] {
    override def validate(t: Int): Either[String, Int] =
      if (t < n) Right(t) else Left(t + " more than or equal " + n)
  }

  val nonEmpty : Validator[String] = new Validator[String] {
    override def validate(t: String): Either[String, String] =
      if (!t.isEmpty) Right(t) else Left("Empty String")
  }

  val isPersonValid = new Validator[Person] {
    // Returns valid only when the name is not empty and age is in range [1-99].
    override def validate(value: Person): Either[String, Person] = {
      val nameNotEmpty = nonEmpty validate value.name
      if (nameNotEmpty.isLeft)
        Left(nameNotEmpty.left.get)
      else {
        val ageInRange = ((positiveInt and lessThan(100)) validate value.age)
        if (ageInRange.isLeft)
          Left(ageInRange.left.get)
        else
          Right(value)
      }
    }
  }
}

object ValidApp {
  import Validator._

  implicit class AnyValidator[T](t: T) {
    def validate(validator: Validator[T]): Either[String, T] = {
      validator.validate(t)
    }
  }

  2 validate (positiveInt and lessThan(10))

  "" validate Validator.nonEmpty

  Person(name = "John", age = 25) validate isPersonValid
}

object ImplicitValidApp {
  import Validator._

  implicit val isPersVal = isPersonValid
  implicit val notEmptyStr = nonEmpty
  implicit val posInt = positiveInt

  implicit class AnyValidator[T](t: T) {
    def validate(implicit validator: Validator[T]): Either[String, T] = {
      validator.validate(t)
    }
  }

  Person(name = "John", age = 25) validate

  "asdasd" validate

  234.validate
}


case class Person(name: String, age: Int)