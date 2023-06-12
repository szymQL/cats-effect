package com.rockthejvm.part1recap

object ContextualAbstractionsScala3 {

    def increment(x: Int)(using amount: Int): Int = x + amount
    given defaultAmount: Int = 10
    val twelve: Int = increment(2) // (10) automatically by the compiler

    def multiply(x: Int)(using factor: Int): Int = x * factor
    val aHundred = multiply(10)

    //more complex use case
    trait Combiner[A] {
        def combine(x: A, y: A): A
        def empty: A
    }

    def combineAll[A](values: List[A])(using combiner: Combiner[A]): A = {
        values.foldLeft(combiner.empty)(combiner.combine)
    }

    given intCombiner: Combiner[Int] with {
        override def combine(x: Int, y: Int): Int = x + y
        override def empty: Int = 0
    }

    given stringCombiner: Combiner[String] with {
        override def combine(x: String, y: String): String = x + y
        override def empty: String = ""
    }

    val numbers = (1 to 10).toList
    val sum10 = combineAll(numbers)

    val strings = List("Cats", "Scala", "3")
    val sumStrings = combineAll(strings)

    // synthesize given instances
    given optionCombiner[T](using combiner: Combiner[T]): Combiner[Option[T]] with {
        override def empty: Option[T] = Some(combiner.empty)
        override def combine(x: Option[T], y: Option[T]): Option[T] = for {
            vx <- x
            vy <- y
        } yield combiner.combine(vx, vy)
    }

    val sumOptions: Option[Int] = combineAll((1 to 10).toList.map(Option(_)))

    // extension methods
    case class Person(name: String) {
        def greet(): String = s"Hi, my name is $name"
    }

    extension (name: String) {
        def greet(): String = Person(name).greet()
    }

    // generic extension
    extension [T](list: List[T]) {
        def reduceAll(using combiner: Combiner[T]): T = {
            list.foldLeft(combiner.empty)(combiner.combine)
        }
    }

    val sum10_v2 = numbers.reduceAll
    val sumStrings_v2 = strings.reduceAll

     // type classes

    object TypeClassesScala3 {
        case class Person(name: String, age: Int)

        // part 1 - Type class definition
        trait JSONSerializer[T] {
            def toJson(value: T): String
        }

        // part 2 - type class instances
        given stringSerializer: JSONSerializer[String] with {
            override def toJson(value: String): String = "\"" + value + "\""
        }

        given intSerializer: JSONSerializer[Int] with {
            override def toJson(value: Int): String = value.toString
        }

        given personSerializer: JSONSerializer[Person] with {
            override def toJson(value: Person): String =
                s"""
                   |{"name": "${value.name}", "age": ${value.age}}
                   |""".stripMargin.trim
        }

        def convertToJson[T](value: T)(using serializer: JSONSerializer[T]): String = serializer.toJson(value)

        def convertListToJson[T](list: List[T])(using serializer: JSONSerializer[T]): String = {
            list.map(serializer.toJson).mkString("[", ",", "]")
        }

        // part 4 - extension methods just for the types we support
        extension [T](value: T) {
            def toJson(using serializer: JSONSerializer[T]): String = serializer.toJson(value)
        }
    }

    def main(args: Array[String]): Unit = {
        import TypeClassesScala3.{intSerializer, stringSerializer, personSerializer, Person => Persona, convertListToJson, toJson}
        val listTest = List(Persona("Alice", 23), Persona("Bob", 46))
        println(convertListToJson(listTest))
        println(Persona("Bob", 46).toJson)
    }
}
