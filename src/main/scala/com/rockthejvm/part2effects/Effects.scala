package com.rockthejvm.part2effects

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.io.StdIn

object Effects {

    // pure functional programming
    // substitution
    def combine(a: Int, b: Int): Int = a + b

    val five: Int = combine(2, 3)
    val five_v2: Int = 2 + 3
    val five_v3: Int = 5

    // referential transparency = can replace and expression with its value
    // as many times as we want without changing behavior

    // example: print to the console
    // val printSomething: Unit = println("Cats Effect")
    val printSomething_v2: Unit = () // not the same - impure

    // example: change a variable
    var anInt = 0
    val changingVar: Unit = anInt += 1
    val changingVar_v2: Unit = () // not the same - impure

    // side effects are inevitable for useful programs

    // effect
    /*
    Desires:
     - type signature describes the kind of calculation that will be performed
     - type signature describes the VALUE that will be calculated
     - when side effects are needed, effect construction is separate from effect execution
    */

    /*
    example: Option
     - describes a possibly absent value
     - computes a value of type A, if it exists
     - side effects are not needed
    */
    val anOption: Option[Int] = Option(42)

    /*
    example: Future
     - describes an asynchronous computation
     - computes a value of type A, if it's successful
     - side effect is required (allocating/scheduling a thread), execution is NOT separate from construction
    */
    given ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

    val aFuture: Future[Int] = Future(42)

    /*
    example: MyIO
     - describes any computation that might produce side effects
     - calculates a value of type A, if it's successful
     - side effects are required for the evaluation of () => A, AND creation of MyIO does NOT produces side effects on construction
    */
    case class MyIO[A](unsafeRun: () => A) {
        def map[B](f: A => B): MyIO[B] = MyIO(() => f(unsafeRun()))

        def flatMap[B](f: A => MyIO[B]): MyIO[B] = MyIO(() => f(unsafeRun()).unsafeRun())
    }

    val anIO: MyIO[Unit] = MyIO(() => {
        println("Cats Effect in IO")
    })

    /**
     * Exercises
     * 1. An IO which returns the current time of the system
     * 2. An IO which measures the duration of a computation (hint: use ex 1)
     * 3. An IO which prints something to the console
     * 4. An IO which reads a line (a string) from the std input
     */
    // 1.
    val currentTimeIO: MyIO[Long] = MyIO(() => {
        System.currentTimeMillis()
    })
    // 2.
    def measure[A](computation: MyIO[A]): MyIO[Long] = {
        for {
            startTime <- currentTimeIO
            _ <- computation
            endTime <- currentTimeIO
        } yield endTime - startTime
    }
    // 3.
    def printToConsole(message: String): MyIO[Unit] = {
        for {
            _ <- MyIO(() => println(message))
        } yield ()
    }
    def putStrLn(line: String): MyIO[Unit] = MyIO(() => println(line))
    // 4.
    def readFromConsole: MyIO[Unit] = {
        for {
            message <- MyIO(scala.io.StdIn.readLine)
            _ <- MyIO(() => println(s"Hello, $message"))
        } yield ()
    }
    val read: MyIO[String] = MyIO(() => StdIn.readLine())

    def testConsole(): Unit = {
        val program: MyIO[Unit] = for {
            line1 <- read
            line2 <- read
            _ <- putStrLn(line1 + line2)
        } yield ()
        program.unsafeRun()
    }

    def main(args: Array[String]): Unit = {
        println(measure(anIO).unsafeRun())
        printToConsole("test").unsafeRun()
        readFromConsole.unsafeRun()
        MyIO(() => println("test")).unsafeRun()
        testConsole()
    }

}
