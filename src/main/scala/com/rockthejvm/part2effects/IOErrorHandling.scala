package com.rockthejvm.part2effects

import cats.effect.IO
import cats.effect.unsafe.implicits.global

import scala.util.{Failure, Success, Try}

object IOErrorHandling {

    val aFailedCompute: IO[Int] = IO.delay(throw new RuntimeException("A FAILURE"))
    val aFailure: IO[Int] = IO.raiseError(new RuntimeException("a proper fail"))

    val dealWithIt = aFailure.handleErrorWith {
        case _: RuntimeException => IO(println("I'm still here"))
        // more cases
    }

    // turn into an Either
    val effectAsEither: IO[Either[Throwable, Int]] = aFailure.attempt

    // redeem: transform the failure and the success in one go
    val resultAsString: IO[String] = aFailure.redeem(ex => s"Fail: $ex", value => s"Success: $value")

    // redeemWith
    val resultAsEffect = aFailure.redeemWith(ex => IO(println(s"Fail: $ex")), value => IO(println(s"Success: $value")))

    /**
     * Exercises
     */
    // 1 - construct potentially failed IOs from standard data types (Option, Try, Either)
    def option2IO[A](anOption: Option[A])(ifEmpty: Throwable): IO[A] = {
        anOption match
            case Some(value) => IO(value)
            case None => IO.raiseError(ifEmpty)
    }

    def try2IO[A](aTry: Try[A]): IO[A] = {
        aTry match
            case Failure(exception) => IO.raiseError(exception)
            case Success(value) => IO(value)
    }

    def either2IO[A](anEither: Either[Throwable, A]): IO[A] = {
        anEither match
            case Left(ex) => IO.raiseError(ex)
            case Right(value) => IO(value)
    }
    
    // 2 - handleError, handleErrorWith
    def handleIOError[A](io: IO[A])(handler: Throwable => A): IO[A] = {
        io.redeem(handler, identity)
    }

    def handleIOErrorWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] = {
        io.redeemWith(handler, IO.pure)
    }


    def main(args: Array[String]): Unit = {
        aFailure.unsafeRunSync()
    }

}
