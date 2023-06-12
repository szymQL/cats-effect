package com.rockthejvm.part2effects

import cats.effect.IO
import cats.effect.unsafe.implicits.global

import scala.annotation.tailrec
import scala.io.StdIn

object IOIntroduction {

    val ourFirstIO: IO[Int] = IO.pure(42)
    val delayedIO: IO[Int] = IO.delay({
        println("I'm producing an integer")
        54
    })

    val aDelayedIO_v2: IO[Int] = IO {
        println("I'm producing an integer")
        54
    }

    def smallProgram(): IO[Unit] = {
        for {
            line1 <- IO(StdIn.readLine())
            line2 <- IO(StdIn.readLine())
            _ <- IO(println(line1 + line2))
        } yield ()
    }

    import cats.syntax.apply.*

    def smallProgram_v2(): IO[Unit] = {
        (IO(StdIn.readLine()), IO(StdIn.readLine())).mapN(_ + _).map(println)
    }

    // 1 - sequence two IOs and take the result of the LAST one
    // hint: use flatMap
    def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] = {
        ioa.flatMap(_ => iob)
    }

    // 2 - sequence two IOs and take the result of the FIRST one
    def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] = {
        ioa.flatMap(a => iob.map(_ => a))
    }

    // 3 - repeat an IO effect forever
    def forever[A](io: IO[A]): IO[A] = {
        io.flatMap(_ => forever(io))
    }

    // 4 - convert an IO to a different type
    def convert[A, B](ioa: IO[A], value: B): IO[B] = {
        ioa.map(_ => value)
    }

    // 5 - discard value inside an IO, just return Unit
    def asUnit[A](ioa: IO[A]): IO[Unit] = {
        ioa.map(_ => ())
    }

    // 6 - fix stack recursion
    def sum(n: Int): Int = {
        if (n <= 0) 0
        else n + sum(n - 1)
    }

    def sumIO(n: Int): IO[Int] = {
        if (n <= 0) IO(0)
        else for {
            lastNumber <- IO(n)
            prevSum <- sumIO(n - 1)
        } yield prevSum + lastNumber
    }

    // 7 - write fibonacci function IO that does NOT crash on recursion
    def fibonacci(n: Int): IO[BigInt] = {
        if (n <= 2) IO(1)
        else for {
            last <- IO(fibonacci(n - 1)).flatten
            prev <- IO(fibonacci(n - 2)).flatten
        } yield last + prev
    }

    def main(args: Array[String]): Unit = {
        // smallProgram().unsafeRunSync()
        (1 to 20).foreach(i => println(fibonacci(i).unsafeRunSync()))
    }

}
