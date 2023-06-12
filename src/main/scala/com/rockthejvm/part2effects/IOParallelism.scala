package com.rockthejvm.part2effects

import cats.Parallel
import cats.effect.IO.Par
import cats.effect.{IO, IOApp}
import cats.syntax.apply.*
import com.rockthejvm.utils.*
import cats.effect.implicits.*
import cats.syntax.parallel.*

object IOParallelism extends IOApp.Simple {
    // IOs are usually sequential
    val aniIO = IO(s"[${Thread.currentThread().getName}] Ani")
    val kamranIO = IO(s"[${Thread.currentThread().getName}] Kamran")

    val composedIO = for {
        ani <- aniIO
        kamran <- kamranIO
    } yield s"$ani and $kamran love Rock the JVM"

    val meaningOfLife: IO[Int] = IO(42)
    val favLang: IO[String] = IO("Scala")
    val goalInLife = (meaningOfLife.myDebug, favLang.myDebug).mapN((num, str) => s"my goal in life is $num and $str")

    // parallelism on IOs
    // convert a sequential IO to parallel IO
    val parIO1: IO.Par[Int] = Parallel[IO].parallel(meaningOfLife.myDebug)
    val parIO2: IO.Par[String] = Parallel[IO].parallel(favLang.myDebug)
    val goalInLifeParallel: IO.Par[String] = (parIO1, parIO2).mapN((num, str) => s"my goal in life is $num and $str")
    // turn back to sequential
    val goalInLife_v2: IO[String] = Parallel[IO].sequential(goalInLifeParallel)

    // shorthand:
    val goalInLife_v3: IO[String] = (meaningOfLife.myDebug, favLang.myDebug).parMapN((num, str) => s"my goal in life is $num and $str")

    // regarding failure:
    val aFailure: IO[String] = IO.raiseError(new RuntimeException("I can't do this!"))
    val parallelWithFailure: IO[String] = (favLang.myDebug, aFailure.myDebug).parMapN(_ + _)

    val anotherFailure = IO.raiseError(new RuntimeException("I can't do this version 2!"))
    val twoFailures = (aFailure.myDebug, anotherFailure.myDebug).parMapN(_ + _)

    override def run: IO[Unit] = twoFailures.myDebug.void
}
