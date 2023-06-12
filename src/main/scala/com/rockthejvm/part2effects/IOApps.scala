package com.rockthejvm.part2effects

import scala.io.StdIn
import cats.effect.{ExitCode, IO, IOApp}

object IOApps {
    val program = for {
        line <- IO(StdIn.readLine())
        _ <- IO(println(line))
    } yield ()

}

object TestApp {

    def main(args: Array[String]): Unit = {
        import cats.effect.unsafe.implicits.global
        import IOApps.*
        program.unsafeRunSync()
    }
}

object FirstCEApp extends IOApp {
    import IOApps.*
    override def run(args: List[String]): IO[ExitCode] = {
        program.as(ExitCode.Success)
    }
}

object MySimpleApp extends IOApp.Simple {
    import IOApps.*

    override def run: IO[Unit] = program
}
