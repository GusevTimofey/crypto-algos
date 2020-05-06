package benches

import java.util.concurrent.TimeUnit

import benches.SHA1Benches.SHA1BenchesState
import algos.hash.HashFunction
import org.openjdk.jmh.annotations.{ Benchmark, Mode, Scope, State }
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.profile.GCProfiler
import org.openjdk.jmh.runner.options.{ OptionsBuilder, VerboseMode }
import org.openjdk.jmh.runner.{ Runner, RunnerException }

class SHA1Benches {

  @Benchmark
  def mb1bench(state: SHA1BenchesState, bh: Blackhole): Unit =
    bh.consume(HashFunction.apply.make(state.mb1))

  @Benchmark
  def mb10bench(state: SHA1BenchesState, bh: Blackhole): Unit =
    bh.consume(HashFunction.apply.make(state.mb10))

  @Benchmark
  def mb100bench(state: SHA1BenchesState, bh: Blackhole): Unit =
    bh.consume(HashFunction.apply.make(state.mb100))
}

object SHA1Benches {
  @throws[RunnerException]
  def main(args: Array[String]): Unit = {
    val opt = new OptionsBuilder()
      .include(".*" + classOf[SHA1Benches].getSimpleName + ".*")
      .forks(1)
      .threads(1)
      .warmupIterations(10)
      .measurementIterations(10)
      .mode(Mode.SingleShotTime)
      .timeUnit(TimeUnit.SECONDS)
      .verbosity(VerboseMode.EXTRA)
      .addProfiler(classOf[GCProfiler])
      .build
    new Runner(opt).run
  }

  @State(Scope.Benchmark)
  class SHA1BenchesState {
    val mutableState1mb   = new scala.collection.mutable.StringBuilder(5000000)
    val mutableState10mb  = new scala.collection.mutable.StringBuilder(50000000)
    val mutableState100mb = new scala.collection.mutable.StringBuilder(500000000)

    (0 to 500000).foreach(_ => mutableState1mb.append("e"))
    (0 to 5000000).foreach(_ => mutableState10mb.append("e"))
    (0 to 50000000).foreach(_ => mutableState100mb.append("e"))

    val mb1: String   = mutableState1mb.toString()
    val mb10: String  = mutableState10mb.toString()
    val mb100: String = mutableState100mb.toString()
  }
}
