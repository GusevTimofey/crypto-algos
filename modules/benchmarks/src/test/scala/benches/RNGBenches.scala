package benches

import java.util.concurrent.TimeUnit

import algos.prng.RNG
import benches.RNGBenches.RNGBenchesState
import org.openjdk.jmh.annotations.{ Benchmark, Mode, Scope, State }
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.profile.GCProfiler
import org.openjdk.jmh.runner.options.{ OptionsBuilder, TimeValue, VerboseMode }
import org.openjdk.jmh.runner.{ Runner, RunnerException }

class RNGBenches {
  @Benchmark
  def rng(state: RNGBenchesState, bh: Blackhole): Unit =
    bh.consume(state.rng.next())
}

object RNGBenches {
  @throws[RunnerException]
  def main(args: Array[String]): Unit = {
    val opt = new OptionsBuilder()
      .include(".*" + classOf[RNGBenches].getSimpleName + ".*")
      .forks(1)
      .threads(1)
      .warmupIterations(1)
      .measurementIterations(2)
      .mode(Mode.AverageTime)
      .warmupTime(TimeValue.minutes(1))
      .measurementTime(TimeValue.minutes(1))
      .timeUnit(TimeUnit.SECONDS)
      .verbosity(VerboseMode.EXTRA)
      .shouldDoGC(true)
      .addProfiler(classOf[GCProfiler])
      .build
    new Runner(opt).run
  }

  @State(Scope.Benchmark)
  class RNGBenchesState {
    val rng: RNG = RNG(System.nanoTime() * 8682522807148012L * 181783497276652981L)
  }
}
