package benches

import java.util.concurrent.TimeUnit

import algos.digital.signature.DS
import benches.DSBenches.DSBenchesState
import org.openjdk.jmh.annotations.{Benchmark, Mode, Scope, State}
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.profile.GCProfiler
import org.openjdk.jmh.runner.{Runner, RunnerException}
import org.openjdk.jmh.runner.options.{OptionsBuilder, TimeValue, VerboseMode}

class DSBenches {
  @Benchmark
  def ds1mbE(state: DSBenchesState, bh: Blackhole): Unit =
    bh.consume(state.ds.cipher(state.mb1, state.keys._2))

  @Benchmark
  def ds10mbE(state: DSBenchesState, bh: Blackhole): Unit =
    bh.consume(state.ds.cipher(state.mb10, state.keys._2))

  @Benchmark
  def ds100mbE(state: DSBenchesState, bh: Blackhole): Unit =
    bh.consume(state.ds.cipher(state.mb100, state.keys._2))

  @Benchmark
  def ds1mbD(state: DSBenchesState, bh: Blackhole): Unit =
    bh.consume(state.ds.decipher(state.dsR1, state.keys._1))

  @Benchmark
  def ds10mbD(state: DSBenchesState, bh: Blackhole): Unit =
    bh.consume(state.ds.decipher(state.dsR10, state.keys._1))

  @Benchmark
  def ds100mbD(state: DSBenchesState, bh: Blackhole): Unit =
    bh.consume(state.ds.decipher(state.dsR100, state.keys._1))
}

object DSBenches {
  @throws[RunnerException]
  def main(args: Array[String]): Unit = {
    val opt = new OptionsBuilder()
      .include(".*" + classOf[DSBenches].getSimpleName + ".*")
      .forks(1)
      .threads(1)
      .warmupIterations(1)
      .measurementIterations(2)
      .mode(Mode.SingleShotTime)
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
  class DSBenchesState {
    val ds: DS = DS.apply
    val keys: (DS.PublicKey, DS.PrivateKey) = ds.formKeysPair(1024)

    val mutableState1mb   = new scala.collection.mutable.StringBuilder(500000)
    val mutableState10mb  = new scala.collection.mutable.StringBuilder(5000000)
    val mutableState100mb = new scala.collection.mutable.StringBuilder(50000000)

    (0 to 500000).foreach(_ => mutableState1mb.append("e"))
    (0 to 5000000).foreach(_ => mutableState10mb.append("c"))
    (0 to 50000000).foreach(_ => mutableState100mb.append("d"))

    val mb1: String   = mutableState1mb.toString()
    val mb10: String  = mutableState10mb.toString()
    val mb100: String = mutableState100mb.toString()

    val dsR1: DS.CipherResult = ds.cipher(mb1, keys._2)
    val dsR10: DS.CipherResult = ds.cipher(mb10, keys._2)
    val dsR100: DS.CipherResult = ds.cipher(mb100, keys._2)
  }
}